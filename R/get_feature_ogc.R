#' Download features from an OGC API Features service
#'
#' @description
#' `get_feature_ogc()` provides a modern alternative to `get_feature_wfs()`. It
#' retrieves vector data from an OGC API Features service.
#'
#' @param url A character string with the base URL of the OGC API (the landing
#'   page).
#' @param collection A character string with the ID of the collection (layer).
#' @param bbox A bounding box to filter features. Can be a numeric vector of
#'   length 4 (`c(xmin, ymin, xmax, ymax)`) or an object of class `bbox` (from
#'   `sf::st_bbox()`). Note: OGC APIs generally expect the bbox coordinates to
#'   be in WGS84 (EPSG:4326).
#' @param datetime A character string representing a time instant or time
#'   interval (e.g., `"2018-02-12T23:20:50Z"` or
#'   `"2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"`).
#' @param properties A character vector of specific column names to return.
#' @param cql_filter A character string containing a CQL2-text filter to apply
#'   attribute or complex spatial filtering on the server.
#' @param limit Numeric. Maximum number of features to retrieve. If `NULL`
#'   (default), all available features will be fetched via automatic API
#'   pagination.
#' @param crs Target Coordinate Reference System (CRS) as an EPSG code or CRS
#'   object. If provided, the output will be automatically transformed to this
#'   CRS.
#' @param quiet Logical. Should the download progress be suppressed? Defaults to
#'   `TRUE`.
#' @param ... Additional name-value pairs passed on to `httr2::req_url_query`.
#'
#' @return An `sf` (simple feature) object.
#' @export
#' @family topics on using web services
#'
#' @importFrom assertthat assert_that is.string is.number
#' @importFrom sf read_sf st_bbox st_transform st_crs st_as_sfc
#' @importFrom httr2
#' request
#' req_url_query
#' req_perform
#' resp_body_raw
#' resp_headers
#'
#' @examples
#' \dontrun{
#' # Base URL for the Digitaal Vlaanderen Wegenregister (Road Register)
#' api_url <- "https://geo.api.vlaanderen.be/Wegenregister/ogc/features/v1"
#'
#' # 1. Basic usage: Fetch 10 road segments
#' roads_sample <- get_feature_ogc(
#'   url = api_url,
#'   collection = "Wegsegment",
#'   limit = 10
#' )
#'
#' # 2. Spatial Filtering & CRS Transformation:
#' # Fetch features within a WGS84 bounding box and project to Belgian Lambert72
#' my_bbox <- sf::st_bbox(
#'   c(xmin = 4.39, ymin = 51.21, xmax = 4.40, ymax = 51.22),
#'   crs = sf::st_crs(4326)
#' )
#' roads_bbox <- get_feature_ogc(
#'   url = api_url,
#'   collection = "Wegsegment",
#'   bbox = my_bbox,
#'   crs = 31370
#' )
#'
#' # Base URL for Historical Land Use (Ferraris map)
#' hist_url <- "https://geo.api.vlaanderen.be/HistLandgebruik/ogc/features/v1"
#'
#' # 3. Attribute Selection:
#' # Fetch only specific columns
#' ferraris_lite <- get_feature_ogc(
#'   url = hist_url,
#'   collection = "Lgbrk1778",
#'   limit = 100,
#'   properties = c("KLASSE")
#' )
#'
#' # 4. Advanced CQL2 Attribute Filtering:
#' # Filter features directly on the server
#' roads_filtered <- get_feature_ogc(
#'   url = api_url,
#'   collection = "Wegsegment",
#'   limit = 50,
#'   cql_filter = "morfologischeWegklasse = 'dienstweg'"
#' )
#' }
get_feature_ogc <- function(
  url, collection, bbox = NULL, datetime = NULL,
  properties = NULL, cql_filter = NULL, limit = NULL,
  crs = NULL, quiet = TRUE, ...
) {
  assertthat::assert_that(
    assertthat::is.string(url),
    assertthat::is.string(collection),
    is.logical(quiet)
  )

  url <- sub("/+$", "", url)
  check_ogc_collection(url, collection)

  req <- build_ogc_request(
    url, collection, bbox, datetime, properties, cql_filter, limit, ...
  )

  if (!quiet) message("Connecting via optimized GeoPackage pagination...")

  next_url <- req$url
  results_list <- list()
  total_fetched <- 0L

  while (!is.null(next_url)) {
    if (!quiet) message("Fetching page: ", next_url)

    resp <- httr2::request(next_url) |> httr2::req_perform()
    page_data <- fetch_ogc_page(resp, quiet)

    if (nrow(page_data) == 0L) {
      if (length(results_list) == 0L) return(page_data)
      break
    }

    results_list <- c(results_list, list(page_data))
    total_fetched <- total_fetched + nrow(page_data)

    reached_limit <- !is.null(limit) && total_fetched >= limit
    if (reached_limit) break

    next_url <- extract_next_url(resp)
  }

  feature_data <- do.call(rbind, results_list)
  postprocess_features(feature_data, limit, properties, crs)
}


# -- Helpers -------------------------------------------------------------------

#' Build the initial `httr2` request with all OGC query parameters
#'
#' @inheritParams get_feature_ogc
#' @return An `httr2_request` object.
#' @noRd
build_ogc_request <- function(
  url, collection, bbox, datetime, properties, cql_filter, limit, ...
) {
  page_size <- 10000L
  if (!is.null(limit)) {
    assertthat::assert_that(assertthat::is.number(limit))
    page_size <- as.integer(min(limit, 10000L))
  }

  req <- httr2::request(sprintf("%s/collections/%s/items", url, collection)) |>
    httr2::req_url_query(
      f = "application/geopackage+sqlite3", limit = page_size
    )

  req <- apply_bbox_param(req, bbox)
  req <- apply_optional_params(req, datetime, properties, cql_filter)
  req |> httr2::req_url_query(...)
}


#' Attach a bbox query parameter, converting CRS to WGS84 when needed
#'
#' @param req An `httr2_request` object.
#' @param bbox A bbox object, a numeric vector of length 4, or `NULL`.
#' @return The modified `httr2_request` object.
#' @noRd
apply_bbox_param <- function(req, bbox) {
  if (is.null(bbox)) return(req)

  if (!inherits(bbox, "bbox")) {
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
    bbox <- sf::st_bbox(bbox)
  }

  needs_transform <- !is.na(sf::st_crs(bbox)) && sf::st_crs(bbox)$epsg != 4326
  if (needs_transform) {
    bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bbox), 4326))
  }

  req |> httr2::req_url_query(bbox = paste(as.numeric(bbox), collapse = ","))
}


#' Attach `datetime`, `properties`, and `cql_filter` query parameters
#'
#' @param req An `httr2_request` object.
#' @inheritParams get_feature_ogc
#' @return The modified `httr2_request` object.
#' @noRd
apply_optional_params <- function(req, datetime, properties, cql_filter) {
  if (!is.null(datetime)) {
    assertthat::assert_that(assertthat::is.string(datetime))
    req <- req |> httr2::req_url_query(datetime = datetime)
  }

  if (!is.null(properties)) {
    assertthat::assert_that(is.character(properties))
    req <- req |>
      httr2::req_url_query(properties = paste(properties, collapse = ","))
  }

  if (!is.null(cql_filter)) {
    assertthat::assert_that(assertthat::is.string(cql_filter))
    req <- req |> httr2::req_url_query(
      `filter-lang` = "cql2-text",
      filter        = cql_filter
    )
  }

  req
}


#' Perform one HTTP request and return its features as an sf object
#'
#' Writes the raw `GeoPackage` response to a temp file, reads it with sf, and
#' deletes the temp file immediately to keep disk usage low.
#'
#' @param resp An `httr2_response` object.
#' @param quiet Passed through to [sf::read_sf()].
#' @return An `sf` object (possibly with zero rows).
#' @noRd
fetch_ogc_page <- function(resp, quiet) {
  tmp_file <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp_file), add = TRUE)

  writeBin(httr2::resp_body_raw(resp), tmp_file)
  sf::read_sf(tmp_file, quiet = quiet)
}


#' Extract the URL of the next page from a Link response header
#'
#' Handles multiple `link` headers, encoded ampersands (`&amp;`), and the
#' standard `<url>; rel="next"` format.
#'
#' @param resp An `httr2_response` object.
#' @return A character string with the next URL, or `NULL` if there is none.
#' @noRd
extract_next_url <- function(resp) {
  headers   <- httr2::resp_headers(resp)
  all_links <- unlist(headers[names(headers) == "link"])

  if (length(all_links) == 0L) return(NULL)

  next_link_str <- all_links[grepl('rel="next"', all_links)]

  if (length(next_link_str) == 0L) return(NULL)

  next_url <- sub(".*<([^>]+)>.*", "\\1", next_link_str[[1L]])
  gsub("&amp;", "&", next_url)
}


#' Trim, subset columns, and re-project a collected sf object
#'
#' @param feature_data An `sf` object produced by `rbind-ing` all pages.
#' @inheritParams get_features_ogc
#' @return The post-processed `sf` object.
#' @noRd
postprocess_features <- function(feature_data, limit, properties, crs) {
  if (!is.null(limit) && nrow(feature_data) > limit) {
    feature_data <- feature_data[seq_len(limit), ]
  }

  if (!is.null(properties) && nrow(feature_data) > 0L) {
    geom_col     <- attr(feature_data, "sf_column")
    valid_props  <- intersect(properties, names(feature_data))
    feature_data <- feature_data[, c(valid_props, geom_col), drop = FALSE]
  }

  if (!is.null(crs) && nrow(feature_data) > 0L) {
    feature_data <- sf::st_transform(feature_data, crs)
  }

  feature_data
}


#' Check if a collection exists in an OGC API Features service
#'
#' @description
#' A lightweight helper function that queries the `/collections` endpoint of an
#' OGC API using JSON. It verifies if a specified collection exists and provides
#' a helpful error message with valid names if it does not.
#'
#' @param url A character string with the base URL of the OGC API.
#' @param collection A character string with the ID of the collection to check.
#'
#' @return `TRUE` invisibly if the collection exists, otherwise throws an error.
#' @export
#' @family topics on using web services
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom jsonlite read_json
#' @examples
#'  \dontrun{
#' api_url <- "https://geo.api.vlaanderen.be/Wegenregister/ogc/features/v1"
#' check <- check_ogc_collection(api_url, "Wegsegment")
#' check
#'
#' # An informative error is thrown when collection does not exist
#' check <- try(check_ogc_collection(api_url, "foutieve_laag"))
#' }
check_ogc_collection <- function(url, collection) {
  assertthat::assert_that(
    assertthat::is.string(url),
    assertthat::is.string(collection)
  )

  url              <- sub("/+$", "", url)
  collections_url  <- sprintf("%s/collections?f=json", url)

  api_meta <- tryCatch({
    jsonlite::read_json(collections_url)
  }, error = function(e) {
    stop(
      sprintf(
        paste0(
          "Could not connect to or parse the OGC API at '%s'.",
          "\nCheck the URL or your network connection."
        ),
        collections_url
      ),
      call. = FALSE
    )
  })

  available_collections <- sapply(api_meta$collections, function(x) x$id)

  if (!collection %in% available_collections) {
    stop(
      sprintf(
        paste0(
          "Collection '%s' not found in this OGC API.",
          "\nValid available collections are:\n  - %s"
        ),
        collection,
        paste(available_collections, collapse = "\n  - ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
