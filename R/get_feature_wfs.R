#' Get a layer from a web feature service
#'
#' This function constructs a `URL` request from its arguments and either reads
#' in the resulting vector layer as a `sf` object or returns the number of
#' features that are requested.
#' The request is made up of key-value pairs and additional key-value pairs can
#' be passed to the function.
#' The full documentation for the `WFS` standard can be consulted from
#' \url{https://www.ogc.org/standard/wfs/}.
#'
#' @param wfs Web address for the service which you want to query features from
#' @param version Version number for the service.
#' For instance `"2.0.0"`.
#' @param layername Optional name of a layer hosted by the web feature service
#' @param crs Optional coordinate reference system to represent the features.
#' For instance `"EPSG:31370"`.
#' @param bbox Optional bounding box.
#' Pass this as a named vector with names `"xmin"`, `"xmax"`, `"ymin"`,
#' `"ymax"`.
#' @param filter Optional
#' [standard OGC filter](https://www.ogc.org/standard/filter/) specification
#' @param cql_filter Optional
#' [Contextual Query Language](https://portal.ogc.org/files/96288) filter.
#' This currently only works if the `WFS` is hosted on a `GeoServer`.
#' @param output_format Optional output format supported by the `WFS`.
#' @param property_name Optional character string.
#' Which fields or columns to return?
#' If you want to specify multiple columns, separate them by a comma.
#' The column containing the feature geometry is usually called `geom`,
#' `geometry` or `SHAPE`.
#' @param result_type For version `"2.x.x"`, this can be either `"results"`
#' (default) or `"hits"`.
#' The former returns the requested features, the latter returns the number of
#' requested features.
#' @param ... Additional key-value pairs passed on to the WFS query.
#'
#' @importFrom httr parse_url build_url GET HEAD content
#' @importFrom sf read_sf
#' @importFrom xml2 as_list
#' @importFrom assertthat assert_that is.string
#'
#' @details See
#' \url{https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/}
#'  for more information.
#' @family topics on using web services
#'
#' @export
#' @examples
#' \dontrun{
#' vlaanderen <- get_feature_wfs(
#'   wfs = paste0(
#'     "https://eservices.minfin.fgov.be/",
#'     "arcgis/services/R2C/Regions/MapServer/WFSServer"
#'   ),
#'   layername = "regions",
#'   crs = "EPSG:31370",
#'   filter = paste0(
#'     "<Filter><PropertyIsEqualTo><PropertyName>",
#'     "regions:NameDUT</PropertyName><Literal>'Vlaams Gewest'",
#'     "</Literal></PropertyIsEqualTo></Filter>"
#'   )
#' )
#' }
get_feature_wfs <- function(
    wfs,
    version = "2.0.0",
    layername = NULL,
    crs = NULL,
    bbox = NULL,
    filter = NULL,
    cql_filter = NULL,
    output_format = NULL,
    property_name = NULL,
    result_type = c("results", "hits"),
    ...) {
  result_type <- match.arg(result_type)
  url <- parse_url(wfs)
  assert_that(grepl("\\d\\.\\d\\.\\d", version))
  assert_that(is.null(crs) || grepl("EPSG:\\d+", crs))
  assert_that(is.null(layername) || is.string(layername))
  assert_that(is.null(filter) || is.string(filter))
  assert_that(is.null(cql_filter) || is.string(cql_filter))
  assert_that(is.null(property_name) || is.string(property_name))

  if (!is.null(bbox)) {
    assert_that(length(bbox) == 4)
    assert_that(all(names(bbox) %in% c("xmin", "xmax", "ymin", "ymax")))
    bbox <- paste(
      bbox[["xmin"]],
      bbox[["ymin"]],
      bbox[["xmax"]],
      bbox[["ymax"]],
      sep = ","
    )
  }
  if (grepl(pattern = "^2", x = version)) {
    url$query <- list(
      service = "wfs",
      request = "GetFeature",
      version = version,
      typeNames = layername,
      srsName = crs,
      bbox = bbox,
      filter = filter,
      cql_filter = cql_filter,
      outputFormat = output_format,
      propertyName = property_name,
      resultType = result_type,
      ...
    )
  }
  if (grepl(pattern = "^1", x = version)) {
    url$query <- list(
      service = "wfs",
      request = "GetFeature",
      version = version,
      typeName = layername,
      srsName = crs,
      bbox = bbox,
      filter = filter,
      cql_filter = cql_filter,
      outputFormat = output_format,
      propertyName = property_name,
      resultType = NULL,
      ...
    )
  }

  request <- build_url(url)

  get_result <- GET(request)
  handle_result_types(
    get_result,
    result_type = result_type,
    property_name = property_name,
    request = request
  )
}

handle_result_types <- function(result, result_type, property_name, request) {
  if (result$status_code != 200L) {
    parsed <- as_list(content(result, "parsed", encoding = "UTF-8"))
    if (names(parsed) == "ExceptionReport") {
      message <- unlist(parsed$ExceptionReport$Exception$ExceptionText)
      old_op <- options(warning.length = max(nchar(message), 1000))
      on.exit(options(old_op))
      stop(sprintf(
        paste0(message, "\nThe requested url was: %s"),
        request
      ))
    }
    stop(sprintf("Exited with HTTP status code %s", result$status_code))
  }

  if (result_type == "hits") {
    parsed <- as_list(content(result, "parsed", encoding = "UTF-8"))
    n_features <- attr(parsed$FeatureCollection, "numberMatched")
    return(n_features)
  }

  content <- content(result, encoding = "UTF-8")
  # Write the content to disk
  destfile <- store_as_gml(content = content)

  # Read the temporary GML file back in
  result <- read_sf(destfile)
  # Sometimes CRS is missing
  if (is.na(sf::st_crs(result))) {
    srs <- xml2::read_xml(destfile)
    srs <- xml2::xml_find_first(srs, ".//@srsName") |>
      xml2::xml_text()
    srs <- regmatches(
      srs,
      regexpr(pattern = "\\d+$", text = srs)
    )
    sf::st_crs(result) <- as.integer(srs)
  }
  # avoid that non nillable fields are mandatory
  # and remove fields all NA
  if (!is.null(property_name)) {
    result <- result[, strsplit(property_name, split = ",")[[1]]]
  }
  return(result)
}

store_as_gml <- function(content, ...) {
  UseMethod("store_as_gml", content)
}

store_as_gml.raw <- function(
    content, destfile = tempfile(fileext = "gml"), ...) {
  writeBin(content, destfile, useBytes = TRUE)
  return(destfile)
}

#' @importFrom xml2 write_xml
store_as_gml.xml_document <- function(
    content, destfile = tempfile(fileext = "gml"), ...) {
  write_xml(content, destfile)
  return(destfile)
}
