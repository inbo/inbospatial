#' Get a layer from a web feature service
#'
#' This function constructs a `URL` request from its arguments and either reads
#' in the resulting vector layer as a `sf` object or returns the number of
#' features that are requested.
#' The request is made up of key-value pairs and additional key-value pairs can
#' be passed to the function.
#' The full documentation for the `WFS` standard can be consulted from
#' \url{https://www.ogc.org/standards/wfs/}.
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
#' [standard OGC filter](https://www.ogc.org/standards/filter/)
#' specification
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
#' @details See
#' \url{https://tutorials.inbo.be/tutorials/spatial_wfs_services/}
#'  for more information.
#' @return An `sf` (simple feature) object.
#' @export
#' @family topics on using web services
#'
#' @importFrom httr2
#' request
#' req_url_query
#' req_perform
#' resp_status
#' resp_body_xml
#' resp_body_raw
#' resp_url
#' @importFrom sf read_sf
#' @importFrom xml2 as_list
#' @importFrom assertthat assert_that is.string
#'
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

  if (!exists("require_pkgs")) source("misc.R")
  require_pkgs(c("httr2", "sf", "xml2", "assertthat"))

  result_type <- match.arg(result_type)

  assertthat::assert_that(grepl("\\d\\.\\d\\.\\d", version))
  assertthat::assert_that(
    is.null(crs) || grepl("EPSG:\\d+", crs)
  )
  assertthat::assert_that(
    is.null(layername) || assertthat::is.string(layername)
  )
  assertthat::assert_that(
    is.null(filter) || assertthat::is.string(filter)
  )
  assertthat::assert_that(
    is.null(cql_filter) || assertthat::is.string(cql_filter)
  )
  assertthat::assert_that(
    is.null(property_name) || assertthat::is.string(property_name)
  )

  if (!is.null(bbox)) {
    assertthat::assert_that(length(bbox) == 4)
    assertthat::assert_that(
      all(names(bbox) %in% c("xmin", "xmax", "ymin", "ymax"))
    )
    bbox <- paste(
      bbox[["xmin"]],
      bbox[["ymin"]],
      bbox[["xmax"]],
      bbox[["ymax"]],
      sep = ","
    )
  }

  if (grepl(pattern = "^2", x = version)) {
    query <- list(
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
    query <- list(
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

  get_result <- httr2::request(wfs) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_perform()

  handle_result_types(
    get_result,
    result_type = result_type,
    property_name = property_name
  )
}


#' different ways of handling different query outcomes
#'
#' @keywords internal
#' @noRd
#'
handle_result_types <- function(result, result_type, property_name) {
  status <- httr2::resp_status(result)

  if (status != 200L) {
    parsed <- xml2::as_list(httr2::resp_body_xml(result))
    if (names(parsed) == "ExceptionReport") {
      message <- unlist(parsed$ExceptionReport$Exception$ExceptionText)
      old_op <- options(warning.length = max(nchar(message), 1000))
      on.exit(options(old_op))
      stop(sprintf(
        paste0(message, "\nThe requested url was: %s"),
        httr2::resp_url(result)
      ))
    }
    stop(sprintf("Exited with HTTP status code %s", status))
  }

  if (result_type == "hits") {
    parsed <- xml2::as_list(httr2::resp_body_xml(result))
    n_features <- attr(parsed$FeatureCollection, "numberMatched")
    return(n_features)
  }

  # Write the content to disk and read back in as sf
  destfile <- store_as_gml(result)

  sf_result <- sf::read_sf(destfile)
  # Sometimes CRS is missing
  if (is.na(sf::st_crs(sf_result))) {
    srs <- xml2::read_xml(destfile)
    srs <- xml2::xml_find_first(srs, ".//@srsName") |>
      xml2::xml_text()
    srs <- regmatches(
      srs,
      regexpr(pattern = "\\d+$", text = srs)
    )
    sf::st_crs(sf_result) <- as.integer(srs)
  }
  # avoid that non nillable fields are mandatory
  # and remove fields all NA
  if (!is.null(property_name)) {
    sf_result <- sf_result[, strsplit(property_name, split = ",")[[1]]]
  }
  return(sf_result)
}


#' store httr response to either a geographic xml file, or raw binary
#'
#' @keywords internal
#' @noRd
#'
store_as_gml <- function(result, destfile = tempfile(fileext = ".gml")) {
  content_type <- httr2::resp_content_type(result)

  if (grepl("xml", content_type)) {
    xml2::write_xml(httr2::resp_body_xml(result), destfile)
  } else {
    writeBin(httr2::resp_body_raw(result), destfile, useBytes = TRUE)
  }

  return(destfile)
}
