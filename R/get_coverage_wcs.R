#' Get a layer from a web coverage service within a bounding box
#'
#' The function sends a query to a WCS service, downloads it to a temporary file
#' from which it is read with `terra::rast()` - if needed reprojected -
#' and returned as a `SpatRaster`object
#'
#' @param wcs One of `"dtm"`, `"dsm"`, `"omz"`, `"omw"`, `"dhmv"`
#' @param bbox An object of class bbox of length 4.
#' @param layername Character string; name of the layer
#' @param resolution Output resolution in meters
#' @param wcs_crs Native CRS in which the raster layers are stored on the `WCS`
#' @param output_crs Output CRS.
#' May involve reprojection.
#' @param bbox_crs CRS in which bbox coordinates are passed
#' @param version `WCS` version to be used.
#' @param ... Additional key-value pairs passed on to the `WCS` query
#'
#' @details The following WCS services can currently be used:
#'   - `"omz"`: orthophotomosaic summer images Flanders
#'   - `"omw"`: orthophotomosaic winter images Flanders
#'   - `"dtm"`: digital terrain model Flanders
#'   - `"dsm"`: digital surface model Flanders
#'   - `"dhmv"`: digital elevation model Flanders (contains dtm and dsm data)
#' See [metadata Vlaanderen](https://metadata.vlaanderen.be/srv/eng/catalog.search#/search?any=WCS) for more information # nolint: line_length_linter.
#'
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom terra rast `res<-` project
#' @importFrom assertthat assert_that
#' @importFrom httr parse_url build_url GET write_disk stop_for_status
#' @importFrom stringr str_extract str_replace
#'
#' @export
#' @family topics on using web services
#' @return A `SpatRaster` object
#' @examples
#' \dontrun{
#' bbox <- sf::st_bbox(
#'   c(xmin = 155800, xmax = 155850, ymin = 132700, ymax = 132750),
#'   crs = sf::st_crs(31370)
#' )
#' get_coverage_wcs(
#'   wcs = "dsm",
#'   bbox = bbox,
#'   layername = "EL.GridCoverage.DSM",
#'   resolution = 1
#' )
#' }
#'
get_coverage_wcs <- function(
    wcs = c("dtm", "dsm", "omz", "omw", "dhmv"),
    bbox,
    layername,
    resolution,
    wcs_crs = c("EPSG:4258", "EPSG:31370"),
    output_crs = "EPSG:31370",
    bbox_crs = "EPSG:31370",
    version = c("1.0.0", "2.0.1"),
    ...) {

  # prelim check
  version <- match.arg(version)
  wcs <- tolower(wcs) # case insensitive wcs
  wcs <- match.arg(wcs)
  wcs_crs <- match.arg(wcs_crs)
  bbox_crs <- match.arg(bbox_crs)


  # constrain version | wcs
  if (wcs == "dhmv") {
    # warn incompatible versions
    if (!(version %in% c("1.0.0", "2.0.1"))) {
      message("WCS `DHMV` is only compatible with versions `1.0.0` or `2.0.1`.
        Consider using `version=\"2.0.1\"`")
    }
    # recommend crs specification
    if (wcs_crs != "EPSG:31370") {
      message("WCS `DHMV` only supports CRS Belgian Lambert 72 (`EPSG:31370`).
        Consider specifying `wcs_csr=\"EPSG:31370\"`")
    }
  }

  # set url
  wcs <- switch(wcs,
    omz = "https://geo.api.vlaanderen.be/oi-omz/wcs",
    omw = "https://geo.api.vlaanderen.be/oi-omw/wcs",
    dtm = "https://geo.api.vlaanderen.be/el-dtm/wcs",
    dsm = "https://geo.api.vlaanderen.be/el-dsm/wcs",
    dhmv = "https://geo.api.vlaanderen.be/DHMV/wcs"
  )

  # data type assertions
  assert_that(is.character(layername))
  assert_that(is.character(output_crs))
  assert_that(inherits(bbox, "bbox"))

  # resolution <=0 will give a `404`
  assert_that(is.numeric(resolution) && resolution > 0)

  # assemble the bounding box
  matrix(bbox, ncol = 2, byrow = TRUE) |>
    as.data.frame() |>
    st_as_sf(coords = c("V1", "V2"), crs = bbox_crs) |>
    st_transform(crs = wcs_crs) |>
    st_coordinates() |>
    as.vector() -> bbox
  names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

  # prepare url request
  url <- parse_url(wcs)

  # variant: version 2.0.1
  if (version == "2.0.1") {
    epsg_code <- str_extract(wcs_crs, "\\d+")
    url$query <- list(
      SERVICE = "WCS",
      VERSION = version,
      REQUEST = "GetCoverage",
      COVERAGEID = layername,
      CRS = wcs_crs,
      SUBSET = paste0(
        "x,http://www.opengis.net/def/crs/EPSG/0/",
        epsg_code, "(",
        bbox[["xmin"]],
        ",",
        bbox[["xmax"]], ")"
      ),
      SUBSET = paste0(
        "y,http://www.opengis.net/def/crs/EPSG/0/",
        epsg_code,
        "(",
        bbox[["ymin"]],
        ",",
        bbox[["ymax"]], ")"
      ),
      SCALEFACTOR = resolution,
      FORMAT = "image/tiff",
      RESPONSE_CRS = wcs_crs,
      ...
    )

    # build and run the http request
    request <- build_url(url)
    mht_file <- tempfile(fileext = ".mht")
    http_response <- GET(
      url = request,
      write_disk(mht_file)
    )

    # multipart file extract tif part
    tif_file <- unpack_mht(mht_file)
  } # /version 2.0.1


  # variant: version 1.0.0
  if (version == "1.0.0") {
    url$query <- list(
      SERVICE = "WCS",
      VERSION = version,
      REQUEST = "GetCoverage",
      COVERAGE = layername,
      CRS = wcs_crs,
      BBOX = paste(
        bbox[["xmin"]],
        bbox[["ymin"]],
        bbox[["xmax"]],
        bbox[["ymax"]],
        sep = ","
      ),
      RESX = resolution,
      RESY = resolution,
      FORMAT = "geoTIFF",
      RESPONSE_CRS = wcs_crs,
      ...
    )

    # build and run the http request
    request <- build_url(url)
    tif_file <- tempfile(fileext = ".tif")
    http_response <- GET(
      url = request,
      write_disk(tif_file)
    )
  }

  # raise http errors
  stop_for_status(http_response)

  # assemble the spatial raster
  raster <- rast(tif_file)
  template <- project(raster, output_crs)
  res(template) <- resolution
  raster <- project(raster, template)

  return(raster)
}


#' Unpack or extract the `tif` file part from an `mht` file.
#'
#' This helper function is needed on some `WCS` services from which an `mht`
#' file is downloaded rather than a `tif` file; returns the path to the `tif`
#'
#' @param path A path to the `mht` file
#'
#' @importFrom readr read_lines_raw read_lines read_file_raw write_file
#' @importFrom assertthat assert_that
#' @importFrom stringr str_detect str_replace
#'
#' @return tif_path the path to the extracted geoTIFF.
#'
#' @keywords internal
#'
#' @details Need three ways to read in the `mht` file to get the `tif` file out.
#' `read_lines()` cannot read all lines due to embedded `nulls`.
#' Therefore, also `read_lines_raw()` needed for positioning of `tif` part in
#' file.
#' `write_lines()` does not work correctly on `lines_raw[start:end]`
#' possibly a bug or edge case in `write_lines()`
#' Therefore, also `read_file_raw()` needed to extract from the raw vector
unpack_mht <- function(path) {
  lines_raw <- read_lines_raw(path)
  lines_char <- suppressWarnings(read_lines(path))
  raw_vector <- read_file_raw(path)

  assert_that(any(str_detect(lines_char, "image/tiff")))
  start <- which(str_detect(lines_char, "^II\\*"))
  end <- length(lines_raw) - 1
  pos_start <- length(unlist(lines_raw[1:(start - 1)])) + start
  pos_end <- length(raw_vector) - (length(lines_raw[end + 1]) + 1)

  tif <- raw_vector[pos_start:pos_end]
  tif_path <- str_replace(path, "mht", "tif")
  write_file(
    tif,
    tif_path
  )
  return(tif_path)
}
