#' Get a layer from a web coverage service within a bounding box
#'
#' The function sends a query to a WCS service, downloads it to a temporary file
#' from which it is read with `terra::rast()` - if needed reprojected -
#' and returned as a `SpatRaster` object
#'
#' @param wcs One of `"dtm"`, `"dsm"`, `"omz"`, `"omw"`, `"dhmv"`,
#'  `"mercatornet"`
#' @param bbox An object of class bbox of length 4.
#' @param layername Character string; name of the layer
#' @param resolution Output resolution in meters
#' @param wcs_crs Native CRS in which the raster layers are stored on the `WCS`
#' @param output_crs Output CRS.
#' May involve reprojection.
#' @param bbox_crs CRS in which bbox coordinates are passed
#' @param version `WCS` version to be used. Default is `1.0.0`.
#' @param ... Additional key-value pairs passed on to the `WCS` query
#'
#' @details The following WCS services can currently be used:
#'   - `"omz"`: orthophotomosaic summer images Flanders
#'   - `"omw"`: orthophotomosaic winter images Flanders
#'   - `"dtm"`: digital terrain model Flanders
#'   - `"dsm"`: digital surface model Flanders
#'   - `"dhmv"`: digital elevation model Flanders (contains dtm and dsm data)
#'   - `"mercatornet"`: Public Download Service Flemish Government -
#'     department environment - cooperation `MercatorNet`
#'
#' For more information, see metadata Vlaanderen:
#'
#'   <https://metadata.vlaanderen.be/srv/eng/catalog.search#/search?any=WCS>
#'
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom terra rast `res<-` project
#' @importFrom assertthat assert_that
#' @importFrom httr2
#' request
#' req_url_query
#' req_perform
#' resp_check_status
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
    wcs = c("dtm", "dsm", "omz", "omw", "dhmv", "mercatornet"),
    bbox,
    layername,
    resolution,
    wcs_crs = c("EPSG:4258", "EPSG:31370"),
    output_crs = "EPSG:31370",
    bbox_crs = "EPSG:31370",
    version = c("1.0.0", "2.0.1"),
    ...) {

  require_pkgs(c("sf", "terra", "httr2", "stringr"))

  # prelim check
  version <- match.arg(version)
  wcs <- tolower(wcs) # case insensitive wcs
  wcs <- match.arg(wcs)
  wcs_crs <- match.arg(wcs_crs)
  bbox_crs <- match.arg(bbox_crs)

  # warn for wcs specifics
  problems <- character()
  problems <- c(
    problems,
    sprintf(
      "WCS `%s` only supports CRS Belgian Lambert 72 (`EPSG:31370`).
      Consider specifying `wcs_csr=\"EPSG:31370\"`",
      wcs
    )[wcs %in% c("dhmv", "mercatornet") & wcs_crs != "EPSG:31370"]
  )
  problems <- c(
    problems,
    sprintf(
      "WCS `%s` doesn't yet work for version %s.
      Please switch to version 1.0.0.",
      wcs,
      version
    )[wcs %in% c("mercatornet") & version == "2.0.1"]
  )
  if (length(problems) > 0) {
    warning(paste(problems, collapse = "\n\n"), call. = FALSE)
  }

  # data type assertions
  assertthat::assert_that(is.character(layername))
  assertthat::assert_that(is.character(output_crs))
  assertthat::assert_that(inherits(bbox, "bbox"))

  # check if layername is available
  layernames <- get_wcs_layers(wcs = wcs, version = version)$layername
  assertthat::assert_that(
    layername %in% layernames,
    msg = sprintf(
      "%s is not in available layernames for this WCS: %s",
      layername,
      paste(layernames, collapse = ", ")
    )
  )

  # set url
  wcs_url <- get_wcs_url(wcs)

  # resolution <=0 will give a `404`
  assertthat::assert_that(is.numeric(resolution) && (resolution > 0))

  # assemble the bounding box
  matrix(bbox, ncol = 2, byrow = TRUE) |>
    as.data.frame() |>
    sf::st_as_sf(coords = c("V1", "V2"), crs = bbox_crs) |>
    sf::st_transform(crs = wcs_crs) |>
    sf::st_coordinates() |>
    as.vector() -> bbox
  names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

  # variant: version 2.0.1
  if (version == "2.0.1") {
    epsg_code <- stringr::str_extract(wcs_crs, "\\d+")
    mht_file <- tempfile(fileext = ".mht")

    httr2::request(wcs_url) |>
      httr2::req_url_query(
        SERVICE = "WCS",
        VERSION = version,
        REQUEST = "GetCoverage",
        COVERAGEID = layername,
        CRS = wcs_crs,
        SUBSET = paste0(
          "x,http://www.opengis.net/def/crs/EPSG/0/",
          epsg_code, "(",
          bbox[["xmin"]], ",", bbox[["xmax"]], ")"
        ),
        SUBSET = paste0(
          "y,http://www.opengis.net/def/crs/EPSG/0/",
          epsg_code, "(",
          bbox[["ymin"]], ",", bbox[["ymax"]], ")"
        ),
        SCALEFACTOR = resolution,
        FORMAT = "image/tiff",
        RESPONSE_CRS = wcs_crs,
        ...
      ) |>
      httr2::req_perform(path = mht_file) |>
      httr2::resp_check_status()

    # multipart file extract tif part
    tif_file <- unpack_mht(mht_file)
  } # /version 2.0.1

  # variant: version 1.0.0
  if (version == "1.0.0") {
    tif_file <- tempfile(fileext = ".tif")

    httr2::request(wcs_url) |>
      httr2::req_url_query(
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
        FORMAT = ifelse(wcs == "mercatornet", "image/tiff", "geoTIFF"),
        RESPONSE_CRS = wcs_crs,
        ...
      ) |>
      httr2::req_perform(path = tif_file) |>
      httr2::resp_check_status()
  } # /version 1.0.0

  # assemble the spatial raster
  raster <- terra::rast(tif_file)
  template <- terra::project(raster, output_crs)
  terra::res(template) <- resolution
  raster <- terra::project(raster, template)

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
#' @importFrom utils tail
#'
#' @return tif_path the path to the extracted geoTIFF.
#'
#' @keywords internal
#' @noRd
unpack_mht <- function(path) {

  require_pkgs(c("readr", "utils", "stringr"))

  raw_vector <- readr::read_file_raw(path)

  # 1. Match start of tiff part ^(II|MM)\*
  # Can be little or big endian
  # Look for \nII* (0x0a + 49 49 2a) or \nMM* (0x0a + 4d 4d 2a)
  match_ii <- grepRaw(as.raw(c(0x0a, 0x49, 0x49, 0x2a)), raw_vector)[1]
  match_mm <- grepRaw(as.raw(c(0x0a, 0x4d, 0x4d, 0x2a)), raw_vector)[1]

  valid_matches <- c(match_ii, match_mm)
  valid_matches <- valid_matches[!is.na(valid_matches)]

  if (length(valid_matches) > 0) {
    # +1 to step over the newline character and start exactly at 'I' or 'M'
    pos_start <- min(valid_matches) + 1
  } else {
    # Edge case: If it's on the very first line of the file (no preceding \n)
    if (
      all(raw_vector[1:3] == as.raw(c(0x49, 0x49, 0x2a))) ||
        all(raw_vector[1:3] == as.raw(c(0x4d, 0x4d, 0x2a)))
    ) {
      pos_start <- 1
    } else {
      stop("Could not find TIFF header (II* or MM*) at the start of any line.")
    }
  }

  # 2. Drop the last line
  # MHT files end with a boundary string that usually starts with two hyphens
  # We search for \n-- to safely find the closing boundary and cut the file.
  boundary_matches <- grepRaw(
    as.raw(c(0x0a, 0x2d, 0x2d)), raw_vector, all = TRUE
  )

  if (length(boundary_matches) > 0) {
    pos_end <- tail(boundary_matches, 1) - 1
    # Check for Windows \r\n and step back one more byte if needed
    if (raw_vector[pos_end] == as.raw(0x0d)) pos_end <- pos_end - 1
  } else {
    # Fallback if no boundary exists: chop at the last newline
    newlines <- grepRaw(as.raw(0x0a), raw_vector, all = TRUE)
    pos_end <- tail(newlines, 1) - 1
    if (raw_vector[pos_end] == as.raw(0x0d)) pos_end <- pos_end - 1
  }

  # 3. Extract and Write
  tif <- raw_vector[pos_start:pos_end]
  tif_path <- stringr::str_replace(path, "\\.mht$", ".tif")

  readr::write_file(tif, tif_path)

  return(tif_path)
}
