#' Get available layers from a web coverage service
#'
#' The function sends a `GetCapabilities` query to a WCS service,
#' parses the XML response, and returns a `data.frame` containing
#' the names of the available layers and their descriptions.
#'
#' @inheritParams get_coverage_wcs
#'
#' @details The following WCS services can currently be used:
#'   - `"omz"`: orthophotomosaic summer images Flanders
#'   - `"omw"`: orthophotomosaic winter images Flanders
#'   - `"dtm"`: digital terrain model Flanders
#'   - `"dsm"`: digital surface model Flanders
#'   - `"dhmv"`: digital elevation model Flanders (contains dtm and dsm data)
#'   - `"mercatornet"`: Public Download Service Flemish Government -
#'     department environment - cooperation `MercatorNet`
#' For more information, see metadata Vlaanderen:
#'   https://metadata.vlaanderen.be/srv/eng/catalog.search#/search?any=WCS
#'
#' @importFrom assertthat assert_that
#' @importFrom httr2
#' request
#' req_url_query
#' req_perform
#' resp_check_status
#' resp_body_string
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text
#' @importFrom stringr str_extract str_to_lower
#'
#' @export
#' @family topics on using web services
#' @return A `data.frame` object with columns `layername` and `description`.
#' @examples
#' \dontrun{
#' get_wcs_layers(wcs = "dsm")
#' get_wcs_layers(wcs = "omz", version = "2.0.1")
#' }
#'
get_wcs_layers <- function(
    wcs = c("dtm", "dsm", "omz", "omw", "dhmv", "mercatornet"),
    version = c("1.0.0", "2.0.1"),
    ...) {

  # prelim check
  version <- match.arg(version)
  wcs <- tolower(wcs) # case insensitive wcs
  wcs <- match.arg(wcs)

  # set url
  wcs_url <- get_wcs_url(wcs)

  # build and run the http request
  http_response <- httr2::request(wcs_url) |>
    httr2::req_url_query(
      SERVICE = "WCS",
      VERSION = version,
      REQUEST = "GetCapabilities",
      ...
    ) |>
    httr2::req_perform()

  # raise http errors
  httr2::resp_check_status(http_response)

  # parse the xml response
  xml_data <- xml2::read_xml(httr2::resp_body_string(http_response))

  # Helper function to extract text bypassing namespaces
  get_tag_text <- function(node, tag_name) {
    # Searches for any descendant node with the matching local name
    xpath <- sprintf(".//*[local-name()='%s']", tag_name)
    val <- xml2::xml_text(xml2::xml_find_first(node, xpath))
    ifelse(is.na(val), "", val)
  }

  # extract information based on WCS version
  if (version == "1.0.0") {

    # Find all CoverageOfferingBrief nodes, regardless of namespace
    nodes <- xml2::xml_find_all(
      xml_data,
      "//*[local-name()='CoverageOfferingBrief']"
    )
    layernames <- sapply(nodes, get_tag_text, "name")
    descriptions <- sapply(nodes, get_tag_text, "label")

  } else { # variant: version 2.0.1

    # Find all CoverageSummary nodes, regardless of namespace
    nodes <- xml2::xml_find_all(
      xml_data,
      "//*[local-name()='CoverageSummary']"
    )
    layernames <- sapply(nodes, get_tag_text, "CoverageId")
    descriptions <- sapply(nodes, get_tag_text, "Abstract")

    # if no Abstract, use Title
    if (descriptions[1] == "") {
      descriptions <- sapply(nodes, get_tag_text, "Title")
    }
    # Clean up empty/hierarchical nodes that don't have a direct CoverageId
    valid_layers <- layernames != ""
    layernames <- layernames[valid_layers]
    descriptions <- descriptions[valid_layers]

  }

  # assemble and return the data.frame
  data.frame(
    layername = layernames,
    description = descriptions,
    stringsAsFactors = FALSE
  )
}
