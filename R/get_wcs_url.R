#' Get WCS URL
#'
#' @description
#' Maps a short service name to its corresponding Web Coverage Service (WCS) URL
#'
#'
#' @inheritParams get_coverage_wcs
#'
#' @return A character string containing the full URL.
#'
#' @keywords internal
#' @noRd
get_wcs_url <- function(wcs) {
  switch(
    wcs,
    omz = "https://geo.api.vlaanderen.be/oi-omz/wcs",
    omw = "https://geo.api.vlaanderen.be/oi-omw/wcs",
    dtm = "https://geo.api.vlaanderen.be/el-dtm/wcs",
    dsm = "https://geo.api.vlaanderen.be/el-dsm/wcs",
    dhmv = "https://geo.api.vlaanderen.be/dhmv/wcs",
    mercatornet = paste0(
      "https://www.mercator.vlaanderen.be/",
      "raadpleegdienstenmercatorpubliek/wcs"
    ),
    stop("Unknown WCS service provided: ", wcs, call. = FALSE)
  )
}
