#' Return the scale factor for a specific parallel in a Lambert Conic Conformal
#' projection
#'
#' The scale factor is the distance distortion.
#' In this case (a conic conformal projection) it holds in any direction.
#'
#' The function applies to normal LCC projections, not the oblique ones.
#' The applied formulas are for the sphere, hence approximate.
#'
#' @return
#' Numeric vector, same length as \code{par_deg}.
#'
#' @family functions to explore properties of coordinate reference systems
#'
#' @references
#' Snyder J.P. (1987). Map Projections - A Working Manual.
#' U.S. Geological Survey Professional Paper, Nr. 1395. United States
#' Government Printing Office, Washington, 397 p.
#'
#' @param par_deg Numeric vector.
#' The latitude in decimal degrees of one or more parallels for which to do the
#' calculation.
#' @param par1_deg Latitude of the first standard parallel of the LCC projection
#' in decimal degrees.
#' @param par2_deg Latitude of the second standard parallel of the LCC
#' projection in decimal degrees.
#'
#' @examples
#' sf::st_crs(31370)
#' x <- seq(49.5, 52.5, 0.1)
#' y <- scalefactor_lcc(x, 51.1666672333333, 49.8333339)
#' data.frame(latitude = x, scalefactor = y)
#'
#' @author
#' Floris Vanderhaeghe, \url{https://github.com/florisvdh}
#'
#' @export
scalefactor_lcc <- function(par_deg, par1_deg, par2_deg) {
  par <- par_deg / 180 * pi
  par1 <- par1_deg / 180 * pi
  par2 <- par2_deg / 180 * pi
  cos(par1) * (tan(pi / 4 + par1 / 2))^coneconst_lcc(par1, par2) /
    (cos(par) * (tan(pi / 4 + par / 2))^coneconst_lcc(par1, par2))
}

#' Calculate the cone constant for a normal LCC projection
#'
#' @keywords internal
coneconst_lcc <- function(par1, par2) {
  log(cos(par1) / cos(par2)) / log(tan(pi / 4 + par2 / 2) /
    tan(pi / 4 + par1 / 2))
}






#' Return the scale factor for the angular distance from the central meridian
#' in an (unscaled) transverse cylindrical projection
#'
#'
#' The scale factor is the distance distortion in the direction of the
#' meridians.
#' In the case of the Transverse Mercator projection (a cylindrical conformal
#' projection) it holds in any direction.
#'
#' The formulas applied are for the sphere, hence approximate.
#'
#' The best known map projection system in this context is the Universal
#' Transverse Mercator (UTM), which superposes a central meridian scale factor
#' of 0.9996.
#'
#' @return
#' Numeric vector, same length as `ang_dist`.
#'
#' @family functions to explore properties of coordinate reference systems
#'
#' @references
#' Iliffe J. & Lott R. (2008). Datums and Map Projections. For Remote Sensing,
#' GIS and Surveying. 2nd edn. Whittles Publishing, Caithness, UK, 208 p.
#'
#' @param ang_dist Numeric vector.
#' One or more angular distances from the central meridian, in decimal degrees.
#'
#' @examples
#' x <- seq(0, 3, 0.1)
#' y <- scalefactor_tcyl(x)
#' data.frame(ang_dist = x, scalefactor = y)
#'
#' @author
#' Floris Vanderhaeghe, \url{https://github.com/florisvdh}
#'
#' @export
scalefactor_tcyl <- function(ang_dist) {
  ang_dist <- ang_dist / 180 * pi
  1 / cos(ang_dist)
}
