# nolint start: object_usage_linter.

attrib_digvl <- paste0(
  "&copy; <a href=https://www.vlaanderen.be/",
  "digitaal-vlaanderen/onze-oplossingen/geografische-webdiensten/",
  "ons-gis-aanbod>Digitaal Vlaanderen</a>"
)
attrib_ngi <- paste0(
  "&copy; <a href=https://www.ngi.be/website/aanbod>",
  "Nationaal Geografisch Instituut</a>"
)
attrib_gdi <- paste0(
  "&copy; <a href=https://www.vlaanderen.be/geopunt/vlaams-geoportaal/",
  "gdi-vlaanderen>GDI-Vlaanderen</a>"
)
attrib_nb <-
  "&copy; <a href=https://georegister.brabant.nl>Provincie Noord-Brabant</a>"
attrib_ngeo <-
  "&copy; <a href=https://www.nationaalgeoregister.nl>Nationaal Georegister</a>"


#' addTiles() wrapper functions for WMTS services
#'
#' The `add_wmts_*()` functions are shorthand alternatives for a fully specified
#' [leaflet::addTiles()] statement.
#' Appropriate attribution is added to the Leaflet map depending on the layer.
#'
#' On condition that these functions continue being maintained, using the
#' shorthand functions should make your scripts more futureproof.
#'
#' @returns A `leaflet` HTML widget object.
#'
#' @section Abbreviations:
#' ## Prefixes
#' \describe{
#' \item{\code{be_}}{Belgium}
#' \item{\code{nl_}}{Netherlands}
#' }
#' ## Suffixes
#' \describe{
#' \item{\code{_ortho}}{Orthophotographs}
#' \item{\code{_brt}}{Topographic BRT layers (Netherlands; 'Basisregistratie
#' Topografie')}
#' }
#'
#' @inheritParams leaflet::addTiles
#' @inheritParams add_wms_be_ortho
#' @param ... Further arguments passed to [leaflet::addTiles()].
#'
#' @family web_services
#'
#' @examples
#' library(leaflet)
#' leaflet() |>
#'   setView(lng = 5.5, lat = 52.5, zoom = 9) |>
#'   add_wmts_nl_brt()
#' leaflet() |>
#'   setView(lng = 5.4, lat = 52.2, zoom = 14) |>
#'   add_wmts_nl_ortho()
#'
#' @author
#' Floris Vanderhaeghe, \url{https://github.com/florisvdh}
#'
#' @rdname add_wmts
#' @export
add_wmts_nl_brt <- function(map,
                            layer = c("standaard", "grijs", "pastel", "water"),
                            ...) {
  require_pkgs("leaflet")
  layer <- match.arg(layer)
  leaflet::addTiles(
    map = map,
    urlTemplate = paste0(
      "https://service.pdok.nl/brt/achtergrondkaart/wmts/v2_0/",
      layer,
      "/EPSG:3857/{z}/{x}/{y}.png"
    ),
    attribution = attrib_ngeo,
    ...
  )
}


#' @rdname add_wmts
#' @export
add_wmts_nl_ortho <- function(map,
                              ...) {
  require_pkgs("leaflet")
  leaflet::addTiles(
    map = map,
    urlTemplate = paste0(
      "https://service.pdok.nl/hwh/luchtfotorgb/wmts/v1_0/Actueel_orthoHR/",
      "EPSG:3857/{z}/{x}/{y}.jpeg"
    ),
    attribution = attrib_ngeo,
    ...
  )
}




#' addWMSTiles() and addWMS() wrapper functions for WMS services
#'
#' The `add_wms_*()` functions are shorthand alternatives for a fully specified
#' [leaflet::addWMSTiles()] or [leaflet.extras2::addWMS()] statement.
#' Appropriate attribution is added to the Leaflet map depending on the layer.
#'
#' On condition that these functions continue being maintained, using the
#' shorthand functions should make your scripts more futureproof.
#'
#' @returns A `leaflet` HTML widget object.
#'
#' @section Abbreviations:
#' ## Prefixes
#' \describe{
#' \item{\code{be_}}{Belgium}
#' \item{\code{fl_}}{Flanders}
#' \item{\code{nl_}}{Netherlands}
#' \item{\code{nlnb_}}{Netherlands: Province 'Noord-Brabant'}
#' }
#' ## Suffixes
#' ### Background layers
#' \describe{
#' \item{\code{_ortho}}{Orthophotographs}
#' \item{\code{_cartoweb}}{Topographic Cartoweb layers (Belgium)}
#' \item{\code{_grbmap}}{GRB basemap (Flanders; 'Grootschalig
#' Referentiebestand')}
#' }
#' ### Feature layers
#' \describe{
#' \item{\code{_habitatmap}}{Natura 2000 habitat types}
#' \item{\code{_habitatlabels}}{Natura 2000 habitat type labels}
#' \item{\code{_forestnature}}{Public forest and nature areas managed by the
#' Flemish Nature & Forest Agency}
#' \item{\code{_agriculture}}{Parcels with the agricultural use in a specific
#' year}
#' \item{\code{_nnb}}{Netherlands; 'Natuurnetwerk Brabant
#' (Rijk en Provincie NNB)'}
#' \item{\code{_natte_natuurparels}}{Netherlands; 'Natte natuurparels'}
#' \item{\code{_ambitie_landschapstypen}}{Netherlands;
#' 'Natuurbeheerplan - Ambitiekaart Landschaptypen'}
#' \item{\code{_ambitie_natuurtypen}}{Netherlands;
#' 'Natuurbeheerplan - Ambitiekaart Natuurtypen'}
#' \item{\code{_landschapstypen}}{Netherlands;
#' 'Natuurbeheerplan - Beheertypenkaart_Landschaptypen'}
#' \item{\code{_natuurtypen}}{Netherlands;
#' 'Natuurbeheerplan - Beheertypenkaart Natuurtypen'}
#' }
#'
#' @inheritParams leaflet::addWMSTiles
#' @inheritParams leaflet.extras2::addWMS
#' @param layer String that defines which layer to use, if the function supports
#' more than one.
#' In such case, the available strings are shown in the Usage section.
#' The first value is used if missing.
#' @param year Year to be applied in selecting the WMS
#' @param add_wms_legend Logical.
#' Is a legend to be added for this WMS?
#' Note that the legend cannot be toggled on and off in the map;
#' it is plotted as a separate, static legend.
#' @param ... Further arguments passed to [leaflet::addWMSTiles()] or
#' [leaflet.extras2::addWMS()].
#'
#' @family web_services
#'
#' @examples
#' library(leaflet)
#' leaflet() |>
#'   setView(lng = 4.5, lat = 51.45, zoom = 11) |>
#'   add_wmts_nl_brt("grijs") |>
#'   add_wms_be_cartoweb("topo_grey") |>
#'   add_wms_fl_forestnature() |>
#'   add_wms_nlnb_nnb()
#'
#' # It can also use mapview objects, if you first extract the leaflet map slot:
#' library(mapview)
#' kmi_stations <- sf::read_sf(
#'   system.file("extdata/kmi_stations.geojson", package = "inbospatial")
#' )
#' mapview(kmi_stations, map.types = "CartoDB.Positron")@map |>
#'   add_wms_be_cartoweb()
#' # alternative syntax:
#' \dontrun{
#' mapview(kmi_stations, map.types = "CartoDB.Positron") |>
#'   _@map |>
#'   add_wms_be_cartoweb()
#' }
#'
#' @author
#' Floris Vanderhaeghe, \url{https://github.com/florisvdh}
#'
#' @rdname add_wms
#' @importFrom magrittr %>%
#' @importFrom assertthat is.number
#' @export
add_wms_be_cartoweb <- function(
    map,
    layer = c("topo", "topo_grey", "overlay", "crossborder"),
    options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE),
    ...) {
  require_pkgs("leaflet")
  layer <- match.arg(layer)
  leaflet::addWMSTiles(
    map = map,
    baseUrl = "https://cartoweb.wms.ngi.be/service",
    layers = layer,
    options = options,
    attribution = attrib_ngi,
    ...
  )
}



#' @rdname add_wms
#' @export
add_wms_be_ortho <- function(
    map,
    options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE),
    ...) {
  require_pkgs("leaflet")
  leaflet::addWMSTiles(
    map = map,
    baseUrl = "https://wms.ngi.be/inspire/ortho/service",
    layers = "orthoimage_coverage",
    options = options,
    attribution = attrib_ngi,
    ...
  )
}

#' @rdname add_wms
#' @export
add_wms_nl_ortho <- function(
    map,
    options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE),
    ...) {
  require_pkgs("leaflet")
  leaflet::addWMSTiles(
    map = map,
    baseUrl = "https://service.pdok.nl/hwh/luchtfotorgb/wms/v1_0",
    layers = "Actueel_orthoHR",
    options = options,
    attribution = attrib_ngeo,
    ...
  )
}


#' @rdname add_wms
#' @export
add_wms_fl_grbmap <- function(
    map,
    options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE),
    ...) {
  require_pkgs("leaflet")
  leaflet::addWMSTiles(
    map = map,
    baseUrl = "https://geo.api.vlaanderen.be/GRB-basiskaart/wms",
    layers = "GRB_BSK",
    options = options,
    attribution = attrib_digvl,
    ...
  )
}

#' @rdname add_wms
#' @export
add_wms_fl_habitatmap <- function(
    map,
    options = leaflet::WMSTileOptions(
      format = "image/png",
      transparent = TRUE,
      info_format = "text/html"
    ),
    popupOptions = leaflet::popupOptions(maxWidth = 1200), # nolint: object_name_linter, line_length_linter.
    add_wms_legend = FALSE,
    ...) {
  require_pkgs(c("leaflet.extras", "leaflet.extras2"))
  leaflet.extras2::addWMS(
    map = map,
    baseUrl = "https://geo.api.vlaanderen.be/INBO/wms",
    layers = "BWK2Hab",
    options = options,
    popupOptions = popupOptions,
    attribution = attrib_digvl,
    ...
  ) %>%
    {
      if (!add_wms_legend) {
        .
      } else {
        leaflet.extras::addWMSLegend(
          .,
          paste0(
            "https://geo.api.vlaanderen.be/INBO/wms?",
            "request=GetLegendGraphic%26version=1.3.0%26",
            "format=image/png%26layer=BWK2Hab"
          )
        )
      }
    }
}


#' @rdname add_wms
#' @export
add_wms_fl_habitatlabels <- function(
    map,
    options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE),
    ...) {
  require_pkgs("leaflet")
  leaflet::addWMSTiles(
    map = map,
    baseUrl = "https://geo.api.vlaanderen.be/INBO/wms",
    layers = "BWK2HabLabel",
    options = options,
    attribution = attrib_digvl,
    ...
  )
}




#' @rdname add_wms
#' @export
add_wms_fl_forestnature <- function(
    map,
    options = leaflet::WMSTileOptions(
      format = "image/png",
      transparent = TRUE,
      info_format = "text/html"
    ),
    popupOptions = leaflet::popupOptions(maxWidth = 600), # nolint: object_name_linter, line_length_linter.
    add_wms_legend = FALSE,
    ...) {
  require_pkgs(c("leaflet.extras", "leaflet.extras2"))
  leaflet.extras2::addWMS(
    map = map,
    baseUrl =
      "https://www.mercator.vlaanderen.be/raadpleegdienstenmercatorpubliek/wms",
    layers = "am:am_patdat",
    options = options,
    popupOptions = popupOptions,
    attribution = attrib_digvl,
    ...
  ) %>%
    {
      if (!add_wms_legend) {
        .
      } else {
        leaflet.extras::addWMSLegend(
          .,
          paste0(
            "https://www.mercator.vlaanderen.be/raadpleegdiensten",
            "mercatorpubliek/ows?service=WMS&version=1.3.0&",
            "request=GetLegendGraphic&format=image%2Fpng&width=20&",
            "height=20&layer=am%3Aam_patdat"
          )
        )
      }
    }
}




#' @rdname add_wms
#' @export
add_wms_fl_agriculture <- function(
    map,
    year,
    options = leaflet::WMSTileOptions(
      format = "image/png",
      transparent = TRUE,
      info_format = "text/html"
    ),
    popupOptions = leaflet::popupOptions(maxWidth = 1200), # nolint: object_name_linter, line_length_linter.
    add_wms_legend = FALSE,
    ...) {
  require_pkgs(c("leaflet.extras", "leaflet.extras2"))
  assert_that(is.number(year))
  assert_that(year >= 2008, year < 2100)
  leaflet.extras2::addWMS(
    map = map,
    baseUrl = "https://geo.api.vlaanderen.be/ALV/wms",
    layers = paste0("LbGebrPerc", year),
    options = options,
    popupOptions = popupOptions,
    attribution = attrib_digvl,
    ...
  ) %>%
    {
      if (!add_wms_legend) {
        .
      } else {
        leaflet.extras::addWMSLegend(
          ., paste0(
            "https://geo.api.vlaanderen.be/ALV/wms?",
            "request=GetLegendGraphic%26version=1.3.0%26",
            "format=image/png%26layer=LbGebrPerc",
            year
          )
        )
      }
    }
}






#' @rdname add_wms
#' @export
add_wms_nlnb_nnb <- function(
    map,
    options = leaflet::WMSTileOptions(
      format = "image/png",
      transparent = TRUE,
      info_format = "text/html"
    ),
    popupOptions = leaflet::popupOptions(maxWidth = 600), # nolint: object_name_linter, line_length_linter.
    add_wms_legend = FALSE,
    ...) {
  require_pkgs(c("leaflet.extras", "leaflet.extras2"))
  leaflet.extras2::addWMS(
    map = map,
    baseUrl = paste0(
      "https://atlas.brabant.nl/arcgis/services/natuurbeheerplan_vastgesteld/",
      "MapServer/WMSServer"
    ),
    layers = "8",
    options = options,
    popupOptions = popupOptions,
    attribution = attrib_nb,
    ...
  ) %>%
    {
      if (!add_wms_legend) {
        .
      } else {
        leaflet.extras::addWMSLegend(
          .,
          paste0(
            "https://atlas.brabant.nl/arcgis/services/",
            "natuurbeheerplan_vastgesteld/MapServer/WmsServer?",
            "request=GetLegendGraphic&version=1.3.0&format=image/png&layer=8&"
          )
        )
      }
    }
}


#' @rdname add_wms
#' @export
add_wms_nlnb_natte_natuurparels <- function( # styler: off # nolint: object_length_linter, line_length_linter.
    map,
    options = leaflet::WMSTileOptions(
      format = "image/png",
      transparent = TRUE,
      info_format = "text/html"
    ),
    popupOptions = leaflet::popupOptions(maxWidth = 2000), # nolint: object_name_linter, line_length_linter.
    add_wms_legend = FALSE,
    ...) {
  require_pkgs(c("leaflet.extras", "leaflet.extras2"))
  leaflet.extras2::addWMS(
    map = map,
    baseUrl = paste0(
      "https://atlas.brabant.nl/arcgis/services/natuurbeheerplan_vastgesteld/",
      "MapServer/WMSServer"
    ),
    layers = "4",
    options = options,
    popupOptions = popupOptions,
    attribution = attrib_nb,
    ...
  ) %>%
    {
      if (!add_wms_legend) {
        .
      } else {
        leaflet.extras::addWMSLegend(
          .,
          paste0(
            "https://atlas.brabant.nl/arcgis/services/",
            "natuurbeheerplan_vastgesteld/MapServer/WmsServer?",
            "request=GetLegendGraphic&version=1.3.0&format=image/png&layer=4&"
          )
        )
      }
    }
}


#' @rdname add_wms
#' @export
add_wms_nlnb_ambitie_landschapstypen <- function( # styler: off # nolint: object_length_linter, line_length_linter.
    map,
    options = leaflet::WMSTileOptions(
      format = "image/png",
      transparent = TRUE,
      info_format = "text/html"
    ),
    popupOptions = leaflet::popupOptions(maxWidth = 2000), # nolint: object_name_linter, line_length_linter.
    add_wms_legend = FALSE,
    ...) {
  require_pkgs(c("leaflet.extras", "leaflet.extras2"))
  leaflet.extras2::addWMS(
    map = map,
    baseUrl = paste0(
      "https://atlas.brabant.nl/arcgis/services/natuurbeheerplan_vastgesteld/",
      "MapServer/WMSServer"
    ),
    layers = "12",
    options = options,
    popupOptions = popupOptions,
    attribution = attrib_nb,
    ...
  ) %>%
    {
      if (!add_wms_legend) {
        .
      } else {
        leaflet.extras::addWMSLegend(
          .,
          paste0(
            "https://atlas.brabant.nl/arcgis/services/",
            "natuurbeheerplan_vastgesteld/MapServer/WmsServer?",
            "request=GetLegendGraphic&version=1.3.0&format=image/png&layer=12&"
          )
        )
      }
    }
}


#' @rdname add_wms
#' @export
add_wms_nlnb_ambitie_natuurtypen <- function( # styler: off # nolint: object_length_linter, line_length_linter.
    map,
    options = leaflet::WMSTileOptions(
      format = "image/png",
      transparent = TRUE,
      info_format = "text/html"
    ),
    popupOptions = leaflet::popupOptions(maxWidth = 2000), # nolint: object_name_linter, line_length_linter.
    add_wms_legend = FALSE,
    ...) {
  require_pkgs(c("leaflet.extras", "leaflet.extras2"))
  leaflet.extras2::addWMS(
    map = map,
    baseUrl = paste0(
      "https://atlas.brabant.nl/arcgis/services/natuurbeheerplan_vastgesteld/",
      "MapServer/WMSServer"
    ),
    layers = "13",
    options = options,
    popupOptions = popupOptions,
    attribution = attrib_nb,
    ...
  ) %>%
    {
      if (!add_wms_legend) {
        .
      } else {
        leaflet.extras::addWMSLegend(
          .,
          paste0(
            "https://atlas.brabant.nl/arcgis/services/",
            "natuurbeheerplan_vastgesteld/MapServer/WmsServer?",
            "request=GetLegendGraphic&version=1.3.0&format=image/png&layer=13&"
          )
        )
      }
    }
}


#' @rdname add_wms
#' @export
add_wms_nlnb_landschapstypen <- function(
    map,
    options = leaflet::WMSTileOptions(
      format = "image/png",
      transparent = TRUE,
      info_format = "text/html",
      minZoom = 13
    ),
    popupOptions = leaflet::popupOptions(maxWidth = 2000), # nolint: object_name_linter, line_length_linter.
    add_wms_legend = FALSE,
    ...) {
  require_pkgs(c("leaflet.extras", "leaflet.extras2"))
  leaflet.extras2::addWMS(
    baseUrl = paste0(
      "https://atlas.brabant.nl/arcgis/services/natuurbeheerplan_vastgesteld/",
      "MapServer/WMSServer"
    ),
    layers = "9",
    options = options,
    popupOptions = popupOptions,
    attribution = attrib_nb,
    ...
  ) %>%
    {
      if (!add_wms_legend) {
        .
      } else {
        leaflet.extras::addWMSLegend(
          .,
          paste0(
            "https://atlas.brabant.nl/arcgis/services/",
            "natuurbeheerplan_vastgesteld/MapServer/WmsServer?",
            "request=GetLegendGraphic&version=1.3.0&format=image/png&layer=9&"
          )
        )
      }
    }
}


#' @rdname add_wms
#' @export
add_wms_nlnb_natuurtypen <- function(
    map,
    options = leaflet::WMSTileOptions(
      format = "image/png",
      transparent = TRUE,
      info_format = "text/html",
      minZoom = 13
    ),
    popupOptions = leaflet::popupOptions(maxWidth = 2000), # nolint: object_name_linter, line_length_linter.
    add_wms_legend = FALSE,
    ...) {
  require_pkgs(c("leaflet.extras", "leaflet.extras2"))
  leaflet.extras2::addWMS(
    map = map,
    baseUrl = paste0(
      "https://atlas.brabant.nl/arcgis/services/natuurbeheerplan_vastgesteld/",
      "MapServer/WMSServer"
    ),
    layers = "10",
    options = options,
    popupOptions = popupOptions,
    attribution = attrib_nb,
    ...
  ) %>%
    {
      if (!add_wms_legend) {
        .
      } else {
        leaflet.extras::addWMSLegend(
          .,
          paste0(
            "https://atlas.brabant.nl/arcgis/services/",
            "natuurbeheerplan_vastgesteld/MapServer/WmsServer?",
            "request=GetLegendGraphic&version=1.3.0&format=image/png&layer=10&"
          )
        )
      }
    }
}

# nolint end
