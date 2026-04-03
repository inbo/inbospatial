# addTiles() wrapper functions for WMTS services

The `add_wmts_*()` functions are shorthand alternatives for a fully
specified
[`leaflet::addTiles()`](https://rstudio.github.io/leaflet/reference/map-layers.html)
statement. Appropriate attribution is added to the Leaflet map depending
on the layer.

## Usage

``` r
add_wmts_nl_brt(map, layer = c("standaard", "grijs", "pastel", "water"), ...)

add_wmts_nl_ortho(map, ...)
```

## Arguments

- map:

  a map widget object created from
  [`leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)

- layer:

  String that defines which layer to use, if the function supports more
  than one. In such case, the available strings are shown in the Usage
  section. The first value is used if missing.

- ...:

  Further arguments passed to
  [`leaflet::addTiles()`](https://rstudio.github.io/leaflet/reference/map-layers.html).

## Value

A `leaflet` HTML widget object.

## Details

On condition that these functions continue being maintained, using the
shorthand functions should make your scripts more futureproof.

## Abbreviations

### Prefixes

- `be_`:

  Belgium

- `nl_`:

  Netherlands

### Suffixes

- `_ortho`:

  Orthophotographs

- `_brt`:

  Topographic BRT layers (Netherlands; 'Basisregistratie Topografie')

## See also

Other topics on using web services:
[`add_wms_be_cartoweb()`](https://inbo.github.io/inbospatial/reference/add_wms.md),
[`check_ogc_collection()`](https://inbo.github.io/inbospatial/reference/check_ogc_collection.md),
[`get_coverage_wcs()`](https://inbo.github.io/inbospatial/reference/get_coverage_wcs.md),
[`get_feature_ogc()`](https://inbo.github.io/inbospatial/reference/get_feature_ogc.md),
[`get_feature_wfs()`](https://inbo.github.io/inbospatial/reference/get_feature_wfs.md),
[`get_wcs_layers()`](https://inbo.github.io/inbospatial/reference/get_wcs_layers.md)

## Author

Floris Vanderhaeghe, <https://github.com/florisvdh>

## Examples

``` r
library(leaflet)
leaflet() |>
  setView(lng = 5.5, lat = 52.5, zoom = 9) |>
  add_wmts_nl_brt()

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[52.5,5.5],9,[]],"calls":[{"method":"addTiles","args":["https://service.pdok.nl/brt/achtergrondkaart/wmts/v2_0/standaard/EPSG:3857/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=https://www.nationaalgeoregister.nl>Nationaal Georegister<\/a>"}]}]},"evals":[],"jsHooks":[]}leaflet() |>
  setView(lng = 5.4, lat = 52.2, zoom = 14) |>
  add_wmts_nl_ortho()

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[52.2,5.4],14,[]],"calls":[{"method":"addTiles","args":["https://service.pdok.nl/hwh/luchtfotorgb/wmts/v1_0/Actueel_orthoHR/EPSG:3857/{z}/{x}/{y}.jpeg",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=https://www.nationaalgeoregister.nl>Nationaal Georegister<\/a>"}]}]},"evals":[],"jsHooks":[]}
```
