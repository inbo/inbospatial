# inbospatial 0.0.3

## Features

* `get_coverage_wcs()` can now query data from the 
  Digital Elevation Model, Flanders 
  (DHMV, ``Digitaal Hoogtemodel Vlaanderen``).

## Documentation

* added `spatial_dhmv_query` vignette demonstrating WCS queries 
  with and beyond `get_coverage_wcs()`

## Bug fixes

* fix parsing of `geoTIFF` part from `mht` files (internal function)

# inbospatial 0.0.2

## New functions

* utility functions to calculate distance distortions in some
  projection methods
* `add_wms_*()`and `add_wmts_*()` family of functions, which are shorthand
  alternatives for a fully specified `leaflet::addWMSTiles()`,
  `leaflet.extras2::addWMS()` or
  `leaflet::addTiles()` statement.

## Enhancements

* updated vignette demonstrating `get_features_wfs()` and `get_coverage_wcs()`
* improved error handling in `get_features_wfs()` (#3)

## Bug fixes

* Minor changes to fix failing GitHub actions

# inbospatial 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* New function `get_features_wfs` to get features (vector data) from a Web
  Feature Service (`WFS`)
* New function `get_coverage_wcs` to get raster data from a Web Coverage Service
  (`WCS`)
