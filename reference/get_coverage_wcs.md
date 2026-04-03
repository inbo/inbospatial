# Get a layer from a web coverage service within a bounding box

The function sends a query to a WCS service, downloads it to a temporary
file from which it is read with
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html) -
if needed reprojected - and returned as a `SpatRaster` object

## Usage

``` r
get_coverage_wcs(
  wcs = c("dtm", "dsm", "omz", "omw", "dhmv", "mercatornet"),
  bbox,
  layername,
  resolution,
  wcs_crs = c("EPSG:4258", "EPSG:31370"),
  output_crs = "EPSG:31370",
  bbox_crs = "EPSG:31370",
  version = c("1.0.0", "2.0.1"),
  ...
)
```

## Arguments

- wcs:

  One of `"dtm"`, `"dsm"`, `"omz"`, `"omw"`, `"dhmv"`, `"mercatornet"`

- bbox:

  An object of class bbox of length 4.

- layername:

  Character string; name of the layer

- resolution:

  Output resolution in meters

- wcs_crs:

  Native CRS in which the raster layers are stored on the `WCS`

- output_crs:

  Output CRS. May involve reprojection.

- bbox_crs:

  CRS in which bbox coordinates are passed

- version:

  `WCS` version to be used. Default is `1.0.0`.

- ...:

  Additional key-value pairs passed on to the `WCS` query

## Value

A `SpatRaster` object

## Details

The following WCS services can currently be used:

- `"omz"`: orthophotomosaic summer images Flanders

- `"omw"`: orthophotomosaic winter images Flanders

- `"dtm"`: digital terrain model Flanders

- `"dsm"`: digital surface model Flanders

- `"dhmv"`: digital elevation model Flanders (contains dtm and dsm data)

- `"mercatornet"`: Public Download Service Flemish Government -
  department environment - cooperation `MercatorNet`

For more information, see metadata Vlaanderen:

<https://metadata.vlaanderen.be/srv/eng/catalog.search#/search?any=WCS>

## See also

Other topics on using web services:
[`add_wms_be_cartoweb()`](https://inbo.github.io/inbospatial/reference/add_wms.md),
[`add_wmts_nl_brt()`](https://inbo.github.io/inbospatial/reference/add_wmts.md),
[`check_ogc_collection()`](https://inbo.github.io/inbospatial/reference/check_ogc_collection.md),
[`get_feature_ogc()`](https://inbo.github.io/inbospatial/reference/get_feature_ogc.md),
[`get_feature_wfs()`](https://inbo.github.io/inbospatial/reference/get_feature_wfs.md),
[`get_wcs_layers()`](https://inbo.github.io/inbospatial/reference/get_wcs_layers.md)

## Examples

``` r
if (FALSE) { # \dontrun{
bbox <- sf::st_bbox(
  c(xmin = 155800, xmax = 155850, ymin = 132700, ymax = 132750),
  crs = sf::st_crs(31370)
)
get_coverage_wcs(
  wcs = "dsm",
  bbox = bbox,
  layername = "EL.GridCoverage.DSM",
  resolution = 1
)
} # }
```
