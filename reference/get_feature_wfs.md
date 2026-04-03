# Get a layer from a web feature service

This function constructs a `URL` request from its arguments and either
reads in the resulting vector layer as a `sf` object or returns the
number of features that are requested. The request is made up of
key-value pairs and additional key-value pairs can be passed to the
function. The full documentation for the `WFS` standard can be consulted
from <https://www.ogc.org/standards/wfs/>.

## Usage

``` r
get_feature_wfs(
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
  ...
)
```

## Arguments

- wfs:

  Web address for the service which you want to query features from

- version:

  Version number for the service. For instance `"2.0.0"`.

- layername:

  Optional name of a layer hosted by the web feature service

- crs:

  Optional coordinate reference system to represent the features. For
  instance `"EPSG:31370"`.

- bbox:

  Optional bounding box. Pass this as a named vector with names
  `"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`.

- filter:

  Optional [standard OGC filter](https://www.ogc.org/standards/filter/)
  specification

- cql_filter:

  Optional [Common Query
  Language](https://docs.ogc.org/is/21-065r2/21-065r2.html) filter. This
  currently only works if the `WFS` is hosted on a `GeoServer`.

- output_format:

  Optional output format supported by the `WFS`.

- property_name:

  Optional character string. Which fields or columns to return? If you
  want to specify multiple columns, separate them by a comma. The column
  containing the feature geometry is usually called `geom`, `geometry`
  or `SHAPE`.

- result_type:

  For version `"2.x.x"`, this can be either `"results"` (default) or
  `"hits"`. The former returns the requested features, the latter
  returns the number of requested features.

- ...:

  Additional key-value pairs passed on to the WFS query.

## Value

An `sf` (simple feature) object.

## Details

See <https://tutorials.inbo.be/tutorials/spatial_wfs_services/> for more
information.

## See also

Other topics on using web services:
[`add_wms_be_cartoweb()`](https://inbo.github.io/inbospatial/reference/add_wms.md),
[`add_wmts_nl_brt()`](https://inbo.github.io/inbospatial/reference/add_wmts.md),
[`check_ogc_collection()`](https://inbo.github.io/inbospatial/reference/check_ogc_collection.md),
[`get_coverage_wcs()`](https://inbo.github.io/inbospatial/reference/get_coverage_wcs.md),
[`get_feature_ogc()`](https://inbo.github.io/inbospatial/reference/get_feature_ogc.md),
[`get_wcs_layers()`](https://inbo.github.io/inbospatial/reference/get_wcs_layers.md)

## Examples

``` r
if (FALSE) { # \dontrun{
vlaanderen <- get_feature_wfs(
  wfs = paste0(
    "https://eservices.minfin.fgov.be/",
    "arcgis/services/R2C/Regions/MapServer/WFSServer"
  ),
  layername = "regions",
  crs = "EPSG:31370",
  filter = paste0(
    "<Filter><PropertyIsEqualTo><PropertyName>",
    "regions:NameDUT</PropertyName><Literal>'Vlaams Gewest'",
    "</Literal></PropertyIsEqualTo></Filter>"
  )
)
} # }
```
