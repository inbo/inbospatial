# Get available layers from a web coverage service

The function sends a `GetCapabilities` query to a WCS service, parses
the XML response, and returns a `data.frame` containing the names of the
available layers and their descriptions.

## Usage

``` r
get_wcs_layers(
  wcs = c("dtm", "dsm", "omz", "omw", "dhmv", "mercatornet"),
  version = c("1.0.0", "2.0.1"),
  ...
)
```

## Arguments

- wcs:

  One of `"dtm"`, `"dsm"`, `"omz"`, `"omw"`, `"dhmv"`, `"mercatornet"`

- version:

  `WCS` version to be used. Default is `1.0.0`.

- ...:

  Additional key-value pairs passed on to the `WCS` query

## Value

A `data.frame` object with columns `layername` and `description`.

## Details

The following WCS services can currently be used:

- `"omz"`: orthophotomosaic summer images Flanders

- `"omw"`: orthophotomosaic winter images Flanders

- `"dtm"`: digital terrain model Flanders

- `"dsm"`: digital surface model Flanders

- `"dhmv"`: digital elevation model Flanders (contains dtm and dsm data)

- `"mercatornet"`: Public Download Service Flemish Government -
  department environment - cooperation `MercatorNet` For more
  information, see metadata Vlaanderen:
  https://metadata.vlaanderen.be/srv/eng/catalog.search#/search?any=WCS

## See also

Other topics on using web services:
[`add_wms_be_cartoweb()`](https://inbo.github.io/inbospatial/reference/add_wms.md),
[`add_wmts_nl_brt()`](https://inbo.github.io/inbospatial/reference/add_wmts.md),
[`check_ogc_collection()`](https://inbo.github.io/inbospatial/reference/check_ogc_collection.md),
[`get_coverage_wcs()`](https://inbo.github.io/inbospatial/reference/get_coverage_wcs.md),
[`get_feature_ogc()`](https://inbo.github.io/inbospatial/reference/get_feature_ogc.md),
[`get_feature_wfs()`](https://inbo.github.io/inbospatial/reference/get_feature_wfs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
get_wcs_layers(wcs = "dsm")
get_wcs_layers(wcs = "omz", version = "2.0.1")
} # }
```
