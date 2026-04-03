# Check if a collection exists in an OGC API Features service

A lightweight helper function that queries the `/collections` endpoint
of an OGC API using JSON. It verifies if a specified collection exists
and provides a helpful error message with valid names if it does not.

## Usage

``` r
check_ogc_collection(url, collection)
```

## Arguments

- url:

  A character string with the base URL of the OGC API.

- collection:

  A character string with the ID of the collection to check.

## Value

`TRUE` invisibly if the collection exists, otherwise throws an error.

## See also

Other topics on using web services:
[`add_wms_be_cartoweb()`](https://inbo.github.io/inbospatial/reference/add_wms.md),
[`add_wmts_nl_brt()`](https://inbo.github.io/inbospatial/reference/add_wmts.md),
[`get_coverage_wcs()`](https://inbo.github.io/inbospatial/reference/get_coverage_wcs.md),
[`get_feature_ogc()`](https://inbo.github.io/inbospatial/reference/get_feature_ogc.md),
[`get_feature_wfs()`](https://inbo.github.io/inbospatial/reference/get_feature_wfs.md),
[`get_wcs_layers()`](https://inbo.github.io/inbospatial/reference/get_wcs_layers.md)

## Examples

``` r
 if (FALSE) { # \dontrun{
api_url <- "https://geo.api.vlaanderen.be/Wegenregister/ogc/features/v1"
check <- check_ogc_collection(api_url, "Wegsegment")
check

# An informative error is thrown when collection does not exist
check <- try(check_ogc_collection(api_url, "foutieve_laag"))
} # }
```
