# Download features from an OGC API Features service

`get_feature_ogc()` provides a modern alternative to
[`get_feature_wfs()`](https://inbo.github.io/inbospatial/reference/get_feature_wfs.md).
It retrieves vector data from an OGC API Features service. See
<https://ogcapi.ogc.org>.

## Usage

``` r
get_feature_ogc(
  url,
  collection,
  bbox = NULL,
  datetime = NULL,
  properties = NULL,
  cql_filter = NULL,
  limit = NULL,
  crs = NULL,
  quiet = TRUE,
  ...
)
```

## Arguments

- url:

  A character string with the base URL of the OGC API (the landing
  page).

- collection:

  A character string with the ID of the collection (layer).

- bbox:

  A bounding box to filter features. Can be a numeric vector of length 4
  (`c(xmin, ymin, xmax, ymax)`) or an object of class `bbox` (from
  [`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)).
  Note: OGC APIs generally expect the bbox coordinates to be in WGS84
  (EPSG:4326).

- datetime:

  A character string representing a time instant or time interval (e.g.,
  `"2018-02-12T23:20:50Z"` or
  `"2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"`).

- properties:

  A character vector of specific column names to return.

- cql_filter:

  A character string containing a CQL2-text filter to apply attribute or
  complex spatial filtering on the server. See [Common Query
  Language](https://docs.ogc.org/is/21-065r2/21-065r2.html) filter
  specifications.

- limit:

  Numeric. Maximum number of features to retrieve. If `NULL` (default),
  all available features will be fetched via automatic API pagination.

- crs:

  Target Coordinate Reference System (CRS) as an EPSG code or CRS
  object. If provided, the output will be automatically transformed to
  this CRS.

- quiet:

  Logical. Should the download progress be suppressed? Defaults to
  `TRUE`.

- ...:

  Additional name-value pairs passed on to
  [`httr2::req_url_query`](https://httr2.r-lib.org/reference/req_url.html).

## Value

An `sf` (simple feature) object.

## See also

Other topics on using web services:
[`add_wms_be_cartoweb()`](https://inbo.github.io/inbospatial/reference/add_wms.md),
[`add_wmts_nl_brt()`](https://inbo.github.io/inbospatial/reference/add_wmts.md),
[`check_ogc_collection()`](https://inbo.github.io/inbospatial/reference/check_ogc_collection.md),
[`get_coverage_wcs()`](https://inbo.github.io/inbospatial/reference/get_coverage_wcs.md),
[`get_feature_wfs()`](https://inbo.github.io/inbospatial/reference/get_feature_wfs.md),
[`get_wcs_layers()`](https://inbo.github.io/inbospatial/reference/get_wcs_layers.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Base URL for the Digitaal Vlaanderen Wegenregister (Road Register)
api_url <- "https://geo.api.vlaanderen.be/Wegenregister/ogc/features/v1"

# 1. Basic usage: Fetch 10 road segments
roads_sample <- get_feature_ogc(
  url = api_url,
  collection = "Wegsegment",
  limit = 10
)

# 2. Spatial Filtering & CRS Transformation:
# Fetch features within a WGS84 bounding box and project to Belgian Lambert72
my_bbox <- sf::st_bbox(
  c(xmin = 4.39, ymin = 51.21, xmax = 4.40, ymax = 51.22),
  crs = sf::st_crs(4326)
)
roads_bbox <- get_feature_ogc(
  url = api_url,
  collection = "Wegsegment",
  bbox = my_bbox,
  crs = 31370
)

# Base URL for Historical Land Use (Ferraris map)
hist_url <- "https://geo.api.vlaanderen.be/HistLandgebruik/ogc/features/v1"

# 3. Attribute Selection:
# Fetch only specific columns
ferraris_lite <- get_feature_ogc(
  url = hist_url,
  collection = "Lgbrk1778",
  limit = 100,
  properties = c("KLASSE")
)

# 4. Advanced CQL2 Attribute Filtering:
# Filter features directly on the server
roads_filtered <- get_feature_ogc(
  url = api_url,
  collection = "Wegsegment",
  limit = 50,
  cql_filter = "morfologischeWegklasse = 'dienstweg'"
)
} # }
```
