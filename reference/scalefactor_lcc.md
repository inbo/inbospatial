# Return the scale factor for a specific parallel in a Lambert Conic Conformal projection

The scale factor is the distance distortion. In this case (a conic
conformal projection) it holds in any direction.

## Usage

``` r
scalefactor_lcc(par_deg, par1_deg, par2_deg)
```

## Arguments

- par_deg:

  Numeric vector. The latitude in decimal degrees of one or more
  parallels for which to do the calculation.

- par1_deg:

  Latitude of the first standard parallel of the LCC projection in
  decimal degrees.

- par2_deg:

  Latitude of the second standard parallel of the LCC projection in
  decimal degrees.

## Value

Numeric vector, same length as `par_deg`.

## Details

The function applies to normal LCC projections, not the oblique ones.
The applied formulas are for the sphere, hence approximate.

## References

Snyder J.P. (1987). Map Projections - A Working Manual. U.S. Geological
Survey Professional Paper, Nr. 1395. United States Government Printing
Office, Washington, 397 p.

## See also

Other functions to explore properties of coordinate reference systems:
[`scalefactor_tcyl()`](https://inbo.github.io/inbospatial/reference/scalefactor_tcyl.md)

## Author

Floris Vanderhaeghe, <https://github.com/florisvdh>

## Examples

``` r
sf::st_crs(31370)
#> Coordinate Reference System:
#>   User input: EPSG:31370 
#>   wkt:
#> PROJCRS["BD72 / Belgian Lambert 72",
#>     BASEGEOGCRS["BD72",
#>         DATUM["Reseau National Belge 1972",
#>             ELLIPSOID["International 1924",6378388,297,
#>                 LENGTHUNIT["metre",1]]],
#>         PRIMEM["Greenwich",0,
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>         ID["EPSG",4313]],
#>     CONVERSION["Belgian Lambert 72",
#>         METHOD["Lambert Conic Conformal (2SP)",
#>             ID["EPSG",9802]],
#>         PARAMETER["Latitude of false origin",90,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8821]],
#>         PARAMETER["Longitude of false origin",4.36748666666667,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8822]],
#>         PARAMETER["Latitude of 1st standard parallel",51.1666672333333,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8823]],
#>         PARAMETER["Latitude of 2nd standard parallel",49.8333339,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8824]],
#>         PARAMETER["Easting at false origin",150000.013,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8826]],
#>         PARAMETER["Northing at false origin",5400088.438,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8827]]],
#>     CS[Cartesian,2],
#>         AXIS["easting (X)",east,
#>             ORDER[1],
#>             LENGTHUNIT["metre",1]],
#>         AXIS["northing (Y)",north,
#>             ORDER[2],
#>             LENGTHUNIT["metre",1]],
#>     USAGE[
#>         SCOPE["Engineering survey, topographic mapping."],
#>         AREA["Belgium - onshore."],
#>         BBOX[49.5,2.5,51.51,6.4]],
#>     ID["EPSG",31370]]
x <- seq(49.5, 52.5, 0.1)
y <- scalefactor_lcc(x, 51.1666672333333, 49.8333339)
data.frame(latitude = x, scalefactor = y)
#>    latitude scalefactor
#> 1      49.5   1.0000840
#> 2      49.6   1.0000553
#> 3      49.7   1.0000296
#> 4      49.8   1.0000069
#> 5      49.9   0.9999872
#> 6      50.0   0.9999705
#> 7      50.1   0.9999568
#> 8      50.2   0.9999461
#> 9      50.3   0.9999385
#> 10     50.4   0.9999339
#> 11     50.5   0.9999323
#> 12     50.6   0.9999338
#> 13     50.7   0.9999383
#> 14     50.8   0.9999459
#> 15     50.9   0.9999566
#> 16     51.0   0.9999703
#> 17     51.1   0.9999871
#> 18     51.2   1.0000070
#> 19     51.3   1.0000300
#> 20     51.4   1.0000560
#> 21     51.5   1.0000852
#> 22     51.6   1.0001175
#> 23     51.7   1.0001530
#> 24     51.8   1.0001915
#> 25     51.9   1.0002332
#> 26     52.0   1.0002781
#> 27     52.1   1.0003261
#> 28     52.2   1.0003772
#> 29     52.3   1.0004315
#> 30     52.4   1.0004890
#> 31     52.5   1.0005497
```
