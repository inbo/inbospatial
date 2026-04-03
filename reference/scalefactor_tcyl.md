# Return the scale factor for the angular distance from the central meridian in an (unscaled) transverse cylindrical projection

The scale factor is the distance distortion in the direction of the
meridians. In the case of the Transverse Mercator projection (a
cylindrical conformal projection) it holds in any direction.

## Usage

``` r
scalefactor_tcyl(ang_dist)
```

## Arguments

- ang_dist:

  Numeric vector. One or more angular distances from the central
  meridian, in decimal degrees.

## Value

Numeric vector, same length as `ang_dist`.

## Details

The formulas applied are for the sphere, hence approximate.

The best known map projection system in this context is the Universal
Transverse Mercator (UTM), which superposes a central meridian scale
factor of 0.9996.

## References

Iliffe J. & Lott R. (2008). Datums and Map Projections. For Remote
Sensing, GIS and Surveying. 2nd edn. Whittles Publishing, Caithness, UK,
208 p.

## See also

Other functions to explore properties of coordinate reference systems:
[`scalefactor_lcc()`](https://inbo.github.io/inbospatial/reference/scalefactor_lcc.md)

## Author

Floris Vanderhaeghe, <https://github.com/florisvdh>

## Examples

``` r
x <- seq(0, 3, 0.1)
y <- scalefactor_tcyl(x)
data.frame(ang_dist = x, scalefactor = y)
#>    ang_dist scalefactor
#> 1       0.0    1.000000
#> 2       0.1    1.000002
#> 3       0.2    1.000006
#> 4       0.3    1.000014
#> 5       0.4    1.000024
#> 6       0.5    1.000038
#> 7       0.6    1.000055
#> 8       0.7    1.000075
#> 9       0.8    1.000097
#> 10      0.9    1.000123
#> 11      1.0    1.000152
#> 12      1.1    1.000184
#> 13      1.2    1.000219
#> 14      1.3    1.000257
#> 15      1.4    1.000299
#> 16      1.5    1.000343
#> 17      1.6    1.000390
#> 18      1.7    1.000440
#> 19      1.8    1.000494
#> 20      1.9    1.000550
#> 21      2.0    1.000610
#> 22      2.1    1.000672
#> 23      2.2    1.000738
#> 24      2.3    1.000806
#> 25      2.4    1.000878
#> 26      2.5    1.000953
#> 27      2.6    1.001030
#> 28      2.7    1.001111
#> 29      2.8    1.001195
#> 30      2.9    1.001282
#> 31      3.0    1.001372
```
