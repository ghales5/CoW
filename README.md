# Centre of Western Public Health Unit

<!-- badges: start -->
<!-- badges: end -->

Calculates the population-weighted centre of the Western Public Health Unit, and
compares with input data using an interactive dashboard.

## Installation

You can install the development version of CoW from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ghales5/CoW")
```

You will need a mapbox access token to use this package. To register for a free
mapbox account and set up your access token, please see [here](https://docs.mapbox.com/help/getting-started/access-tokens/).

Pass the mapbox access token to the shiny app using:

```r
CoW::runWPHUCentre(mapbox_accesstoken = "YOUR_TOKEN_HERE")
```

## Issues

If you find a bug or have an issue using the software, please post using the [issue tracker](https://github.com/ghales5/CoW/issues)
