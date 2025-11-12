# Scan cellular neighbourhoods.

Scan cellular neighbourhoods.

## Usage

``` r
scanHoods(
  m,
  mode = c("proximityFocused", "smoothFadeout"),
  tau = NA,
  t_init = NA
)
```

## Arguments

- m:

  Distance matrix. Can be obtained from function findNearCells.

- mode:

  Character. Either proximityFocused or smoothFadeout. By default is
  proximityFocused.

- tau:

  The hyperparameter tau, by default is median(m\*\*2)/5

- t_init:

  An initial tau. In the smoothFadeout mode, user can provide an initial
  tau for optimization.

## Value

A probability matrix.

## Examples

``` r
m <- matrix(abs(rnorm(1000 * 100)), 1000, 100)

pm <- scanHoods(m)
#> Tau is set to: 0.0919819146408074
```
