# Merge probability matrix into SpatialExperiment object.

Merge probability matrix into SpatialExperiment object.

## Usage

``` r
mergeHoodSpe(spe, pm, val_names = NULL)
```

## Arguments

- spe:

  A SpatialExperiment object.

- pm:

  Probability matrix. Can be obtained by the function mergeByGroup.

- val_names:

  Character vector with length of the ncol of pm.

## Value

A SpatialExperiment object. Cell-level neighborhood information are
saved in the colData of the SpatialExperiment object.

## Examples

``` r
data("spe_test")

spe <- readHoodData(spe, anno_col = "celltypes")

fnc <- findNearCells(spe, k = 100)

pm <- scanHoods(fnc$distance)
#> Tau is set to: 22747.4

pm2 <- mergeByGroup(pm, fnc$cells)

spe <- mergeHoodSpe(spe, pm2)
```
