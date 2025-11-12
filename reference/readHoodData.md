# Read cellular position and annotation data into a list object.

Read cellular position and annotation data into a list object.

## Usage

``` r
readHoodData(
  spe = NA,
  anno_col = NA,
  cell_pos_dat = NA,
  cell_anno_dat = NA,
  pos_col = NA
)
```

## Arguments

- spe:

  SpatialExperiment object.

- anno_col:

  Character. The column name of the annotation to be used in the
  following neighbourhood analysis.

- cell_pos_dat:

  data.frame object contains the cellular positions.

- cell_anno_dat:

  data.frame object contains the cell annotations.

- pos_col:

  Character. If the x and y are in the colData instead of in the
  SpatialCoords of spe, can specify this parameter.

## Value

A SpatialExperiment object.

## Examples

``` r
data("spe_test")

spe <- readHoodData(spe, anno_col = "celltypes")
```
