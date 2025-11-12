# Find the k-th nearest cells for each cell

Find the k-th nearest cells for each cell

## Usage

``` r
findNearCells(
  dat,
  k = 100,
  targetCell = FALSE,
  reportCellID = FALSE,
  reportDist = TRUE,
  anno_col = 0
)
```

## Arguments

- dat:

  A SpatialExperiment object, can be generated using function
  `readHoodData`.

- k:

  The maximum number of nearest cells to compute.

- targetCell:

  Specify the cells to be the target cell for finding nearest cells.

- reportCellID:

  Logical. Set to TRUE to report cell id instead of cell types.

- reportDist:

  Logical. Set to TRUE to report the distance matrix.

- anno_col:

  Character vector. The name of annotation column to use.

## Value

A list includes a data.frame and a matrix, describing the cell types and
distances of the k-th nearest cells of each cell.

## Details

The `findNearCells` function uses the `nn2` function from the `RANN`
package, which uses the Approximate Near Neighbor (ANN) C++ library. For
more infromation on the ANN library please see
http://www.cs.umd.edu/~mount/ANN/.

## Examples

``` r
data("spe_test")

spe <- readHoodData(spe, anno_col = "celltypes")

fnc <- findNearCells(spe, k = 100)
```
