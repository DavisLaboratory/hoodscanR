# Cluster the probability matrix with K-means

Cluster the probability matrix with K-means

## Usage

``` r
clustByHood(object, ...)

# S4 method for class 'matrix'
clustByHood(object, k = 2^ncol(object) - 1, iter_max = 1000, nstart = 5)

# S4 method for class 'SpatialExperiment'
clustByHood(
  object,
  pm_cols,
  k = 0,
  iter_max = 1000,
  nstart = 5,
  algo = "Hartigan-Wong",
  val_name = "clusters"
)
```

## Arguments

- object:

  A probability matrix or a SpatialExperiment.

- ...:

  Ignore parameter.

- k:

  The number of clusters. By default is 2^ncol(object)-1.

- iter_max:

  the maximum number of iterations allowed.

- nstart:

  how many random sets should be chosen.

- pm_cols:

  The colnames of probability matrix. This is requires for
  SpatialExperiment input. Assuming that the probability is stored in
  the colData.

- algo:

  Algorithm to be used. Options include Hartigan-Wong, Lloyd, and
  MacQueen.

- val_name:

  Character. Column name used to store the clusters.

## Value

A probability matrix or a SpatialExperiment object. For latter, the
clustering results are saved in the colData of the SpatialExperiment
object.

## Examples

``` r
m <- matrix(abs(rnorm(1000 * 100)), 1000, 100)

clust <- clustByHood(m, k = 3)
```
