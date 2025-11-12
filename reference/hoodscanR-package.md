# Method to identify cellular spatial neighbourhood from single cell spatial transcriptomics data.

`hoodscanR` implements a novel method to scan for cell neighbourhood
from spatial transcriptomics data at single cell level, such as CosMx
and MERFISH etc. `hoodscanR` takes the cellular position and cell type
annotations as inputs, allowing cellular spatial neighbourhood analysis.

## Details

Key neighborhood analysis functions include
[`findNearCells`](https://davislaboratory.github.io/hoodscanR/reference/findNearCells.md)`, `[`scanHoods`](https://davislaboratory.github.io/hoodscanR/reference/scanHoods.md)`, `[`mergeByGroup`](https://davislaboratory.github.io/hoodscanR/reference/mergeByGroup.md)`, `[`calcMetrics`](https://davislaboratory.github.io/hoodscanR/reference/calcMetrics.md)`, `[`clustByHood`](https://davislaboratory.github.io/hoodscanR/reference/clustByHood.md).

Key visualisation functions include
[`plotTissue`](https://davislaboratory.github.io/hoodscanR/reference/plotTissue.md)`, `[`plotHoodMat`](https://davislaboratory.github.io/hoodscanR/reference/plotHoodMat.md)`, `[`plotColocal`](https://davislaboratory.github.io/hoodscanR/reference/plotColocal.md)`, `[`plotProbDist`](https://davislaboratory.github.io/hoodscanR/reference/plotProbDist.md).

## See also

Useful links:

- <https://github.com/DavisLaboratory/hoodscanR>

- <https://davislaboratory.github.io/hoodscanR/>

- Report bugs at <https://github.com/DavisLaboratory/hoodscanR/issues>

## Author

Ning Liu <liu.n@wehi.edu.au>
