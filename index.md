# hoodscanR: Cellular neighborhoods scanning from spatial transcriptomics data in R ![](reference/figures/hoodscanR_sticker.png)

[![R-CMD-check](https://github.com/DavisLaboratory/hoodscanR/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/DavisLaboratory/hoodscanR/actions)
[![Coverage
status](https://codecov.io/gh/DavisLaboratory/hoodscanR/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/DavisLaboratory/hoodscanR?branch=devel)

Install released version from Bioconductor

    if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

    BiocManager::install("hoodscanR")

Install development version from GitHub

    library(devtools)
    devtools::install_github("DavisLaboratory/hoodscanR")
