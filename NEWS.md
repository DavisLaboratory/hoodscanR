# hoodscanR 1.1.1

Small bug fix:

- when data have cell_id within the colData.

# hoodscanR 1.1.0

Published in Bioconductor.

# hoodscanR 0.99.5

Remove codes that are commented out.

# hoodscanR 0.99.4

### user perspective/experience

- documentation update about output values
- output variable/names parameter added to many functions
- validity checks for spatialCoords are added to related functions
- for loops are gone.

# hoodscanR 0.99.3

### code
- sapply change to vapply.
- validity checks on inputs are now added to all functions 
using either is(spe, "SpatialExperiment") or is(m, "matrix") etc.

### vignette
- Explanation of plots and algorithm inside different 
functions are now added to the vignette briefly.
- BiocStyle are now used.
- BiocStyle to hyperlink packages are now used.
- Fixed typo

### documentation
- data description of the test data has been added.
- cross-link key function have added to the package help page.
- the equation and theory of the calcMetrics are 
now added in the help page.

### other
- Bioconductor installation instructions is now added to the readme file.
- code coverage is now almost 85%.
- The LICENSE placeholders is now edited.

# hoodscanR 0.99.0

First submission to Bioconductor.
