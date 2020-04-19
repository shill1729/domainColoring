
# domainColoring

<!-- badges: start -->
<!-- badges: end -->

This package provides a single function for generating domain colorings of single-variable mappings of the complex numbers. Features include controlling resolution, luminosity and optional automatic saving of the generated plots as jpegs. More to come...

## Installation

You can install the latest GitHub version with:

``` r
devtools::install_github("shill1729/domainColoring")
```

## Example

How to use:

``` r
library(domainColoring)
## basic example code
f <- function(z) sin(z)
# Default low-res, not saved plot
p <- domain_coloring(f)
```

