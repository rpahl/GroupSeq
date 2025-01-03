
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
release](https://www.r-pkg.org/badges/version/GroupSeq)](https://cran.r-project.org/package=GroupSeq)
[![Dependencies](https://tinyverse.netlify.app/badge/GroupSeq)](https://CRAN.R-project.org/package=GroupSeq)
[![R-CMD-check
status](https://github.com/rpahl/GroupSeq/workflows/R-CMD-check/badge.svg)](https://github.com/rpahl/GroupSeq/actions)
[![CRAN
checks](https://badges.cranchecks.info/summary/GroupSeq.svg)](https://cran.r-project.org/web/checks/check_results_container.html)
[![Downloads per
month](https://cranlogs.r-pkg.org/badges/last-month/GroupSeq)](https://cran.r-project.org/package=GroupSeq)
[![Downloads
total](https://cranlogs.r-pkg.org/badges/grand-total/GroupSeq)](https://cran.r-project.org/package=GroupSeq)
[![Last
commit](https://img.shields.io/github/last-commit/rpahl/GroupSeq.svg)](https://github.com/rpahl/GroupSeq/commits/master)
[![Lifecycle
status](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

# GroupSeq <img src="man/figures/logo.png" alt="logo" align="right" width="115" height="163"/>

GroupSeq computes probabilities related to group sequential designs for
normally distributed test statistics. It enables to derive critical
boundaries, power, drift, and confidence intervals of such designs,
mostly using the alpha spending approach of DeMets and Lan
([1994](#ref-pmid7973215)).

The main motivation for the development of this package was to make this
area of statistics freely available for a broad audience, which is one
of the reasons why all functionality is provided by a graphical user
interface.

### Installation

``` r
# Install release version from CRAN
install.packages("GroupSeq")

# Install development version from GitHub
devtools::install_github("rpahl/GroupSeq")
```

### Usage

Load the library to start the graphical user interface.

``` r
library("GroupSeq")
```

<img src="man/figures/menu-after-load.png" alt="menu-after-load" width="30%" />

<br>

To get started see the [General
Introduction](https://rpahl.github.io/GroupSeq/articles/GroupSeq.html)
page.

### Alternative tools

Since the package was written back in 2005, the graphical user interface
may appear a bit outdated. Still, it does it’s job and at least has
stood the test of time[$^1$](#refs). Luckily in recent years others have
started to develop R-based tools with graphical user interface and
similar (and more) statistical functionality:

- [gsDesign](https://CRAN.R-project.org/package=gsDesign) with a free
  [shiny web interface](https://gsdesign.shinyapps.io/prod/) including
  tutorials.
- [rpact](https://CRAN.R-project.org/package=rpact) also with a free
  [shiny web interface](https://rpact.shinyapps.io/public) and lots of
  [vignettes](https://www.rpact.com/vignettes).

------------------------------------------------------------------------

1.  The package author is considering to work on a second version with
    more functionality and a revised user interface in the near future.
    Feel free to request new features
    [here](https://github.com/rpahl/GroupSeq/issues).

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-pmid7973215" class="csl-entry">

DeMets, D. L., and K. K. Lan. 1994. “<span class="nocase">Interim
analysis: the alpha spending function approach</span>.” *Stat Med* 13
(13-14): 1341–52. <https://doi.org/10.1002/sim.4780131308>.

</div>

</div>
