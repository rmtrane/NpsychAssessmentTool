

<!-- README.md is generated from README.Rmd. Please edit that file -->

# NpsychAssessmentTool

<!-- badges: start -->

[![R-CMD-check](https://github.com/rmtrane/NpsychAssessmentTool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rmtrane/NpsychAssessmentTool/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/rmtrane/NpsychAssessmentTool/graph/badge.svg?token=FRFC07GNEW)](https://codecov.io/gh/rmtrane/NpsychAssessmentTool)
<!-- badges: end -->

This package provides an [R Shiny](https://shiny.posit.co) application
used for diagnosis of cognitive status based on neuropsychological
scores.

## Screenshots

<div id="gallery-carousel" class="carousel carousel-dark slide" data-bs-ride="carousel">
<div class="carousel-indicators">
<button type="button" data-bs-target="#gallery-carousel" data-bs-slide-to="0" aria-label="Slide 1" class="active" aria-current="true"></button>
<button type="button" data-bs-target="#gallery-carousel" data-bs-slide-to="1" aria-label="Slide 2"></button>
<button type="button" data-bs-target="#gallery-carousel" data-bs-slide-to="2" aria-label="Slide 3"></button>
<button type="button" data-bs-target="#gallery-carousel" data-bs-slide-to="3" aria-label="Slide 4"></button>
<button type="button" data-bs-target="#gallery-carousel" data-bs-slide-to="4" aria-label="Slide 5"></button>
<button type="button" data-bs-target="#gallery-carousel" data-bs-slide-to="5" aria-label="Slide 6"></button>
</div>
<div class="carousel-inner">
<div class="carousel-item active" data-bs-interval="5000">
<a>
<img src="https://www.github.com/rmtrane/NpsychAssessmentTool/main/tree//tests/testthat/_snaps/mac-4.5/screenshots_for_intro/shinyApp-001.png" class="d-block  mx-auto border" width="100%"/>
</a>
<div class="carousel-caption d-none d-md-block">
<p class="fw-light">Data Selection. Data can be imported straight from REDCap or from a .csv file.</p>
</div>
</div>
<div class="carousel-item" data-bs-interval="5000">
<a>
<img src="https://www.github.com/rmtrane/NpsychAssessmentTool/main/tree//tests/testthat/_snaps/mac-4.5/screenshots_for_intro/shinyApp-002.png" class="d-block  mx-auto border" width="100%"/>
</a>
<div class="carousel-caption d-none d-md-block">
<p class="fw-light">Data Selection. A demo data set is included for users to explore the program's capabilities.</p>
</div>
</div>
<div class="carousel-item" data-bs-interval="5000">
<a>
<img src="https://www.github.com/rmtrane/NpsychAssessmentTool/main/tree//tests/testthat/_snaps/mac-4.5/screenshots_for_intro/shinyApp-003.png" class="d-block  mx-auto border" width="100%"/>
</a>
<div class="carousel-caption d-none d-md-block">
<p class="fw-light">Scoring Tables and Figures. The main table shows scores from single visit, while plots include longitudinal (standardized) scores across all available visits.</p>
</div>
</div>
<div class="carousel-item" data-bs-interval="5000">
<a>
<img src="https://www.github.com/rmtrane/NpsychAssessmentTool/main/tree//tests/testthat/_snaps/mac-4.5/screenshots_for_intro/shinyApp-004.png" class="d-block  mx-auto border" width="100%"/>
</a>
<div class="carousel-caption d-none d-md-block">
<p class="fw-light">Scoring Tables and Figures. Switching between participants is as easy as selecting the ID from the dropdown menu.</p>
</div>
</div>
<div class="carousel-item" data-bs-interval="5000">
<a>
<img src="https://www.github.com/rmtrane/NpsychAssessmentTool/main/tree//tests/testthat/_snaps/mac-4.5/screenshots_for_intro/shinyApp-005.png" class="d-block  mx-auto border" width="100%"/>
</a>
<div class="carousel-caption d-none d-md-block">
<p class="fw-light">Scoring Tables and Figures. The longitudinal (standardized) scores are also available in the form of a color coded table.</p>
</div>
</div>
<div class="carousel-item" data-bs-interval="5000">
<a>
<img src="https://www.github.com/rmtrane/NpsychAssessmentTool/main/tree//tests/testthat/_snaps/mac-4.5/screenshots_for_intro/shinyApp-006.png" class="d-block  mx-auto border" width="100%"/>
</a>
<div class="carousel-caption d-none d-md-block">
<p class="fw-light">Scoring Tables and Figures. Diagnoses from previous visits are also made available.</p>
</div>
</div>
</div>
<button class="carousel-control-prev" type="button" data-bs-target="#gallery-carousel" data-bs-slide="prev">
<span class="carousel-control-prev-icon" aria-hidden="true"></span>
<span class="visually-hidden">Prevoius</span>
</button>
<button class="carousel-control-next" type="button" data-bs-target="#gallery-carousel" data-bs-slide="next">
<span class="carousel-control-next-icon" aria-hidden="true"></span>
<span class="visually-hidden">Next</span>
</button>
</div>

## Installation

You can install the development version of NpsychAssessmentTool from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("rmtrane/NpsychAssessmentTool")
```

or

``` r
# install.packages("remotes")
remotes::install_github("rmtrane/NpsychAssessmentTool")
```

Note that some features of the application mean additional resources.
For example, the R package
[`REDCapR`](https://ouhscbbmc.github.io/REDCapR/) is necessary to be
able to pull data straight from REDCap. To install the packages needed
to enable all features, use

``` r
pak::pak("rmtrane/NpsychAssessmentTool", dependencies = TRUE)
```

or

``` r
remotes::install_github("rmtrane/NpsychAssessmentTool", dependencies = TRUE)
```

## Example

You can start the application as follows:

``` r
library(NpsychAssessmentTool)
shinyAssessmentApp()
```

Alternatively, the app is also available as an RStudio add-in. I.e.,
once the package is installed, you should be able to launch it from the
RStudio add-in dropdown menu:

![Screenshot showing the RStudio add-in.](man/figures/rstudio-addin.png)
