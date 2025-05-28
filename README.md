
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NpsychAssessmentTool

<!-- badges: start -->

[![R-CMD-check](https://github.com/rmtrane/NpsychAssessmentTool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rmtrane/NpsychAssessmentTool/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package provides an [R Shiny](https://shiny.posit.co) application
used for diagnosis cognitive status based on neuropsychological scores.

## Installation

You can install the development version of NpsychAssessmentTool from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("rmtrane/NpsychAssessmentTool")
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

<figure>
<img src="img/rstudio-addin.png"
alt="Screenshot showing the RStudio add-in." />
<figcaption aria-hidden="true">Screenshot showing the RStudio
add-in.</figcaption>
</figure>
