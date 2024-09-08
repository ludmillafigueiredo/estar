
<!-- README.md is generated from README.Rmd. Please edit Rmd only file -->

# estar <img class="resize" src="man/figures/logo.png" width="200" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/ludmillafigueiredo/estar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ludmillafigueiredo/estar/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ludmillafigueiredo/estar/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ludmillafigueiredo/estar?branch=main)
<!-- badges: end -->

`estar` provides two sets of functions.

The first set corresponds to functions that can be applied to univariate
data, i.e., a time series of a system’s state variable (e.g., individual
body mass). This set of metrics includes:

- Resistance
- Extent of recovery
- Rate of recovery
- Invariability
- Persistence

The second set of functions can be applied to multivariate data
represented by the time series of the abundances of all species in a
community. The functions in this set measure the stability of a
community at the short and the long time scales. In the short term,
stability is measured as:

- Reactivity
- Maximal amplification
- Initial resilience

In the long term, stability can be measured as:

- Asymptotic resilience
- Intrinsic stochastic invariability

The package includes two vignettes demonstrating the use of all
functions, as well as a brief introduction to multivariate
autoregressive state-space models necessary for the second set of
metrics.

<!-- You can install the released version of estar from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("estar") -->
<!-- ``` -->
