
<!-- README.md is generated from README.Rmd. Please edit Rmd only file -->

# estar <img class="resize" src="man/figures/logo.png" width="200" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/ludmillafigueiredo/estar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ludmillafigueiredo/estar/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ludmillafigueiredo/estar/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ludmillafigueiredo/estar?branch=main)
<!-- badges: end -->

`estar` provides two sets of functions.

The first set corresponds to functions that measure stability at any
level of organisation, from individual to community and can be applied
to a time series of a system’s state variables (e.g., body mass,
population abundance, or species diversity). The properties included in
this set are:

- Resistance
- Extent of recovery
- Rate of recovery
- Invariability
- Persistence
- Overall Ecological Vulnerability

The second set of functions can be applied to Jacobian matrices. The
functions in this set measure the stability of a community at short and
long time scales. In the short term, you can measure:

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
