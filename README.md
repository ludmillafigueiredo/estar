
<!-- README.md is generated from README.Rmd. Please edit only the Rmd file -->

# eStar <img class="resize" src="man/figures/logo.png" width="200" align="right" />

<!-- badges: start -->
<!-- badges: end -->

`eStar` provides standardized functions to measure the following metrics
of stability:

- Resistance
- Extent of recovery
- Rate of recovery
- Invariability
- Persistences
- Maximal amplification
- Time to maximal amplification
- Robustness
- Asymptotic resilience
- Initial resilience
- Intrinsic stochastic invariability

## Installation

<!-- You can install the released version of eStar from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("eStar") -->
<!-- ``` -->

You can install the released version of eStar from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ludmillafigueiredo/eStar")
```

`estar` provides two sets of functions. The first set corresponds to
functions that can be applied to univariate data, i.e., a time series of
a system’s state variable (e.g., individual body mass). This set of
metrics includes invariability, resistance, extent and rate of recovery,
and persistence. The second set of functions can be applied to
multivariate data represented by the time series of the abundances of
all species in a community. The functions in this set measure the
stability of a community at the short and the long time scales. In the
short term, stability is measured as a perturbation’s maximal
amplification, as well as the system’s reactivity and its initial
resilience (i.e. its initial rate of return to equilibrium). In the long
term, stability can be measured as the system’s asymptotic resilience
and intrinsic stochastic invariability.

The package includes two vignettes demonstrating the use of all
functions, as well as a brief introduction to multivariate
autoregressive state-space models necessary for the second set of
metrics.

<!-- ## Example -->
<!-- This is a basic example which shows you how to solve a common problem: -->
<!-- ```{r example} -->
<!-- library(eStar) -->
<!-- ## basic example code -->
<!-- ``` -->
<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub! -->
