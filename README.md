
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EWSmethods

<!-- badges: start -->
<!-- badges: end -->

*EWSmethods* is a user friendly interface to various methods of
performing Early Warning Signal (EWS) assessments. This R package allows
the user to input univariate or multivariate data and perform either
traditional rolling window or expanding window EWS approaches.
Publication standard and ggplot inspired figures can also be generated
during this process. *EWSmethods* also provides an R interface to
[**EWSNET**](https://ewsnet.github.io), a deep learning modelling
framework for predicting critical transitions.

## Installation

You can install the development version of EWSmethods from
[GitHub](https://github.com/duncanobrien/EWSmethods) with:

``` r
# install.packages("devtools")
#devtools::install_github("duncanobrien/EWSmethods")
```

## Examples

Imagine we have 50 years of monitoring data for a local population of
skylarks (*Alauda arvensis*) as well as mean body mass data. We could
calculate either rolling or expanding window EWSs for the abundance data
as would do so using EWSmethods as below:

``` r
library(EWSmethods)
set.seed(125) #seed to ensure reproducible results

skylark_data <- data.frame(time = seq(1:50), abundance = rnorm(50,mean = 100,sd=20), trait = rnorm(50,mean=40,sd=5)) #dummy skylark dataset

ews_metrics <- c("SD","ar1","skew") #the early warning signal metrics we wish to compute

roll_ews <- univariate_EWS_wrapper(data = skylark_data[,1:2],metrics =  ews_metrics,method = "rolling",winsize = 50, ggplotIt =TRUE, y_lab = "Skylark abundance") #lets using a rolling window approach

print(roll_ews$EWS$cor) #return the Kendall Tau correlations for each EWS metric
#>            SD        ar1       skew
#> tau 0.5446154 -0.3907692 -0.7046154
```

We can then use the resulting figures to identify oncoming transitions.
In this case, we expect no transition as the data is randomly sampled
from a normal distribution and this is evident in the Kendall Tau
values, with no strong positive correlation with time:

<img src="man/figures/README-rolling_plot-1.png" width="100%" />
Alternatively, we may be more interested in expanding windows as that
approach standardises the changing EWS metrics over time and therefore
allows the strength of multiple signals to be combined. We could achieve
this using the following code:

``` r
exp_ews <- univariate_EWS_wrapper(data = skylark_data[,1:2],metrics =  ews_metrics,method = "expanding",burn_in = 10, threshold = 2, tail.direction = "one.tailed", ggplotIt =TRUE, y_lab = "Skylark abundance") #lets use a rolling window approach
#> Warning: Removed 7 row(s) containing missing values (geom_path).
#> Warning: Removed 7 rows containing missing values (geom_point).

head(exp_ews$EWS) #return the head of the EWS dataframe
#>   time metric.score metric.code rolling.mean rolling.sd threshold.crossed
#> 1   10    0.0000000         ar1    0.0000000         NA                 0
#> 2   11   -0.7071068         ar1   -0.3535534  0.5000000                 0
#> 3   12   -0.9223669         ar1   -0.5431579  0.4825449                 0
#> 4   13   -0.1500572         ar1   -0.4448827  0.4403012                 0
#> 5   14    0.8428688         ar1   -0.1873324  0.6906950                 0
#> 6   15   -1.8668306         ar1   -0.4672488  0.9229121                 0
#>   count.used        str
#> 1  108.91249         NA
#> 2   93.56813 -0.7071068
#> 3  109.56960 -0.7858522
#> 4  103.92341  0.6695997
#> 5  114.29655  1.4915428
#> 6   80.79726 -1.5164845
```

And again, we can then use the resulting figures to identify oncoming
transitions. Whilst we have some trangressions of the 2$threshold, we
only consider these signals “warnings” if two or more consecutive
signals are identified (Clements *et al.* 2019).

<img src="man/figures/README-expanding_plot-1.png" width="100%" /> A
second benefit of the expanding window approach is that additional
information can be used to improve the reliability of the assessment.
For example, in our hypothetical skylark dataset, we have measured
average population body mass. This data can then be delivered to
*EWSmethods* using the `trait` argument.

``` r
trait_metrics <- c("SD", "ar1", "trait")
exp_ews_trait <- univariate_EWS_wrapper(data = skylark_data[,1:2],metrics =  trait_metrics,method = "expanding",burn_in = 10, threshold = 2, tail.direction = "one.tailed", ggplotIt =TRUE, y_lab = "Skylark abundance",
trait = skylark_data$trait, trait_lab = "Body mass (g)", trait_scale = 5)
#> Warning: Removed 7 row(s) containing missing values (geom_path).
#> Warning: Removed 7 rows containing missing values (geom_point).
```

<img src="man/figures/README-expand_trait_example-1.png" width="100%" /><img src="man/figures/README-expand_trait_example-2.png" width="100%" />

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.
