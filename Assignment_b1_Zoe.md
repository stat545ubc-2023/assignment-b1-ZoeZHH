STAT545B - Assignment 1
================
Hehan (Zoe) Zhang
2023-11-03

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(datateachr)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.2

    ## Warning: package 'ggplot2' was built under R version 4.1.2

    ## Warning: package 'tibble' was built under R version 4.1.2

    ## Warning: package 'tidyr' was built under R version 4.1.2

    ## Warning: package 'readr' was built under R version 4.1.2

    ## Warning: package 'purrr' was built under R version 4.1.2

    ## Warning: package 'stringr' was built under R version 4.1.2

    ## Warning: package 'forcats' was built under R version 4.1.2

    ## Warning: package 'lubridate' was built under R version 4.1.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.4
    ## ✔ ggplot2   3.4.2     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.2     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.1     ✔ tidyr     1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

``` r
library(ggplot2)
```

My function idea: When I did the mini data analysis 2 on examining the
relationships between 2 variables by using apt_buildings dataset, I keep
producing the figure between two numeric variables and checking if
there’s a linear relationship between them, thus I would like make a
plot function on examining 2 variables’ linear relationship.

``` r
# Exercise 1 & Exercise 2
#' Title: Linear Relationship Plot between Two Variables
#'
#' Function Description: Quickly generates a scatter plot to examine the linear relationship between two specified variables in a given data frame. It fits a linear model (lm) to the data and adds it to the plot.
#'
#' @param df A data frame containing the variables to be plotted, the abbreviation of dataframe is always used as df.
#' @param x_var The name of the independent variable column in the data frame as a string, dependent variable is always represented as x variable in statistics, so we get the abbreviation here, as x_var.
#' @param y_var The name of the dependent variable column in the data frame as a string, dependent variable is always represented as y variable in statistics, so we get the abbreviation here, as y_var.
#' @param na.rm Logical. Whether to remove rows with missing values (NA) in the specified columns before plotting. I set the default as FALSE.
#' @param ... Additional arguments passed to geom_smooth() function for customization.
#'
#' @return A ggplot object representing the scatter plot with a linear fit. The plot can be further modified or directly displayed.

lm_check <- function (df, x_var, y_var, na.rm = FALSE, ...) {
  # Check if variables exist in the dataframe
  if (!(x_var %in% names(df)) || !(y_var %in% names(df))) {
    stop("Variables not found in the dataframe.")
  }
  # Check for zero-length data
  if (nrow(df) == 0) {
    stop("data must have at least 1 row to plot")
  }
  # Prepare data, remove missing values if na.rm is TRUE
  data_to_plot <- df
  if (na.rm) {
    data_to_plot <- na.omit(data_to_plot[, c(x_var, y_var)])
  }
  # Create the plot
  plot <- ggplot(data_to_plot, aes (x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue", se = FALSE, ...) +
    theme_minimal() 
  return(plot)
}
```

``` r
# Tidy the apt_buildings dats a little bit for a more constrained plot
apt_buildings <- apt_buildings %>% 
  filter(no_of_units <= 1000)
```

``` r
# Exercise 3 (Example 1) - check the linear relationship between no_of_units and no_of_storeys in apt_buildings dataset and fit a linear regression line, we can see that there's a positive relationship between no_of_units and no_of_storeys. 
check1 <- lm_check(df = apt_buildings, x_var = "no_of_units", y_var = "no_of_storeys", na.rm = TRUE)
print(check1)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Assignment_b1_Zoe_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Exercise 3 (Example 2) - check the relationship between no_of_units and no_of_elevators in apt_buildings dataset and fit a linear regression line, we can see that there's a positive relationship between no_of_units and no_of_elevators. 
check2 <- lm_check(df = apt_buildings, x_var = "no_of_units", y_var = "no_of_elevators", na.rm = TRUE)
print(check2)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Assignment_b1_Zoe_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# Exercise 4
# Test 1: Check that the function returns a ggplot object with valid input and no NAs
test_that("lm_check returns a ggplot object with no NAs", {
  data <- data.frame(x = 1:10, y = 1:10) # Vector with no NAs
  expect_is(lm_check(data, "x", "y"), "ggplot", 
            info = "lm_check should return a ggplot object with valid input and no NAs.")
})
```

    ## Test passed 🥳

``` r
# Test 2: Check that the function still works and omits NAs when na.rm is TRUE
test_that("lm_check handles NAs correctly when na.rm is TRUE", {
  data_with_na <- data.frame(x = c(1:5, NA, 7:10), y = c(1:5, NA, 7:10)) # Vector with NAs
  plot <- lm_check(data_with_na, "x", "y", na.rm = TRUE)
  expect_is(plot, "ggplot", 
            info = "lm_check should handle NAs correctly when na.rm is TRUE.")
  expect_equal(sum(is.na(data_with_na$x)), 1, 
               info = "There should be exactly one NA in the test data.")
})
```

    ## Test passed 😸

``` r
# Test 3: Check for correct error message when supplied with a vector of length 0
test_that("lm_check handles vector of length 0 correctly", {
  data_empty <- data.frame(x = numeric(0), y = numeric(0)) # Vector of length 0
  expect_error(lm_check(data_empty, "x", "y"), 
               "data must have at least 1 row to plot", 
               info = "lm_check should error with informative message when vector length is 0.")
})
```

    ## Test passed 🥇
