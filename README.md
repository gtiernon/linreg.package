  <!-- badges: start -->
  [![R-CMD-check](https://github.com/gtiernon/linreg.package/workflows/R-CMD-check/badge.svg)](https://github.com/gtiernon/linreg.package/actions)
  <!-- badges: end -->
  
  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/gtiernon/linreg.package/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gtiernon/linreg.package?branch=main)
  <!-- badges: end -->

# LINREG

This package was created for the BIOSTAT 625 course at the University of Michigan. It contains the linreg() function which fits a linear regression model to a dataset in R. The goal is to match the output of the already existing lm() function in R. 

## Background

In the field of statistics, linear regression is a way to model the relationship between an outcome and one or more response variables. If there are more than one response variables, we call this multiple linear regression. This form of analysis predicts the value of the outcome (dependent variable) based on the response (independent variable). The result is a straight line that is fitted to the data. 

## Usage

In order to use this function, real or simulated data is needed. The user needs to input a formula that contains the desired outcome and predictor(s) as well as the dataset that the variables come from. 

## Example

<img width="699" alt="demo" src="https://user-images.githubusercontent.com/105001724/203406278-62fc5eae-f091-4abc-b2bf-c986e7838375.png">

## Comparison to lm()

As mentioned above, the goal is to match the output of the already exisitng lm() function. Using the same data and model as the above example, one can see that the lm() function returns the same call and coefficients.

<img width="696" alt="compare" src="https://user-images.githubusercontent.com/105001724/203407496-1fcbddcf-4b67-49a0-a4bd-9db57463db88.png">

## Graph of Fitted Line

Using the "swiss" dataset in R as demonstrated above, here's an example of the line that the linreg() function fits. The variables fertility and education are plotted, and the predicted values generated by linreg() are used to plot a line on top. This graph models the relationship of interest. Specifically, that fertility and education have a linearly negative relationship because as education increases, fertility decreases. 

![scatterplot](https://user-images.githubusercontent.com/105001724/203403243-e1a8417e-16df-47fc-ae77-5d645cbe410e.jpeg)

## Installation

```{r}
install.packages("linreg")
```
