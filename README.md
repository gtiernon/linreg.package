# LINREG

This package was created for the BIOSTAT 625 course at the University of Michigan. It contains the linreg() function which fits a linear regression model to a dataset in R. The goal is to match the output of the already existing lm() function in R. 

## Background

In the field of statistics, linear regression is a way to model the relationship between an outcome and one or more response variables. If there are more than one response variables, we call this multiple linear regression. This form of analysis predicts the value of the outcome (dependent variable) based on the response (independent variable). The result is a straight line that is fitted to the data. 

## Usage

In order to use this function, real or simulated data is needed. The user needs to input a formula that contains the desired outcome and predictor(s) as well as the dataset that the variables come from. 

## Example

```{r}
data("swiss)
dat = swiss
linreg(formula = Fertility ~ Education, data = dat)
```

<img width="699" alt="demo" src="https://user-images.githubusercontent.com/105001724/203406278-62fc5eae-f091-4abc-b2bf-c986e7838375.png">

## Comparison to lm()

As mentioned above, the goal is to match the output of the already exisitng lm() function. Using the same data and model as the above example, one can see that the lm() function returns the same call and coefficients.

```{r}
lm(formula = Fertility ~ Education, data = dat)
```

<img width="696" alt="compare" src="https://user-images.githubusercontent.com/105001724/203407496-1fcbddcf-4b67-49a0-a4bd-9db57463db88.png">

## Graph of Fitted Line

Using the "cars" dataset in R, here's an example of the line that the linreg() function fits. The variables speed and distance are plotted, and the predicted values generated by linreg() are used to plot a line on top. This graph models a positive, linear relationship speed and distance. Specifically, as distance increases, speed increases. 

```{r}
#import test data from R
data("cars")
dat1 = cars

#fit the model
fit = linreg(speed ~ dist, dat1)

#plot a scatter plot of height vs weight
plot(speed ~ dist, dat1)

#add the regression line to the plot
abline(fit$Coefficients)
```

<img width="574" alt="cars_plot" src="https://user-images.githubusercontent.com/105001724/203465719-1d270a4c-9bd3-4ea3-84c1-19e384ebc6ee.png">

## Installation

To install, copy and paste the following into your RStudio console. 

```{r}
install.packages("linreg")
```
