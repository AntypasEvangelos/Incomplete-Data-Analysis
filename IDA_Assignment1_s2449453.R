## This file contains the solution for the first assignment in
## Incomplete Data Analysis(MATH11185) by Evangelos Antypas (s2449453)
##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------
## Question 3
##-----------------------------------------------------------------------------
## Part 3a

## Seed for reproducibility
set.seed(1)

## Sample size
n <- 500

## We have that Z1,Z2,Z3 are i.i.d standard normal variables
## Simulation
Z1 <- rnorm(n)
Z2 <- rnorm(n)
Z3 <- rnorm(n)


## We obtain Y1,Y2
## Complete data set 
Y1 <- 1 + Z1
Y2 <- 5 + 2*Z1 + Z2

## We will now impose missingness on Y2 based on the condition ::
## a(Y1-1) + b(Y2-5) + Z3 < 0, a,b are given parameters
a <- 2
b <- 0 

## Defining the condition
cond <- a*(Y1-1) + b*(Y2-5) + Z3

## Boolean vector
cond <- (cond<0)

## Obtaining the observed Y2, i.e. after the missing values have been imposed
Y2_obs <- Y2[cond==FALSE]

## Visualization of the marginal of Y2 for complete and observed data
plot(density(Y2), lwd = 2, col = 'blue', xlab = 'Data values',
main = 'Density of Y2: Observed vs Complete Data', ylim = c(0,0.3))
lines(density(Y2_obs), lwd = 2, col = "red")

legend('topright',legend = c('Observed data', 'Complete data'), 
       col = c('red','blue'), lty = c(1,1), lwd = c(2,2),bty ="n")
##-----------------------------------------------------------------------------
## Part 3b
## We perform imputation by stochastic regression
## First we fit a linear regression model and then we add some random noise

## Y2 with missing values
Y2_mis <- Y2
Y2_mis[cond] <- NA

## Data set
data <- data.frame(Y1,Y2_mis)

## Linear Regression
fit <- lm(Y2_mis~Y1, data = data)

## Creating the predictions and adding noise
preds <- predict(fit, newdata = data) + rnorm(nrow(data), 0, sigma(fit))

## Creating the completed Y2 by adding the predictions where we had NAs
Y2_comp <- ifelse(is.na(data$Y2_mis), preds, Y2_mis)


