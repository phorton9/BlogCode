library(quantreg)
library(ggplot2)
library(quantregForest)
library(stats)

## Set alpha value
alpha <- 0.1

##Generate data
generator <- function(x) {
  output <- rep(0, length(x))
  for (i in 1:length(x)) {
    output[i] <- rpois(1,sin(x[i])^2+0.1)+0.03*x[i]*rnorm(1)+25*(runif(1,0,1)<0.01)*rnorm(1)
  }
  output
}

x_train <- runif(2000,1,5)
y_train <- generator(x_train)
x_test <- runif(5000,1,5)
y_test <- generator(x_test)

x_train <- as.data.frame(x_train)
x_test <- as.data.frame(x_test)
colnames(x_train) <- c('X')
colnames(x_test) <- c('X')

##Train quantile regressor
rf <- quantregForest(x=x_train, y=y_train, ntree = 500, maxnodes = 5)

## Quantiles for 1-alpha interval
interval_low<-predict(rf, x_test, what=alpha/2)
interval_high<-predict(rf, x_test, what=1-alpha/2)


interval_low <- c(interval_low)
##Plot testing data with quantiles.
sample_data <- data.frame(x=x_test$X,
                          low = interval_low,
                          high = interval_high,
                          y = y_test)
sample_data <- sample_data[order(sample_data$x),]


plot(sample_data$x, sample_data$y,main = 'Test Data with Original Interval')
points(sample_data$x, sample_data$low, col = 'red')
points(sample_data$x, sample_data$high, col = 'green')

##Plot with new y-axis
plot(sample_data$x, sample_data$y,main = 'Test Data with Original Interval', ylim = c(-2,6))
points(sample_data$x, sample_data$low, col = 'red')
points(sample_data$x, sample_data$high, col = 'green')

##Calculate residuals relative to quantiles
sample_data$E <- pmax(sample_data$low - sample_data$y,sample_data$y - sample_data$high)

##Calculate Q coefficient for 1-alpha coverage
Q <- (1-alpha) * (1+1/1000)*quantile(sample_data$E, probs = c(1-alpha))

##New prediction intervals
sample_data$lowQ <- sample_data$low - Q
sample_data$highQ <- sample_data$high + Q


##Plot test data with prediction interval
plot(sample_data$x, sample_data$y, main = "Test Data With Updated Prediction Interval",ylim = c(-2,6),  xlab = "X", ylab = "Y")
lines(sample_data$x, sample_data$lowQ, col = 'red', lwd=4.0)
lines(sample_data$x, sample_data$highQ, col = 'green', lwd=4.0)


##Percent of test observations outside of 
mean((sample_data$y < sample_data$low) | (sample_data$y > sample_data$high))*100

##Percent of test observations outside of new prediction interval
mean((sample_data$y < sample_data$lowQ) | (sample_data$y > sample_data$highQ))*100

##
x_train$oldQLow <- predict(rf, x_train, what=alpha/2)
x_train$oldQHigh <- predict(rf, x_train, what=1-alpha/2)

newQLow<-predict(rf, x_train, what=alpha/2)-Q
newQHigh<-predict(rf, x_train, what=1-alpha/2)+Q

x_train$newQLow <- newQLow
x_train$newQHigh <- newQHigh
x_train$y <- y_train

plot(x_train$X, y_train, main = "Train Data with Updated Boundary",ylim = c(-2,6))
points(x_train$X, newQLow, col = 'red')
points(x_train$X, newQHigh, col = 'green')

##Percent of train observations outside of old prediction interval
mean((y_train < x_train$oldQLow) | (y_train > x_train$oldQHigh))*100

##Percent of train observations outside of new prediction interval
mean((y_train < newQLow) | (y_train > newQHigh))*100


#################################################
#### Coverage check over N iterations
iters <- 100
results <- rep(0,iters)
resultsUnAdj <- rep(0, iters)
for (i in 1:iters){
n <- 1000
x_calib <- runif(n,1,5)
y_calib <- generator(x_calib)


x_valid <- runif(n,1,5)
y_valid <- generator(x_valid)

x_valid <- as.data.frame(x_valid)
colnames(x_valid) <- c('X')
x_calib <- as.data.frame(x_calib)
colnames(x_calib) <- c('X')

interval_low<-predict(rf, x_calib, what=0.05)
interval_high<-predict(rf, x_calib, what=0.95)


interval_low <- c(interval_low)
##Plot testing data with quantiles.
sample_data <- data.frame(x=x_calib$X,
                          low = interval_low,
                          high = interval_high,
                          y = y_calib)
sample_data <- sample_data[order(sample_data$x),]


##Calculate residuals relative to quantiles
sample_data$E <- pmax(sample_data$low - sample_data$y,sample_data$y - sample_data$high)

##Calculate Q coefficient for alpha
Q <-(1- alpha) * (1+1/n)*quantile(sample_data$E, probs = c(1-alpha))

##New prediction intervals
sample_data$lowQ <- sample_data$low - Q
sample_data$highQ <- sample_data$high + Q


newQLow<-predict(rf, x_valid, what=0.05)-Q
newQHigh<-predict(rf, x_valid, what=0.95)+Q

newLow<-predict(rf, x_valid, what=0.05)
newHigh<-predict(rf, x_valid, what=0.95)


##Percent of train observations outside of new prediction interval
results[i] <- mean((y_valid < newQLow) | (y_valid > newQHigh))*100
resultsUnAdj[i] <- mean((y_valid < newLow) | (y_valid > newHigh))*100

print(i)
}

hist(results, main = "Histogram of Coverage by Iteration", xlab = "Coverage (Percent)")
mean(results)
results

hist(resultsUnAdj)
mean(resultsUnAdj)

