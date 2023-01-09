resultx <- c()
resulty <- c()

alpha <- 0.95
beta <- 0.99

plotobs <- c()
talphas <- c()
tbetas <- c()
count <- 0
T0 <- 0
talpha <- 0
tbeta <- 0


while (count < 5 | (T0 < talpha & T0 > tbeta )) {
varx <- 1
vary <- 1

mux <- 10.25
muy <- 10

count <- count + 1

obsx <- rnorm(1,mux,varx)
obsy <- rnorm(1,muy,vary)

resultx <- rbind(resultx, obsx)
resulty <- rbind(resulty, obsy)

Sdx <- sd(resultx)
Sdy <- sd(resulty)

apV <- ((Sdx^2)/count+(Sdy^2)/count)^0.5

xbar <- mean(resultx)
ybar <- mean(resulty)

num <- ((Sdx^2)/count+(Sdy^2)/count)^2
denom <- (((Sdx^2)/count)^2/(count-1) + ((Sdy^2)/count)^2/(count-1))
df <- num/denom

talpha <- qt(alpha,count)
tbeta <- qt(1-beta,count)

talphas <- rbind(talphas,talpha)
tbetas <- rbind(tbetas,tbeta)

T0 <- (xbar - ybar) / apV
plotobs <- rbind(plotobs,T0)
}

plotobs[0] <- 0

plot(plotobs, ylim = c(-5,5), main = "Number of Observations for Sequential Analysis", ylab = "T-score", xlab = "Iteration")
lines(talphas)
lines(tbetas)
