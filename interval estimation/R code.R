### Assignment 2 R code


### Analysis 1
#1b
setwd("D:/Desktop/COURSE/2B/STAT231/A3")
dataset <- read.csv('stat231f23dataset21035117.csv')

n <- nrow(dataset)
n
thetahat <- mean(dataset$subject.age)
thetahat

#1c
## plot RLF
PoisRLF <- function(theta, n, thetahat) {exp(n * thetahat * log(theta/thetahat) + n * (thetahat - theta))}
theta <- seq(from = 35.5, to = 38, by = 0.001)
plot(theta, PoisRLF(theta, n, thetahat), main = "Possion relative likelihood function",
     xlab = expression(theta), ylab = expression(paste("R(",theta,")")),
     type = "l", lwd = 2,  las = 1)
## add horizontal line at 0.2 for 20% likelihood interval
abline(h = 0.15, col = "red", lwd = 2)

#1d
## find likelihood interval using uniroot()
fifteen_lower <- uniroot(function(x) PoisRLF(x, n, thetahat) - 0.15, lower = 36, upper = 36.5)$root # lower bound
fifteen_upper <- uniroot(function(x) PoisRLF(x, n, thetahat) - 0.15, lower = 37, upper = 37.5)$root # upper bound
fifteen_lower
fifteen_upper

#1e
## find quantile
a15 <- qnorm((1 + 0.15)/2)
a90 <- qnorm((1 + 0.90)/2)
a95 <- qnorm((1 + 0.95)/2)
## find approximate confidence interval
thetahat - a15*sqrt(thetahat/n) # lower bound
thetahat + a15*sqrt(thetahat/n) # upper bound
thetahat - a90*sqrt(thetahat/n) # lower bound
thetahat + a90*sqrt(thetahat/n) # upper bound
thetahat - a95*sqrt(thetahat/n) # lower bound
thetahat + a95*sqrt(thetahat/n) # upper bound

#1f
### equivalent confidence interval
pchisq(-2 * log(0.15), 1)

#1h
PoisRLF(39, n, thetahat)

### Analysis 2
#2b
chicago <- subset(dataset, subset = (city == "chicago"))
chitable <- table(chicago$subject.sex)
chitable
n <- nrow(chicago)
n

#2c
BiRLF <- function(theta, n, y, thetahat) 
{exp(y*log(theta/thetahat) + (n-y)*log((1-theta)/(1-thetahat)))}
n <- nrow(chicago)
y <- 137
thetahat <- 137/417
theta <- seq(0.2, 0.45, by = 0.001)

plot(theta, BiRLF(theta, n, y, thetahat), main = "Binomial relative likelihood function",
     xlab = expression(theta), ylab = expression(paste("R(", theta, ")")), type = "l",
     lwd = 2, las = 1)
abline(h=0.10, col=2, lwd=2)

uniroot(function(x) BiRLF(x, n, y, thetahat) - 0.1, lower = 0.25, upper = 0.30)$root
uniroot(function(x) BiRLF(x, n, y, thetahat) - 0.1, lower = 0.35, upper = 0.40)$root

#2d
q10 <- pchisq(-2 * log(0.1), 1)
q10
a10 <- qnorm((1 + q10)/2)
## find approximate confidence interval
thetahat - a10*sqrt((thetahat*(1-thetahat))/n) # lower bound
thetahat + a10*sqrt((thetahat*(1-thetahat))/n) # upper bound

### Analysis 3
#3b
mean_lat <- mean(chicago$lat) 
mean_lat
mean_lng <- mean(chicago$lng)
mean_lng
sd_lat <- sd(chicago$lat) 
sd_lng <- sd(chicago$lng)
qnorm(0.025, mean_lat, sd(chicago$lat))
qnorm(0.025, mean_lng, sd(chicago$lng))
qnorm(0.975, mean_lat, sd(chicago$lat))
qnorm(0.975, mean_lng, sd(chicago$lng))


#3c
###µt
a_lat <- qnorm((1 + 0.95) / 2)
mean_lat - a_lat * (sd_lat / sqrt(n)) # lower bound
mean_lat + a_lat * (sd_lat / sqrt(n)) # upper bound
###µg
a_lng <- qnorm((1 + 0.95) / 2)
mean_lng - a_lng *(sd_lng / sqrt(n)) # lower bound
mean_lng +a_lng * (sd_lng / sqrt(n)) # upper bound

#3f
###σt 
lowert <- qchisq((1 - 0.95) / 2, df = n - 1)
uppert <- qchisq((1 + 0.95) / 2, df = n - 1)
sqrt((n - 1) * sd_lat^2 / uppert) # lower
sqrt((n - 1) * sd_lat^2 / lowert) # upper

###σg
lowerg <- qchisq((1 - 0.95) / 2, df = n - 1)
upperg <- qchisq((1 + 0.95) / 2, df = n - 1)
sqrt((n - 1) * sd_lng^2 / upperg) # lower
sqrt((n - 1) * sd_lng^2 / lowerg) # upper



