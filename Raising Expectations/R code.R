### Assignment 2 R code

### Analysis 1
#1c
setwd("D:/Desktop/COURSE/2B/STAT231/A2")
dataset <- read.csv('stat231f23dataset21035117.csv')
citytable <- table(dataset$city)
citytable

chicago <- subset(dataset, subset = (city == "chicago"))
chitable <- table(chicago$subject.sex)
chitable

sf <- subset(dataset, subset = (city == "sf"))
sftable <- table(sf$subject.sex)
sftable

#1d
n <- nrow(chicago)
thetahat <- 137/417
theta <- seq(thetahat-0.1, thetahat+0.1, by = 0.001)
y <- 137
BiRLF <- function(theta, n, y, thetahat) 
  {exp(y*log(theta/thetahat) + (n-y)*log((1-theta)/(1-thetahat)))}
plot(theta, BiRLF(theta, n, y, thetahat), xlab = expression(theta),
     ylab = expression(paste("R(", theta, ")")), type = "l", lwd = 2,
      main = "Binomial relative likelihood function for Chicago", las = 1)

n <- nrow(sf)
thetahat <- 130/466
theta <- seq(thetahat-0.1, thetahat+0.1, by = 0.001)
y <- 130
BiRLF <- function(theta, n, y, thetahat) 
{exp(y*log(theta/thetahat) + (n-y)*log((1-theta)/(1-thetahat)))}
plot(theta, BiRLF(theta, n, y, thetahat), xlab = expression(theta),
     ylab = expression(paste("R(", theta, ")")), type = "l", lwd = 2,
     main = "Binomial relative likelihood function in sf", las = 1)


### Analysis 2
#2b
n <- nrow(dataset)
mean <- mean(dataset$subject.age)
median <- median(dataset$subject.age)
sd <- sd(dataset$subject.age)
n 
mean 
median 
sd

#2c
library(MASS)

truehist(dataset$subject.age,
         main="Relative frequency histogram of subject.age",
         xlab="Age(years)",ylab="Relative Frequencylas",
         col="dodgerblue3",
         density=25, angle=4, xlim=c(0,100), ylim=c(0,0.04))
curve(dexp(x, rate=(1/mean)),col=2, add=TRUE, lwd=1.5)

#2d
plot(ecdf(dataset$subject.age), xlab = "age(years)",
     main = "e.c.d.f. of of subject.age",
     las = 1, lwd = 2, pch = NA)
curve(pexp(x, rate=(1/mean)), col = "red", add = TRUE, lwd = 1.5, lty = 2)

#2f
qqnorm(dataset$subject.age, pch = 1, cex = 0.5)
qqline(dataset$subject.age, col = "red", lwd = 2, lty = 2)



### Analysis 3
#3c
Pomle <- mean
Pomle

#3d
probabilitymle <- ppois(30, Pomle)
probabilitymle 

#3e
PoisRLF <- function(theta, n, thetahat) {
  exp(n * thetahat * log(theta/thetahat) + n * (thetahat -
                                                  theta))
}
R39 <- PoisRLF(39, n, Pomle)
R39


### Analysis 4
#4c
SFtable <- table(sf$subject.race)
SFtable

sfpop <- nrow(sf)
expected <- c(0.378, 0.057, 0.159, 0.027, 0.379)*sfpop
expected

#4d
EXtable <- table(c("asian/pacific islander", "black", "hispanic", "white", "other"))
EXtable[1:5] <- expected

both <- t(cbind(SFtable, EXtable))

barplot(both, beside=T)

#4e
boxplot(dataset$subject.age ~ factor(dataset$subject.race, 
        levels = c("asian/pacific islander", "black", "hispanic","other", "white")),
        col=c("red", "orange", "yellow", "green4","blue"),
        xlab="subject.race", ylab = "age", main = "age of subject.race")
