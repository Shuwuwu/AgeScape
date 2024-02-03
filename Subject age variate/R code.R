### Assignment 1 R code

### Analysis 1
dataset <- read.csv('stat231f23dataset21035117.csv')

## 1b
chicago <- subset(dataset, subset = (city == "chicago"))
sf <- subset(dataset, subset = (city == "sf"))

sf$subject.race <- factor(sf$subject.race, levels = c("asian/pacific islander", "black", "hispanic", "white", "other"))

SFtable <- table(sf$subject.race, sf$subject.sex)
SFtable
prop.table(SFtable, 2)

## 1c bar
female <- SFtable[, "female"]
barplot(female, 
        xlab = "subject.race", ylab = "Frequency", las = 1,
        main = "Bar plot of subject.race for female", col = "red",
        ylim = c(0, 80), density = 40, angle = 45)

male <- SFtable[, "male"]
barplot(male, 
        xlab = "subject.race", ylab = "Frequency", las = 1,
        main = "Bar plot of subject.race for male", col = "blue",
        ylim = c(0, 150), density = 40, angle = 45)

## 1e table
gendertable <- subset(dataset)
gendertable$subject.race <- factor(dataset$city, levels = c("chicago", "sf"))

gendertable <- table(dataset$city, dataset$subject.sex)
gendertable
prop.table(gendertable, 1)

### Analysis 2
## 2b
plot(chicago$lng, chicago$lat, xlab = "lng", ylab = "lat",
     main = "Scatterplot of latitude vs. longtitude in Chicago", pch = 1, cex = 0.5, col = "navy", las = 1, cex.axis = 0.5)
plot(sf$lng, sf$lat, xlab = "lng", ylab = "lat",
     main = "Scatterplot of latitude vs. longtitude in San Francisco", pch = 1, cex = 0.5, col = "navy", las = 1, cex.axis = 0.5)

## 2c
cor(chicago$lat, chicago$lng)
cor(sf$lat, sf$lng)

## 2d
chicago.mean <- round(mean(chicago$lat), 3)
sf.mean <- round(mean(sf$lat), 3)

round(median(chicago$lat), 3)
round(median(sf$lat), 3)

chicago.sd <- round(sd(chicago$lat), 3)
sf.sd <- round(sd(sf$lat), 3)

# define skewness and kurtosis functions
skewness <- function(x) {(sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)}
kurtosis <- function(x) {(sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2}
round(skewness(chicago$lat), 3)
round(skewness(sf$lat), 3)
round(kurtosis(chicago$lat), 3)
round(kurtosis(sf$lat), 3)

## 2e
library(MASS)
# relative frequency histogram with Gaussian pdf
truehist(chicago$lat, xlab = "latitude in Chicago",
         ylab = "Relative Frequency",
         main = "Relative frequency histogram of latitude for Chicago", 
         las = 1, col = "dodgerblue3", density = 25, angle = 45)
curve(dnorm(x, chicago.mean, chicago.sd), col = "red", add = TRUE, lwd = 1.5)

truehist(sf$lat, xlab = "latitude in San Francisco",
         ylab = "Relative Frequency",
         main = "Relative frequency histogram of latitude for San Francisco", 
         las = 1, col = "dodgerblue3", density = 25, angle = 45, nbins = 10)
curve(dnorm(x, sf.mean, sf.sd), col = "red", add = TRUE, lwd = 1.5)

## 2f
# chicago
plot(ecdf(chicago$lat), xlab = "latitude in Chicago",
     main = "e.c.d.f. of latitude in Chicago",
     las = 1, lwd = 2, pch = NA)
curve(pnorm(x, chicago.mean, chicago.sd), col = "red", add = TRUE, lwd = 1.5, lty = 2)
quantile(chicago$lat, prob = 0.5)

# sf
plot(ecdf(sf$lat), xlab = "latitude in San Francisco",
     main = "e.c.d.f. of latitude in San Francisco",
     las = 1, lwd = 2, pch = NA)
curve(pnorm(x, sf.mean, sf.sd), col = "red", add = TRUE, lwd = 1.5, lty = 2)
quantile(sf$lat, prob = 0.5)
