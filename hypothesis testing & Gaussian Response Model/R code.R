### Assignment 3 R code
setwd("D:/Desktop/COURSE/2B/STAT231/A3")
dataset <- read.csv('stat231f23dataset21035117.csv')
chicago <- subset(dataset, subset = (city == "chicago"))

### Analysis 1
#1c
n <- nrow(chicago)
n
theta0 <- 0.513
thetahat <- 137/n
thetahat

# test statistic:
lambda <- (-2*log((theta0/thetahat)^(n*thetahat)*((1 - theta0)/(1 - thetahat))^(n - n*thetahat)))
lambda
# p-value:
w <- pchisq(lambda, df = 1)
pvalue <- 1 - w
pvalue

### Analysis 2
#2b
# d
mean_lat <- mean(chicago$lat) 
mean_lat
sd_lat <- sd(chicago$lat) 
sd_lat
# test statistic:
test_lat <- abs(41.8781 - mean_lat) / (sd_lat/sqrt(n))
test_lat
# p-value:
pvalue_lat <- 2 *(1 - pt(test_lat, n - 1))
pvalue_lat

### Analysis 3
# 3b
chicago$lat <- chicago$lat
chicago$lng <- chicago$lng
summary(chicago$lat)
summary(chicago$lng)

plot(chicago$lng, chicago$lat)

mod <- lm(lat ~ lng, data = chicago)
abline(coef(mod), lwd = 2, lty = 2, col = "red")
coef(mod)

confint(mod, level = 0.95)

# 3c
sigma(mod)

# 3f
plot(chicago$lng, stdres, main = "Std residuals vs. longitude", 
     xlab = "longitude", ylab = "Standardized Residuals", pch = 1, 
     col = "navy", cex = 0.5, las = 1)
qqnorm(stdres, main = "qqplot of std residuals", xlab = "G(0, 1) Quantiles", 
       ylab = "Standardized Residuals", pch = 1, col = "navy", cex = 0.5)

# 3h
predict(mod, newdata = data.frame(lng = -100), interval = "prediction", level = 0.95)

# 3i
summary(mod)

### Analysis 4
# 4b
chicago$subject.age.log <- log(chicago$subject.age)
chicago$female.binary <- as.numeric(chicago$subject.sex == "female")

chifemale <- subset(chicago, subset = (female.binary == 1))
chimale <- subset(chicago, subset = (female.binary == 0))

dim(chifemale)
dim(chimale)
summary(chifemale$subject.age.log)
summary(chimale$subject.age.log)

sd(chifemale$subject.age.log)
sd(chimale$subject.age.log)

# 4c
femaleage <- chifemale$subject.age.log
maleage <- chimale$subject.age.log
femalenum <- nrow(chifemale)
malenum <- nrow(chimale)

# calculation of pooled standard deviation estimate
test.result <- t.test(maleage, femaleage, var.equal = TRUE)
test.result
test.result$p.value

sqrt(((femalenum - 1) * (sd(chifemale$subject.age.log))^2 + 
        (malenum - 1) * (sd(chimale$subject.age.log))^2)/(nrow(chicago) - 2))