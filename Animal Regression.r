# Look at the data
data(Animals, package = "MASS")
Animals
plot(Animals)

# The measurements are highly skewed and do not follow a linear relationship.

# Simple logarithmic linear regression model

LogAnimals <- log(Animals)
m1 <- lm(brain ~ body, data = LogAnimals)
summary(m1)
plot(LogAnimals, col = 4)
abline(m1, lty = 2)
par(mfrow = c(1, 2))
plot(m1, 1:2)

# Dinosaurs are outliers, after omitting them:
LogAnimals2 <- LogAnimals[-c(6, 16, 26), ] 
m2 <- lm(brain ~ body, data = LogAnimals2)
summary(m2)
plot(LogAnimals, col = 4)
abline(m2, lty = 2)


# 95% Confidence Interval
newdata = data.frame(body = log(500))
camelConfLog <- predict(m2, newdata, interval = "confidence")
camelConfLog
exp(camelConfLog[-1])