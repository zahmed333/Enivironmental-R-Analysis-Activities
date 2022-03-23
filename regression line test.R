flower <- iris[iris$Species=='virginica',]

fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)
summary(fit)

plot(flower$Sepal.Length, flower$Petal.Length,
     main = "Iris virginica",
     xlab="Sepal Length", ylab="residuals",
     col = "purple",
     pch=16)


plot(flower$Sepal.Length, summary(fit)$residuals,
xlab="Sepal Length", ylab="Residuals", col = "purple",
pch=16)

abline(h=0,
       lty = "dashed")

hist(summary(fit)$residuals,
     main = "Regression Residuals",
     xlab="Residuals",
     pch=16)

shapiro.test(summary(fit)$residuals)

#qqplot 

qqnorm(summary(fit)$residuals, pch = 16)

qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, pch = 16)
