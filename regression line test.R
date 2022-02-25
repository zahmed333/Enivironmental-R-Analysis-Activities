flower <- iris[iris$Species=='virginica',]

fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)

summary(fit)

plot(flower$Sepal.Length, summary(fit)$residuals,
xlab="Sepal Length", yla="residuals", ps=0)