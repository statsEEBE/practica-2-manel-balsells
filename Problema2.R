#Codigo para problema 2
mis_dades <- iris
x <- mis_dades$Petal.Length
mean(x) #media
sd(x) #desviasio tipica
hist(x)

y <- mis_dades$Sepal.Length
mean(y)

plot(x,y)

m <- sum((x-mean(x))*(y-mean(y))) / sum((x-mean(x))^2)
m
b <- mean(y) - m*mean(x)
b
m*1.5+b

######

mod <- lm(y~x) #modelo lineal que t'ensenya les coses mes importants
mod
summary(mod)

data.frame(x=x)

ypred <- predict(mod, data.frame(x=x))
plot(x,y)
lines(x, ypred)
dev.off()
Rsq <- sum((ypred-mean(y))^2)/sum((y-mean(y))^2) #capacitat de predecir el modelo
Rsq
