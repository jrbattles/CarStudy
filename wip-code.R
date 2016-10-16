## Motor Trends Car Study Work in Progress Sample Code
## Jason R. Battles
data(mtcars)
mtcars.orig <- mtcars
library(ggplot2)
library(corrplot)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))
str(mtcars)

## hypothesize a simple linear regression model and analyze

fit1 <- lm(mpg ~ factor(am), data = mtcars)
summary(fit1)

## find a better model
init_model <- lm(mpg ~ ., data = mtcars)
best_model <- step(init_model, direction = "both")

## propose a better model
fit6 <- lm(mpg ~ wt + cyl + disp + drat + vs + factor(am), data = mtcars)
summary(fit6)
fit4 <- lm(mpg ~ cyl + hp + wt + am, data = mtcars)
summary(fit4)
fit5 <- lm(mpg ~ wt + hp + factor(am), data = mtcars)
summary(fit5)


p <- ggplot(mtcars, aes(factor(am), mpg))
p + geom_boxplot() + geom_jitter() +
    labs(title="Box Plot of MPG vs. Transmission Type", x = "Transmission")

library(corrplot)
M <- cor(mtcars.orig)
corrplot(M, method = "circle", type = "upper")

cor.mtest <- function(mat, conf.level = 0.95){
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for(i in 1:(n-1)){
        for(j in (i+1):n){
            tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
            p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
            lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
            uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}
res1 <- cor.mtest(mtcars.orig,0.95)
res2 <- cor.mtest(mtcars.orig,0.99)
corrplot.mixed(M,p.mat = res2[[1]], sig.level=0.0005)

## Model Residuals and Diagnostics
par(mfrow=c(2, 2))
plot(best_model)
