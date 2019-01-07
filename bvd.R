# Libraries -----------------------------------------------------------------------------------------------------------

library(zoo)
library(lmtest)

df <- read.table("dataset.txt", header = TRUE)
x <- df$xdata
y <- df$ydata

# Model 1 -------------------------------------------------------------------------------------------------------------

model_1 <- lm(y~x)
fit_1 <- model_1$fitted
res_1 <- model_1$residual
plot(fit_1,res_1)
abline(h = 0)
anova(model_1)
summary(model_1)
bptest(model_1)
qqnorm(res_1, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)



#BOxCox transformation-------------------------------------------------------------------------------------------------

library(MASS)
bc <- boxcox(model_1,seq(1,2,0.1))

# Model 2 -------------------------------------------------------------------------------------------------------------

model_2 <- lm(y^2~x)
fit_2 <- model_2$fitted
res_2 <- model_2$residual
plot(fit_2,res_2)
abline(h=0)
anova(model_2)
summary(model_2)
bptest(model_2)
qqnorm(res_2, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)


# Model 4 -------------------------------------------------------------------------------------------------------------

wts <- as.numeric(1/fitted(lm(abs(residuals(model_2)) ~ x))^2)

model_3 <- lm(y^2 ~ x, weights = wts)
fit_3 <- model_3$fitted
res_3 <- model_3$residual
plot(fit_3,res_3)
abline(h=0)
anova(model_3)
summary(model_3)
bptest(model_3)
qqnorm(res_3, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)


# Model 5 -------------------------------------------------------------------------------------------------------------

mod5.logit <- glm(y~x,family = binomial)
fit_5 <- mod5.logit$fitted.values
res_5 <- mod5.logit$residuals
plot(fit_5,res_5)

library(boot)

z <- seq(1,100,1)
lp <- mod5.logit$coefficients[1] + mod5.logit$coefficients[2]*z
pr <- exp(lp)/(1+exp(lp))
plot(x,y,xlim = c(1,100), ylim = c(0.8,1), xlab = "N", ylab = "Errors")
lines(z,pr,lty = 1, col = "red")

# Model 6--------------------------------------------------------------------------------------------------------------

mod6 <- nls(y ~ exp(a + b*x), start = list(a = 0, b = 0))
fit_6 <- mod6$fitted.values
res_6 <- mod6$residuals
plot(fit_6,res_6)


nlm.1 <- nls(y ~ beta1*exp(beta2*x),
             start = list(beta1 = exp(-0.0421121), beta2 = -0.0013428 ))

plot(x,y,ylim = c(0.85,1)); 
xgrid <- seq(1,100,1)
lines(xgrid, coef(nlm.1)[1]*exp(coef(nlm.1)[2]*xgrid), col = "red" )


# Model 7-------------------------------------------------------------------------------------------------------------

library(splines)

regr.spline <- lm(y ~ bs(x, df = NULL, knots=c(0.20), degree = 3, intercept=T))


x.values <- seq(from=min(x), to=max(x), length=100)
options(warn = -1)
plot(x, y, xlim = c(1,100), ylim = c(0.85,1)); 
lines(x.values, predict(regr.spline, data.frame(x=x.values)), col = "red")

smoothspline.reg <- smooth.spline(x, y)
plot(x, y); lines(smoothspline.reg, col = "red")
smoothspline.reg$spar


