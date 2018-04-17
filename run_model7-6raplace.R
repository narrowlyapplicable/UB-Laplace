library(rstan)
setwd('~/')
d <- read.csv(file='~/RScripts/RStanBook-master/chap04/input/data-salary.txt')
d[6,"X"] <- 24
d[12,"X"] <- 25

X_new <- as.numeric(c(23:65))
data <- list(N=nrow(d), X=d$X, Y=d$Y, U=0.000002, N_new=length(X_new), X_new=X_new)
stanmodel <- stan_model(file = 'model/model-UBLaplace_pred.stan')

fit <- vb(stanmodel, data=data, seed=1234)
# fit <- stan(file='model/model-UBLaplace_pred.stan', data=data,
#             iter=10000, chain=8, warmup=5000, seed=1234)

#save.image('output/result-model7-6UBLaplace_pred.RData')

result <- rstan::summary(fit)$summary
library(ggplot2)
plot_data <- data.frame(X=d$X, Xtrue=result[5:24,1],
                        Y=d$Y, Ypred=result[1,1]+result[2,1]*result[5:24,1])
p <- ggplot(plot_data)
p <- p + geom_point(aes(x=X, y=Y), colour="red")
p <- p + geom_line(aes(x = Xtrue, y = Ypred))
p <- p + geom_point(aes(x = Xtrue, y = Y),shape=1, colour="blue")
#plot(p)
ms <- rstan::extract(fit)
upr90 <- apply(ms$Y_pred, 2, quantile, 0.95)
lwr90 <- apply(ms$Y_pred, 2, quantile, 0.05)
upr50 <- apply(ms$Y_pred, 2, quantile, 0.75)
lwr50 <- apply(ms$Y_pred, 2, quantile, 0.25)
pred_data <- data.frame(X_new = X_new, upr90=upr90, lwr90=lwr90,
                        upr50=upr50, lwr50=lwr50)
p <- p + geom_ribbon(data=pred_data, aes(x=X_new, ymax=upr90, ymin=lwr90), 
                     alpha=0.3)
p <- p + geom_ribbon(data=pred_data, aes(x=X_new, ymax=upr50, ymin=lwr50), 
                     colour="grey", alpha=0.5)
plot(p)