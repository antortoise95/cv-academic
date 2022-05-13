library(ggplot2)
library(grf)

df <- read.csv("C:/Users/Andong Yan/Dropbox/UCR/R_cd/Koop-Tobias.csv")

W <- 1*(df[[2]]>10)
W.2 <- df[[2]]
X <- data.matrix(df[c(4:10)])
Y <- df[[3]]

c.forest <- causal_forest(X, Y, W.2, num.trees = 4000)


n <- 21
p <- 7
X.test <- matrix(0, n, p)
X.test[,1] <- seq(min(df[[4]]), max(df[[4]]),length.out = n) # experience
# X.test[,1] <- sample(df[[4]], n, replace=TRUE)
X.test[,2] <- sample(df[[5]], n, replace=TRUE)
X.test[,3] <- sample(df[[6]], n, replace=TRUE)
X.test[,4] <- sample(df[[7]], n, replace=TRUE)
X.test[,5] <- sample(df[[8]], n, replace=TRUE)
X.test[,6] <- sample(df[[9]], n, replace=TRUE)
X.test[,7] <- sample(df[[10]], n, replace=TRUE) 

c.pred <- predict(c.forest, X.test, estimate.variance = TRUE)

c.sigma <- sqrt(c.pred$variance.estimates)

average_treatment_effect(c.forest, target.sample = "all")
# estimate    std.err 
# 0.33655542 0.02272894 

average_treatment_effect(c.forest, target.sample = "treated")
# estimate    std.err 
# 0.35279013 0.03037442 

average_partial_effect(c.forest)
# estimate     std.err 
# 0.081779748 0.003437456 

df$HIGHEDUC <- 1*(df[[2]]>10)

fit <- lm(LOGWAGE ~ POTEXPER + TIMETRND + ABILITY + MOTHERED + FATHERED + BRKNHOME + SIBLINGS + HIGHEDUC, data=df)
fit$coefficients[9]
# HIGHEDUC 
# 0.2037224

fit <- lm(LOGWAGE ~ POTEXPER + TIMETRND + ABILITY + MOTHERED + FATHERED + BRKNHOME + SIBLINGS + EDUC, data=df)
fit$coefficients[9]
# EDUC 
# 0.08477538

plot(X.test[, 1], c.pred$predictions, ylim = range(c.pred$predictions + 1.96 * c.sigma, c.pred$predictions - 1.96 * c.sigma, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(X.test[, 1], c.pred$predictions + 1.96 * c.sigma, col = 2, lty = 2)
lines(X.test[, 1], c.pred$predictions - 1.96 * c.sigma, col = 2, lty = 2)
lines(X.test[, 1], rep(0.2037224, n), col = 3, lty = 2)
# lines(X.test[, 1], rep(0.08477538, n), col = 3, lty = 2)

df.plot <- data.frame(
  x = X.test[,1],
  y.1 = c.pred$predictions,
  y.2 = c.pred$predictions + 1.96 * c.sigma,
  y.3 = c.pred$predictions - 1.96 * c.sigma,
  y.4 = rep(0.2037224, n),
  y.5 = rep(0.08477538, n)
)

png(file="grf_emp_2.png", width=2000, height = 2000, res=300)
ggplot(df.plot, aes(x, fill=x))+
  geom_line(aes(y = y.1, colour = "Estimated CATE")) + 
  geom_line(aes(y = y.2, colour = "95% CI"), linetype = "dashed") +
  geom_line(aes(y = y.3, colour = "95% CI"), linetype = "dashed") +
  #geom_line(aes(y = y.4, colour = "OLS"), linetype = "dashed") + 
  geom_line(aes(y = y.5, colour = "OLS"), linetype = "dashed") + 
  theme(legend.position = c(0.12, 0.87),
        legend.direction = "vertical",
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill="lightgray", size=0.5)) + 
  ggtitle("Estimated CATE over POTEXPER") + xlab("POTEXPER") + ylab("CATE") + 
  scale_color_manual(name="", values=c("Estimated CATE"="black", "95% CI"="red", "OLS"="blue"))
dev.off()

rm(list=ls())
