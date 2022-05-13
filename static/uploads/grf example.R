n <- 2000
p <- 10
X <- matrix(rnorm(n * p), n, p)
X.test <- matrix(0, 101, p)
X.test[, 1] <- seq(-2, 2, length.out = 101)

W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))
Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)

tau.forest <- causal_forest(X, Y, W, num.trees = 4000)

tau.hat <- predict(tau.forest, X.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)

df.plot <- data.frame(
  x = X.test[,1],
  y.1 = tau.hat$predictions,
  y.2 = tau.hat$predictions + 1.96 * sigma.hat,
  y.3 = tau.hat$predictions - 1.96 * sigma.hat
)

png(file="grf_example.png", width=2000, height = 2000, res=300)
ggplot(df.plot, aes(x, fill=x))+
  geom_line(aes(y = y.1, colour = "Estimated CATE")) + 
  geom_line(aes(y = y.2, colour = "95% CI"), linetype = "dashed") +
  geom_line(aes(y = y.3, colour = "95% CI"), linetype = "dashed") +
  geom_line(aes(y = pmax(0, X.test[, 1]), colour = "truth")) + 
  theme(legend.position = c(0.12, 0.87),
        legend.direction = "vertical",
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill="lightgray", size=0.5)) + 
  ggtitle("Estimated CATE over X") + xlab("X") + ylab("CATE") + 
  scale_color_manual(name="", values=c("Estimated CATE"="black", "95% CI"="red", "truth"="blue"))
dev.off()

rm(list=ls())
