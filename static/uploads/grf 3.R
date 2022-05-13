library(ggplot2)
library(grf)
library(np)

#generate data
n <- 2000
p <- 40
X <- matrix(runif(n*p, -1, 1), n, p)
Y.1 <- rnorm(n, 0.8*(X[,1]>-0.9), 1)

#test data
X.test <- matrix(0, 1001, p)
X.test[,1] <- seq(-1, 1, length.out = 1001)

#train quantile forest by GRF
q.forest.1 <- quantile_forest(X, Y.1, quantiles= c(0.1, 0.5, 0.9))
q.hat.1 <- predict(q.forest.1, X.test, quantiles= c(0.1, 0.5, 0.9))

#train quantile forest by averaging
meins.forest <- quantile_forest(X, Y.1, regression.splitting = TRUE, quantiles= c(0.1, 0.5, 0.9))
q.tau.1 <- predict(meins.forest, X.test, quantiles = c(0.1, 0.5, 0.9))

#train kernel quantile regression
bw <- npcdistbw(formula=Y.1~X[,1])

model.q0.10 <- npqreg(bws=bw, tau=0.10, exdat = X.test[,1])
model.q0.50 <- npqreg(bws=bw, tau=0.50, exdat = X.test[,1])
model.q0.90 <- npqreg(bws=bw, tau=0.90, exdat = X.test[,1])

q.star.1 <- model.q0.10$quantile
q.star.2 <- model.q0.50$quantile
q.star.3 <- model.q0.90$quantile

#combine all predictions and plot graph
df <- data.frame(
  x = X.test[,1],
  y.1 = q.hat.1[,1],
  y.2 = q.hat.1[,2],
  y.3 = q.hat.1[,3],
  y.4 = q.tau.1[,1],
  y.5 = q.tau.1[,2],
  y.6 = q.tau.1[,3],
  y.7 = q.star.1,
  y.8 = q.star.2,
  y.9 = q.star.3
)

png(file="grf3.png", width=2000, height=2000, res=300)
ggplot(df, aes(x, fill=x)) +
  geom_line(aes(y = y.1, colour = "GRF") , linetype = "dashed") + 
  geom_line(aes(y = y.2, colour = "GRF")) + 
  geom_line(aes(y = y.3, colour = "GRF") , linetype = "dashed") +
  geom_line(aes(y = y.4, colour = "quantregForest"), linetype = "dashed") + 
  geom_line(aes(y = y.5, colour = "quantregForest")) + 
  geom_line(aes(y = y.6, colour = "quantregForest"), linetype = "dashed") +
  geom_line(aes(y = y.7, colour = "Kernelquantreg"), linetype = "dashed") + 
  geom_line(aes(y = y.8, colour = "Kernelquantreg")) + 
  geom_line(aes(y = y.9, colour = "Kernelquantreg"), linetype = "dashed") +
  geom_line(aes(y = 0.8*1*(X.test[,1]>-0.9), colour = "truth")) +
  geom_line(aes(y = -1.2816 + 0.8*1*(X.test[,1]>-0.9)), linetype = "dashed") +
  geom_line(aes(y = 1.2816 + 0.8*1*(X.test[,1]>-0.9)), linetype = "dashed") +
  theme(legend.position = c(0.85, 0.75), 
        legend.direction = 'vertical', 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill="lightgray", size=0.5)) +
  ggtitle("(edge) mean shift comparision") + xlab("X") + ylab("Y") +
  scale_colour_manual(name="", values=c("GRF"="red","truth"="black",
                                        "quantregForest"="blue", "Kernelquantreg"="orange"))
dev.off()

rm(list=ls())


