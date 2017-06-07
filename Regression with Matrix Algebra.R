

# --------------------------------------------------
# Multiple Regression "by Hand" Using Matrix Algebra
# --------------------------------------------------

attach(mtcars)

# Use 3 predictors
Y <- as.matrix(mpg) # DV
X <- cbind(constant = 1, as.matrix(cbind(hp,cyl,wt))) # IV

# Solve
B <- solve(t(X)%*%X,t(X)%*%Y) # (X'X)^-1 * (X'Y)

# Compute standard errors
s2 <- sum((Y - X%*%B)^2)/(nrow(X) - ncol(X))
VCV <- s2*solve(t(X)%*%X)
SE <- sqrt(diag(VCV))

# Compute t-values
t <- B/SE

# Compute p-values
p <- 2*pt(abs(t),nrow(X) - ncol(X), lower.tail = FALSE)

# Compute adjusted R-squared
Y_hat <- X%*%B 
SSr <- sum((Y - Y_hat)^2)
SSt <- sum((Y - mean(Y))^2)
R2 <- 1 - (SSr/SSt)
adj.R2 <- 1 - ((1 - R2)*(nrow(X) - 1))/(nrow(X) - ncol(X[,-1]) - 1)

# Compare to lm()
lm <- lm(mpg ~ hp + cyl + wt)
summary(lm)

Table <- as.data.frame(round(cbind(B,SE,t,p), digits = 3))
names(Table)[1:4] <- c("Estimate","Standard Error","t-value","p-value")
Table
adj.R2
