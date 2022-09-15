# To get the rest you need to extract the residuals
E1 <- resid(M7, type = "pearson")
F1 <- fitted(M7)
op <- par(mfrow = c(1,3), mar = c(4,4,3,2))
MyYlab = "Residuals"

plot(x = F1, y = E1, xlab = "Fitted values", ylab = MyYlab)
boxplot(E1 ~ fbeach, data = beach, main="Beach", ylab = MyYlab)
boxplot(E1 ~ fExp, data=beach, main="Exposure", ylab = MyYlab)
par(op)

# Still a mess in terms of the explanatory variables
# refit with nlme (lmer) and a VarIdent correlation structure within the mixed model!!!!
# NOTE: this is linear (no error structures can be used - i.e. poisson....)
