library(rpart)

decision.tree = rpart(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = `winequality-red`, method = "anova")

printcp(decision.tree)
plotcp(decision.tree)

plot(decision.tree, uniform = TRUE, main = "Wine Quality")
text(decision.tree, use.n = FALSE, all = TRUE, cex = 0.8)

