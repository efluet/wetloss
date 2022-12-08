install.packages('lhs')
library(lhs)

# An n by k Latin Hypercube Sample matrix with values uniformly distributed on [0,1]
set.seed(1234)
a <- randomLHS(10,3)
a <- a *  50
a

# Latin Hypercube sampling generates more efficient estimates of desired parameters than simple Monte Carlo sampling.
p <- improvedLHS(1000, 3, 5)
p <- p * 40
p
# b <- augmentLHS(a, 2)