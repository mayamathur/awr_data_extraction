
# Why are HC0 SEs often more precise rather than less precise
#  when used to estimate RR with Poisson?

data("warpbreaks")



mod = glm( wool == "B" ~ breaks + tension, 
           data = warpbreaks,
           family = "poisson" )
summary(mod)

library(sandwich)
sqrt( diag( vcovHC(mod, type="HC0") ) )