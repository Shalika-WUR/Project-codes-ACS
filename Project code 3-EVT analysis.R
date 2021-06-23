#Extreme value analysis on simulated crop yields
#convert OUT file from simulations to csv
library(extRemes)
dat <- read.csv("C:/Users/S.VYAS/Desktop/CIMMYT/Work/MAR/2020/Bundling/Future-CSA.csv")
data(dat)
#choose threshold type=mean, median etc
fit <- fevd(yield, dat, threshold=4670, type="PP", units="Kg/Ha", verbose=TRUE)
fit
#see AIC, model fit
return.level( fit)
par("mar")
par(mar=c(2,2,2,2))
plot(fit)
plot(fit, "trace")
ci(fit, type="parameter")
#compare results by simulation types







