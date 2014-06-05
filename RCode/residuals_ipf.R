# setwd("/Users/kellymccaskey/Dropbox/Projects/Taxation/")

# load data
# d <- read.dta("Data/apsr_replication.dta")

# model
# m <- lm(lstocka ~ Llgdpproduct2 + Lldist + Lcontig 
#        + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
#        + Ldtt + Lpta2 + LI + LB + commonlegal 
#        + commonreligion + Ldtt*LI + as.factor(ifs_o) 
#        + as.factor(ifs_d), data = d) 

## residuals vs. predicted plot
y.hat <- predict(m)
e.hat <- residuals(m)

# create 3 sets of fake residuals
fr1 <- rnorm(length(y.hat), 0, sd(e.hat))
fr2 <- rnorm(length(y.hat), 0, sd(e.hat))
fr3 <- rnorm(length(y.hat), 0, sd(e.hat))

par(mfrow = c(2, 2), mar = c(.5, .5, 1, .5),
    oma = c(3, 3, 2, 1))

eplot(xlim = mm(y.hat),
      ylim = mm(c(e.hat, fr1, fr2, fr3)),
      xlab = "Predicted",
      ylab = "Residuals",
      main = "Actual Residuals")
abline(h = 0, lty = 3)
points(y.hat, e.hat)

aplot("Fake Residuals")
abline(h = 0, lty = 3)
points(y.hat, fr1)

aplot("Fake Residuals")
abline(h = 0, lty = 3)
points(y.hat, fr2)

aplot("Fake Residuals")
abline(h = 0, lty = 3)
points(y.hat, fr3)

## df betas
head(dfbeta(m))
# hist(dfbeta(m)[, "LI"])
# hist(dfbeta(m)[, "Ldtt"])