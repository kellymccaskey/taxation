## IPF Outliers Test

# Set the working directory
# setwd("/Users/kellymccaskey/Dropbox/Projects/Taxation/")

# load packages
library(foreign)
library(arm)
library(compactr)
library(car)
library(texreg)

# load data
# d <- read.dta("Data/apsr_replication.dta")

# model estimation
m <- lm(lstocka ~ Llgdpproduct2 + Lldist + Lcontig 
        + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
        + Ldtt + Lpta2 + LI + LB + commonlegal 
        + commonreligion + LI*Ldtt + as.factor(ifs_o) 
        + as.factor(ifs_d), data = d) 
display(m, detail = TRUE)

# influential data?
summary(influence.measures(m))

# outlier test
outlierTest(m)

# model estimation without outlier
m.o <- lm(lstocka ~ Llgdpproduct2 + Lldist + Lcontig 
          + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
          + Ldtt + Lpta2 + LI + LB + commonlegal 
          + commonreligion + LI*Ldtt + as.factor(ifs_o) 
          + as.factor(ifs_d), data = d, subset = rownames(d) != "131379")
display(m.o, detail = TRUE)

# texreg table output
l <- list(m, m.o)
texreg(l, ci.force.level = 0.9)

## finding leverage points
e.hat <- residuals(m)
hat.values <- hatvalues(m)
cooks <- cooks.distance(m)

par(mar = c(4, 3.75, 1.75, 1))
eplot(xlim = c(0,0.6),
      ylim = mm(e.hat),
      xlab = "Hat-Values",
      ylab = "Residuals",
      main = "IPF Leverage Points")
points(hat.values, e.hat, cex = 500*cooks)