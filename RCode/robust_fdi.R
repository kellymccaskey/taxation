## Robust Regressions of Migrant Stock x Dual Taxation Treaty Interaction


# Set the working directory
# setwd("/Users/kellymccaskey/Dropbox/Projects/Taxation/")

# load packages
library(foreign)
library(arm)
library(compactr)
library(robust)

# load data
# d <- read.dta("Data/apsr_replication.dta")
ld <- na.omit(d[, c("loutflowsa", "Llgdpproduct2", "Lldist", "Lcontig", 
                    "Lcomlang_off", "Lgrowcorr", "Lcommoncurrency", 
                    "Ldtt", "Lpta2", "commonlegal", "commonreligion", 
                    "LI", "LB", "ifs_o", "ifs_d")])

# model estimation
m <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig 
        + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
        + Ldtt + Lpta2 + LI + LB + commonlegal
        + commonreligion + LI*Ldtt + as.factor(ifs_o) 
        + as.factor(ifs_d), data = ld) 
display(m, detail = TRUE)


# robust model estimation
m.m <- lmRob(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig 
             + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
             + Ldtt + Lpta2 + LI + LB + commonlegal 
             + commonreligion + LI*Ldtt + as.factor(ifs_o) 
             + as.factor(ifs_d), data = ld)
summary(m.m)


# plot the coefficients
k <- c(2:13, 310)
beta.lm <- coef(m)[k]
beta.rlm <- coef(m.m)[k]
summ <- summary(m.m)
Sigma.lm <- vcov(m)[k, k]
Sigma.rlm <- (summ$cov.unscaled*summ$sigma^2)[k, k]

par(mfrow = c(1,1))
eplot(xlim = c(-2, 3), ylim = c(.5, length(beta.lm) + 2),
      xlab = "Coefficient Estimate",
      main = "FDI Robust Coefficient Estimate Comparison")
abline(v = 0, lty = "dotted")

Sigma.rlm <- (summ$cov.unscaled*summ$sigma^2)[k, k]
for (i in 1:length(beta.lm)) {
  text(beta.lm[i], i, labels = names(beta.lm[i]), pos = 3)
  points(beta.lm[i], i, pch = 19)
  points(beta.rlm[i], i - .2, pch = 19, col = "red")
  lines(c(beta.lm[i] - 1.64*sqrt(Sigma.lm[i, i]), beta.lm[i] 
          + 1.64*sqrt(Sigma.lm[i, i])),
        c(i, i))
  lines(c(beta.rlm[i] - 1.64*sqrt(Sigma.rlm[i, i]), beta.rlm[i] 
          + 1.64*sqrt(Sigma.rlm[i, i])),
        c(i-0.2, i-0.2), col = "red")
}
