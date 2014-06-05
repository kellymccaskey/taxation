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
ld <- na.omit(d[, c("lstocka", "Llgdpproduct2", "Lldist", "Lcontig", 
                    "Lcomlang_off", "Lgrowcorr", "Lcommoncurrency", 
                    "Ldtt", "Lpta2", "commonlegal", "commonreligion", 
                    "LI", "LB", "ifs_o", "ifs_d")])

d$case.name <- paste(as.character(round(d$dyad, 3)), d$year, sep = "-")
rownames(d) <- d$case.name

# model estimation
m <- lm(lstocka ~ Llgdpproduct2 + Lldist + Lcontig 
        + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
        + Ldtt + Lpta2 + LI + LB + commonlegal
        + commonreligion + LI*Ldtt + as.factor(ifs_o) 
        + as.factor(ifs_d), data = ld) 
display(m, detail = TRUE)

# robust model estimation
m.m <- lmRob(lstocka ~ Llgdpproduct2 + Lldist + Lcontig 
             + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
             + Ldtt + Lpta2 + LI + LB + commonlegal 
             + commonreligion + LI*Ldtt + as.factor(ifs_o) 
             + as.factor(ifs_d), data = ld)
summary(m.m)


# plot the coefficients
k <- c(2:13, 232)
beta.lm <- coef(m)[k]
beta.rlm <- coef(m.m)[k]
summ <- summary(m.m)
Sigma.lm <- vcov(m)[k, k]

par(mfrow = c(1,1))
eplot(xlim = c(-1.5, 1.5), ylim = c(.5, length(beta.lm) + 2),
      xlab = "Coefficient Estimate",
      main = "IPF Robust Coefficient Estimate Comparison")
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
