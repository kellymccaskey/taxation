## Simultaneous Equation

# Set the working directory
setwd("~/Dropbox/projects/taxation/")

# Load packages
library(foreign)
library(arm)
library(compactr)
library(sandwich)
library(lmtest)
library(systemfit)

# Load data
d <- read.csv("data/taxation-composite.csv")
ld <- na.omit(d[, c("lstocka", "loutflowsa", "Llgdpproduct2", "Lldist", "Lcontig", "Lcomlang_off", "Lgrowcorr", "Lcommoncurrency", "Ldtt", "Lpta2", "commonlegal", "commonreligion", "LI", "LB",  "ifs_o", "ifs_d", "culture_composite")])

ipf <- lstocka ~ Llgdpproduct2 + Lldist + Lcontig + as.numeric(culture_composite) + Lgrowcorr + Ldtt + Lpta2 + LI + LB + Ldtt*LI + as.factor(ifs_o) + as.factor(ifs_d)
fdi <- loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + as.numeric(culture_composite) + Lgrowcorr + Ldtt + Lpta2 + LI + LB + Ldtt*LI + as.factor(ifs_o) + as.factor(ifs_d)

system <- list(ipf, fdi)

m <- systemfit(system, method = "SUR", data = ld)
summary(m)

m$coefCov[c("eq1_Ldtt:LI", "eq2_Ldtt:LI"),c("eq1_Ldtt:LI", "eq2_Ldtt:LI")]
