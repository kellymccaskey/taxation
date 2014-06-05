## Migration x Taxation on IPF

# rm(list = ls())

# Set the working directory
# setwd("/Users/kellymccaskey/Dropbox/Projects/Taxation/")

# Load packages
library(foreign)
library(arm)
library(DAAG)

# Load data
# d <- read.dta("Data/apsr_replication.dta")
ld <- na.omit(d[, c("lstocka", "Llgdpproduct2", "Lldist", "Lcontig", 
                    "Lcomlang_off", "Lgrowcorr", "Lcommoncurrency", 
                    "Ldtt", "Lpta2", "commonlegal", "commonreligion", 
                    "LI", "LB",  "ifs_o", "ifs_d")])
md <- na.omit(d[, c("loutflowsa", "Llgdpproduct2", "Lldist", "Lcontig", 
                    "Lcomlang_off", "Lgrowcorr", "Lcommoncurrency", 
                    "Ldtt", "Lpta2", "commonlegal", "commonreligion", 
                    "LI", "LB",  "ifs_o", "ifs_d")])

# PI model estimation with product term
m1 <- lm(lstocka ~ Llgdpproduct2 + Lldist + Lcontig 
         + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
         + Ldtt + Lpta2 + LI + LB + commonlegal 
         + commonreligion + as.factor(ifs_o) 
         + as.factor(ifs_d), data = ld)

m2 <- lm(lstocka ~ Llgdpproduct2 + Lldist + Lcontig 
        + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
        + Ldtt + Lpta2 + LI + LB + commonlegal 
        + commonreligion + Ldtt*LI + as.factor(ifs_o) 
        + as.factor(ifs_d), data = ld) 

display(m1, detail = TRUE)
display(m2, detail = TRUE)

BIC(m1, m2)
AIC(m1, m2)


# FDI model 
m3 <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig 
         + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
         + Ldtt + Lpta2 + LI + LB + commonlegal 
         + commonreligion + as.factor(ifs_o) 
         + as.factor(ifs_d), data = md)

m4 <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig 
         + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
         + Ldtt + Lpta2 + LI + LB + commonlegal 
         + commonreligion + Ldtt*LI + as.factor(ifs_o) 
         + as.factor(ifs_d), data = md) 

display(m3, detail = TRUE)
display(m4, detail = TRUE)

BIC(m3, m4)
AIC(m3, m4)
