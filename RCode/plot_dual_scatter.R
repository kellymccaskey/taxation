# rm(list = ls())
# setwd("/Users/kellymccaskey/Dropbox/Projects/Taxation/")

# d <- read.dta("Data/apsr_replication.dta")

m <- lm(lstocka ~ Llgdpproduct2 + Lldist + Lcontig + 
        Lcomlang_off + Lgrowcorr + Lcommoncurrency + 
        Ldtt + Lpta2 + LI + LB + LI*LB + 
        as.factor(ifs_o) + as.factor(ifs_d), 
        data = d, x = TRUE) 

#d <- d[as.numeric(rownames(m$x)), ]

library(compactr)
par(oma = c(3, 3.5, .5, .5),
    mar = c(.5, .5, .5, .5),
    mfrow = c(1, 2))
eplot(xlim = mm(d$LI), 
      ylim = mm(d$lstocka), 
      xlab = "Migrant Stock", 
      ylab = "Portfolio Investment",
      ylabpos = 2.1,
      main = "No Dual Taxation")

points(d$LI[d$Ldtt==0], d$lstocka[d$Ldtt==0])
m <- lm(lstocka ~ LI, data = d, subset = Ldtt==0)
abline(m)
aplot("Dual Taxation")

points(d$LI[d$Ldtt==1], d$lstocka[d$Ldtt==1])
m <- lm(lstocka ~ LI, data = d, subset = Ldtt==1)
abline(m)
