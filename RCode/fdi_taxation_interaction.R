## Migration x Taxation on FDI

# Set the working directory
setwd("~/Dropbox/projects/taxation/")

# Load packages
library(arm)
library(compactr)
library(sandwich)
library(lmtest)

# Load data
d <- read.csv("data/taxation-composite.csv")
ld <- na.omit(d[, c("loutflowsa", "Llgdpproduct2", "Lldist", "Lcontig", "Lcomlang_off", "Lgrowcorr", "Lcommoncurrency", "Ldtt", "Lpta2", "commonlegal", "commonreligion", "LI", "LB", "ifs_o", "ifs_d", "culture_composite")])

vi <- c(1:13)

# model estimation
m <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + as.numeric(culture_composite) + Lgrowcorr + Ldtt + Lpta2 + LI + LB + Ldtt*LI + as.factor(ifs_o) + as.factor(ifs_d), data = ld) 
m2 <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + as.numeric(culture_composite) + Lgrowcorr + Ldtt + Lpta2 + LI + LB + as.factor(ifs_o) + as.factor(ifs_d), data = ld) 

m3 <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + Lgrowcorr + Ldtt + Lpta2 + LI + LB + Ldtt*LI + Lcommoncurrency + Lcomlang_off + commonlegal + commonreligion + as.factor(ifs_o) + as.factor(ifs_d), data = ld) 
m4 <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + Lgrowcorr + Ldtt + Lpta2 + LI + LB + Lcommoncurrency + Lcomlang_off + commonlegal + commonreligion + as.factor(ifs_o) + as.factor(ifs_d), data = ld) 

mclx <- 
  function(fm, dfcw, cluster1, cluster2){
    # available from: http://people.su.se/~ma/clustering.pdf
    # *slightly* modified by Carlisle Rainey
    
    # R-codes (www.r-project.org) for computing multi-way 
    # clustered-standard errors. Mahmood Arai, Jan 26, 2008. 
    # See: Thompson (2006), Cameron, Gelbach and Miller (2006)
    # and Petersen (2006).
    # reweighting the var-cov matrix for the within model
    
    # The arguments of the function are:
    # fitted model, cluster1 and cluster2
    # You need to install libraries `sandwich' and `lmtest'
    library(sandwich); library(lmtest)
    cluster12 = paste(cluster1,cluster2, sep="")
    M1  <- length(unique(cluster1))
    M2  <- length(unique(cluster2))   
    M12 <- length(unique(cluster12))
    N   <- length(cluster1)          
    K   <- fm$rank             
    dfc1  <- (M1/(M1-1))*((N-1)/(N-K))  
    dfc2  <- (M2/(M2-1))*((N-1)/(N-K))  
    dfc12 <- (M12/(M12-1))*((N-1)/(N-K))  
    u1j   <- apply(estfun(fm), 2, function(x) tapply(x, cluster1,  sum)) 
    u2j   <- apply(estfun(fm), 2, function(x) tapply(x, cluster2,  sum)) 
    u12j  <- apply(estfun(fm), 2, function(x) tapply(x, cluster12, sum)) 
    vc1   <-  dfc1*sandwich(fm, meat=crossprod(u1j)/N )
    vc2   <-  dfc2*sandwich(fm, meat=crossprod(u2j)/N )
    vc12  <- dfc12*sandwich(fm, meat=crossprod(u12j)/N)
    vcovMCL <- (vc1 + vc2 - vc12)*dfcw
    tests <- coeftest(fm, vcovMCL)
    V <- list(V = vcovMCL, tests = tests)
    return(V)
  }

rob.est <- mclx(m, 1, ld$ifs_o, ld$ifs_d) #[vi, vi]
rob.est2 <- mclx(m2, 1, ld$ifs_o, ld$ifs_d)

# to get the coefficients and standard errors of Model 2 
# (Table 2)
rob.est2$test # no interaction
rob.est$tests # interaction

# pull coefficients
b.hat <- coef(m)

# pull covariance matrix
V <- rob.est$V

# a set of values of Ldtt to compute effect of LI
z.Ldtt1 <- c(0, 1)

# calculate effect of LI
dFDI.dLI <- b.hat["LI"] + b.hat["Ldtt:LI"]*z.Ldtt1

# calculate standard error of each effect, 90% CI of effect
se.dFDI.dLI <- sqrt(V["LI", "LI"] 
                   + z.Ldtt1^2*V["Ldtt:LI", "Ldtt:LI"] 
                   + 2*z.Ldtt1*V["LI", "Ldtt:LI"])
uprFDI <- dFDI.dLI + 1.64*se.dFDI.dLI
lwrFDI <- dFDI.dLI - 1.64*se.dFDI.dLI
