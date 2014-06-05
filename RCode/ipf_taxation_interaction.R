## Migration x Taxation on IPF

# Set the working directory
# setwd("/Users/kellymccaskey/Dropbox/Projects/Taxation/")

# Load packages
library(foreign)
library(arm)
library(compactr)
library(sandwich)
library(lmtest)

# Load data
# d <- read.dta("Data/apsr_replication.dta")
ld <- na.omit(d[, c("lstocka", "Llgdpproduct2", "Lldist", "Lcontig", 
                    "Lcomlang_off", "Lgrowcorr", "Lcommoncurrency", 
                    "Ldtt", "Lpta2", "commonlegal", "commonreligion", 
                    "LI", "LB",  "ifs_o", "ifs_d")])

vi <- c(1:13)

# PI model estimation with product term
m <- lm(lstocka ~ Llgdpproduct2 + Lldist + Lcontig 
        + Lcomlang_off + Lgrowcorr + Lcommoncurrency 
        + Ldtt + Lpta2 + LI + LB + commonlegal 
        + commonreligion + Ldtt*LI + as.factor(ifs_o) 
        + as.factor(ifs_d), data = ld) 
# display(m, detail = TRUE)

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

# to get the coefficients and the standard errors for model 1
# (the left side of table 2)
rob.est$tests

# PI - pull coefficients
b.hat <- coef(m)

# PI - pull covariance matrix
# V <- vcov(m) 
V <- rob.est$V

# PI - a set of values of Ldtt to compute effect of LI
z.Ldtt1 <- c(0, 1)

# PI - calculate effect of LI
dPI.dLI <- b.hat["LI"] + b.hat["Ldtt:LI"]*z.Ldtt1

# PI - calculate standard error of each effect, 90% CI of effect
se.dPI.dLI <- sqrt(V["LI", "LI"] 
                   + z.Ldtt1^2*V["Ldtt:LI", "Ldtt:LI"] 
                   + 2*z.Ldtt1*V["LI", "Ldtt:LI"])
uprPI <- dPI.dLI + 1.64*se.dPI.dLI
lwrPI <- dPI.dLI - 1.64*se.dPI.dLI
