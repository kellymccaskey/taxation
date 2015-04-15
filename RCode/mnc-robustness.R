# Set the working directory
# setwd("~/Dropbox/projects/taxation/")

# Load packages
library(arm)
library(compactr)
library(sandwich)
library(lmtest)

# Load data
# d <- read.csv("data/taxation-composite.csv")
ld <- na.omit(d[, c("lstocka", "loutflowsa", "Llgdpproduct2", "Lldist", "Lcontig", "Lcomlang_off", "Lgrowcorr", "Lcommoncurrency", "Ldtt", "Lpta2", "commonlegal", "commonreligion", "LI", "LB",  "ifs_o", "ifs_d", "culture_composite")])

noMNC <- ld[which(d$ifs_o != 111 & d$ifs_o != 156 & d$ifs_o !=158 & d$ifs_o != 146 & d$ifs_o != 193 & d$ifs_o != 142 & d$ifs_o != 132 & d$ifs_o != 134 & d$ifs_o != 112 & d$ifs_o != 138 & d$ifs_o !=136 & d$ifs_o != 144 & d$ifs_o != 172 & d$ifs_o != 184 & d$ifs_o !=178 & d$ifs_o !=545 & d$ifs_o !=548 & d$ifs_o !=576), ]

# fdi
m <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + as.numeric(culture_composite) + Lgrowcorr + Ldtt + Lpta2 + LI + LB + Ldtt*LI + as.factor(ifs_o) + as.factor(ifs_d), data = noMNC) 
m2 <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + as.numeric(culture_composite) + Lgrowcorr + Ldtt + Lpta2 + LI + LB + as.factor(ifs_o) + as.factor(ifs_d), data = noMNC) 



# for two-way clustering of standard errors, clustered on investment source and destination country
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

rob.est <- mclx(m, 1, noMNC$ifs_o, noMNC$ifs_d)
rob.est2 <- mclx(m2, 1, noMNC$ifs_o, noMNC$ifs_d)

# to get the coefficients and the standard errors for the models
rob.est.2$tests # no interaction - portfolio investment
rob.est$tests # interaction - portfolio investment
rob.est.4$tests # no interaction - fdi
rob.est.3$tests # interaction - portfolio investment
