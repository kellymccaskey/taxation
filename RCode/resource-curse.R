# Set the working directory
setwd("~/Dropbox/projects/taxation/")

# Load packages
library(arm)
library(compactr)
library(sandwich)
library(lmtest)

# Load data
# d <- read.csv("data/taxation-composite.csv")
ld <- na.omit(d[, c("lstocka", "loutflowsa", "Llgdpproduct2", "Lldist", "Lcontig", "Lcomlang_off", "Lgrowcorr", "Lcommoncurrency", "Ldtt", "Lpta2", "commonlegal", "commonreligion", "LI", "LB",  "ifs_o", "ifs_d", "culture_composite")])

# exclude resource curse (all minerals), resource curse (all fuel), all resource curse
# rc_min <- ld[which(ld$ifs_d != 616 & ld$ifs_d != 754 & ld$ifs_d != 634 & ld$ifs_d != 947 & ld$ifs_d != 366 & ld$ifs_d != 228 & ld$ifs_d != 682 & ld$ifs_d != 688 & ld$ifs_d != 678 & ld$ifs_d != 724 & ld$ifs_d != 853 & ld$ifs_d != 728 & ld$ifs_d != 343), ]
# rc_f <- ld[which(ld$ifs_d != 433 & ld$ifs_d != 672 & ld$ifs_d != 642 & ld$ifs_d != 612 & ld$ifs_d != 628 & ld$ifs_d != 694 & ld$ifs_d != 453 & ld$ifs_d != 732 & ld$ifs_d != 456 & ld$ifs_d != 299 & ld$ifs_d != 429 & ld$ifs_d != 634 & ld$ifs_d != 646 & ld$ifs_d != 353), ]
no_rc <- ld[which(ld$ifs_d != 433 & ld$ifs_d != 672 & ld$ifs_d != 642 & ld$ifs_d != 612 & ld$ifs_d != 628 & ld$ifs_d != 694 & ld$ifs_d != 453 & ld$ifs_d != 732 & ld$ifs_d != 456 & ld$ifs_d != 299 & ld$ifs_d != 429 & ld$ifs_d != 634 & ld$ifs_d != 646 & ld$ifs_d != 353 & ld$ifs_d != 616 & ld$ifs_d != 754 & ld$ifs_d != 634 & ld$ifs_d != 947 & ld$ifs_d != 366 & ld$ifs_d != 228 & ld$ifs_d != 682 & ld$ifs_d != 688 & ld$ifs_d != 678 & ld$ifs_d != 724 & ld$ifs_d != 853 & ld$ifs_d != 728 & ld$ifs_d != 343), ]


norc_nomnc <- ld[which(ld$ifs_d != 433 & ld$ifs_d != 672 & ld$ifs_d != 642 & ld$ifs_d != 612 & ld$ifs_d != 628 & ld$ifs_d != 694 & ld$ifs_d != 453 & ld$ifs_d != 732 & ld$ifs_d != 456 & ld$ifs_d != 299 & ld$ifs_d != 429 & ld$ifs_d != 634 & ld$ifs_d != 646 & ld$ifs_d != 353 & ld$ifs_d != 616 & ld$ifs_d != 754 & ld$ifs_d != 634 & ld$ifs_d != 947 & ld$ifs_d != 366 & ld$ifs_d != 228 & ld$ifs_d != 682 & ld$ifs_d != 688 & ld$ifs_d != 678 & ld$ifs_d != 724 & ld$ifs_d != 853 & ld$ifs_d != 728 & ld$ifs_d != 343 & ld$ifs_o != 111 & ld$ifs_o != 156 & ld$ifs_o !=158 & ld$ifs_o != 146 & ld$ifs_o != 193 & ld$ifs_o != 142 & ld$ifs_o != 132 & ld$ifs_o != 134 & ld$ifs_o != 112 & ld$ifs_o != 138 & ld$ifs_o !=136 & ld$ifs_o != 144 & ld$ifs_o != 172 & ld$ifs_o != 184 & ld$ifs_o !=178 & ld$ifs_o !=545 & ld$ifs_o !=548 & ld$ifs_o !=576), ]

# estimate models
m <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + as.numeric(culture_composite) + Lgrowcorr + Ldtt + Lpta2 + LI + LB + Ldtt*LI + as.factor(ifs_o) + as.factor(ifs_d), data = no_rc) 
m2 <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + as.numeric(culture_composite) + Lgrowcorr + Ldtt + Lpta2 + LI + LB + as.factor(ifs_o) + as.factor(ifs_d), data = no_rc) 

m3 <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + as.numeric(culture_composite) + Lgrowcorr + Ldtt + Lpta2 + LI + LB + Ldtt*LI + as.factor(ifs_o) + as.factor(ifs_d), data = norc_nomnc) 
m4 <- lm(loutflowsa ~ Llgdpproduct2 + Lldist + Lcontig + as.numeric(culture_composite) + Lgrowcorr + Ldtt + Lpta2 + LI + LB + as.factor(ifs_o) + as.factor(ifs_d), data = norc_nomnc) 

display(m, detail = TRUE)
display(m2, detail = TRUE)
display(m3, detail= TRUE)
display(m4, detail = TRUE)
