## Do all to replicate "Overcoming Structural Barriers: The Conditional
## Effect of Migrant Networks and Coordinated Financial Policies on
## International Investment" by Kelly McCaskey

rm(list = ls())

# set working directory
setwd("~/Dropbox/projects/taxation/") # <---- change this 

# load packages
library(arm)
library(compactr)
library(sandwich)
library(lmtest)
library(robust)
library(ggplot2)

# load data
# d <- read.csv("data/taxation.csv")

# create cultural similarity composite measure
# d$culture_composite <- d$Lcomlang_off + d$Lcommoncurrency + d$commonlegal + d$commonreligion
# summary(d$culture_composite)
# write.csv(d, "data/taxation-composite.csv")
d <- read.csv("data/taxation-composite.csv")

# migration x DTT on international portfolio investment 
# (the left side of Table 2)
source("R/ipf_taxation_interaction.R")

# migration x DTT on FDI
# (the right side of Table 2)
source("R/fdi_taxation_interaction.R")

# marginal effects plot 
# (Table 1, Figure 1)
source("R/plot_marginal_effects.R")

# international portfolio investment outliers & leverage points
# (Table 3, Figure 2)
source("R/ipf_outliers.R")

# FDI outliers & leverage points
# (Figure 3)
source("R/fdi_outliers.R")

# international portfolio investment robust estimate coefficient plot
# (Figure 4)
# (this is a robustness check so it is not necessary to run but be aware it will
# take 30+ minutes to run)
# source("R Code/robust_ipf.R")

# FDI robust estimate coefficient plot
# (Figure 5)
# (this is a robustness check so it is not necessary to run but be aware it will
# take 30+ minutes to run)
# source("R Code/robust_fdi.R")
