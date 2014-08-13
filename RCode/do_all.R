## Do all to replicate "Overcoming Structural Barriers: The Conditional
## Effect of Migrant Networks and Coordinated Financial Policies on
## International Investment" by Kelly McCaskey

rm(list = ls())

# set working directory
setwd("/Users/kellymccaskey/Dropbox/Projects/Taxation/") # <---- change this 

# load packages
library(foreign)
library(arm)
library(compactr)
library(sandwich)
library(lmtest)
library(robust)

# load data
d <- read.dta("Data/apsr_replication.dta")

# migration x DTT on international portfolio investment 
# (the left side of Table 2)
source("RCode/ipf_taxation_interaction.R")

# migration x DTT on FDI
# (the right side of Table 2)
source("RCode/fdi_taxation_interaction.R")

# marginal effects plot 
# (Table 1, Figure 1)
source("RCode/plot_marginal_effects.R")

# international portfolio investment outliers & leverage points
# (Table 3, Figure 2)
source("RCode/ipf_outliers.R")

# FDI outliers & leverage points
# (Figure 3)
source("RCode/fdi_outliers.R")

# international portfolio investment robust estimate coefficient plot
# (Figure 4)
# (this is a robustness check so it is not necessary to run but be aware it will
# take 30+ minutes to run)
# source("RCode/robust_ipf.R")

# FDI robust estimate coefficient plot
# (Figure 5)
# (this is a robustness check so it is not necessary to run but be aware it will
# take 30+ minutes to run)
source("RCode/robust_fdi.R")
