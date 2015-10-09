## IPF - FDI Migration x Taxation Interaction Marginal Effects Plot

# setwd("~/Dropbox/projects/taxation/")

# library(ggplot2)

# make sure you still have your objects from running ipf_taxation_interaction.R
# and fdi_taxation_interaction.R in your environment before running this script

# plot the effect - this plot will give you Figure 1 and the values in Table 1
par(mfrow = c(1,2), mar = c(0.75, 0.75, 0.75, 0.75), oma = c(1,3,3.5,1))
eplot(xlim = mm(c(-0.5, 1.5)), ylim = mm(c(0.55, 0)), 
      ylab = "Marginal Effect of Migrant Stock",
      ylabpos = "2.1",
      main = "Portfolio Investment",
      xat = c(0,1),
      xticklab = c("No DTT", "DTT"))
points(z.Ldtt1[1], dPI.dLI[1], pch = 19, col = "#7570b3")
points(z.Ldtt1[2], dPI.dLI[2], pch = 19, col = "#d95f02")
lines(c(0, 0), c(uprPI[1], lwrPI[1]), lwd = 3, col= "#7570b3")
lines(c(1, 1), c(uprPI[2], lwrPI[2]), lwd = 3, lty = 3, col = "#d95f02")
abline(h = 0.1, col = "lightgrey")
abline(h = 0.2, col = "lightgrey")
abline(h = 0.3, col = "lightgrey")
abline(h = 0.4, col = "lightgrey")
abline(h = 0.5, col = "lightgrey")

aplot(main = "FDI")
points(z.Ldtt1[1], dFDI.dLI[1], pch = 19, col = "#7570b3")
points(z.Ldtt1[2], dFDI.dLI[2], pch = 19, col = "#d95f02")
lines(c(0, 0), c(uprFDI[1], lwrFDI[1]), lwd = 3, col = "#7570b3")
lines(c(1, 1), c(uprFDI[2], lwrFDI[2]), lwd = 3, lty = 3, col = "#d95f02")
abline(h = 0.1, col = "lightgrey")
abline(h = 0.2, col = "lightgrey")
abline(h = 0.3, col = "lightgrey")
abline(h = 0.4, col = "lightgrey")
abline(h = 0.5, col = "lightgrey")

mtext("Marginal Effect of Migrant Stock for Countries 
      with and without a Dual Taxation Treaty", outer = TRUE, line = 0.55)