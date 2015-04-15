## IPF - FDI Migration x Taxation Interaction Marginal Effects Plot

# setwd("~/Dropbox/projects/taxation/")

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
points(z.Ldtt1, dPI.dLI, pch = 19)
lines(c(0, 0), c(uprPI[1], lwrPI[1]), lwd = 3)
lines(c(1, 1), c(uprPI[2], lwrPI[2]), lwd = 3)

aplot(main = "FDI")
points(z.Ldtt1, dFDI.dLI, pch = 19)
lines(c(0, 0), c(uprFDI[1], lwrFDI[1]), lwd = 3)
lines(c(1, 1), c(uprFDI[2], lwrFDI[2]), lwd = 3)

mtext("Marginal Effect of Migrant Stock for Countries 
      with and without a Dual Taxation Treaty", outer = TRUE, line = 0.55)