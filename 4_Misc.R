# jonashaslbeck@protonmail.com; June 16th, 2025

# ------------------------------------------
# -------- What is happening here? ---------
# ------------------------------------------

# Make individual time+marginal plots of empirical data, predictions, residuals, and simulated data
# which I will use in the systems figure


# ------------------------------------------
# -------- Load Data -----------------------
# ------------------------------------------

# Load Simulated Data estimated Models
l_out <- readRDS("Files/Simulated_Data.RDS")

source("helpers2.R")


# ------------------------------------------
# -------- Make Figures --------------------
# ------------------------------------------

# ----- Global -----
i <- 1
x = l_out[[i]]$x
x_hat= l_out[[i]]$xhat
x_res = l_out[[i]]$res
x_ppc = l_out[[i]]$xsim
R2 = FitAR1(l_out[[i]]$x)$R2
RMSE = FitAR1(l_out[[i]]$x)$RMSE
ylim = c(-5, 5)
cex_info = 1
legend=FALSE

v_title <- rep("", 4)

sc <- 1

# ----- Empirical Time Series -----
pdf("Figures/Fig_Sys_emp.pdf", width=6*sc, height=4*sc)
lo <- layout(matrix(1:2, nrow=1), widths = c(1, 0.2))
par(mar=c(3,3,1,1))
plot.new()
plot.window(xlim=c(1, 200), ylim=ylim)
grid()
axis(1)
axis(2, las=2)
lines(x)
title(main=v_title[1], font.main=1, line=-1, cex.main=1.5)
par(mar=c(3,0,2,0.5))
PlotMarg(x, ylim=ylim)
dev.off()

# ----- Predicted Time Series -----
pdf("Figures/Fig_Sys_pred.pdf", width=6*sc, height=4*sc)
lo <- layout(matrix(1:2, nrow=1), widths = c(1, 0.2))
par(mar=c(3,3,1,1))
plot.new()
plot.window(xlim=c(1, 200), ylim=ylim)
grid()
axis(1)
axis(2, las=2)
lines(x_hat)
title(main=v_title[2], font.main=1, line=-1, cex.main=1.5)
par(mar=c(3,0,2,0.5))
PlotMarg(x_hat, ylim=ylim)
dev.off()

# ----- Residuals -----
pdf("Figures/Fig_Sys_resid.pdf", width=6*sc, height=4*sc)
lo <- layout(matrix(1:2, nrow=1), widths = c(1, 0.2))
par(mar=c(3,3,1,1))
plot.new()
plot.window(xlim=c(1, 200), ylim=ylim)
grid()
axis(1)
axis(2, las=2)
lines(x_res, lty=1, col="black", cex.main=1.5)
title(main=v_title[3], font.main=1, line=-1)
par(mar=c(3,0,2,0.5))
PlotMarg(x_hat, ylim=ylim)
dev.off()

# ----- Simulated -----
pdf("Figures/Fig_Sys_ppc.pdf", width=6*sc, height=4*sc)
lo <- layout(matrix(1:2, nrow=1), widths = c(1, 0.2))
par(mar=c(3,3,1,1))
plot.new()
plot.window(xlim=c(1, 200), ylim=ylim)
grid()
axis(1)
axis(2, las=2)
lines(x_ppc, lty=1, col="black", cex.main=1.5)
title(main=v_title[4], font.main=1, line=-1)
par(mar=c(3,0,2,0.5))
PlotMarg(x_ppc, ylim=ylim)
dev.off()








