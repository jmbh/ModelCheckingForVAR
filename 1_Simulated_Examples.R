# jonashaslbeck@protonmail.com

# ------------------------------------------
# -------- What is happening here? ---------
# ------------------------------------------

# Here we generate data from 8 different models, two correctly specified, six misspecified with respect to the AR(1) model.
# These are the basis for the respective figures in the main text and the appendix

# ------------------------------------------
# -------- Load Packages & Source ----------
# ------------------------------------------

# For Plotting
library(RColorBrewer)

# Fitting HMMs
library(depmixS4)

# Load helper functions
source("0_Helpers.R")


# ---------------------------------------------------
# -------- Setup Models & Generate Data -------------
# ---------------------------------------------------

# Here we define the 8 models and generate data from them

l_synth <- list()

# -------- DGM1: Correctly specified, AR>0 -----
N <- 200
x <- rep(NA, N)
x[1] <- 0
ar <- 0.6
int <- 0
int / (1-ar) # mean
set.seed(2)
for(i in 2:N) x[i] <- int + ar*x[i-1] + rnorm(1, 0, 1)
l_synth[[1]] <- x

# -------- DGM2: Correctly specified, AR=0 -----
N <- 200
x <- rep(NA, N)
x[1] <- 0
ar <- 0
int <- 0
set.seed(3)
for(i in 2:N) x[i] <- int + ar*x[i-1] + rnorm(1, 0, 1)
l_synth[[2]] <- x

# -------- DGM3: Misspecified, Trend  -----
N <- 200
x <- rep(NA, N)
x[1] <- -15
ar <- 0.6
int <- -6
int / (1-ar) # mean
set.seed(2)
for(i in 2:N) x[i] <- ar*x[i-1] + rnorm(1, 0, 1) + int + 0.06*i
x <- (x/sd(x))*2
l_synth[[3]] <- x

# -------- DGM4: Misspecified, Non-linear/Switching -----
N <- 200
x <- c(rep(-2, 30), 
       rep(2, 25), 
       rep(-2, 40), 
       rep(2, 10),
       rep(-2, 60),
       rep(2, 35)) + rnorm(N, mean=0, sd=0.5)
l_synth[[4]] <- x



# -------- DGM5: Misspecified, Heteroscedasticity -----
N <- 200
x <- rep(NA, N)
x[1] <- 0
ar <- 0.6
int <- 0
int / (1-ar) # mean
set.seed(2)
for(i in 2:N) x[i] <- ar*x[i-1] + rnorm(1, 0, 0.1 + 0.012*i) + int
# plot(x, type="l")
l_synth[[5]] <- x

# -------- DGM6: Misspecified, Seasonality -----
N <- 200
x <- rep(NA, N)
x[1] <- 0
ar <- 0.6
int <- -1
int / (1-ar) # mean
beta <- 2 # weekend effect on intercept
set.seed(4)
# make a weekend effect:
weekend <- rep(c(rep(0, 5), rep(1, 2)), 29)[1:N]
for(i in 2:N) x[i] <- int + beta * weekend[i] + ar*x[i-1] + rnorm(1, 0, 1)
# plot(x, type="l")
l_synth[[6]] <- x

# -------- DGM7: Misspecified, State-dependent Innovations -----
N <- 200
x <- rep(NA, N)
x[1] <- 0
ar <- 0.6
int <- 0
int / (1-ar) # mean
beta <- 0.3 # effect of state x on innovation SD
set.seed(2)
for(i in 2:N) x[i] <- ar*x[i-1] + rnorm(1, 0, 1 + beta*x[i-1]) + int
# plot(x, type="l")
l_synth[[7]] <- x

# -------- DGM8: Misspecified, Non-Gaussian noise -> Heavy Tails -----
N <- 200
x <- rep(NA, N)
x[1] <- 0
ar <- 0.6
int <- -1
int / (1-ar) # mean
set.seed(2)
for(i in 2:N) x[i] <- int + ar*x[i-1] + rexp(1, rate=1)
# plot(x, type="l")
l_synth[[8]] <- x


# ---------------------------------------------------
# -------- Fit AR1, Compute Residuals, Simulate -----
# ---------------------------------------------------

# We compute residuals and simulate data from the fitted AR(1) models
# For that, we use the functions FitAR1() and AR1PPC() from Helpers.R

l_out <- list()
for(i in 1:8) {

  # ---- Predict with AR(1) Model -----
  fitAR1 <- FitAR1(l_synth[[i]])
  xhat <- fitAR1$xhat
  res <- fitAR1$res
  res_var <- fitAR1$res_var

  # ---- PPC -----
  out_AR1PPC <- AR1PPC(fitAR1$lm_mod, fitAR1$res_var, N, seed=92)

  # ---- Save in List ----
  l_out[[i]] <- list(x = l_synth[[i]],
                     xhat = xhat,
                     res = res,
                     xsim = out_AR1PPC)
} # end for


# Save results
saveRDS(l_out, "Files/Simulated_Data.RDS")

# ---- Numbers reported in Paper -----
# Compute some numbers we report in the paper

# Coefficient DGM4 (Switching)
fitAR1 <- FitAR1(l_synth[[4]])
round(fitAR1$lm_mod$coefficients[2], 2)
# Coefficient DGM3 (Trend)
fitAR1 <- FitAR1(l_synth[[3]])
round(fitAR1$lm_mod$coefficients[2], 2)

# RMSE of Linear Trend
fitAR1_3 <- FitAR1(l_synth[[3]])
fitAR1_3$R2
fitAR1_3$RMSE

# RMSE of Heteroscedasticity (DGM5)
fitAR1_3 <- FitAR1(l_synth[[5]])
fitAR1_3$R2
fitAR1_3$RMSE


# -----------------------------------------------
# ----- Figure: Correctly specified AR(1) -------
# -----------------------------------------------

sel <- 1:2
v_names <- c("Dependence", "Independence")

# --- Plotting ----
pdf("Figures/Fig_AR1_Correct.pdf",  width=11, height=6)

# Set up plotting area
lmat <- matrix(4:24, nrow=3, byrow = TRUE)
lmat <- cbind(1:3, lmat)
lo <- layout(mat = lmat,
             heights = c(0.1, 1, 1, 1, 1),
             widths = c(0.1, 1,0.15,1,0.15, 1,1,0.15))
# layout.show(lo)

## Plot Labels
cex_lab <- 1.5
## Row labels
PlotLabel("", cex=cex_lab)
PlotLabel(v_names[1], cex=cex_lab, srt = 90)
PlotLabel(v_names[2], cex=cex_lab, srt = 90)
## Column labels
PlotLabel("Data & Predictions", cex=cex_lab, ypos=0.3)
PlotLabel("", cex=cex_lab)
PlotLabel("   Residuals vs. Time", cex=cex_lab)
PlotLabel("", cex=cex_lab)
PlotLabel("Residuals vs. Predictions", cex=cex_lab)
PlotLabel("Simulated Data", cex=cex_lab)
PlotLabel("", cex=cex_lab)

## Plot Data
for(i in 1:2) {
  i_sel <- sel[i]
  Plot1Row(x = l_out[[i]]$x,
           x_hat= l_out[[i]]$xhat,
           x_res = l_out[[i]]$res,
           x_ppc = l_out[[i]]$xsim,
           R2 = FitAR1(l_out[[i]]$x)$R2,
           RMSE = FitAR1(l_out[[i]]$x)$RMSE,
           ylim=c(-5, 5),
           showresAR = TRUE,
           xlab=c(FALSE, TRUE)[i],
           legend=TRUE, colpred = "orange")
}

dev.off()

# -----------------------------------------------
# ----- Figure: Three Main Misspecifications ----
# -----------------------------------------------

sel <- c(4,3,5)
v_names <- c("Trend", "Switching/Non-Linear", "Changing Innovations")

# --- Plotting ----
pdf("Figures/Fig_AR1_Misspecified_Main.pdf",  width=11, height=8)

# Set up plotting area
lmat <- matrix(5:32, nrow=4, byrow = TRUE)
lmat <- cbind(1:4, lmat)
lo <- layout(mat = lmat,
             heights = c(0.1, 1, 1, 1, 1),
             widths = c(0.1, 1,0.15,1,0.15, 1,1,0.15))
# layout.show(lo)

## Plot Labels
cex_lab <- 1.5
## Row labels
PlotLabel("", cex=cex_lab)
PlotLabel(v_names[1], cex=cex_lab, srt = 90)
PlotLabel(v_names[2], cex=cex_lab, srt = 90)
PlotLabel(v_names[3], cex=cex_lab, srt = 90)
## Column labels
PlotLabel("Data & Predictions", cex=cex_lab, ypos=0.3)
PlotLabel("", cex=cex_lab)
PlotLabel("   Residuals vs. Time", cex=cex_lab)
PlotLabel("", cex=cex_lab)
PlotLabel("Residuals vs. Predictions", cex=cex_lab)
PlotLabel("Simulated Data", cex=cex_lab)
PlotLabel("", cex=cex_lab)

## Plot Data
for(i in 1:3) {
  i_sel <- sel[i]
  Plot1Row(x = l_out[[i_sel]]$x,
           x_hat= l_out[[i_sel]]$xhat,
           x_res = l_out[[i_sel]]$res,
           x_ppc = l_out[[i_sel]]$xsim,
           R2 = FitAR1(l_out[[i_sel]]$x)$R2,
           RMSE = FitAR1(l_out[[i_sel]]$x)$RMSE,
           ylim=c(-5, 5),
           showresAR = TRUE,
           xlab=c(FALSE, FALSE, TRUE)[i],
           legend=c(TRUE, FALSE, FALSE)[i],
           colpred = "orange")
}

dev.off()


# -----------------------------------------------
# ----- Figure: Three Extra Misspecifications ---
# -----------------------------------------------

sel <- 6:8
v_names <- c("Seasonality", "State-Dependent Innovations", "Non-Gaussian Innovations")

# --- Plotting ----
pdf("Figures/Fig_AR1_Misspecified_Extra.pdf",  width=11, height=8)

# Set up plotting area
lmat <- matrix(5:32, nrow=4, byrow = TRUE)
lmat <- cbind(1:4, lmat)
lo <- layout(mat = lmat,
             heights = c(0.1, 1, 1, 1, 1),
             widths = c(0.1, 1,0.15,1,0.15, 1,1,0.15))
# layout.show(lo)

## Plot Labels
cex_lab <- 1.5
## Row labels
PlotLabel("", cex=cex_lab)
PlotLabel(v_names[1], cex=cex_lab, srt = 90, ypos=0.45)
PlotLabel(v_names[2], cex=cex_lab, srt = 90, ypos=0.45)
PlotLabel(v_names[3], cex=cex_lab, srt = 90, ypos=0.45)
## Column labels
PlotLabel("Data & Predictions", cex=cex_lab, ypos=0.3)
PlotLabel("", cex=cex_lab)
PlotLabel("   Residuals vs. Time", cex=cex_lab)
PlotLabel("", cex=cex_lab)
PlotLabel("Residuals vs. Predictions", cex=cex_lab)
PlotLabel("Simulated Data", cex=cex_lab)
PlotLabel("", cex=cex_lab)

## Plot Data
for(i in 1:3) {
  i_sel <- sel[i]
  Plot1Row(x = l_out[[i_sel]]$x,
           x_hat= l_out[[i_sel]]$xhat,
           x_res = l_out[[i_sel]]$res,
           x_ppc = l_out[[i_sel]]$xsim,
           R2 = FitAR1(l_out[[i_sel]]$x)$R2,
           RMSE = FitAR1(l_out[[i_sel]]$x)$RMSE,
           ylim=c(-5, 5),
           showresAR = TRUE,
           xlab=c(FALSE, FALSE, TRUE)[i],
           legend=c(TRUE, FALSE, FALSE)[i],
           colpred = "orange")
}

dev.off()



# -----------------------------------------------
# ----- Fit HMM to Switching data from DGM4 -----
# -----------------------------------------------

# Fit a 2-state HMM with Gaussian emissions
mod <- depmix(response = l_synth[[4]] ~ 1,
              family = gaussian(),
              nstates = 2,
              data = data.frame(x=x))
fit <- fit(mod)


# Compute Residuals using ResAnalysisHMM() from Helpers.R
out <- ResAnalysisHMM(fit = fit,
                      x = l_synth[[4]])

# Simulate from estimated HMM
set.seed(4)
sim_HMM <- simulate(fit, nsim = 200)
sim_data <- sim_HMM@response[[1]][[1]]@y[1:200,1]


# --- Plotting ----
pdf("Figures/Fig_HMM_ModelFit.pdf",  width=11, height=3)

# Set up plotting area
lmat <- matrix(1:14, nrow=2, byrow = TRUE)
lo <- layout(mat = lmat,
             heights = c(0.1, 1),
             widths = c(1,0.15,1,0.15, 1,1,0.15))

# Column labels
PlotLabel("Data & Predictions", cex=cex_lab, ypos=0.3)
PlotLabel("", cex=cex_lab)
PlotLabel("   Residuals vs. Time", cex=cex_lab)
PlotLabel("", cex=cex_lab)
PlotLabel("Residuals vs. Predictions", cex=cex_lab)
PlotLabel("Simulated Data", cex=cex_lab)
PlotLabel("", cex=cex_lab)

Plot1Row(x = out$Emp,
         x_hat= out$Pred,
         x_res = out$Res,
         x_ppc = sim_data,
         R2 = out$R2,
         RMSE = out$RMSE,
         ylim=c(-5, 5),
         showresAR = TRUE,
         legend=TRUE,
         colpred = "orange",
         layout=FALSE, xlab=TRUE)

dev.off()






