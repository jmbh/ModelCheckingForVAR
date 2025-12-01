# jonashaslbeck@protonmail.com

# ------------------------------------------
# -------- What is happening here? ---------
# ------------------------------------------

# This file contains helper functions to:
# - Perform Residual Analysis and Simulate Data from AR(1) models
# - Perform Residual Analysis and Simulate Data from (ml)VAR(1) models
# - Perform Residual analysis for HMM models
# - Plotting functions
# - Simulate data from (ml)VAR(1) models
# - Perform HAC test for time trends

# ------------------------------------------
# -------- Loading Packages ----------------
# ------------------------------------------

library(scales)

# ------------------------------------------
# -------- Fit AR(1), Predict, Residuals ---
# ------------------------------------------

FitAR1 <- function(x){
  N <- length(x)
  lm_mod <- lm(x[-1] ~ x[-N])
  xhat <- rep(NA, N)
  xhat[2:N] <- predict(lm_mod)
  res <- xhat-x
  res_var <- sd(res[-1])
  
  # compure R2
  lm_mod_sum <- summary(lm_mod)
  
  # compute RMSE
  rmse <- sqrt(mean(res^2, na.rm=TRUE))
  
  return(list(lm_mod = lm_mod,
              xhat = xhat,
              res = res,
              res_var = res_var,
              R2 = round(lm_mod_sum$r.squared, 2),
              RMSE = round(rmse, 2)))
}


# ------------------------------------------
# -------- Simulate from AR(1) -------------
# ------------------------------------------

AR1PPC <- function(lm_mod, res_var, N, seed=92){
  xsim <- rep(NA, N)
  xsim[1] <- 0
  set.seed(seed)
  for(i in 2:N) xsim[i] <- lm_mod$coefficients[1] + lm_mod$coefficients[2]*xsim[i-1] + rnorm(1, 0, res_var)
  return(xsim)
}

# ------------------------------------------
# -------- Residual Analysis for HMM -------
# ------------------------------------------

# Function for HMM residual analysis
ResAnalysisHMM <- function(fit, x) {
  
  # Posterior probabilities and state assignments
  post <- posterior(fit)
  
  # Extract parameters for each state
  pars <- getpars(fit)
  
  # Extract the Gaussian means and sds
  mu <- pars[grep("\\(Intercept\\)", names(pars))]
  sigma <- pars[grep("^sd", names(pars))]
  
  nstates <- length(mu)
  # Posterior state probabilities = last nstates columns
  probs <- as.matrix(post[, (ncol(post) - nstates + 1):ncol(post)])
  
  # Predicted values: weighted average of state means
  pred <- probs %*% mu
  
  # Residuals
  resid <- x - pred
  
  # Return list
  outlist <- list("Emp" = x,
                  "Pred" = pred,
                  "Res" = resid,
                  "RMSE" = sqrt(mean(resid^2)),
                  "R2" = 1- var(resid)/var(x))
  return(outlist)
  
} # eoF

# ------------------------------------------
# -------- 1 Row of Multi Panel Figure -----
# ------------------------------------------

Plot1Row <- function(x,
                     x_hat,
                     x_res,
                     x_ppc,
                     R2 = NULL,
                     RMSE = NULL,
                     showresAR=FALSE,
                     ylim=c(-2, 2),
                     legend=FALSE,
                     alpha = 0.6,
                     colpred = "blue",
                     cex_info=1,
                     layout=FALSE,
                     xlab=FALSE) {
  
  # Mar Time series plots
  mar_ts <- c(3,3,0,1)
  
  # Layout
  if(layout) lo <- layout(matrix(1:7, nrow=1), widths = c(1,0.15,1,0.15, 1,1,0.15))
  
  l_cols <- list()
  l_cols$emp <- "black"
  l_cols$pred <- "orange"
  l_cols$res <- "black"
  l_cols$ppc <- "black"
  
  par(mar=mar_ts)
  # 1) Time Series + Predictions
  plot.new()
  plot.window(xlim=c(1, 200), ylim=ylim)
  grid()
  axis(1)
  axis(2, las=2)
  if(xlab) title(xlab="Time", line=2)
  lines(x, col= l_cols$emp)
  lines(x_hat, col=l_cols$pred)
  # Plot R2 and RMSE
  if(!is.null(R2)) text(140, ylim[1]+0.77, paste0("R2 = ", round(R2, 3)), adj=0, cex=cex_info)
  if(!is.null(RMSE)) text(140, ylim[1]+0.12, paste0("RMSE = ", round(RMSE, 3)), adj=0, cex=cex_info)
  if(legend) legend("bottomleft", legend=c("Data", "Predictions"), text.col=c(l_cols$emp, l_cols$pred),
                    bty = "n")
  
  h_hist <- 0.0
  par(mar=c(3,0,2,h_hist))
  PlotMarg(x, ylim=ylim)
  
  # 2) Residuals x Time
  par(mar=mar_ts)
  plot.new()
  plot.window(xlim=c(1, 200), ylim=ylim)
  grid()
  axis(1)
  axis(2, las=2)
  if(xlab) title(xlab="Time", line=2)
  lines(x_res, col=l_cols$res)
  # Plot residual autocorrelation
  if(showresAR) {
    lm_ar <- lm(x_res[-1] ~ x_res[-length(x_res)])
    lm_ar_sum <- summary(lm_ar)
    text(10, ylim[1]+0.4, paste0("AR(1) = ", round(lm_ar_sum$coefficients[2, 1], 2), ", p = ", round(lm_ar_sum$coefficients[2, 4], 2)), adj=0, cex=cex_info)
  }
  
  # 2.5) Residual Marginal
  par(mar=c(3,0,2,h_hist))
  PlotMarg(x_res, ylim=ylim)
  
  # 3) Residuals x Predictions
  par(mar=mar_ts)
  plot.new()
  plot.window(xlim=ylim, ylim=ylim)
  grid()
  axis(1)
  axis(2, las=2)
  if(xlab) title(xlab="Predictions", line=2)
  points(x_hat, x_res, col=alpha("black", alpha=alpha), pch=19)
  
  # 4) PCCs
  par(mar=mar_ts)
  plot.new()
  plot.window(xlim=c(1, 200), ylim=ylim)
  grid()
  axis(1)
  axis(2, las=2)
  if(xlab) title(xlab="Time", line=2)
  lines(x_ppc, col="black")
  par(mar=c(3,0,2,h_hist))
  PlotMarg(x_ppc, ylim=ylim)
  
} # eOF


# ------------------------------------------
# -------- Plot Marginals ------------------
# ------------------------------------------
# This is used in the plotting function above 

PlotMarg <- function(x, ylim) {
  
  # Don't show data outside plotting area
  x[x < ylim[1]] <- NA
  x[x > ylim[2]] <- NA
  
  hist_data <- hist(x, plot = FALSE, breaks=seq(ylim[1], ylim[2], length=30))
  bar_centers <- barplot(hist_data$counts,
                         horiz = TRUE,  # Horizontal bars
                         names.arg = NULL,
                         axes=FALSE, 
                         xlim = c(0, max(hist_data$counts)*1.1)) # To make sure that density fits on plot
  x_seq <- seq(ylim[1], ylim[2], length=1000)
  gauss_den <- dnorm(x_seq,
                     mean = mean(x, na.rm = TRUE),
                     sd = sd(x, na.rm = TRUE))
  bin_width <- diff(hist_data$breaks)[1]
  dens_counts <- gauss_den * length(x) * bin_width
  lines(dens_counts, seq(bar_centers[1], bar_centers[length(bar_centers)], length=1000), type = "l", lwd = 2, col = "black")
  
} # eoF


# ------------------------------------------
# -------- Plot Labels ---------------------
# ------------------------------------------
# This is used to plot labels into the layouts of the multi-panel figures

PlotLabel <- function(text, srt=0, cex=1.5,
                      xpos=0.5, ypos=0.5) {
  
  par(mar=rep(0, 4))
  
  plot.new()
  plot.window(xlim=c(0, 1), ylim=c(0,1))
  text(x=xpos, y=ypos, labels=text, srt=srt, cex=cex, adj=0.4)
  
}


# ------------------------------------------
# -------- Det. Predictable Time Points ----
# ------------------------------------------
# Takes dayvar and beepvar, and returns for a lag-1 VAR model whether any given time point can be predicted
# This is used in ResAnalysis()

f_pdb <- function(dayvar, beepvar) {
  n <- length(dayvar)
  v_pdb <- rep(NA, n)
  v_pdb[1] <- FALSE
  for(i in 2:n) {
    day_eq <- dayvar[i] == dayvar[i-1]
    beep_eq <- beepvar[i] == (beepvar[i-1]+1)
    v_pdb[i] <- ifelse(day_eq & beep_eq, TRUE, FALSE)
  }
  return(v_pdb)
} #eoF

# -------------------------------------------------
# -------- Residual Analysis ----------------------
# -------------------------------------------------
# Takes as input either:
# - A list with matrices with intercepts and an array with phi-matrices
# - or the output object of the mlVAR() function
# - it also takes the data as input, with the format as the data used for estimation

# It outputs:
# - The empirical data, predictions, and residuals for all variables
# - the extraced person-specific VAR parameters
# - RMSE and R2 for each variable

ResAnalysis <- function(model,
                        data = data,
                        newmlVAR = FALSE, # This uses a new version of mlVAR that directly provides the residuals
                        subject) {
  
  # ---- Take residuals from mlVAR output ----
  if(newmlVAR) {
    
    u_ptp <- unique(mlVAR_out$step1_residuals$id)
    
    # Subset for person i
    res_i <- mlVAR_out$step1_residuals[mlVAR_out$step1_residuals$id == u_ptp[subject], 1:4]
    emp_i <- mlVAR_out$data[mlVAR_out$data$id == u_ptp[subject], model$fit$var]
    pred_i <- emp_i - res_i
    
    # Get model parameters
    phi_1 <- getNet(model, type="temporal", subject=subject, verbose=FALSE)
    # Get the intercepts
    mu_1 <- model$results$mu$subject[[subject]]
    
    # ----- Compute Fit Measures ------
    v_RMSE <- apply(res_i, 2, function(x) {
      sqrt(mean(na.omit(x)^2))
    })
    
    p <- length(model$fit$var)
    v_R2 <- rep(NA, p)
    for(i in 1:p) v_R2[i] <- 1 - var(res_i[, i], na.rm = TRUE) / var(emp_i[, i], na.rm = TRUE)
    
    # ----- Calculate Residual variances ------
    ResVAR <- apply(res_i, 2, function(x) var(x, na.rm=TRUE))
    
    # ----- Get in the original missing pattern (For Plotting) -----
    id_miss <- apply(data[, model$fit$var], 1, function(x) any(is.na(x)))
    
    # ----- Return -----
    outlist <- list("Emp" = emp_i,
                    "Pred" = pred_i,
                    "Res" = res_i,
                    "ResVar" = ResVAR,
                    "phi" = phi_1,
                    "intc" = mu_1,
                    "RMSE" = v_RMSE,
                    "R2" = v_R2,
                    "id" = res_i$id[1],
                    "vars" = vars)
    
    # ---- Compute Residuals manually (before Sacha's mlVAR implementation that provides residuals directly) ----
  } else {
    
    # ---- Drop NA -----
    data <- na.omit(data)
    
    # ---- Some Basic info ----
    j <- subject
    u_pers <- unique(data$id)
    if(class(model) == "mlVAR") {
      vars <- model$fit$var
    } else {
      vars <- model$vars
    }
    p <- length(vars)
    
    # ---- Get parameters from output ----
    if(class(model) == "mlVAR") {
      # Get Lagged effects
      phi_1 <- getNet(model, type="temporal", subject=j, verbose=FALSE)
      # Get the intercepts
      mu_1 <- model$results$mu$subject[[j]]
      # intc <- (diag(p) - phi_1) %*% matrix(mu_1, nrow=p) # Getting intercepts with c = (I-phi)mu
      # intc <- as.numeric(intc)
      intc <- mu_1 # check with Sacha whether mus are intercepts; Yes they are!
      
    } else {
      phi_1 <- model$Ind_phi[ , , j] 
      intc <- model$Ind_mu[ j, ] 
    }
    
    # ---- Prepare data ----
    # Grand-mean center and scale data
    data_sc <- data
    data_sc[, 4:7] <- as.data.frame(apply(data[, 4:7], 2, scale))
    
    # Subset Data
    data_j <- data_sc[data_sc$id==u_pers[j], ]
    N <- nrow(data_j)
    
    # Find out which data points are predictable by VAR(1)
    pdb_j <- f_pdb(data_j$day, data_j$beep)
    
    # ----- Loop Through Variables & Compute Residuals ----
    a_res <- array(NA, dim=c(N, p, 3))
    for(i in 1:p) {
      # Center predictors
      data_jc <- data_j
      data_jc[, -c(1:3, i+3)] <- apply(data_j[, -c(1:3, i+3)], 2, function(x) x - mean(x, na.rm=TRUE))
      
      ## Compute
      # Empirical Data
      a_res[, i, 1] <- Emp_ji <- data_jc[, vars[i]]
      a_res[, i, 1][pdb_j==FALSE] <- NA
      # Prediction
      a_res[2:N, i, 2] <- intc[i] + rowSums(as.matrix(data_jc[-N, vars])  %*% matrix(phi_1[i, ], nrow=p)) # Predict with VAR
      a_res[, i, 2][pdb_j==FALSE] <- NA # Set predictions that are not over lag-1 to zero (Yes, this is a bit ugly!)
      # Residual
      a_res[, i, 3] <- Emp_ji - a_res[, i, 2]
    } # end for i
    
    # ----- Compute Fit Measures ------
    v_RMSE <- apply(a_res[, , 3], 2, function(x) {
      sqrt(mean(na.omit(x)^2))
    })
    
    v_R2 <- rep(NA, p)
    for(i in 1:p) v_R2[i] <- 1 - var(a_res[, i, 3], na.rm = TRUE) / var(a_res[, i, 1], na.rm = TRUE)
    
    # ----- Calculate Residual variances ------
    ResVAR <- apply(a_res[, , 3], 2, function(x) var(x, na.rm=TRUE))
    
    # ----- Return -----
    outlist <- list("Emp" = a_res[, , 1],
                    "Pred" = a_res[, , 2],
                    "Res" = a_res[, , 3],
                    "ResVar" = ResVAR,
                    "phi" = phi_1,
                    "intc" = intc,
                    "RMSE" = v_RMSE,
                    "R2" = v_R2,
                    "id" = data_j$id[1],
                    "vars" = vars)
    
  } # end if: Old mlVAR
  
  return(outlist)
  
} # eoF


# -------------------------------------------------
# -------- PPC: Simulate Data ---------------------
# -------------------------------------------------

SimPPC <- function(data,
                   model,
                   subject,
                   init,
                   Nt) {
  
  # ---- Get basic info ----
  if(class(model) == "mlVAR") {
    vars <- model$fit$var
  } else {
    vars <- model$vars
  }
  
  # ---- Get Parameters for Given Subject -----
  if(class(model) == "mlVAR") {
    # Get Lagged effects
    phi_1 <- getNet(model, type="temporal", subject=subject, verbose=FALSE)
    # Get the intercepts
    mu_1 <- model$results$mu$subject[[subject]]
    intc <- mu_1 # Those are intercepts
    
  } else {
    phi_1 <- model$Ind_phi[,,subject] 
    intc <- model$Ind_mu[subject,] 
  }
  
  # ---- Subset Data ----
  u_ptp <- unique(data$id)
  data_j <- data[data$id==u_ptp[subject], ]
  N <- nrow(data_j)
  
  # ---- Compute Residuals to get Residual Variance ----
  pdb_j <- f_pdb(data_j$day, data_j$beep)
  # For mlVAR: scale data
  data_j_sc <- data_j
  data_j_sc[, 4:7] <- apply(data_j_sc[, 4:7], 2, scale)
  data_j <- data_j_sc
  
  p <- 4
  a_res <- array(NA, dim=c(N, p, 3))
  for(i in 1:p) {
    ## Compute
    # Empirical Data
    a_res[, i, 1] <- Emp_ji <- data_j[, vars[i]]
    a_res[, i, 1][pdb_j==FALSE] <- NA
    # Prediction
    a_res[2:N, i, 2] <- intc[i] + rowSums(as.matrix(data_j[-N, vars])  %*% matrix(phi_1[i, ], nrow=p)) # Predict with VAR
    a_res[, i, 2][pdb_j==FALSE] <- NA # Set predictions that are not over lag-1 to zero (this could be done in a safer way ...)
    # Residual
    a_res[, i, 3] <- Emp_ji - a_res[, i, 2]
  } # end for i
  # Compute residual variances
  res_sd <- apply(a_res[, , 3], 2, sd, na.rm=TRUE)
  m_res <- diag(p) * res_sd^2 # residual covariance matrix
  
  # ---- Simulate Data -----
  data_sim <- simulateVAR(pars = phi_1,
                          means = intc,
                          Nt = Nt,
                          residuals = m_res) # residual variance = 1
  
  
  # ---- Compile Outlist -----
  outlist <- list("data_emp" = data_j[, 4:7],
                  "data_sim" = data_sim,
                  "subj" = u_ptp[subject])
  
  return(outlist)
  
} # EoF


# ----------------------------------------------------------------
# -------- HAC-robust (Newey–West) test of a time-trend ----------
# ----------------------------------------------------------------
# This is used in the end of Section 4

trend_test_hac <- function(y, time = seq_along(y), prewhite = TRUE) {
  stopifnot(length(y) == length(time))
  fit <- lm(y ~ time)
  
  n   <- length(y)
  # Plug-in bandwidth often used for HAC (Andrews–Newey–West style)
  lag <- floor(4 * (n / 100)^(2/9))
  
  Vnw <- NeweyWest(fit, lag = lag, prewhite = prewhite, adjust = TRUE)
  ct  <- coeftest(fit, vcov = Vnw)
  
  # Pull slope row; name will match the predictor ("time")
  pval <- ct["time", "Pr(>|t|)"]
  outlist <- list(
    method   = "HAC-robust (Newey–West) test for linear time trend",
    estimate = unname(ct["time", "Estimate"]),
    se       = unname(ct["time", "Std. Error"]),
    t        = unname(ct["time", "t value"]),
    p.value  = unname(pval),
    lag_used = lag
  )
  
  outlist$p.value
}
