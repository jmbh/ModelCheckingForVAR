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
# -------- Diagnostic Plots (ggplot new) ---
# ------------------------------------------

PlotDiagnostics <- function(l_ResObj, 
                            l_PPCs, 
                            subject) {
  
  
  l_row_plots <- list()
  
  for(j in 1:4) {
  
    # Make df with all we need
    N_data <- length(l_ResObj[[subject]]$Emp[, j])
    df_i <- data.frame(time = 1:N_data,
                       emp = l_ResObj[[subject]]$Emp[, j], 
                       pred = l_ResObj[[subject]]$Pred[, j], 
                       res = l_ResObj[[subject]]$Res[, j], 
                       sim = l_PPCs[[subject]]$data_sim[, j])
    
    
    # ---- Panel A: Data & Prediction
    # Plot 1: Line plot data + predictions
    p_line <- ggplot(df_i, aes(time, emp)) +
      geom_line() +
      theme_minimal() + 
      geom_line(aes(y = pred), color = "orange") +
      coord_cartesian(ylim = c(0, 100)) + 
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    
    # histogram of y
    mu <- mean(df_i$emp, na.rm = TRUE)
    sd <- sd(df_i$emp, na.rm = TRUE)
    p_hist <- ggplot(df_i, aes(emp)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20) +
      coord_flip(xlim = c(0, 100)) +
      theme_minimal() +
      stat_function(
        fun = dnorm,
        args = list(mean = mu, sd = sd),
        linewidth = 1
      ) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank()
      )
    # ---- Panel B: Residuals over time
    p_line_res <- ggplot(df_i, aes(time, res)) +
      geom_line() +
      theme_minimal() +
      coord_cartesian(ylim = c(-60, 60)) + theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    mu <- mean(df_i$res, na.rm = TRUE)
    sd <- sd(df_i$res, na.rm = TRUE)
    p_hist_res <- ggplot(df_i, aes(res)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20) +
      coord_flip(xlim = c(-60, 60)) +
      theme_minimal() +
      stat_function(
        fun = dnorm,
        args = list(mean = mu, sd = sd),
        linewidth = 1
      ) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank()
      )
    
    # p_line_res + p_hist_res
    # ---- Panel C: Scatter Plot:
    p_scatter <- ggplot(df_i, aes(x = res, y = pred)) +
      geom_point(alpha = 0.4) + 
      theme_minimal() +
      coord_cartesian(ylim = c(0, 100), 
                      xlim = c(-60, 60)) + 
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    # ---- Panel D: Data Simulated from Estimated Model
    p_line_sim <- ggplot(df_i, aes(time, sim)) +
      geom_line() +
      theme_minimal() +
      coord_cartesian(ylim = c(0, 100)) + theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    mu <- mean(df_i$sim, na.rm = TRUE)
    sd <- sd(df_i$sim, na.rm = TRUE)
    p_hist_sim <- ggplot(df_i, aes(sim)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20) +
      coord_flip(xlim = c(0, 100)) +
      theme_minimal() +
      stat_function(
        fun = dnorm,
        args = list(mean = mu, sd = sd),
        linewidth = 1
      ) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank()
      )
    
    p_line_sim + p_hist_sim
    
    l_row_plots[[j]] <- list(p_line,
                             p_hist,
                             p_line_res,
                             p_hist_res,
                             p_scatter,
                             p_line_sim,
                             p_hist_sim)
    
  } # end for: plot rows
  
  
  # Createlabels
  label_plot <- function(label, angle = 0, size = 5) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = label, angle = angle, size = size) +
      theme_void() +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
  }
  col1 <- label_plot("   Empirical & Predicted")
  col2 <- label_plot("   Residuals")
  col3 <- label_plot("Residual vs. Predicted")
  col4 <- label_plot("   Simulated")
  
  row1l <- label_plot("Happy", angle = 90)
  row2l <- label_plot("Relaxed", angle = 90)
  row3l <- label_plot("Sad", angle = 90)
  row4l <- label_plot("Angry", angle = 90)
  
  # Break up into lists for nicer looking code below:
  r1 <- l_row_plots[[1]]
  r2 <- l_row_plots[[2]]
  r3 <- l_row_plots[[3]]
  r4 <- l_row_plots[[4]]
  
  # Assemble all
  widths <- c(0.6, 4, 1, 4, 1, 4, 4, 1)
  heights <- c(0.6, 4, 4, 4, 4)
  top_row <- plot_spacer() + 
    col1 + plot_spacer() + col2 + plot_spacer() + col3 + col4 + plot_spacer() +
    plot_layout(widths = widths)
  row1 <- (row1l + r1[[1]] + r1[[2]] + r1[[3]] + r1[[4]] + r1[[5]] + r1[[6]] + r1[[7]]) +
    plot_layout(widths = widths)
  row2 <- (row2l + r2[[1]] + r2[[2]] + r2[[3]] + r2[[4]] + r2[[5]] + r2[[6]] + r2[[7]]) +
    plot_layout(widths = widths)
  row3 <- (row3l + r3[[1]] + r3[[2]] + r3[[3]] + r3[[4]] + r3[[5]] + r3[[6]] + r3[[7]]) +
    plot_layout(widths = widths)
  row4 <- (row4l + r4[[1]] + r4[[2]] + r4[[3]] + r4[[4]] + r4[[5]] + r4[[6]] + r4[[7]]) +
    plot_layout(widths = widths)
  
  
  top_row / row1 / row2 / row3 / row4 + plot_layout(heights = heights)
  
} # End of ggplot function

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
                     ylim=c(0, 100),
                     ylim_res = c(-50, 50),
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
  plot.window(xlim=c(1, 200), ylim=ylim_res)
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
# UPDATE BELOW

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
                        subject) {
  
  # ------------------------
  # ----- 3) Mplus ---------
  # ------------------------
  
  # ---- Some Basic info ----
  u_pers <- unique(data$id)
  vars <- model$vars
  p <- length(vars)
  
  # Get parameters
  phi_1 <- model$Ind_phi[ , , subject] 
  intc <- (diag(p) - phi_1) %*% matrix(model$Ind_mu[ subject, ], nrow=p) # Transform to intercepts
  
  # ---- Prepare data ----
  # Subset Data
  data_j <- data[data$id==u_pers[subject], ]
  # Within-person scale data
  # Joran: Mplus: Within-person scaling to match standardized coefficients
  
  data_j_sc <- data_j
  data_j_sc[, vars] <- apply(data_j[, vars], 2, scale)
  N <- nrow(data_j)
  
  # Find out which data points are predictable by VAR(1)
  pdb_j <- f_pdb(data_j$day, data_j$beep)
  
  # ----- Loop Through Variables & Compute Residuals ----
  a_res <- array(NA, dim=c(N, p, 3))
  
  for(i in 1:p) {
    ## Compute Residuals
    emp <- data_j_sc[, vars[i]] # Empirical Data
    pred <- intc[i] + rowSums(as.matrix(data_j_sc[, vars]) %*% matrix(phi_1[i, ], nrow=p) )
    
    pred[pdb_j==FALSE] <- NA # Flag time points for which predictions are not possible at lag1
    res <- emp - pred # Residuals
    
    # Save
    a_res[, i, 1] <- emp
    a_res[, i, 2] <- pred
    a_res[, i, 3] <- res
    
  } # end for i (variables)
  
  # ----- Compute Fit Measures ------
  v_RMSE <- apply(a_res[, , 3], 2, function(x) {
    sqrt(mean(na.omit(x)^2))
  })
  
  v_R2 <- rep(NA, p)
  for(i in 1:p) v_R2[i] <- 1 - var(a_res[, i, 3], na.rm = TRUE) / var(a_res[, i, 1], na.rm = TRUE)
  
  # ----- Calculate Residual variances ------
  ResVAR <- apply(a_res[, , 3], 2, function(x) var(x, na.rm=TRUE))
  
  # ----- Return ----------------
  
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
  
  return(outlist)
  
} # eoF




# -------------------------------------------------
# -------- PPC: Simulate Data ---------------------
# -------------------------------------------------

SimPPC <- function(data,
                   model,
                   subject,
                   ResObj,
                   init,
                   Nt) {
  
  # ---- Get Parameters for Given Subject -----
  if(class(model) == "mlVAR") {
    
    # Get Lagged effects
    phi_1 <- getNet(model, 
                    type = "temporal", 
                    subject = subject, 
                    verbose = FALSE)
    # Get the intercepts
    # mu_1 <- model$results$mu$subject[[subject]]
    # intc <- mu_1 # Those are intercepts

    # --- Rescale regression coefficients back to [0, 100]-scale ---
    # TO FIX: The below is roughly correct, but not exactly, because:
    # - mlVAR might rescale slightly differently
    # - There might be some interaction between scaling and mixed effects estimation
    # - Unknown unknowns in mlVAR
    
    # Get standard deviations of all variables across whole dataset
    sds <- apply(data[, c("happy", "relaxed", "sad", "angry")], 2, sd, na.rm=TRUE)
    # Rescaling with matrix calc; parameter wise I rescale with fraction: sd_predicted / sd_predictor
    D <- diag(sds)
    phi_1_resc <- D %*% phi_1 %*% solve(D)
    phi_1 <- phi_1_resc
    
    # Compute sample mean
    u_ptp <- unique(data$id)
    data_j <- data[data$id == u_ptp[[subject]], c("happy", "relaxed", "sad", "angry")]
    mu_emp <- apply(data_j[, c("happy", "relaxed", "sad", "angry")], 2, mean, na.rm=TRUE)
    # Compute Intercept
    intc <- (diag(4) - phi_1_resc) %*% mu_emp
    
  } else {
    phi_1 <- model$Ind_phi[,,subject] 
    intc <- model$Ind_mu[subject,] 
  }
  
  # ---- Get Residual Variance ----
  m_res <- ResObj[[subject]]$ResVar
  
  # browser()
  
  # ---- Simulate Data -----
  data_sim <- simulateVAR(pars = phi_1,
                          means = as.numeric(intc),
                          Nt = Nt,
                          residuals = m_res) # residual variance = 1
  
  
  # ---- Compile Outlist -----
  outlist <- list("data_emp" =  ResObj[[subject]]$Emp,
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
