# jonashaslbeck@protonmail.com; March 2026

# ------------------------------------------
# -------- What is happening here? ---------
# ------------------------------------------

# Helper functions for plotting, computing residuals and simulating for the model from the DSEM model, computing residuals for the HMM,  computing summaries on the residuals, and some other smaller tasks.

# ------------------------------------------
# -------- Loading Packages ----------------
# ------------------------------------------



# ------------------------------------------
# -------- Plot 1 Row of Diagnostics -------
# ------------------------------------------

Plot1Row <- function(emp, 
                     pred, 
                     res, 
                     sim,
                     legpos = "topleft", 
                     ylim_data = NULL, 
                     ylim_res = NULL,
                     posR2 = NULL,
                     posAR = NULL,
                     legend = TRUE, 
                     label_size = 5) {
  
  # Settings
  linewidth = 0.35
  hist_col <- "grey50"
  
  if(legpos == "topleft") legend.position <- c(0.3, 0.9)
  if(legpos == "bottomright") legend.position <- c(0.75, 0.3)

  # Compute R2
  R2 <- 1 - var(res, na.rm = TRUE) / var(emp, na.rm = TRUE)
  
  # Make df with all we need
  N_data <- length(emp)
  df_i <- data.frame(time = 1:N_data,
                     emp = emp, 
                     pred = pred, 
                     res = res, 
                     sim = sim)

  # ---- Panel A: Data & Prediction
  # Plot 1: Line plot data + predictions
  p_line <- ggplot(df_i, aes(time, emp)) +
    geom_line(aes(y = emp, color = "Empirical"), linewidth = linewidth) +
    geom_line(aes(y = pred, color = "Predictions"), linewidth = linewidth) +
    annotate(
      "text",
      x = posR2[1],
      y = posR2[2],
      label = paste0("R^2==", round(R2, 2)),
      parse=TRUE,
      size = 2.75
    ) +
    theme_minimal() + 
    coord_cartesian(ylim = ylim_data) + 
    scale_color_manual(
      values = c(
        "Empirical" = "black",
        "Predictions" = "darkorange2"
      )
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = if (legend) legend.position else "none",
      legend.spacing.y = unit(0.04, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.title = element_blank()
    )
  
  # histogram of y
  mu <- mean(df_i$emp, na.rm = TRUE)
  sd <- sd(df_i$emp, na.rm = TRUE)
  p_hist <- ggplot(df_i, aes(emp)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20, fill = hist_col) +
    coord_flip(xlim = ylim_data) +
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
  # Fit linear trend
  lm_trnd <- lm(res[-1]~res[-nrow(df_i)], data=df_i)
  Pnt <- round(lm_trnd$coefficients[2], 2)
  CIs <- round(confint(lm_trnd, level = 0.95)[2, ], 2)
  # Line plot
  p_line_res <- ggplot(df_i, aes(time, res)) +
    geom_line(linewidth = linewidth) +
    theme_minimal() +
    coord_cartesian(ylim = ylim_res) + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) + 
    annotate(
      "text",
      x = posAR[1],
      y = posAR[2],
      label = paste0("AR(1): ", Pnt, ", 95%CI: [", CIs[1], ", ", CIs[2], "]"),
      size = 2.75
    )
  
  mu <- mean(df_i$res, na.rm = TRUE)
  sd <- sd(df_i$res, na.rm = TRUE)
  p_hist_res <- ggplot(df_i, aes(res)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20, fill = hist_col) +
    coord_flip(xlim = ylim_res) +
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
  
  p_line_res+p_hist_res
  
  # ---- Panel C: Scatter Plot:
  p_scatter <- ggplot(df_i, aes(x = res, y = pred)) +
    geom_point(alpha = 0.4) + 
    theme_minimal() +
    coord_cartesian(ylim = ylim_data, 
                    xlim = ylim_res) + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  # ---- Panel D: Data Simulated from Estimated Model
  p_line_sim <- ggplot(df_i, aes(time, sim)) +
    geom_line(linewidth = linewidth) +
    theme_minimal() +
    coord_cartesian(ylim = ylim_data) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  mu <- mean(df_i$sim, na.rm = TRUE)
  sd <- sd(df_i$sim, na.rm = TRUE)
  p_hist_sim <- ggplot(df_i, aes(sim)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20, fill = hist_col) +
    coord_flip(xlim = ylim_data) +
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
  
  outlist <- list(p_line,
                  p_hist,
                  p_line_res,
                  p_hist_res,
                  p_scatter,
                  p_line_sim,
                  p_hist_sim)
  
  return(outlist)
  
} # EoF

# ------------------------------------------
# -------- Diagnostic Plots (Empirical) ----
# ------------------------------------------

PlotDiagnosticsEmp <- function(l_ResObj, 
                               l_PPCs, 
                               subject, 
                               v_legend,
                               legpos = "topleft", 
                               ylim_data = c(0,100), 
                               ylim_res = c(-60, 60),
                               posR2 = c(180, 100),
                               posAR = c(110, -55),
                               label_size = 5) {
  
  # Storing plots
  l_row_plots <- list()
  for(j in 1:4) l_row_plots[[j]] <- Plot1Row(emp = l_ResObj[[subject]]$Emp[, j],
                                             pred = l_ResObj[[subject]]$Pred[, j],
                                             res = l_ResObj[[subject]]$Res[, j],
                                             sim = l_PPCs[[subject]]$data_sim[, j],
                                             legpos = legpos,
                                             ylim_data = ylim_data,
                                             ylim_res = ylim_res,
                                             posR2 = posR2,
                                             posAR = posAR,
                                             legend = v_legend[j])
  
  # Create labels
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
# -------- Diagnostic Plots (Simulated) ----
# ------------------------------------------


PlotDiagnosticsSim <- function(l_out, # object with data 
                               sel,  # selection of cases
                               legpos = "topleft", 
                               labels, 
                               v_legend,
                               ylim_data = c(-4,4), 
                               ylim_res = c(-4,4), 
                               posR2 = c(180, 4),
                               posAR = c(110, -3.5),
                               label_size = 5) {
  
  n_row <- length(sel)
  
  # Storing plots
  l_row_plots <- list()
  for(j in 1:n_row) l_row_plots[[j]] <- Plot1Row(emp = l_out[[sel[j]]]$x,
                                                 pred = l_out[[sel[j]]]$xhat,
                                                 res = l_out[[sel[j]]]$res,
                                                 sim = l_out[[sel[j]]]$xsim,
                                                 legpos = legpos, 
                                                 ylim_data = ylim_data, 
                                                 ylim_res = ylim_res, 
                                                 posR2 = posR2,
                                                 
                                                 posAR = posAR,
                                                 legend = v_legend[j])
  
  # Create labels
  label_plot <- function(label, angle = 0, size = label_size) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = label, angle = angle, size = size) +
      theme_void() +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
  }
  col1 <- label_plot("   Empirical & Predicted")
  col2 <- label_plot("   Residuals")
  col3 <- label_plot("Residual vs. Predicted")
  col4 <- label_plot("   Simulated")
  
  l_rowplots <- list()
  for(j in 1:n_row) l_rowplots[[j]] <- label_plot(labels[j], angle = 90)
  
  # Split two cases: 1 vs 2 vs 3 rows
  # --- 1 Row (for HMM) ---
  if(n_row == 1) {
    
    # Break up into lists for nicer looking code below:
    r1 <- l_row_plots[[1]]
    # Assemble all
    widths <- c(0.6, 4, 1, 4, 1, 4, 4, 1)
    heights <- c(0.6, 4)
    top_row <- plot_spacer() + 
      col1 + plot_spacer() + col2 + plot_spacer() + col3 + col4 + plot_spacer() +
      plot_layout(widths = widths)
    row1 <- ( l_rowplots[[1]] + r1[[1]] + r1[[2]] + r1[[3]] + r1[[4]] + r1[[5]] + r1[[6]] + r1[[7]]) +
      plot_layout(widths = widths)
    
    print(top_row / row1 + plot_layout(heights = heights))
  } # end: if
  
  
  # --- 2 Rows ---
  if(n_row == 2) {
    
    # Break up into lists for nicer looking code below:
    r1 <- l_row_plots[[1]]
    r2 <- l_row_plots[[2]]
    # Assemble all
    widths <- c(0.6, 4, 1, 4, 1, 4, 4, 1)
    heights <- c(0.6, 4, 4)
    top_row <- plot_spacer() + 
      col1 + plot_spacer() + col2 + plot_spacer() + col3 + col4 + plot_spacer() +
      plot_layout(widths = widths)
    row1 <- ( l_rowplots[[1]] + r1[[1]] + r1[[2]] + r1[[3]] + r1[[4]] + r1[[5]] + r1[[6]] + r1[[7]]) +
      plot_layout(widths = widths)
    row2 <- (l_rowplots[[2]] + r2[[1]] + r2[[2]] + r2[[3]] + r2[[4]] + r2[[5]] + r2[[6]] + r2[[7]]) +
      plot_layout(widths = widths)
    
    print(top_row / row1 / row2 + plot_layout(heights = heights))
  } # end: if
  
  # --- 3 Rows ---
  if(n_row == 3) {
    
    # Break up into lists for nicer looking code below:
    r1 <- l_row_plots[[1]]
    r2 <- l_row_plots[[2]]
    r3 <- l_row_plots[[3]]
    # Assemble all
    widths <- c(0.6, 4, 1, 4, 1, 4, 4, 1)
    heights <- c(0.6, 4, 4, 4)
    top_row <- plot_spacer() + 
      col1 + plot_spacer() + col2 + plot_spacer() + col3 + col4 + plot_spacer() +
      plot_layout(widths = widths)
    row1 <- (l_rowplots[[1]] + r1[[1]] + r1[[2]] + r1[[3]] + r1[[4]] + r1[[5]] + r1[[6]] + r1[[7]]) +
      plot_layout(widths = widths)
    row2 <- (l_rowplots[[2]] + r2[[1]] + r2[[2]] + r2[[3]] + r2[[4]] + r2[[5]] + r2[[6]] + r2[[7]]) +
      plot_layout(widths = widths)
    row3 <- (l_rowplots[[3]] + r3[[1]] + r3[[2]] + r3[[3]] + r3[[4]] + r3[[5]] + r3[[6]] + r3[[7]]) +
      plot_layout(widths = widths)
    
    print(top_row / row1 / row2 / row3 + plot_layout(heights = heights))
    
  } # end: if
  
} # End of ggplot function


# ------------------------------------------
# -------- Plot Single Time Series Panel ---
# ------------------------------------------
# This is used to plot the individual pieces of figure 1

Plot1Panel <- function(df) {
  
  ylim = c(-5, 5)
  
  # Plot 1: Line plot data + predictions
  plot_line <- ggplot(df, aes(time, x)) +
    geom_line(aes(y = x, color = "Empirical"), linewidth = 0.5) +
    theme_minimal() + 
    coord_cartesian(ylim = ylim) + 
    scale_color_manual(
      values = c(
        "Empirical" = "black",
        "Predictions" = "darkorange2"
      )
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = if (legend) legend.position else "none",
      legend.spacing.y = unit(0.04, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.title = element_blank()
    )
  
  # histogram of y
  mu <- mean(df$x, na.rm = TRUE)
  sd <- sd(df$x, na.rm = TRUE)
  p_hist <- ggplot(df, aes(x)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "grey50") +
    coord_flip(xlim = ylim) +
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
  
  plot_line + p_hist +  plot_layout(widths = c(4,1))
  
} # eoF


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
# This is purely for Mplus/DSEM
# The new mlVAR package version coming with the paper 
# has the build-in functions predict() and residuals()

ResAnalysis <- function(model,
                        data = data,
                        subject) {
  
  
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
  N <- nrow(data_j)
  
  # Find out which data points are predictable by VAR(1)
  pdb_j <- f_pdb(data_j$day, data_j$beep)
  
  # ----- Loop Through Variables & Compute Residuals ----
  a_res <- array(NA, dim=c(N, p, 3))
  
  for(i in 1:p) {
    ## Compute Residuals
    emp <- data_j[, vars[i]] # Empirical Data
    pred <- intc[i] + rowSums(as.matrix(data_j[, vars]) %*% matrix(phi_1[i, ], nrow=p) )
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
# This is purely used for Mplus/DSEM
# The new mlVAR package version coming with the paper 
# has the build-in function resimulate()

SimPPC <- function(data,
                   model,
                   ResObj,
                   subject) {
  
  # Get Sample size for person
  u_ptp <- unique(data$id)
  data_j <- data[data$id == u_ptp[subject], ]
  Nt <- nrow(data_j)
  
  # ---- Get Parameters for Given Subject -----
    phi_1 <- model$Ind_phi[ , , subject] 
    intc <- model$Ind_mu[subject, ] 

  # ---- Get Residual Variance ----
  m_res <- ResObj[[subject]]$ResVar
  
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

# ----------------------------------------------------------------
# -------- Computing Statistics om Time Series -------------------
# ----------------------------------------------------------------

RMSSD <- function(x) {
  n <- length(x)
  x_diff <- sqrt(mean((x[-1] - x[-n])^2, na.rm=T))
}

count_modes <- function(x, bw = "nrd0") {
  x <- na.omit(x)
  d <- density(x, bw = bw)
  sum(diff(sign(diff(d$y))) == -2)
}
is_bimodal <- function(x, alpha = 0.05) {
  if (!requireNamespace("diptest", quietly = TRUE)) {
    stop("Please install the 'diptest' package: install.packages('diptest')")
  }
  test <- diptest::dip.test(x)
  # Null = unimodal. Rejecting -> multimodal.
  return(test$p.value < alpha)
}

Trend <- function(x) {
  time <- 1:length(x)
  lm_obj <- lm(x ~ time)
  sum_obj <- summary(lm_obj)
  return(sum_obj$coefficients[2, 4] < 0.05)
}


