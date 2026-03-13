# jonashaslbeck@protonmail.com; March 2026

# ------------------------------------------
# -------- What is happening here? ---------
# ------------------------------------------

# Here we:
# 1) Fit an mlVAR model to the subset data of Grommisch et al. (2020)
# 2) And we perform the residual analysis we report in the paper


# ------------------------------------------
# -------- Load Packages & Source ----------
# ------------------------------------------

# Fit mlVAR Model
# Estimation
library(devtools)
# install_github("SachaEpskamp/mlVAR") # Needed for residuals in output
library(mlVAR)


# Wrangling
library(tidyr)
library(dplyr)
library(reshape2) 

# Plotting
library(RColorBrewer)
library(qgraph)
library(ggplot2)
library(patchwork)

# For testing trends
library(lmtest)
library(sandwich)

source("0_Helpers.R")

# ------------------------------------------
# -------- Load Data -----------------------
# ------------------------------------------

data <- readRDS("Data/Grommisch2020_subset.RDS")


# ------------------------------------------
# -------- Overview ------------------------
# ------------------------------------------

head(data)

u_ptp <- unique(data$id)
n_ptp <- length(u_ptp)
capitalize_first <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
labels <- capitalize_first(colnames(data)[4:7])


# ------------------------------------------
# -------- Fit mlVAR -----------------------
# ------------------------------------------

# Here we comment the actual fitting out and instead load the fitted model, 
# so this R file runs through more quickly

# mlVAR_out <- mlVAR(data = data,
#                    vars = colnames(data)[4:7],
#                    idvar = colnames(data)[1],
#                    lags = 1,
#                    dayvar = colnames(data)[3],
#                    beepvar = colnames(data)[2],
#                    contemporaneous = "correlated",
#                    temporal = "correlated",
#                    scale = TRUE,
#                    verbose = FALSE)
# saveRDS(mlVAR_out, "Files/Mod_Paper_mlVAR_Grommisch.RDS")

# We load the estimated model
mlVAR_out <- readRDS("Files/Mod_Paper_mlVAR_Grommisch.RDS")

# residuals(mlVAR_out)
# predict(mlVAR_out)

# ------------------------------------------
# -------- Compute Residuals ---------------
# ------------------------------------------

# Get predictions and residuals using new mlVAR functions
res <- residuals(mlVAR_out)
pred <- predict(mlVAR_out)

# Loop over subjects
l_ResObj <- list()
for(i in 1:n_ptp) {
  # Subset 
  emp_i <- data[data$id == u_ptp[i], 4:7]
  pred_i <- pred[pred$id == u_ptp[i], 4:7]
  res_i <- res[res$id == u_ptp[i], 4:7]
  
  # Compute Prediction Errors
  v_RMSE <- apply(res_i, 2, function(x) sqrt(mean(na.omit(x)^2)))
  v_R2 <- rep(NA, 4)
  for(j in 1:4) v_R2[j] <- 1 - var(res_i[, j], na.rm = TRUE) / var(emp_i[, j], na.rm = TRUE)  
  
  # Compute Residual variance (for PPCs below)
  ResVAR <- apply(res_i, 2, function(x) var(x, na.rm=TRUE))
  
  # Save
  l_ResObj[[i]] <- list(Emp = emp_i,
                        Pred = pred_i,
                        Res = res_i,
                        R2 = v_R2,
                        RMSE = v_RMSE, 
                        ResVar = ResVAR)
} # end: for


# ------------------------------------------
# -------- Simulating Data -----------------
# ------------------------------------------

# We use the function SimPPC() from 0_Helpers.R
l_PPCs <- list()
for(i in 1:n_ptp) l_PPCs[[i]] <- SimPPC(data = data,
                                        model = mlVAR_out,
                                        ResObj = l_ResObj,
                                        subject = i,
                                        Nt = nrow(l_ResObj[[i]]$Emp)) # simulate length of original time series



# Compare with Sacha's implementation
# Data
data_i <- data[data$id == u_ptp[1], 4:7]
apply(data_i, 2, mean, na.rm=TRUE)
apply(data_i, 2, sd, na.rm=TRUE)
# My Sim
apply(l_PPCs[[i]]$data_sim, 2, mean) # Fits only negative emotions
apply(l_PPCs[[i]]$data_sim, 2, sd) # Fits the empirical data
# Sacha
# Testing new function
sim_out <- resimulate(mlVAR_out)
sim_out_i <- sim_out[sim_out$id == u_ptp[1], ]
apply(sim_out_i[, 4:7], 2, mean, na.rm=T) # Fits only the positive emotions
apply(sim_out_i[, 4:7], 2, sd, na.rm=T) # Does not fit the empirical data for any variable

# Additional notes:
# 1) I like the default of simulating the same length, but maybe you can provide an argument for longer time series
# 2) I also like the default of putting the missingness back in, but also here I think it would be nice to have an argument that allows one to get the full time series



# ------------------------------------------
# -------- Diagnostic Plots ----------------
# ------------------------------------------

# ----- Selected Three for Paper ----

# Here we make separate plots for the three selected persons we show in the paper
sel <- c(6, 33, 133)

for(i in 1:3) {
  pdf(paste0("Figures/Fig_EmpAnalysis_R_Diagnostics_", sel[i], ".pdf"), width=11, height=9.5)
  
  print(PlotDiagnosticsEmp(l_ResObj = l_ResObj, 
                           l_PPC = l_PPCs,  
                           subject = sel[i],
                           legpos = c("topleft", "topleft", "bottomright")[i])) 
  
  dev.off()
}


# ----- ALL (for repository) ------
# This plots the diagnostic plots for *all* persons in the data set
# We mention these plots in the paper, but of course cannot show them all there

pdf("Figures/Fig_EmpAnalysis_R_Diagnositics_All.pdf", width=11, height=9.5)

for(i in 1:n_ptp) {
  print(PlotDiagnostics(l_ResObj = l_ResObj, 
                        l_PPC = l_PPCs,  
                        subject = sel[i]))
  print(i) # progress
} # end for: ptp

dev.off()




# ------------------------------------------
# -------- Aggregate: Statistics -----------
# ------------------------------------------
# Here we compute the aggregate statistics across the whole sample we discuss in Section 4.4 in the paper

# ----- Functions -----
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


# ----- Compute -----
# Using this object, because it already contains empirical and simulated data
l_stats <- l_PPCs
N <- length(l_stats)

l_trends_emp <- list()
l_trends_sim <- list()

m_stats <- as.data.frame(matrix(NA, N, 4))
colnames(m_stats) <- c("RMSSD_emp", "RMSSD_sim", "mode_emp", "mode_sim")
for(i in 1:N) {
  # RMSSD (not shown in the paper)
  m_stats$RMSSD_emp[i] <- mean(apply(l_stats[[i]]$data_emp, 2, RMSSD))
  m_stats$RMSSD_sim[i] <- mean(apply(l_stats[[i]]$data_sim, 2, RMSSD))
  # Modes
  m_stats$mode_emp[i] <- mean(apply(l_stats[[i]]$data_emp, 2, is_bimodal))
  m_stats$mode_sim[i] <- mean(apply(l_stats[[i]]$data_sim, 2, is_bimodal))
  # Trend
  alpha <- 0.05
  l_trends_emp[[i]] <- apply(l_stats[[i]]$data_emp, 2, trend_test_hac) < alpha
  l_trends_sim[[i]] <- apply(l_stats[[i]]$data_sim, 2, trend_test_hac) < alpha
}

# Modes
round(mean(m_stats$mode_emp), 3)
round(mean(m_stats$mode_sim), 3)

# Trends
round(mean(unlist(l_trends_emp)), 2)
round(mean(unlist(l_trends_sim)), 2)


# ------------------------------------------
# -------- Aggregate: Prediction Errors ----
# ------------------------------------------

# ----- Extract -----
m_R2 <- m_RMSE <- matrix(NA, n_ptp, 4)

for(i in 1:n_ptp) {
  m_R2[i, ] <- l_ResObj[[i]]$R2
  m_RMSE[i, ] <- l_ResObj[[i]]$RMSE
}

# Exclude extreme values
m_R2[m_R2 < -0.5] <- NA

# ----- Get Medians -----
# RMSE
round(apply(m_RMSE, 2, median, na.rm=TRUE), 0)
# R2
round(apply(m_R2, 2, median, na.rm=TRUE), 2)


# ----- Violin Plots (appendix) -----
# Suppose these are your nice labels for the 4 variables
var_labels <- c("Happy", "Relaxed", "Sad", "Angry")

df <- bind_rows(
  as.data.frame(m_R2)  %>% mutate(metric = "R²"),
  as.data.frame(m_RMSE) %>% mutate(metric = "RMSE")
) %>%
  pivot_longer(-metric, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable,
                           levels = unique(variable),
                           labels = var_labels))

pdf("Figures/Fig_EmpAnalysis_R_R2_RMSE_agg_ggplot.pdf", width = 8, height = 4)
ggplot(df, aes(x = variable, y = value, fill = variable)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.6) +
  facet_wrap(~metric, scales = "free_y") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "")
dev.off()

# ----- Plotting: R2 vs. RMSE -----
df_scatter <- data.frame(
  R2   = as.vector(as.matrix(m_R2)),
  RMSE = as.vector(as.matrix(m_RMSE)),
  variable = rep(var_labels, each = nrow(m_R2))
)

# if you want nice labels:
df_scatter <- df_scatter %>%
  mutate(variable = factor(variable, levels = var_labels,
                           labels = var_labels))

# get global axis limits (same for all panels)
xlims <- range(df_scatter$R2, na.rm = TRUE)
ylims <- range(df_scatter$RMSE, na.rm = TRUE)

pdf("Figures/Fig_EmpAnalysis_R_R2_RMSE_comp_ggplot.pdf", width=8, height=8)
# scatter plots, faceted 2x2
ggplot(df_scatter, aes(x = R2, y = RMSE)) +
  geom_point(alpha = 0.6, size = 2) +
  facet_wrap(~variable, ncol = 2) +
  coord_cartesian(xlim = xlims, ylim = ylims) +
  theme_minimal(base_size = 13) +
  labs(x = "R²", y = "RMSE")
dev.off()


# ------------------------------------------
# -------- Show Group Level Estimates ------
# ------------------------------------------

# ------ Network ------

sc <- 2
pdf("Figures/Fig_EmpAnalysis_R_VAR_Fixed_Effects_Network_PlusRESD.pdf", width = 2*6*sc, height=6*sc)

sc_2 <- 0.6
title_cex <- 2.5

par(mfrow=c(1,2))
## Double checking that we plot edges in the right direction with qgraph
# phi_1 <- getNet(mlVAR_out, type="temporal",verbose=FALSE)
# plot(mlVAR_out, type="temporal", edge.labels=TRUE)

# Network
qgraph(t(mlVAR_out$results$Beta$mean[, , 1]), # Note: In the input matrix columns predict rows; but qgraph() plots it the other way around, so we have to tanspose
       layout="circle",
       labels=labels,
       vsize=18*sc_2,
       esize=12*sc_2,
       asize=10*sc_2,
       edge.labels=TRUE,
       edge.label.cex=1.5,
       fade=F,
       mar=rep(5, 4),
       palette = "colorblind",
       theme= "colorblind",
       title="Fixed Effects Estimates",
       title.cex=title_cex,
       pie=R2_median)

# Network
qgraph(t(mlVAR_out$results$Beta$SD[, , 1]),
       layout="circle",
       labels=labels,
       vsize=18*sc_2,
       esize=12*sc_2,
       asize=10*sc_2,
       edge.labels=TRUE,
       edge.label.cex=1.5,
       fade=F,
       mar=rep(5, 4),
       edge.color="grey",
       palette = "colorblind",
       theme= "colorblind",
       title="Random Effects SDs",
       title.cex=title_cex)

dev.off()

## Some Stats
# Diagonal
round(range(diag(mlVAR_out$results$Beta$mean[, , 1])), 2)
# Off-Diagonal
offdiag <- mlVAR_out$results$Beta$mean[, , 1][upper.tri(mlVAR_out$results$Beta$mean[, , 1])]
round(range(offdiag), 2)

# Random effects of intercepts
mlVAR_out$results$mu$mean
round(mlVAR_out$results$mu$SD, 3)


# ------------------------------------------
# -------- RE Distributions - ggplot ----
# ------------------------------------------


# ----- Lagged Effects ----
# Prepare data frame
a_phi <- array(NA, dim=c(4, 4, n_ptp))
for(i in 1:n_ptp) a_phi[, , i] <- mlVAR_out$results$Beta$subject[[i]][, , 1]
df <- data.frame(matrix(NA, nrow=n_ptp, ncol=4^2))
cnt <- 1
for(i in 1:4) for(j in 1:4) {
  df[, cnt] <- a_phi[i, j, ]
  colnames(df)[cnt] <- paste0("phi_", i, "_", j)
  cnt <- cnt + 1
}

# Plotting individual histograms
l_REplots <- list()
cnt <- 1
for(i in 1:4) for(j in 1:4) local({
  cnt_local <- cnt
  i_local   <- i
  j_local   <- j
  xvar      <- colnames(df)[cnt_local]
  xlim      <- c(-0.2, 0.6)
  bins      <- 60
  
  x <- as.numeric(df[[xvar]])
  x <- x[is.finite(x)]
  
  mu <- mean(x)
  s  <- sd(x)
  bw <- diff(xlim) / bins
  n  <- length(x)
  
  l_REplots[[cnt_local]] <<- ggplot(df, aes(x = .data[[xvar]])) +
    geom_histogram(
      bins = bins,
      fill = "steelblue",
      color = "white",
      linewidth = 0.3
    ) +
    stat_function(
      fun = function(z) dnorm(z, mean = mu, sd = s) * n * bw,
      color = "black",
      linewidth = 1
    ) +
    labs(
      x = "",
      y = "",
      title = bquote(phi[.(i_local)*","*.(j_local)])
    ) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = c(0, 80)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  cnt <<- cnt + 1
})

# Arrange in Layout
sc <- 0.95
pdf("Figures/Fig_mlVAR_RE_Distr_phi_ggplot.pdf", width=12*sc, height=11*sc)
wrap_plots(l_REplots, ncol = 4, nrow = 4, byrow = TRUE)
dev.off()


# ----- Intercepts ----

# Get this into an array
m_int <- array(NA, dim=c(n_ptp, 4))
for(i in 1:n_ptp) m_int[i, ] <- mlVAR_out$results$mu$subject[[i]]
df <- data.frame(m_int)

# Plotting individual histograms
l_REplots <- list()
cnt <- 1
for(i in 1:4) local({
  cnt_local <- cnt
  i_local   <- i
  xvar      <- colnames(df)[cnt_local]
  xlim      <- c(-0.7, 0.7)
  bins      <- 40
  
  x <- as.numeric(df[[xvar]])
  x <- x[is.finite(x)]
  
  mu <- mean(x)
  s  <- sd(x)
  bw <- diff(xlim) / bins
  n  <- length(x)
  
  l_REplots[[cnt_local]] <<- ggplot(df, aes(x = .data[[xvar]])) +
    geom_histogram(
      bins = bins,
      fill = "steelblue",
      color = "white",
      linewidth = 0.3
    ) +
    stat_function(
      fun = function(z) dnorm(z, mean = mu, sd = s) * n * bw,
      color = "black",
      linewidth = 1
    ) +
    labs(
      x = "",
      y = "",
      title = bquote(alpha[.(i_local)])
    ) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = c(0, 20)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  cnt <<- cnt + 1
})

sc <- 0.95
pdf("Figures/Fig_mlVAR_RE_Distr_intcps_ggplot.pdf", width=12*sc, height=3.5*sc)
wrap_plots(l_REplots, ncol = 4, nrow = 1, byrow = TRUE)
dev.off()


