# jonashaslbeck@protonmail.com; Nov 20th, 2025

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
library(ggplot2)
library(qgraph)
library(ggplot2)

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
# saveRDS(mlVAR_out, "Files/Mod_Paper_mlVAR_Grommisch_wRes.RDS")

# We load the estimated model
mlVAR_out <- readRDS("Files/Mod_Paper_mlVAR_Grommisch_wRes.RDS")


# ------------------------------------------
# -------- Compute Residuals ---------------
# ------------------------------------------

# We use the function ResAnalysis() from 0_Helpers.R
l_ResObj <- list()
for(i in 1:n_ptp) l_ResObj[[i]] <- ResAnalysis(model = mlVAR_out,
                                               data = data,
                                               subject = i, 
                                               newmlVAR = FALSE) # For now, until the mlVAR residual output includes the NA-structure; but for people reusing this: definitely use this option, because it makes the residual function much more general!!


# ------------------------------------------
# -------- Simulating Data -----------------
# ------------------------------------------

# We use the function SimPPC() from 0_Helpers.R
l_PPCs <- list()
for(i in 1:n_ptp) l_PPCs[[i]] <- SimPPC(data = data,
                                        model = mlVAR_out,
                                        subject = i,
                                        Nt = 200)


# ------------------------------------------
# -------- Diagnostic Plots ----------------
# ------------------------------------------

# -------------------
# ----- Selected ----
# -------------------

# Here we make separate plots for the three selected persons we show in the paper
sel <- c(6, 33, 133)

out_sel <- l_ResObj[[133]]
round(out_sel$R2, 2)
round(out_sel$RMSE, 2)

for(i in sel) {
  pdf(paste0("Figures/Fig_EmpAnalysis_R_Diagnostics_", i, ".pdf"), width=11, height=9.5)

  # Set up plotting area
  lmat <- matrix(6:40, nrow=5, byrow = TRUE)
  lmat <- cbind(1:5, lmat)
  lo <- layout(mat = lmat,
               heights = c(0.1, 1, 1, 1, 1),
               widths = c(0.1, 1,0.15,1,0.15, 1,1,0.15))
  # layout.show(lo)

  # ----- Plot Labels -----
  cex_lab <- 1.8
  ## Y-axis
  PlotLabel("", cex=cex_lab, srt=90)
  for(j in 1:4) PlotLabel(labels[j], cex=cex_lab, srt=90)
  ## X-Axis
  PlotLabel("Data & Predictions", cex=cex_lab)
  PlotLabel("", cex=cex_lab)
  PlotLabel("   Residuals vs. Time", cex=cex_lab)
  PlotLabel("", cex=cex_lab)
  PlotLabel("Residuals vs. Predictions", cex=cex_lab)
  PlotLabel("Simulated Data", cex=cex_lab)
  PlotLabel("", cex=cex_lab)


  # ----- Plot Data -----
  for(j in 1:4) {

    Plot1Row(x = l_ResObj[[i]]$Emp[, j],
             x_hat= l_ResObj[[i]]$Pred[, j],
             x_res = l_ResObj[[i]]$Res[, j],
             x_ppc = l_PPCs[[i]]$data_sim[, j],
             R2 = round(l_ResObj[[i]]$R2[j], 2),
             RMSE = round(l_ResObj[[i]]$RMSE[j], 2),
             legend = c(TRUE, FALSE, FALSE, FALSE)[j],
             ylim=c(-5,5),
             alpha = 0.4,
             showresAR = TRUE,
             colpred = "orange",
             cex_info=1)

  } # end for: j

  dev.off()

}


# ----------------
# ----- ALL ------
# ----------------
# This plots the diagnostic plots for *all* persons in the data set
# We mention these plots in the paper, but of course cannot show them all there

pdf("Figures/Fig_EmpAnalysis_R_Diagnositics_All.pdf", width=11, height=9.5)

for(i in 1:n_ptp) {
  # for(i in 1:3) {
  
  print(i)
  
  # Set up plotting area
  lmat <- matrix(6:40, nrow=5, byrow = TRUE)
  lmat <- cbind(1:5, lmat)
  lo <- layout(mat = lmat,
               heights = c(0.1, 1, 1, 1, 1),
               widths = c(0.1, 1,0.15,1,0.15, 1,1,0.15))
  # layout.show(lo)
  
  # ----- Plot Labels -----
  cex_lab <- 1.8
  ## Y-axis
  PlotLabel("", cex=cex_lab, srt=90)
  for(j in 1:4) PlotLabel(labels[j], cex=cex_lab, srt=90)
  ## X-Axis
  PlotLabel("Data & Predictions", cex=cex_lab)
  PlotLabel("", cex=cex_lab)
  PlotLabel("   Residuals vs. Time", cex=cex_lab)
  PlotLabel("", cex=cex_lab)
  PlotLabel("Residuals vs. Predictions", cex=cex_lab)
  PlotLabel("PPCs", cex=cex_lab)
  PlotLabel("", cex=cex_lab)
  
  
  # ----- Plot Data -----
  for(j in 1:4) {
    
    Plot1Row(x = l_ResObj[[i]]$Emp[, j],
             x_hat= l_ResObj[[i]]$Pred[, j],
             x_res = l_ResObj[[i]]$Res[, j],
             x_ppc = l_PPCs[[i]]$data_sim[, j],
             R2 = l_ResObj[[i]]$R2[j],
             RMSE = l_ResObj[[i]]$RMSE[j],
             legend = c(TRUE, FALSE, FALSE, FALSE)[j],
             ylim=c(-5,5),
             alpha = 0.4,
             showresAR = TRUE,
             colpred = "orange")
    
  } # end for: j
  
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
round(apply(m_RMSE, 2, median, na.rm=TRUE), 2)
R2_median <- round(apply(m_R2, 2, median, na.rm=TRUE), 2)
R2_median
R2_mean <- apply(m_R2, 2, mean, na.rm=TRUE)


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
# -------- Look at Random Effects Distr ----
# ------------------------------------------

# ----- Lagged Effects ----
# Get this into an array
a_phi <- array(NA, dim=c(4, 4, n_ptp))
for(i in 1:n_ptp) a_phi[, , i] <- mlVAR_out$results$Beta$subject[[i]][, , 1]

sc <- 0.95
pdf("Figures/Fig_mlVAR_RE_Distr_phi.pdf", width=12*sc, height=11*sc)

# Layout
lmat <- matrix(8+(1:4^2), nrow=4)
lmat <- cbind(5:8, lmat)
lmat <- rbind(0:4, lmat)
lo <- layout(mat = lmat,
             heights = c(0.1, 1, 1, 1, 1),
             widths = c(0.1, 1, 1, 1, 1))
# layout.show(lo)


# Labels
for(i in 1:4) PlotLabel(paste0(var_labels[i], " (t-1)"))
for(i in 1:4) PlotLabel(paste0(var_labels[i], " (t)"), srt=90)
# Data

par(mar=c(4,3,1,1))
for(i in 1:4) for(j in 1:4) {
  
  # Histogram
  xlim <- c(-0.65, .65)
  ylim=c(0,100)
  hist_data <- hist(a_phi[i, j, ], 
       breaks=seq(xlim[1], xlim[2], length=40), 
       xlim=c(xlim[1], xlim[2]), 
       main="", xlab="", axes=F, ylim=c(0,100))
  grid()
  hist(a_phi[i, j, ], 
       breaks=seq(xlim[1], xlim[2], length=40), 
       xlim=c(xlim[1], xlim[2]), 
       main="", xlab="", axes=F, add=TRUE, ylim=c(0,100))
  axis(1)
  axis(2, las=2)
  
  # Density plot
  x_seq <- seq(xlim[1], xlim[2], length=1000)
  gauss_den <- dnorm(x_seq,
                     mean = mean(a_phi[i, j, ], na.rm = TRUE),
                     sd = sd(a_phi[i, j, ], na.rm = TRUE))
  
  lines(x_seq, gauss_den * length(a_phi[i, j, ]) * diff(hist_data$breaks)[1], 
        col = "black", lwd = 2)
  
} # end for: loop over phi

dev.off()


# ----- Intercepts ----

# Get this into an array
a_int <- array(NA, dim=c(n_ptp, 4))
for(i in 1:n_ptp) a_int[i, ] <- mlVAR_out$results$mu$subject[[i]]


sc <- 0.95
pdf("Figures/Fig_mlVAR_RE_Distr_intcps.pdf", width=12*sc, height=3.5*sc)

# Layout
lmat <- matrix(5+(1:4), nrow=1)
lmat <- cbind(5, lmat)
lmat <- rbind(0:4, lmat)
lo <- layout(mat = lmat,
             heights = c(0.1, 1),
             widths = c(0.1, 1, 1, 1, 1))
# layout.show(lo)

# Labels
for(i in 1:4) PlotLabel(paste0(var_labels[i]))
PlotLabel("Intercept", srt=90)
# Data

par(mar=c(4,3,1,1))
for(i in 1:4) {
  
  # Histogram
  xlim <- c(-1.5, 1.5)
  ylim=c(0,100)
  hist_data <- hist(a_int[, i], 
                    breaks=seq(xlim[1], xlim[2], length=40), 
                    xlim=c(xlim[1], xlim[2]), 
                    main="", xlab="", axes=F, ylim=c(0,100))
  grid()
  hist(a_int[, i], 
       breaks=seq(xlim[1], xlim[2], length=40), 
       xlim=c(xlim[1], xlim[2]), 
       main="", xlab="", axes=F, add=TRUE, ylim=c(0,100))
  axis(1)
  axis(2, las=2)
  
  # Density plot
  x_seq <- seq(xlim[1], xlim[2], length=1000)
  gauss_den <- dnorm(x_seq,
                     mean = mean(a_int[, i], na.rm = TRUE),
                     sd = sd(a_int[, i], na.rm = TRUE))
  
  lines(x_seq, gauss_den * length(a_int[, i]) * diff(hist_data$breaks)[1], 
        col = "black", lwd = 2)
  
} # end for: loop over phi

dev.off()




