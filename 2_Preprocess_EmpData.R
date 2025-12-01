# jonashaslbeck@protonmail.com; Oct 14th, 2024

# ------------------------------------------
# -------- What is happening here? ---------
# ------------------------------------------

# Here we restructure the data from Grommisch et al. (2020) a bit so they
# are easier to handle later

## Full reference:
# Grommisch, G., Koval, P., Hinton, J. D., Gleeson, J., Hollenstein, T., Kuppens, P., & Lischetzke,
# T. (2020). Modeling individual differences in emotion regulation repertoire in daily life with
# multilevel latent profile analysis. Emotion, 20 (8), 1462.

# ------------------------------------------
# -------- Load Packages -------------------
# ------------------------------------------

library(plyr)
library(tidyr)
library(dplyr)

# ------------------------------------------
# -------- Load Data -----------------------
# ------------------------------------------

# We obtained the data from the OSF page on which the original authors shared their data:
# https://osf.io/r7jw6/
# The folder "Grommisch2020" in our Github repo is downloaded from there

load("Data/Grommisch2020/Feel1_Grommisch_et.al.(2019).rda")
saveRDS(data, "Data/Grommisch2020.RDS") # Save again, we prefer this format

# In what follows, we:
# 1) Subset variables
# 2) Recode the "beep" variable indicating the notification on a given day, so that it restarts at 1 on each day
#


# ------------------------------------------
# -------- Subset Data ---------------------
# ------------------------------------------

data_ss <- data[, c("SEMA_ID", "RowNr", "DayNr", "HAP", "RLX", "SAD", "ANG")]
colnames(data_ss) <- c("id", "beep", "day", "happy", "relaxed", "sad", "angry")
head(data_ss)


# ------------------------------------------
# -------- Recode the Beep Variable --------
# ------------------------------------------

data_ss2 <- ddply(data_ss, .(id, day), function(x) {
  x$beep <- x$beep - min(x$beep) + 1
  return(x)
})

# ------------------------------------------
# -------- Add NAs for Missing Values ------
# ------------------------------------------
# This is especially useful for plotting the data, because then one sees the missing data

data_ss3 <- ddply(data_ss2, .(id, day), function(x) {

  df_full <- data.frame(matrix(NA, 10, 7))
  # Loop through measurements, fill in NA, where missing
  for(i in 1:10) {
    if(i %in% x$beep) {
      df_full[i, ] <- x[x$beep==i, ]
    } else {
      df_full[i, ] <- c(NA, i, rep(NA, 5))
    }
  }
  # Fill up id variables
  df_full$X1 <- unique(df_full$X1[!is.na(df_full$X1)])
  df_full$X2 <- unique(df_full$X2[!is.na(df_full$X2)])
  return(df_full)
})
data_ss4 <- data_ss3[, -(1:2)]
colnames(data_ss4) <- colnames(data_ss)
head(data_ss4)

# ------------------------------------------
# --- Transform data for Mplus analysis ----
# ------------------------------------------

# Need to have continuous beep numbering and empty lines after each day
total_obs_pp <- data_ss4 %>%
  group_by(id) %>%
  count(id)

reps_pp <- total_obs_pp$n / 10

day_mut <- NA

for (i in 1:length(reps_pp)){
  day_mut <- c(day_mut, rep(c(1:reps_pp[i]), each = 10))
}

day_mut <- day_mut[-1]
data_ss4$day_mut <- day_mut

# Adding 11 rows to correct for "missing" beeps
data_mp <- data_ss4 %>%
  group_by(id, day_mut) %>%
  group_modify(~ add_row(.x, .after = 0)) %>%
  group_modify(~ add_row(.x, .after = 0)) %>%
  group_modify(~ add_row(.x, .after = 0)) %>%
  group_modify(~ add_row(.x, .after = 0)) %>%
  group_modify(~ add_row(.x, .after = 0)) %>%
  group_modify(~ add_row(.x, .after = 0)) %>%
  group_modify(~ add_row(.x, .after = 0)) %>%
  group_modify(~ add_row(.x, .after = 0)) %>%
  group_modify(~ add_row(.x, .after = 0)) %>%
  group_modify(~ add_row(.x, .after = 0)) %>%
  group_modify(~ add_row(.x, .after = 0))

data_mp <- data_mp[-c(1:11), ]

day_def <- NA

for (i in 1:length(reps_pp)){
  day_def <- c(day_def, rep(c(1:reps_pp[i]), each = 21))
}

day_def <- day_def[-1]
day_def <- day_def[-c(80735:length(day_def))]

data_def <- as_tibble(cbind(data_mp[,1], day_def, data_mp[5:8]))

head(data_def)
colnames(data_def) <- c("id", "day", "happy", "relaxed", "sad", "angry")

data_def <- data_def %>%
  group_by(id) %>%
  mutate(beep = 1:length(id)) %>%
  select(id, beep, day, happy, relaxed, sad, angry)

data_def[is.na(data_def)] <- -999


# ------------------------------------------
# -------- Save Data -----------------------
# ------------------------------------------

# These are the empirical data we work with in the empirical analysis
saveRDS(data_ss4, "Data/Grommisch2020_subset.RDS")

# Separate format for Mplus analysis
write.table(data_def, file = "data_new.csv",
            sep = "\t", row.names = FALSE, col.names = FALSE)








