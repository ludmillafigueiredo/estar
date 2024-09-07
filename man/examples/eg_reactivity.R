library(MARSS)

# smaller dataset for example:
# 3 functional groups and two insecticide concentrations besides control
data_df <- subset(
  aquacomm_fgps,
  treat %in% c(0.0, 0.9, 6) &
    time >= 1 & time <= 28,
  select = c(time, treat, repli, herb, carn, detr)
)

# estimate z-score transformation and replace zeros with NA
data_df[, c("herb", "carn", "detr")] <- lapply(data_df[, c("herb", "carn", "detr")], MARSS::zscore)
data_df[, c("herb", "carn", "detr")] <- lapply(data_df[, c("herb", "carn", "detr")], function(x)
  replace(x, x == 0, NA))

# reshape data from wide to long format
data_z_ldf <- reshape(
  data_df,
  varying = list(c("herb", "carn", "detr")),
  v.names = "abund_z",
  timevar = "fgp",
  times = c("herb", "carn", "detr"),
  direction = "long",
  idvar = c("time", "treat", "repli")
)

data_z_ldf <- data_z_ldf[order(data_z_ldf$time, data_z_ldf$treat, data_z_ldf$fgp), ]

# summarize mean and sd
data_z_summldf <- aggregate(abund_z ~ time + treat + fgp, data_z_ldf, function(x)
  c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
data_z_summldf <- do.call(data.frame, data_z_summldf)
names(data_z_summldf)[4:5] <- c("abundz_mu", "abundz_sd")

# split dataframe per functional groups
# into list to apply the MARSS model more easily
split_data_z <- split(data_z_summldf[, c("time", "fgp", "abundz_mu")], data_z_summldf$treat)

reshape_to_wide <- function(df) {
  df_wide <- reshape(df,
                     idvar = "fgp",
                     timevar = "time",
                     direction = "wide")
  rownames(df_wide) <- df_wide$fgp
  df_wide <- df_wide[, -1]  # Remove the 'fgp' column
  as.matrix(df_wide)
}

data_z_summls <- lapply(split_data_z, reshape_to_wide)

# fit MARSS models
data.marssls <- list(
  MARSS(
    data_z_summls[[1]],
    model = list(
      B = "unconstrained",
      U = "zero",
      A = "zero",
      Z = "identity",
      Q = "diagonal and equal",
      R = matrix(0, 3, 3),
      tinitx = 1
    ),
    method = "BFGS"
  ),
  MARSS(
    data_z_summls[[2]],
    model = list(
      B = "unconstrained",
      U = "zero",
      A = "zero",
      Z = "identity",
      Q = "diagonal and equal",
      R = matrix(0, 3, 3),
      tinitx = 1
    ),
    method = "BFGS"
  ),
  MARSS(
    data_z_summls[[3]],
    model = list(
      B = "unconstrained",
      U = "zero",
      A = "zero",
      Z = "identity",
      Q = "diagonal and equal",
      R = matrix(0, 3, 3),
      tinitx = 1
    ),
    method = "BFGS"
  )
)

# identify experiments
names(data.marssls) <- paste0("Conc. = ", c("0", "0.9", "44"), " micro g/L")

# extract community matrices (B)
data.Bls <- list(
  extractB(
    data.marssls[[1]],
    states_names = c("Herbivores", "Carnivores", "Detrivores")
  ),
  extractB(
    data.marssls[[2]],
    states_names = c("Herbivores", "Carnivores", "Detrivores")
  ),
  extractB(
    data.marssls[[3]],
    states_names = c("Herbivores", "Carnivores", "Detrivores")
  )
)

# calculate reactivity for each of the B matrices
purrr::map(data.Bls, reactivity)
