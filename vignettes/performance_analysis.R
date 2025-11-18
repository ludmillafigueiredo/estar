library(estar)
library(here)
library(microbenchmark)
library(tidyverse)
library(viridis)
library(MARSS)
source("vignettes/custom_aesthetics.R")

set.seed(23)

# Functional metrics

## Invariability

inv_benchmark <- microbenchmark(
  inv_1 = {invariability(
    type = "functional",
    mode = "lm_res",
    response = "lrr",
    metric_tf = c(1, max(aquacomm_resps$time)),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  inv_2 = {invariability(
    type = "functional",
    mode = "lm_res",
    metric_tf = c(1, max(aquacomm_resps$time)),
    response = "v",
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  inv_3 = {invariability(
    type = "functional",
    response = "lrr",
    mode = "cv",
    metric_tf = c(1, max(aquacomm_resps$time)),
    vd_i = "statvar_db",
    td_i = "time",
    b_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    d_data = aquacomm_resps
  )},
  inv_4 = {invariability(
    type = "functional",
    response = "v",
    mode = "cv",
    metric_tf = c(1, max(aquacomm_resps$time)),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  inv_5 = {invariability(
    type = "compositional",
    metric_tf = c(0.14, 56),
    comm_d = comm_dist,
    comm_b = comm_base,
    comm_t = "time"
  )},
  unit = "s"
)

# plot shown in vignette:
# inv_plot <- autoplot(inv_benchmark, unit = "s") +
#   theme_estar()
# ggplot2::ggsave(filename = file.path(tempdir(), "inv_benchmark.png"),
#                 plot = inv_plot,
#                 device = "png",
#                 dpi = 600,
#                 width = 20,
#                 height = 10,
#                 units = "cm")

## Resistance

resis_benchmark <- microbenchmark(
  res_1 = {resistance(
    type = "functional",
    b = "input",
    res_mode = "lrr",
    res_time = "defined",
    res_t = 1,
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  res_2 = {resistance(
    type = "functional",
    b = "input",
    res_mode = "lrr",
    res_time = "max",
    res_tf = c(1, 20),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  res_3 = {resistance(
    type = "functional",
    b = "input",
    res_mode = "diff",
    res_time = "defined",
    res_t = 1,
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  res_4 = {resistance(
    type = "functional",
    b = "input",
    res_mode = "diff",
    res_time = "max",
    res_tf = c(1, 20),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  res_5 = {resistance(
    type = "functional",
    b = "d",
    b_tf = c(-4, 0.14),
    res_time = "max",
    res_mode = "diff",
    res_tf = c(1, 20),
    vd_i = "statvar_bl",
    td_i = "time",
    d_data = aquacomm_resps
  )},
  res_6 = {resistance(
    type = "functional",
    b = "d",
    b_tf = c(-4, 0.14),
    res_mode = "lrr",
    res_time = "defined",
    res_t = 1,
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps
  )},
  res_7 = {resistance(
    type = "compositional",
    res_time = "defined",
    res_t = 28,
    comm_d = comm_dist,
    comm_b = comm_base,
    comm_t = "time"
  )},
  unit = "s")

# resis_plot <- autoplot(resis_benchmark, unit = "s") +
#   theme_estar()
# ggplot2::ggsave(filename = file.path(tempdir(), "resis_benchmark.png"),
#                 plot = resis_plot,
#                 device = "png",
#                 dpi = 600,
#                 width = 20,
#                 height = 10,
#                 units = "cm")

# Extent of recovery

extent_benchmark <- microbenchmark(
  extent_1 = {recovery_extent(
    type = "functional",
    response = "lrr",
    b = "input",
    t_rec = 28,
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  extent_2 = {recovery_extent(
    type = "functional",
    response = "diff",
    b = "input",
    t_rec = 28,
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  extent_3 = {recovery_extent(
    type = "functional",
    response = "lrr",
    b = "d",
    summ_mode = "mean",
    b_tf = c(5, 10),
    t_rec = 28,
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps
  )},
  extent_4 = {recovery_extent(
    type = "compositional",
    t_rec = 28,
    comm_d = comm_dist,
    comm_b = comm_base,
    comm_t = "time"
  )},
  unit = "s"
)

# extent_plot <- autoplot(extent_benchmark, unit = "s") +
#   theme_estar()
# ggplot2::ggsave(filename = file.path(tempdir(), "extent_benchmark.png"),
#                 plot = extent_plot,
#                 device = "png",
#                 dpi = 600,
#                 width = 20,
#                 height = 10,
#                 units = "cm")

## Rate of recovery

rate_benchmark <- microbenchmark(
  rate_1 = {recovery_rate(
    type = "functional",
    response = "v",
    b = "input",
    metric_tf = c(1, 28),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  rate_2 = {recovery_rate(
    type = "functional",
    response = "lrr",
    b = "input",
    metric_tf = c(1, 28),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  rate_3 = {recovery_rate(
    type = "functional",
    response = "v",
    b = "d",
    metric_tf = c(1, 28),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  rate_4 = {recovery_rate(
    type = "functional",
    response = "lrr",
    b = "d",
    metric_tf = c(1, 28),
    b_tf = c(-4, 0.14),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  rate_5 = {recovery_rate(
    type = "compositional",
    metric_tf = c(0.14, 28),
    comm_d = comm_dist,
    comm_b = comm_base,
    comm_t = "time"
  )},
  unit = "s"
)

rate_plot <- autoplot(rate_benchmark, unit = "s") +
  theme_estar()
# ggplot2::ggsave(filename = here("vignettes/figures/rate_benchmark.png"),
#                 plot = rate_plot,
#                 device = "png",
#                 dpi = 600,
#                 width = 20,
#                 height = 10,
#                 units = "cm")

## Persistence

persist_benchmark <- microbenchmark(
  persist_1 ={persistence(
    type = "functional",
    b = "input",
    metric_tf = c(28, max(aquacomm_resps$time)),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  persist_2 = {persistence(
    type = "functional",
    b = "d",
    b_tf = c(-4, 0.14),
    metric_tf = c(28, max(aquacomm_resps$time)),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps
  )},
  persist_3 = {persistence(
    type = "compositional",
    b = "input",
    metric_tf = c(28, 56),
    comm_d = comm_dist,
    comm_b = comm_base,
    comm_t = "time",
    low_lim = 0.5,
    high_lim = 0.9
  )},
  unit = "s"
)

persist_plot <- autoplot(persist_benchmark, unit = "s") +
  theme_estar()
# ggplot2::ggsave(filename = here("vignettes/figures/persist_benchmark.png"),
#                 plot = persist_plot,
#                 device = "png",
#                 dpi = 600,
#                 width = 20,
#                 height = 10,
#                 units = "cm")

## Overall Ecological Vulnerability

oev_benchmark <- microbenchmark(
  oev_1 = {oev(
    type = "functional",
    response = "lrr",
    metric_tf = c(0.14, 56),
    vd_i = "statvar_db",
    td_i = "time",
    d_data = aquacomm_resps,
    vb_i = "statvar_bl",
    tb_i = "time",
    b_data = aquacomm_resps
  )},
  oev_2 = {oev(
    type = "compositional",
    metric_tf = c(0.14, 56),
    comm_d = comm_dist,
    comm_b = comm_base,
    comm_t = "time"
  )},
  unit = "s"
)

oev_plot <- autoplot(oev_benchmark, unit = "s") +
  theme_estar()
# ggplot2::ggsave(filename = here("vignettes/figures/oev_benchmark.png"),
#                 plot = oev_plot,
#                 device = "png",
#                 dpi = 600,
#                 width = 20,
#                 height = 10,
#                 units = "cm")

fmt_mark_output <- function(benchmark){
  require(tidyverse)
  df_fmted <- data.frame(summary(benchmark)) %>%
    dplyr::rename("Function call" = expr,
                  "Min." = min,
                  "Lower quart." = lq,
                  "Mean" = mean,
                  "Median" = median,
                  "Upper quart." = uq,
                  "Max." = max,
                  "Signif." = cld)
}

### Saving results to be displayed on the vignettes

df_list <- lapply(list(inv_benchmark = inv_benchmark,
                       resis_benchmark = resis_benchmark,
                       rate_benchmark = rate_benchmark,
                       extent_benchmark = extent_benchmark,
                       persist_benchmark = persist_benchmark,
                       oev_benchmark = oev_benchmark),
                  fmt_mark_output)

# save(df_list, file = file.path(tempdir(), "functional_performance.rda"))

# Jacobian metrics

Z_I5 <- matrix(list(0), 5, 5)
diag(Z_I5) <- 1

## calculate z-scores of abundances (same as vignette)
aquacommz_allscen.ldf <- aquacomm_fgps %>%
  dplyr::filter(time >= 1 , time <= 28) %>%
  ungroup() %>%
  dplyr::mutate_at(vars(herb, detr_herb, carn, omni, detr),
                   ~MARSS::zscore(.)) %>%
  dplyr::mutate(across(c(herb, detr_herb, carn, omni, detr),
                       ~dplyr::na_if(., 0))) %>%
  tidyr::pivot_longer(cols = c(herb, detr_herb, carn, omni, detr),
                      names_to = "fgp",
                      values_to = "abund_z")

## summarize abundances over replicates (same as vignette)
aquacommz_allscen.summldf <- aquacommz_allscen.ldf %>%
  dplyr::group_by(time, treat, fgp) %>%
  dplyr::summarize(abundz_mu = mean (abund_z),
                   abundz_sd = sd(abund_z)) %>%
  dplyr::ungroup()

## convert into time-series matrix (same as vignette)
aquacommz_allscen.summmxls <- aquacommz_allscen.summldf %>%
  dplyr::select(time, treat, fgp, abundz_mu) %>%
  split(.$treat) %>%
  purrr::map(~ dplyr::select(., time, fgp, abundz_mu) %>%
               unique() %>%
               tidyr::pivot_wider(id_cols = fgp,
                                  names_from = time, values_from = "abundz_mu",
                                  names_prefix = "time ") %>%
               tibble::column_to_rownames(var = "fgp") %>%
               as.matrix())

R_05 <- matrix(list(0), 5, 5)

aquacommz_allscen.marssls <- aquacommz_allscen.summmxls %>%
  purrr::map(~ MARSS(.,
                     list(B = "unconstrained",
                          U = "zero", A = "zero",
                          Z = "identity",
                          Q = "diagonal and equal",
                          R = R_05,
                          tinitx = 1),
                     method = "BFGS"))
names(aquacommz_allscen.marssls) <- paste0("Conc. = ", c("0", "0.1", "0.9", "6", "44" ), " micro g/L")

aquacomm.Bls <- aquacommz_allscen.marssls %>%
  purrr::map(~estar::extractB(.,
                              states_names = c("Herbivores", "DetHerbivores", "Carnivores", "Omnivores", "Detrivores")))

## Benchmark

jacobian_benchmark <- microbenchmark(

  reactivity = {aquacomm.Bls %>%
      purrr::map(estar::reactivity)},

  max_amp = {aquacomm.Bls %>%
      purrr::map(estar::max_amp)},

  init_resil = {aquacomm.Bls %>%
      purrr::map(estar::init_resil)},

  asympt_resil = {aquacomm.Bls %>%
      purrr::map(estar::asympt_resil)},

  stoch_var = {aquacomm.Bls %>%
      map(estar::stoch_var)},
  unit = "s"
) %>%
  fmt_mark_output

jacobian_plot <- autoplot(jacobian_benchmark, unit = "s") +
  theme_estar()

# ggplot2::ggsave(filename = file.path(tempdir(), "jacobian_benchmark.png"),
#                 plot = jacobian_plot,
#                 device = "png",
#                 dpi = 600,
#                 width = 20,
#                 height = 10,
#                 units = "cm")

#save(jacobian_benchmark, file = file.path(tempdir(), "jacobian_performance.rda"))
