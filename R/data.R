#' Macroinvertebrate aquatic community.
#'
#' Dataset compiled from an ecotoxicological study about the effects of insecticide (chlorpyrifos) use on a macroinvertebrate aquatic community (van den Brink et a. 1996, Wijngaarden_effects_1996). The community is composed of 128 species, classified into 5 functional groups: herbivores, detri-herbivores, carnivores, omnivores, and detrivores.
#'
#' @format A data frame with five variables:
#'
#' @field time sequential week number relative to the application of insecticide
#' @field treat concentration of insecticide
#' @field repli number of replicate
#' @field herb abundance of herbivores
#' @field detr_herb abundance of detri-herbivores
#' @field carn abundance of carnivores
#' @field omni abundance of omnivores
#' @field detr abundance of detrivores
#'
#' @references
#' van den Brink, P. J., Van Wijngaarden, R. P. A., Lucassen, W. G. H., Brock, T. C. M., & Leeuwangh, P. (1996). Effects of the insecticide dursban® 4E (active ingredient chlorpyrifos) in outdoor experimental ditches: II. Invertebrate community responses and recovery. Environmental Toxicology and Chemistry, 15(7), 1143–1153. https://doi.org/10.1002/etc.5620150719
#'
#' Wijngaarden, R. P. A. van, Brink, P. J. van den, Crum, S. J. H., Brock, T. C. M., Leeuwangh, P., & Voshaar, O. J. H. (1996). Effects of the insecticide dursban® 4E (active ingredient chlorpyrifos) in outdoor experimental ditches: I. Comparison of short-term toxicity between the laboratory and the field. Environmental Toxicology and Chemistry, 15(7), 1133–1142. https://doi.org/10.1002/etc.5620150718

"aquacomm_fgps"

#' Macroinvertebrate aquatic community formatted for univariate metrics.
#'
#' Data frame compiled from \code{aquacomm_fgps}
#'
#' @format A data frame with five variables:
#'
#' @field time sequential week number relative to the application of insecticide
#' @field carn_0 mean abundance of carnivores in control replicates
#' @field carn_0.1 mean abundance of carnivores in eplicates subjected to pulse application of 0.1 nano g/L of chlorpyrifos insecticide
#' @field carn_0.9 mean abundance of carnivores in replicates subjected to 0.9 nano g/L
#' @field carn_6 mean abundance of carnivores in replicates subjected to 6 nano g/L
#' @field carn_44 mean abundance of carnivores in replicates subjected to 44 nano g/L
#'
#'@source \code{
#'  aquacomm_resps <- aquacomm_fgps |>
#'  dplyr::select(-c(herb, detr_herb, omni, detr)) |>
#'  dplyr::group_by(time, treat) |>
#'  dplyr::summarize_at("carn", mean) |>
#'  dplyr::ungroup() |>
#'  tidyr::pivot_wider(names_from = treat,
#'  values_from = carn,
#'  names_prefix = "carn_") |>
#'  dplyr::select(time,
#'  "statvar_bl" = carn_0,
#'  "statvar_db" = carn_6)
#'  usethis::use_data(aquacomm_resps)
#'  }
#'
"aquacomm_resps"
