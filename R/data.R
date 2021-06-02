#' Macroinvertebrate aquatic community.
#'
#' Dataset compiled from an ecotoxicological study about the effects of insecticide (chlorpyrifos) use on a macroinvertebrate aquatic community [@vandenbrick_effects_1996; @vanwijngaarden_effects_1996]. The community is composed of 128 species, classified into 5 functional groups: herbivores, detri-herbivores, carnivores, omnivores, and detrivores.
#'
#' @format A data frame with five variables:
#' \describe{
#' \item{time}{sequential week number relative to the application of insecticide}
#' \item{treat}{concentration of insecticide}
#' \item{repli}{number of replicate}
#' \item{herb}{abundance of herbivores}
#' \item{detr_herb}{abundance of detri-herbivores}
#' \item{carn}{abundance of carnivores}
#' \item{omni}{abundance of omnivores}
#' \item{detr}{abundance of detrivores}
#' }
#'

"aquacomm_fgps"

#' Macroinvertebrate aquatic community formatted for univariate metrics.
#'
#' Data frame compiled from \code{aquacomm_fgps}
#'
#' @format A data frame with five variables:
#' \describe{
#' \item{time}{sequential week number relative to the application of insecticide}
#' \item{carn_0}{mean abundance of carnivores in control replicates}
#' \item{carn_0.1}{mean abundance of carnivores in eplicates subjected to pulse application of 0.1 nano g/L of chlorpyrifos insecticide}
#' \item{carn_0.9}{mean abundance of carnivores in replicates subjected to 0.9 nano g/L}
#' \item{carn_6}{mean abundance of carnivores in replicates subjected to 6 nano g/L}
#' \item{carn_44}{mean abundance of carnivores in replicates subjected to 44 nano g/L}
#' }
#'
#'@source \code{
#'  aquacomm_resps <- aquacomm_fgps %>%
#'  dplyr::select(-c(herb, detr_herb, omni, detr)) %>%
#'  dplyr::group_by(time, treat) %>%
#'  dplyr::summarize_at("carn", mean) %>%
#'  dplyr::ungroup() %>%
#'  tidyr::pivot_wider(names_from = treat,
#'  values_from = carn,
#'  names_prefix = "carn_") %>%
#'  dplyr::select(time,
#'  "statvar_bl" = carn_0,
#'  "statvar_db" = carn_6)
#'  usethis::use_data(aquacomm_resps)
#'  }
#'
"aquacomm_resps"
