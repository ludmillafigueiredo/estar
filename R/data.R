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
