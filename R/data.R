#' Three Simulated Transitioning Communities.
#'
#' A dataset containing three simulated five species communities stressed through a critical transition.
#'
#' @format A list of three dataframes with 301 rows and 7 variables each:
#' \describe{
#'   \item{community_id}{the identity of the simulated community}
#'   \item{time}{time index}
#'   \item{spp_1}{density of species 1}
#'   \item{spp_2}{density of species 1}
#'   \item{spp_3}{density of species 1}
#'   \item{spp_4}{density of species 1}
#'   \item{spp_5}{density of species 1}
#'   \item{inflection_pt}{the time index where transition occurs}
#' }
"simTransComms"

#' Three Recovering Cod Populations
#'
#' A dataset containing three simulated cod populations. Community 1 does not recovery whereas Community 100 and 200 do.
#'
#' @format A list of three dataframes with 191 rows and 6 variables each:
#' \describe{
#'   \item{community_id}{the identity of the simulated community}
#'   \item{time}{time index}
#'   \item{biomass}{population total biomass}
#'   \item{mean.size}{average length of cod individuals}
#'   \item{sd.size}{variation in length of cod individuals}
#'   \item{inflection_pt}{the time index where transition occurs}
#' }
#' @source Clements C., McCarthy M., Blanchard J. (2019) Early warning signals of recovery in complex systems. Nature Communications, 10:1681.
"CODrecovery"

