

# Metamodel functions ####

#' Run metamodel
#' 
#' These functions run the metamodel described in Qi et al. 2018 for
#' grassland productivity on temporary (\code{tg()}), permanent (\code{pg()})
#' and semi-natural (rough grazing, \code{rg()}) grassland.
#'
#' @aliases metamodel
#'
#' @param awc Numeric. Available water content
#' 
#' @param pmam Cumulative precipitation for the period March-April-May
#'
#' @param pjj Cumulative precipitation for the period June-July
#' 
#' @param pamj Cumulative precipitation for the period April-May-June
#' 
#' @param pjas Cumulative precipitation for the period July-August-September
#' 
#' @param pas Cumulative precipitation for the period August-September
#' 
#' @param rmam Cumulative global radiation for the period March-April-May
#' 
#' @param ras Cumulative global radiation for the period August-September
#' 
#' @param ramj Cumulative global radiation for the period April-May-June
#' 
#' @param rjas Cumulative global radiation for the period July-August-September
#' 
#' @param tmam Mean air temperature for the period March-April-June
#' 
#' @param tjj Mean air temperature for the period June-July
#' 
#' @param tas Mean air temperature for the period August-September
#' 
#' @param tjas Mean air temperature for the period June-August-September
#' 
#' @return 
#' Numeric value of dry matter production in t/ha.
#' 
#' @details 
#' "Temporary grasslands
#' are the most productive, often consisting of frequently resown
#' perennial ryegrasses (Lolium perenne) and are assumed to receive
#' annual N application rate of 300 kg N ha−1.
#' Permanent grasslands
#' consist of a mixture of sown and indigenous grasses and
#' legumes; they are of intermediate productivity and are assumed to receive
#' annual N applications of 150 kg N ha−1.
#' The extensively used rough grazing
#' are diverse semi-natural grasslands containing various herbaceous species,
#' receive no synthetic N and are areas of low productivity." (Qi et al. 2018)
#' 
#' @examples
#' # Yield of temporary grassland:
#' tgy <- tg(awc = 37.9,
#'           pmam = 105,
#'           pjj = 126.9,
#'           pas = 95.2,
#'           rmam = 1247.7,
#'           ras = 786.8,
#'           tmam = 9.8,
#'           tjj = 15.8,
#'           tas = 15.6)
#' 
#' @export
#'
tg <- function(awc, pmam, pjj, pas, rmam, ras, tmam, tjj, tas){
  
  y <- -2.18 + 0.1267*awc - 0.000315*awc^2 + 0.04013*pmam - 0.0000949*pmam^2 + 
       0.05079*pjj - 0.0001204*pjj^2 + 0.02704*pas - 0.0000717*pas^2 - 
       0.002806*rmam - 0.005512*ras + 0.7105*tmam - 0.2445*tjj - 0.1945*tas
  
  return(y)
  
}


#' @rdname tg
#' 
#' @export
#' 
pg <- function(awc, pamj, pjas, ramj, rjas, tmam, tjas){
  
  y <- -2.915 + 0.09141*awc - 0.0002261*awc^2 +
       0.04381*pamj - 0.00009415*pamj^2 + 0.03457*pjas - 0.00004871*pjas^2 - 
       0.002489*ramj - 0.002826*rjas + 0.1877*tmam - 0.0808*tjas
  
  return(y)
}


#' @rdname tg
#' 
#' @export
#' 
rg <- function(awc, pamj, pjas, ramj, rjas, tmam, tjas){

  y <-  -0.862 + 0.03865*awc - 0.0001028*awc^2 +
        0.0146*pamj - 0.00003894*pamj^2 + 0.01294*pjas -
        0.00002482*pjas^2 - 0.000935*rjas + 0.035*tmam - 0.1046*tjas

  return(y)
}



# Nitrogen modifier functions ####

#' Nitrogen fertilisation effect on yield
#' 
#' This function implements the four-parameter rational equation proposed by
#' Morrison et al. (1980) as parameterised by Qi et al. (2018) for the UK.
#' 
#' @return Numeric. The percentage of dry matter yield relative to
#' the yield obtained with 600 kg/ha N fertilisation obtained when
#' fertilising with a user-selected N quantitiy [kg/ha].
#' 
#' @param N Numeric value of nitrogen fertilisation in kg/ha.
#' 
#' @noRd
#'
Nresp <- function(N){


  # N: kg/ha of nitrogen fertiliser

  a = 22.1636
  b = 0.2373
  c = -0.0001944
  d = 0.000002117

  dmy <- (a + b*N)/(1 + c*N + d*N^2)

  return(dmy)

}



#' Nitrogen fertilisation effect on yield
#' 
#' This function modifies the yield output from the \link{metamodel}
#' functions according user-defined changes in nitrogen fertilisation.
#' 
#' @details This function is based on the four-parameter rational equation
#' proposed by Morrison et al. (1980) as parameterised by Qi et al. (2018)
#' for the UK. See \link{metamodel} for default nitrogen inputs of 
#' grassland types.
#' 
#' @return Numeric. The new yield after modifying nitrogen fertilsation in the
#' same units as \code{yield}.
#' 
#' @param N Numeric value of nitrogen fertilisation in kg/ha.
#' 
#' @param yield Numeric. Current dry matter yield (e.g. in kg/ha or t/ha)
#' 
#' @param mng Character. Current management, either "TG" (Temporary Grassland),
#' "PG" (Permanent Grassland), "RG" (Rough Grazing).
#'
#' @export
#' 
#' @examples 
#' # Yield of temporary grassland:
#' # (Metamodel's default for temporary grassland is 300 kg/ha N fertilisation)
#' tgy <- tg(awc = 37.9,
#'           pmam = 105,
#'           pjj = 126.9,
#'           pas = 95.2,
#'           rmam = 1247.7,
#'           ras = 786.8,
#'           tmam = 9.8,
#'           tjj = 15.8,
#'           tas = 15.6)
#' 
#' # Yield when fertilised with 500 kg/ha
#' tgyN <- Nchange(tgy, "TG", 500)
#' print(c(tgy = tgy, tgyN = tgyN))
#' 
Nchange <- function(yield, mng, N){


  # yield: Numeric. Current dry matter yield [kg/ha]
  # mng: Character. Current management, either 'TG', 'PG', 'RG'.
  # N: Numeric. kg/ha of nitrogen fertiliser

  # Current Nitrogen fertilisation
  if (mng == 'TG') cN = 300 # kg/ha N
  if (mng == 'PG') cN = 150 # kg/ha N
  if (mng == 'RG') cN = 0   # kg/ha N

  # Yield change rate
  yc <- 1 + (Nresp(N) - Nresp(cN))/100

  # New yield
  ny <- yield*yc

  return(ny)

}