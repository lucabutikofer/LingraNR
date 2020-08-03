


#---------------#
#  Soil data ####
#---------------#

#' Example of soil input
#'
#' A dataset containing example soil input data for function Lingranr()
#' for one location
#'
#' @format A data frame with 1 row and 4 variables:
#' \describe{
#'   \item{sdpt}{soil depth [mm]}
#'   \item{mcfc}{moisture content at field capacity [mm3/mm3]}
#'   \item{mcsat}{moisture content at saturation [mm3/mm3]}
#'   \item{mcwp}{moisture content at wielting point [mm3/mm3]}
#'   ...
#' }
"soilExmpl"



#------------------#
#  Weather data ####
#------------------#

#' Example of weather input
#'
#' A dataset containing example weather input data for function Lingranr()
#' for one location and 11 years
#'
#' @format A data frame with 4017 row and 10 variables:
#' \describe{
#'   \item{Year}{Year}
#'   \item{DOY}{Julian day}
#'   \item{RAD}{Total solar radiation [MJ/m2*day]}
#'   \item{MINTMP}{Minimum daily tempterature ['C]}
#'   \item{MAXTMP}{Maximum daily tempterature ['C]}
#'   \item{VP}{Vapour pressure [kPa]}
#'   \item{WIND}{Mean daily wind speed [m/s]}
#'   \item{RAIN}{Total daily rainfall [mm]}
#'   \item{Country}{Country of  location}
#'   \item{Site}{Name of location}
#'
#' }
"weatherExmpl"



#------------------#
#  Harvest data ####
#------------------#

#' Example of harvest input
#'
#' A dataset containing example harvest dates
#'
#' @format A named vector of harvest dates in julian days
#'
"harvestExmpl"



#------------------------#
#  Fertilisation data ####
#------------------------#

#' Example of nitogen fertilisation input
#'
#' A dataset containing example nitrogen fertilisation dates and amounts
#'
#' @format A data frame of harvest dates with 8 row and 2 variables:
#' \describe{
#'   \item{Dates}{Fertilisation dates in julian days}
#'   \item{kgN_ha}{fertilisation amount in kg/ha of equivalent mineral nitrogen}
#'
#' }
#'
"fertilisExmpl"



#------------------------#
#  Lingra output data ####
#------------------------#

#' Example of output form Lingranr() function
#'
#' A dataset the the full output of Lingranr function. This is equivalent to
#' the "Computations" sheed int the LINGRA-N Tool.
#' See ?Lingranr() for details.
#'
#'
"lingraOutputExmpl"
