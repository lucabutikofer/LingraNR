
#  Lingranr ####
#
#' >> Run a LINGRA-N simulation for one year
#'
#' Computes potential or water-limited production on freely draining soils
#' without water table.
#'
#' @param w Dataframe of weather data for one year. Column names must be:
#'
#'    "Year": year
#'
#'    "DOY": julian day
#'
#'    "RAD": total solar radiation [kJ/m2*day]
#'
#'    "MINTMP": minimum daily tempterature [°C]
#'
#'    "MAXTMP": maximum daily tempterature [°C]
#'
#'    "VP": vapour pressure [kPa]
#'
#'    "WIND": mean daily wind speed [m/s]
#'
#'    "RAIN": total daily rainfall [mm]
#'
#'    "Country": slot for inserting country name for reference (optional)
#'
#'    "Site": slot for inserting country name for reference (optional)
#'
#' @param s Dataframe containing soil data. Column names must be:
#'
#'    "sdpt": soil depth [mm]
#'
#'    "mcfc": moisture content at field capacity [mm3/mm3]
#'
#'    "mcsat": moisture content at saturation [mm3/mm3]
#'
#'    "mcwp": moisture content at wilting point [mm3/mm3]
#'
#' @param h Either numeric vector of harvest dates in Julian days with
#'    maximum 11 entries or, if weightCut == T, numeric value of dry
#'    weight of green leaves [kg/ha] used to determine harvest dates.
#'    (Harvests when specified weight is produced.)
#'    if fewer cuts are requested fill remaining spots with NAs
#'    (First entry is date of "Clearing cut".)
#'
#' @param f Dataframe of nitrogen fertilisation events.
#'
#' Column "Dates" containing fertilisation dates in julian days.
#'
#' Column "kgN_ha" containing fertilisation amount in kg/ha of equivalent
#' mineral nitrogen
#'
#' @param lat Numeric. Latitude
#'
#' @param alt Numeric. Altitude
#'
#' @param return Character specifying the variable to return.
#'
#'    "GRASS" returns numeric value of total weight
#'    of harvested green leaves.
#'
#'    "Biomass" returns numeric value of total weight
#'    of harvested biomass.
#'
#'    "all" saves output to disk (file name given in outName)
#'    and returns content of "Calculation" sheet.
#'
#' @param wb Workbook object from package "xlsx" containing the Lingra-n excel
#'     model. Mostly for internal usage.
#'     Note that the function won't load package "xslx" if wb is specified,
#'     terefore you have to run "options(java.parameters = "-Xmx2048m")"
#'     outside of this function.
#'
#' @param weightCut Logical. If FALSE (default) "h" must specify harvest dates.
#'    If TRUE, "h" must specify the biomass threshold at which
#'    harvesting occurs.
#'    
#' @param waterStress Logical. Do you want to model water stress?
#' 
#' @param nitrogenStress Logical. Do you want to model nitrogen stress?
#'    
#' @param outName Character. The name of the output file if required.
#'    File extention must be ".xlsx".
#'    Defaults to "LingraOut.xlsx".
#'
#' @details
#' Lingranr() feeds into LINGRA-N Tool the required input variables,
#' runs the LINGRA-N model and returns the ouput of the model
#' (the "Computations" sheet) as an R object.
#' All grass growth and soil water and mineral balance simulations are
#' carried out internally to the LINGRA-N Tool.
#'
#' \href{https://widgets.figshare.com/articles/11359613/embed?show_title=1}{
#' See the original LINGRA-N Tool daocumentation} for details.
#'
#' @return
#' A data frame of "tbl_df" class (a tibble, see ?tbl_df for details)
#' of 365 rows and 423 variables
#' containing the "computations" sheet of the LINGRA-N Tool.
#' Each row in the dataframe is a day in the simulation.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Run model:
#' lo <- Lingranr(w = weatherExmpl[1:365,],
#'                s = soilExmpl,
#'                h = harvestExmpl,
#'                f = fertilisExmpl,
#'                lat = 50,
#'                alt = 50,
#'                return = 'all')
#'
#' # Plot outputs:
#' op <- par(mfrow = c(3,1))
#' plotw(weatherExmpl[1:365,])
#' plotdm(lo)
#' plotn(lo)
#'
#' }
#'

Lingranr <- function(w, s, h, f, lat, alt,
                     return = 'GRASS',
                     wb = NULL,
                     weightCut = FALSE,
                     waterStress = TRUE,
                     nitrogenStress = TRUE,
                     outName = "LingraOut"){



  # Overall processing time counter
  sto <- Sys.time()

  # Increas Java's heap space to 2GB (necessary to handle big excel files)
  # This needs executing before requiring rJava (xslx dependency) !!!
  if(is.null(wb)){ # if xlsx is not loaded yet
    options(java.parameters = "-Xmx2048m")
    library(xlsx)
  }

  # Run R and Java's garbage collectors before exiting function
  on.exit(expr = XLConnect::xlcFreeMemory())



  #===============#
  # LOAD FILES ####
  #===============#

  # Load excel workbook and extract sheets
  if(is.null(wb)){ # if lingra's workbook is not given in the parameters
    cat('\n  Loading algorithm')
    st1 <- Sys.time()

    wb <- loadWorkbook(system.file("extdata",
                                   "LINGRA-N-Plus-413.xlsx",
                                   package = "LingraNR",
                                   mustWork = TRUE))

    cat(' >> ', round(Sys.time() - st1, 1), ' sec \n')
  }

  wbs <- getSheets(wb)

  # Control sheet
  cs <- wbs$Control



  #==============================#
  # WATER AND NITROGEN STRESS ####
  #==============================#

  if(isFALSE(waterStress)){
    wr <- getRows(cs, rowIndex = 31) # drought stress
    wc <- getCells(wr, colIndex = 2)
    setCellValue(wc[[1]], 0)
  }

  if(isFALSE(nitrogenStress)){
    nr <- getRows(cs, rowIndex = 24) # nitrogen stress
    nc <- getCells(nr, colIndex = 2)
    setCellValue(nc[[1]], 0)
  }



  #============#
  # WEATHER ####
  #============#

  # Weather sheet
  ws <- wbs$Weather_data

  # Load weather in weather sheet
  addDataFrame(w, sheet = ws, startRow = 4, startColumn = 13,
               row.names = FALSE, col.names = TRUE)



  #=========#
  # SOIL ####
  #=========#

  # Modify soil parameters
  sr <- getRows(cs, rowIndex = 118) # soil depth
  sc <- getCells(sr, colIndex = 3)
  setCellValue(sc[[1]], mean(s$sdpt))

  sr <- getRows(cs, rowIndex = 128) # moisture content at field capacity
  sc <- getCells(sr, colIndex = 3)
  setCellValue(sc[[1]], mean(s$mcfc))

  sr <- getRows(cs, rowIndex = 135) # moisture content at saturation
  sc <- getCells(sr, colIndex = 3)
  setCellValue(sc[[1]], mean(s$mcsat))

  sr <- getRows(cs, rowIndex = 127) # moisture content at wilting point
  sc <- getCells(sr, colIndex = 3)
  setCellValue(sc[[1]], mean(s$mcwp))



  #============#
  # HARVEST ####
  #============#

  if (isFALSE(weightCut)){ # if harvest dates are provided
    # Load harves vector in Control sheet
    addDataFrame(h, sheet = cs, startRow = 175, startColumn = 3,
                 row.names = FALSE, col.names = FALSE)

  } else { # if harvest dates depend on leaf dry weight

    if(length(h) != 1){ # error catch
      stop(paste('If "weightCut" is set to false,',
                 'h must be a single numeric value.'))
    }

    hr <- getRows(cs, rowIndex = 18) # grass cutting type
    hc <- getCells(hr, colIndex = 2)
    setCellValue(hc[[1]], 1)

    hr2 <- getRows(cs, rowIndex = 19) # weight of leaves at cutting
    hc2 <- getCells(hr2, colIndex = 2)
    setCellValue(hc2[[1]], h)

  }


  #==================#
  # FERTILISATION ####
  #==================#

  # Load fertilisation dataframe in Control sheet
  addDataFrame(f, sheet = cs, startRow = 198, startColumn = 4,
               row.names = FALSE, col.names = FALSE)



  #==========================#
  # ADDITIONAL PARAMETERS ####
  #==========================#

  # Latitude
  lr <- getRows(cs, rowIndex = 8) # latitude
  lc <- getCells(lr, colIndex = 2)
  setCellValue(lc[[1]], lat)

  # Altitude
  alr <- getRows(ws, rowIndex = 2) # Altitude
  alc <- getCells(alr, colIndex = 15)
  setCellValue(alc[[1]], alt)



  #====================#
  # SAVE AND RELOAD ####
  #====================#

  # Update cells with functions
  cat('  Running algorithm')
  st1 <- Sys.time()
  wb$getCreationHelper()$createFormulaEvaluator()$evaluateAll()
  cat(' >> ', round(Sys.time() - st1, 1), ' sec \n')


  # Save output excel file or return required field

  if(return == 'all' | return == 'allPlots'){ # if write to disk is required

    outName <- gsub("\\.", "", outName)
    cat('  Saving outputs to "', paste0(outName, '.xlsx'), '"', sep ="")
    st1 <- Sys.time()
    saveWorkbook(wb, file = paste0(outName, '.xlsx'))
    cat(' >> ', round(Sys.time() - st1, 1), ' sec \n')

  } else if (return == 'GRASS'){ # if yield value only is required

    # Total weight of harvested leaves
    or <- getRows(cs, rowIndex = 42)
    oc <- getCells(or, colIndex = 2)
    out <- getCellValue(oc[[1]])

  } else if (return == 'Biomass'){ # if yield value only is required

    # Total weight of harvested biomass
    or <- getRows(cs, rowIndex = 45)
    oc <- getCells(or, colIndex = 2)
    out <- getCellValue(oc[[1]])

  }


  # Read "Calculations" sheet of output excel file back in
  if (return == 'all'){
    cat('  Reload as R object')
    st1 <- Sys.time()
    suppressWarnings({

      out <- readxl::read_xlsx(paste0(outName, '.xlsx'),
                               sheet = 'Calculations',
                               col_names = T,
                               col_types = 'numeric',
                               trim_ws = T,
                               skip = 3)
    })
    cat(' >> ', round(Sys.time() - st1, 1), ' sec \n')

    # Tidy up output
    out <- na.omit(out)
    out$Date <- as.Date(paste(out$Year, out$Month, out$Day, sep = '/'))
  }

  cat('  Done >> ', round(Sys.time() - sto, 1), 'sec', '\n')

  return(out)

}
