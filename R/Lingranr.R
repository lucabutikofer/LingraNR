
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
#'    "RAD": total solar radiation [MJ/m2*day]
#'
#'    "MINTMP": minimum daily tempterature ['C]
#'
#'    "MAXTMP": maximum daily tempterature ['C]
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
#'    "sdpt": soil depth
#'
#'    "mcfc": moisture content at field capacity
#'
#'    "mcsat": moisture content at saturation
#'
#'    "mcwp": moisture content at wilting point
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



plotla <- function(lo){

  # lo: Lingranr output.
  #
  # Critical LAI is defined as the LAI when 95% of PAR is intercepted
  # by the crop canopy (Gifford and Jenkins 1982)

  #Harvestable leaf weight in the field plus cutted amounts of grass; (YIELD)
  yi <- lo[,153][[1]]

  # N fertilizer application dates
  nf <- lo$`Fertilizer N application; (0=NO, 1=YES)`
  nf <- lo$Date[nf == 1]

  # Harvest dates
  hd <- lo$`Harvest date`
  hd <- lo$Date[hd == 1]

  # Set par
  po <- par(mar = c(5, 5, 4, 5) + 0.1)

  # Plot stem area index
  plval <- c(lo$`Stem area index`,
               lo$`Leaf area index; LAI`,
               lo$`Critical leaf area index; LAICR`,
               lo$`Leaf and stem area index; LAI`)
  plot(lo$Date, lo$`Stem area index`,
       type = 'n',
       ylim = range(plval),
       main = 'Green are index and leaf yield',
       ylab = expression('Area Index [m'^2*'/m'^2*']'),
       xlab = 'Date')

  # Add marks for N fertilisation
  # abline(v = nf, col = 'grey', lty = 3)
  if(length(nf) != 0){
    points(x = nf, y = rep(max(plval), length(nf)),
           pch = 25,
           col = adjustcolor(4, alpha.f = .4),
           bg = adjustcolor(4, alpha.f = .4),
           cex = 3)
  }

  # Add marks for harvest dates
  # abline(v = hd, col = 4, lty = 3)
  if(length(hd) != 0){
    points(x = hd, y = rep(max(plval), length(hd)),
           pch = 25,
           col = adjustcolor('orange', alpha.f = .4),
           bg = adjustcolor('orange', alpha.f = .4),
           cex = 2)
  }

  # Plot other green are index measures
  lines(lo$Date, lo$`Stem area index`, col = 4)
  lines(lo$Date, lo$`Leaf area index; LAI`, col = 3)
  lines(lo$Date, lo$`Critical leaf area index; LAICR`, col = 6, lty = 3)
  lines(lo$Date, lo$`Leaf and stem area index; LAI`, col = 2)


  # Add yield with its specific axis
  par(new = T)
  plot(lo$Date, yi,
       axes=F, xlab=NA, ylab=NA, type = 'l',
       col = adjustcolor('orange', alpha.f = .4),
       lwd = 3)
  axis(side = 4)
  mtext(side = 4, line = 3, 'Leaf dry matter [kg / ha]')

  # Add legend
  legend('topleft', c('Leaf Area Index',
                      'Stem Area Index',
                      'Combined Area Index',
                      'Critical LAI',
                      'Leaf Yield (scale on the right side)',
                      'N Fertilisation',
                      'Harvest dates'),
         col = c(3, 4, 2, 6, adjustcolor('orange', alpha.f = .4),
                 adjustcolor(4, alpha.f = .4),
                 adjustcolor('orange', alpha.f = .4)),
         pt.bg = c(NA,NA,NA,NA,NA,
                   adjustcolor(4, alpha.f = .4),
                   adjustcolor('orange', alpha.f = .4)),
         lty = c(1, 1, 1, 3, 1, NA, NA),
         lwd = c(1, 1, 1, 1, 3, NA, NA),
         pch = c(NA,NA,NA,NA,NA,25,25),
         bty = 'n')

  par(po)
}



# Plot growth rates
plotgr <- function(lo){

  # lo: Lingranr output.

  # Source limited crop growth rate
  sl <- lo[200][[1]]

  # Conversion to green leaf sink limited carbon demand
  ss <- lo[203][[1]]

  # Actual growth rate (min of source and sink)
  ag <- lo$`Total growth rate of leaves; GTW`

  # N fertilizer application dates
  nf <- lo$`Fertilizer N application; (0=NO, 1=YES)`
  nf <- lo$Date[nf == 1]

  # Harvest dates
  hd <- lo$`Harvest date`
  hd <- lo$Date[hd == 1]

  # Empty plot
  plot(lo$Date, sl,
       ylim = range(c(sl, ss, ag)),
       type = 'n', col = 3,
       main = 'Growth rates',
       ylab = 'Dry matter growth rate [kg / ha*day]',
       xlab = 'Date')

  # Add marks for N fertilisation
  abline(v = nf, col = 'grey', lty = 3)

  # Add marks for harvest dates
  abline(v = hd, col = 4, lty = 3)

  # Add source limited
  lines(lo$Date, sl,
        col = 4,)

  # Add sink limited
  lines(lo$Date, ss,
        col = 6,)

  # Add actual growth
  lines(lo$Date, ag,
        col = adjustcolor(1, alpha.f = .4), lwd = 3)

  # Add legend
  legend('topleft', c('Source limited',
                      'Sink limited',
                      'Actual (min of source and sink)',
                      'N fertilisation',
                      'Harvest'),
         col = c(4, 6, adjustcolor(1, alpha.f = .4), 'grey', 4),
         lty = c(1,1,1,3,3),
         lwd = c(1,1,3,1,1),
         bty = 'n')

}



# Plot weather
plotw <- function(w){

  # Set par
  po <- par(mar = c(5, 5, 4, 5) + 0.1)

  # Timeline
  tl <- as.Date(w$DOY, format = '%j',
                origin = as.Date(paste0(w$Year[1],'0101'),
                                 format = '%Y%m%d') - 1)

  # Empty plot
  plot(x = tl,
       y = w$MINTMP,
       type = 'n',
       ylim = range(c(w$MINTMP, w$MAXTMP)),
       xlab = 'Date',
       ylab = 'Temperature [C]',
       main = paste('Daily weather summary for', w$Year[1]))

  polygon(x = c(tl, rev(tl)),
          y = c(w$MAXTMP ,rev(w$MINTMP)),
          col = adjustcolor('orange', alpha.f = .4),
          border = NA)

  # Average temperature
  lines(tl, apply(cbind(w$MINTMP, w$MAXTMP), 1, mean, na.rm = T),
        col = 'orange')

  # Add rainfall with its specific axis
  par(new = T)
  barplot(w$RAIN, col = 4, border = NA, axes=F, xlab=NA, ylab=NA)

  axis(side = 4)
  mtext(side = 4, line = 3, 'Rainfall [mm]')

  # Add legend
  legend('topleft', c('Mean temp',
                      'Rainfall (scale on the right side)'),
         col = c('orange', 4),
         bty = 'n',
         lty = c(1, 1))

  par(po)

}



# Plot dry matter
plotdm <- function(lo){

  # lo: Lingranr output.

  # Weight of living leaves; WLVG
  lv <- lo[132][[1]]

  # Weight of living stems; WST
  st <- lo[136][[1]]

  # Weight of living roots; WRT
  # rt <- lo[138][[1]]

  # Weight of storage organs; WSO
  so <- lo[141][[1]]

  #Harvestable leaf weight in the field plus cutted amounts of grass; (YIELD)
  yi <- lo[153][[1]]

  # N fertilizer application dates
  nf <- lo$`Fertilizer N application; (0=NO, 1=YES)`
  nf <- lo$Date[nf == 1]

  # Harvest dates
  hd <- lo$`Harvest date`
  hd <- lo$Date[hd == 1]

  # Total harvested weight (CUMHARV)
  hrv <- lo[151][[1]]

  # Total above ground living and dead dry weight plus total harvest
  bm <- lo[133][[1]] + hrv

  # Empty plot
  plot(lo$Date, lv,
       ylim = range(c(lv, st, so, yi)),
       type = 'n',
       main = 'Biomass partitioning',
       sub =  paste("Total above-ground production:",
                    round(tail(bm, 1)), "kg/ha \n",
                    "Total harvest:",
                    round(tail(hrv, 1)), "kg/ha"),
       ylab = 'Dry matter [kg / ha]',
       xlab = '')

  # Add marks for N fertilisation
  # abline(v = nf, col = 'grey', lty = 3)
  if(length(nf) != 0){
  points(x = nf, y = rep(max(c(lv, st, so, yi)), length(nf)),
         pch = 25,
         col = NULL,
         bg = adjustcolor(4, alpha.f = .4),
         cex = 3)
  }

  # Add marks for harvest dates
  # abline(v = hd, col = 4, lty = 3)
  if(length(hd) != 0){
  points(x = hd, y = rep(max(c(lv, st, so, yi)), length(hd)),
         pch = 25,
         col = NULL,
         bg = adjustcolor('orange', alpha.f = .4),
         cex = 2)
  }

  # Add leaves biomass
  lines(lo$Date, lv,
        col = 3,)

  # Add stems biomass
  lines(lo$Date, st,
        col = 4,)

  # Add roots biomass
  # lines(lo$Date, rt,
  #       col = 6)

  # Add storage organs biomass
  lines(lo$Date, so,
        col = 6,)

  # Add leaf yield (harvested plus current leaf biomass)
  lines(lo$Date, yi,
       col = adjustcolor('orange', alpha.f = .4),
       lwd = 3)


  # Add legend
  legend('topleft', c('Living leaves',
                      'Living stems',
                      # 'Living roots',
                      'Storage organs',
                      'Leaf yield (harvested + currently in field)',
                      'N fertilisation',
                      'Harvest dates'),
         col = c(3,4,6,adjustcolor('orange', alpha.f = .4),
                 NA,
                 NA),
         pt.bg = c(NA,NA,NA,NA,
                   adjustcolor(4, alpha.f = .4),
                   adjustcolor('orange', alpha.f = .4)),
         lty = c(1,1,1,1,NA,NA),
         pch = c(NA,NA,NA,NA,25,25),
         lwd = c(1,1,1,3,NA,NA),
         bty = 'n')

}


# Plot dry matter
plotn <- function(lo){

  # lo: Lingranr output.

  # Total mineral N directly available from soil and fertiliser; NMINT [kg / ha]
  na <- lo[270][[1]]

  # Total N demand; NDEMTO [kg / ha]
  nd <- lo[269][[1]]

  # Daily N uptake rate by the crop; NUPTR [kg / ha]
  nu <- lo[271][[1]]

  # N fertilizer application dates
  nf <- lo$`Fertilizer N application; (0=NO, 1=YES)`
  nf <- lo$Date[nf == 1]

  # Harvest dates
  hd <- lo$`Harvest date`
  hd <- lo$Date[hd == 1]

  # Empty plot
  plot(lo$Date, na,
       ylim = range(c(na, nd, nu)),
       type = 'n',
       main = 'Nitrogen balance',
       ylab = 'Kg of nitrogen per hectar',
       xlab = 'Date')

  # Add marks for N fertilisation
  # abline(v = nf, col = 'grey', lty = 3)
  if(length(nf) != 0){
    points(x = nf, y = rep(max(c(na, nd, nu)), length(nf)),
           pch = 25,
           col = NULL,
           bg = adjustcolor(4, alpha.f = .4),
           cex = 3)
  }

  # Add marks for harvest dates
  # abline(v = hd, col = 4, lty = 3)
  if(length(hd) != 0){
    points(x = hd, y = rep(max(c(na, nd, nu)), length(hd)),
           pch = 25,
           col = NULL,
           bg = adjustcolor('orange', alpha.f = .4),
           cex = 2)
  }

  # Add N availability
  lines(lo$Date, na,
        col = adjustcolor(4, alpha.f = .4), lwd = 3)

  # Add N demand
  lines(lo$Date, nd,
        col = adjustcolor('orange', alpha.f = .4), lwd = 3)

  # Add N uptake
  lines(lo$Date, nu,
        col = 6)

  # Add legend
  legend('topleft', c('N availability from soil and fertilisation',
                      'N demand',
                      'N uptake',
                      'N fertilisation',
                      'Harvest dates'),
         col = c(adjustcolor(4, alpha.f = .4),
                 adjustcolor('orange', alpha.f = .4),
                 6,
                 NA,
                 NA),
         pt.bg = c(NA,NA,NA,
                   adjustcolor(4, alpha.f = .4),
                   adjustcolor('orange', alpha.f = .4)),
         lty = c(1,1,1,NA,NA),
         pch = c(NA,NA,NA,25,25),
         lwd = c(3,3,1,NA,NA),
         bty = 'n')

}


# Plot dry matter. Used to compare with empirical readings. Boxplots to be
# added separately, see "Benchmark.R".

plotdm2 <- function(lo, gc = NULL){

  # lo: Lingranr output.
  # gc: Numerical vector of biomass from grass cuttings.
  #     Used to extend ylim if lingra underestimated.
  #
  #
  #==================================================================#
  # Need line showing:
  #   Living leaves DM, living stems DM
  # See if adding the following matches empirical observations better
  #   Living storage organs DM, Dead stems DM, Dead leaves DM
  #==================================================================#


  # BIOMASS:

  # Weight of living leaves; WLVG
  lv <- lo[132][[1]]

  # Weight of living stems; WST
  st <- lo[136][[1]]

  # Weight of storage organs; WSO
  so <- lo[141][[1]]

  # Total harvested weight (CUMHARV)
  hrv <- lo[151][[1]]

  # Productivity
  prd <- lv + st + so

  # Weight of living roots; WRT
  # rt <- lo[138][[1]]

  # Harvestable leaf weight in the field plus cutted amounts of grass; (YIELD)
  # yi <- lo[153][[1]]

  # Total above ground living and dead dry weight plus total harvest
  bm <- lo[133][[1]] + hrv


  # MANAGEMENT:

  # N fertilizer application dates
  nf <- lo$`Fertilizer N application; (0=NO, 1=YES)`
  nf <- lo$Date[nf == 1]

  # Harvest dates
  hd <- lo$`Harvest date`
  hd <- lo$Date[hd == 1]

  # Range of y axis
  yrng <- range(c(prd, gc))

  # Empty plot
  plot(lo$Date, lv,
       ylim = yrng,
       type = 'n',
       main = 'Total above-ground living biomass',
       sub =  paste("Total above-ground production:",
                    round(tail(bm, 1)), "kg/ha \n",
                    "Total harvest:",
                    round(tail(hrv, 1)), "kg/ha"),
       ylab = 'Dry matter [kg / ha]',
       xlab = '')

  # Add marks for N fertilisation
  if(length(nf) != 0){
    points(x = nf, y = rep(max(yrng), length(nf)),
           pch = 25,
           col = NULL,
           bg = adjustcolor(4, alpha.f = .4),
           cex = 3)
  }

  # Add marks for harvest dates
  if(length(hd) != 0){
    points(x = hd, y = rep(max(yrng), length(hd)),
           pch = 25,
           col = NULL,
           bg = adjustcolor('orange', alpha.f = .4),
           cex = 2)
  }


  # Add productivity line
  lines(lo$Date, prd, lwd = 3,
        col = "orange")


  # Add leaves biomass
  # lines(lo$Date, lv,
  #       col = 3)

  # Add stems biomass
  # lines(lo$Date, st,
  #       col = 4)

  # Add roots biomass
  # lines(lo$Date, rt,
  #       col = 6)

  # Add storage organs biomass
  # lines(lo$Date, so,
  #       col = 6,)

  # Add leaf yield (harvested plus current leaf biomass)
  # lines(lo$Date, yi,
  #       col = adjustcolor('orange', alpha.f = .4),
  #       lwd = 3)


  # Add legend

  legend('topleft', c('Above ground living biomass',
                      'N fertilisation',
                      'Harvest dates'),
         col = c("orange",
                 NA,
                 NA),
         pt.bg = c(NA,
                   adjustcolor(4, alpha.f = .4),
                   adjustcolor('orange', alpha.f = .4)),
         lty = c(1,NA,NA),
         pch = c(NA,25,25),
         lwd = c(3,NA,NA),
         bty = 'n')


  # legend('topleft', c('Living leaves',
  #                     'Living stems',
  #                     # 'Living roots',
  #                     'Storage organs',
  #                     'Leaf yield (harvested + currently in field)',
  #                     'N fertilisation',
  #                     'Harvest dates'),
  #        col = c(3,4,6,adjustcolor('orange', alpha.f = .4),
  #                NA,
  #                NA),
  #        pt.bg = c(NA,NA,NA,NA,
  #                  adjustcolor(4, alpha.f = .4),
  #                  adjustcolor('orange', alpha.f = .4)),
  #        lty = c(1,1,1,1,NA,NA),
  #        pch = c(NA,NA,NA,NA,25,25),
  #        lwd = c(1,1,1,3,NA,NA),
  #        bty = 'n')

}
