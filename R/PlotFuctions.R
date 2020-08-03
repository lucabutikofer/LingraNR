


#' Plot leaf area
#'
#' Plots the development of the photosynthetic surface in the field.
#'
#' @param lo Tibble as outputted from function Lingranr()
#'
#' @export
#'
#' @examples
#'
#' data(lingraOutputExmpl)
#' plotla(lingraOutputExmpl)
#'
plotla <- function(lo){

  # lo: Lingranr output.
  #
  # Critical LAI is defined as the LAI when 95% of PAR is intercepted
  # by the crop canopy (Gifford and Jenkins 1982)

  #Harvestable leaf weight in the field plus cutted amounts of grass; (YIELD)
  yi <- lo[,153]

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


#' Plot growth rates
#'
#' Plots the growth rates
#'
#' @param lo Tibble as outputted from function Lingranr()
#'
#' @export
#'
#' @examples
#'
#' data(lingraOutputExmpl)
#' plotgr(lingraOutputExmpl)
#'
#'
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



#' Plot weather
#'
#' Plots the weather input for Lingranr()
#'
#' @param w Weather data as inputted in Lingranr().
#'    See ?lingranr() for details.
#'
#' @export
#'
#' @examples
#'
#' data(weatherExmpl)
#' plotw(weatherExmpl[1:365,])
#'
#'
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



#' Plot dry matter production
#'
#' Plots the dry matter production in a field
#'
#' @param lo Tibble as outputted from function Lingranr().
#'    See ?lingranr() for details.
#'
#' @export
#'
#' @examples
#'
#' data(lingraOutputExmpl)
#' plotdm(lingraOutputExmpl)
#'
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


#' Plot nitrogen balance
#'
#' Plots the nitrogen balance between soil and grass
#'
#' @param lo Tibble as outputted from function Lingranr().
#'    See ?lingranr() for details.
#'
#' @export
#'
#' @examples
#'
#' data(lingraOutputExmpl)
#' plotn(lingraOutputExmpl)
#'
#'
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

