# flux.R, for research paper "A Simple Method To Measure Methane Emissions From Indoor Gas Leaks"
# Dominic Nicholas 2022/2023
# RStudio Version 2023.03.0+386 (2023.03.0+386)

library("lattice") # V 0.20-45
library("ggplot2") # for plotting graphs V 3.3.3
library("ggthemes") # V 4.2.4
library("readxl") # for reading xlsx files V 1.3.1
library("writexl") # for writing xlsx files V 1.4.0
library("stringr") # for str_detect V 1.4.0
library("optparse") # for cli option handling V 1.6.6
library("RColorBrewer") # for palettes V 1.1-3
library("wesanderson") # for colors - not used much any more V 0.3.6
library("stats") # for supsmu V 3.6.2
library("tidyr") # for various V 1.1.3
library("tidyverse") # for various V 1.3.1
library("scales") # for rescaling V 1.1.1 
library("roll")  # for roll_mean V 1.1.6
library("lubridate") # time handling etc V 1.7.10
library("cowplot") # for side by side ggplots with plot_grid() V 1.1.1

# this block of flags control what flux.R does 
processExperiments = 1  # use this to do modelling - these results are then put back into the flux measurements spreadsheet for further analysis 
doAnalysis = 0 # use this and its related options to do analysis 
   analyzeVarying  = 0
   analyzeFlushes = 0
   analyzeControls = 0
   analyzeAmbients = 0
   analyzeLeaks = 0
controlTests = 0
processPressureData = 1

scriptVersion = "0.1"
setwd("/Users/dom/flux") # set to your cwd if you're running in Rstudio
dataFile = "./flux measurements.xlsx" # change this to match the name of the spreadsheet in use, eg 'research data - cleaned.xlsx'
experimentNameFilter = "pressure5" # set to NULL to analyze all experiments, or set it to an experiment name, eg "R1" from the spreadsheet
fluxCalculationMethod = "simple" # by default use the simple method ; --fluxmethod [simple|fitted]
fluxCalculationMethod = "fitted"
nlsIterations = 100 # nls iterations
originWeight = 20000 # weight for nls modelling for first point
simpledMethod_firstNSeconds = 600 # how many seconds over which to calc flux with simple method - 600 is default
startTimeAdjustment =  NULL # add this number of seconds to the start time, or NULL
limitDataToFirstNSeconds = NULL  # Use just this first amount of time to fit to
extraSpace = 100 # pixels of space to add text labels in plot
brewerPalette = "Set1" # brewer.pal palette name
debugPlot = NULL # show two other debug plots before main plot 
justPlot = NULL # TODO this doesn't do the right thing - fix it
plotCO2 = NULL # TBD parameterize
dontModel = NULL # F, or T to just stop after doing basic plot of data and don't do flux calc modelling etc
dontPlotModel = NULL # NULL or T - controls whether modeled stuff is plotted - useful for paper's method chart
plotLegend = 1
paperPlots = 0
plotSimpleFit = 1
reportBackName = NULL # this controls how the 'Calculated flux: RCM Method 1 aka simple slope' barplot looks
figure7 = NULL # null or 1
figure8 = NULL

# plot grid spacing but not used that much
GridSpacingHorizontal = 0.5 # 0.25 ppmv
GridSpacingVertical = 600 # 500 sec

# save orig par
originalPar = par() #mar defaults are bottom, left, top, right : c(5.1, 4.1, 4.1, 2.1)

# RCM colors
colorsRCM =  c("#2a8cbe", "#6baed566" )
colorsSRCM = c("#006d2b", "#74c376")

# --------------------------------------------------
stop_quietly = function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

# --------------------------------------------------
process_cli_options = function ( ) {
  
  # wasn't used and not fully developed or tested - just run this all in RStudio
  
  errors = NULL
  
  optionList = list(
    make_option( c("--datafile"), type = "character", default = NULL, help = "XLSX containing geocoded leaks data" ),
    make_option( c("--fluxmethod"), type = "character", default = "simple", help = "simple or fitted" ),
    make_option( c("--outfile"), type = "character", default = "./results.xlsx", help = "Name of results xlsx [default %default]" ),
    make_option( c("--experimentname"), type = "character", default = NULL, help = "Run calcs for just experiment 'Name'" ),
  )
  
  theUsage = paste( "%prog [options] --datafile <DATA xlsx file>")
  optParser = OptionParser( 
    usage = theUsage, 
    epilogue = paste("Version",scriptVersion, " - Dominic Nicholas March 2022"),
    description = "tbd",
    option_list = optionList 
  )
  
  cliOptions = parse_args( optParser )
  if ( is.null( cliOptions$datafile ) ) {
    print_help(optParser)
    stop( "Supply a filename.", call. =FALSE)
  }
  
  assign( "dataFile", cliOptions$datafile, envir = .GlobalEnv)
  if ( ! is.null(cliOptions$outfile) )     { assign( "outFile", cliOptions$outfile, envir = .GlobalEnv)}
  if ( ! is.null(cliOptions$fluxmethod) )  { assign( "fluxCalculationMethod", cliOptions$fluxmethod, envir = .GlobalEnv)}
  
  if ( ! file.exists(dataFile)) {
    stop( paste0("The data file '", dataFile ,"' doesn't exist."))
  }
  
  assign( "experimentNameFilter", cliOptions$experimentname, envir = .GlobalEnv)
  
  
  if ( ! is.null(errors) ) { 
    stop(errors, call. = FALSE)  
  }
  
}

# --------------------------------------------------
load_methane_data = function ( experiment , times ) {
  
  # find and load all dat files in a given list of folders
  
  datFiles = ""  
  for ( dataFolder in strsplit(experiment$`Data files`,",")[[1]] ) {
    datFiles = append( datFiles, list.files( dataFolder, pattern = "*.dat$", recursive = T, full.names =T ) )
  }

  allData = data.frame()  
  filecount = 0
  cat("\tLoading dat files: ")
  
  for ( filename in datFiles[-1] ) {
    cat (".")
    if ( file.info( filename)$size == 0 ) { 
      cat("\n!!! ERROR : File ", filename," is zero bytes - remove or update it - quitting\n")
      stop_quietly()
      
    }
    datData = read.table(filename, sep = "" , header = T , na.strings ="", stringsAsFactors= F)
    allData = rbind(allData, datData )  
    filecount = filecount+1
  }
  cat("\n\tProcessed", filecount, "files\n")
  
  sortedSelectedData = allData[ order(allData$EPOCH_TIME),]
  
  
  # subset down to start:end range
  if ( is.null(times) ) {
    startTime = as.numeric(experiment$`Start time (epoch seconds)`)
  }
  else {
    startTime = times[1]
  }
  if ( ! is.null(startTimeAdjustment)) { 
    cat("\t!! Adjusting start time by ", startTimeAdjustment, "seconds\n")
    startTime = startTime + startTimeAdjustment
  }

  # (Use limitDataToFirstNSeconds to overide the end time ie make it larger)
  if ( is.null(times) ) {
     endTime = as.numeric(experiment$`End time (epoch seconds)`) 
  }
  else {
     endTime = times[2]
  }
  
  cat ("\tFiltering data to experiment's start/end time range  ", startTime, " to ", endTime, "\n" )
  filteredData = sortedSelectedData[ which( sortedSelectedData$EPOCH_TIME >= startTime & sortedSelectedData$EPOCH_TIME <= endTime ), ]
  
  # Zipping all raw data up based on day results in >200mb which is too large to upload as an SI zip (limit is 100mb)
  # So this creates a dump of the raw data that has been time-bounded ...
  #    filteredDataFilename =  paste0("/Users/dom/flux/PLOSONE-submission/code and data/raw data/cavity ringdown spectrometer data/Picarro GasScouter G4301/", experiment$`Experiment name`, ".dat")
  #    write.table(filteredData, filteredDataFilename, append = FALSE, sep = "\t", col.names = TRUE, quote=F)
  #    cat ("Wrote ", filteredDataFilename, "\n")
  
  rowCount = nrow(filteredData)
  if ( rowCount == 0 ) { 
    cat ("\t!!!", rowCount, "rows of data to process - quitting\n")
    stop_quietly()
  }
  else {
    cat ("\t",rowCount,"rows of data to process\n")
  }
  
  return (filteredData)
}

# --------------------------------------------------
load_experiments_spreadsheet = function( spreadsheet, worksSheet, expectedColumns, skipRows, omitNAs ) {
  # Read in and check the data spreadsheet and returns a data frame of just the required data
  
  # load the data in. Skip=N means skip N rows which contains super-headers
  fluxData = read_excel( spreadsheet, skip = skipRows , workSheet, col_types = "text" ) 
  
  fluxData = fluxData %>% drop_na(`Experiment name`) # skip non named rows (intended to skip blank rows)
  
  # remove any rows after experiment 'STOP'
  fluxData = fluxData[ 1:(which(grepl( "STOP", fluxData$`Experiment name`))-1) , ]
  
  # check expected columns are present in the read in worksheet
  for ( requiredField in expectedColumns ) {
    if ( ! requiredField %in% colnames(fluxData) ) {
      stop(paste0("ERROR - expected column '", requiredField,"' was missing from worksheet '", workSheet, "'"), call. = FALSE  )
    }
  }

  df = fluxData[, expectedColumns ]
  
  # validate data - looking for NA's 
  for ( row in 1:nrow(df)) {
    
    if ( ! is.null(experimentNameFilter)) {
      experimentName = as.character(df[row,"Experiment name"])
      if ( is.na(experimentName) )  { experimentName = "" }
      if ( experimentName != experimentNameFilter) { next }  
    }
    
    for ( requiredField in expectedColumns ) {
       if ( is.na( df[[requiredField]][row] ) ) { 
         cat("\t!!! Error : row", row, ", field",requiredField, "is NULL - this row will be skipped\n")
       }
    }
  }
  
  #  now filter out any rows which have at least one NA
  if ( omitNAs == TRUE ) {
     df = na.omit(df)
  }
  
  return (df)
}

# --------------------------------------------------
load_pressure_data = function( experiment ) {
  # Read in all csv files from the pressure csv's folder 
  # Then constrain down to the time range for the experiment
  
  # Why not use the xls version and load it with load_experiments_spreadsheet() ? Because the xls file saved
  # by the Omega dataviewer is lacking and doesn't load - I tried many ways of loading it and don't want to
  # have to go through a manual step each time. I was able to load the csv however.
  
  skipRows = 11
  # Omega software bug - when enough data has been collected to trigger a rotating of csv files, the column name 'ID.' is 'No' in the <name>-01.csv file
  # so don't bother requiring it for now to keep it simple.
  expectedColumns = c( "No.", "Data", "Time", "CH 1", "Type", "Unit", "CH 2", "Type", "Unit" )
  
  # load all csvs found in the pressure folder - all csvs are going to be in here for now
  csvFiles = list()
  pressureDataFolder=paste0("/Users/dom/flux/pressure/",experimentNameFilter) # location of pressure csvs
  csvFiles = append( csvFiles, list.files( pressureDataFolder, pattern = "*.csv$", recursive = T, full.names = T ) )
  
  # remove barometerplus.csv if its there
  csvFiles <- csvFiles[csvFiles != paste0(pressureDataFolder,"/barometerplus.csv")]
  
  allData = data.frame()  
  filecount = 0
  cat("\tLoading csv files: ")
  
  for ( filename in csvFiles ) { cat("process : " , filename, "'\n") }

  for ( filename in csvFiles ) {
    cat("\tLoading csv file: '", filename, "'\n")
    if ( file.info( filename)$size == 0 ) { 
      cat("\n!!! ERROR : File ", filename," is zero bytes - remove or update it - quitting\n")
      stop_quietly()
    }
    
    # load the data in. Skip=N means skip N rows which contains super-headers
    # in the case of the Omega csv format, there are many rows, some blank, some blank with a tab, of general info 
    pressureData = read_csv( filename, skip = skipRows, trim_ws = T) 
    
    # check expected columns are present in the read in worksheet
    for ( requiredField in expectedColumns ) {
      if ( ! requiredField %in% colnames(pressureData) ) {
        stop(paste0("\nERROR - expected column '", requiredField,"' was missing"), call. = FALSE  )
      }
    }
    
    # handle dumb case of inconsistent column names in Omega's rotated csvs
    if ( "ID" %in% colnames(pressureData))  {
      pressureData = rename( pressureData, c("ID."="ID"))
    }
    
    allData = rbind(allData, pressureData )  
    filecount = filecount+1
  }
  cat("\n\tProcessed", filecount, "files\n")
  
  # remove the spurious empty column from sloppy Omega data gen (which was auto added by read_csv)
  allData = select( allData, -X11)
  
  # remove any rows with 'Time out' - these seem to come in pairs ie in both CH 1 and CH 2 positions but don't assume.
  allData = allData[ allData$`CH 1` != 'Time out', ]
  allData = allData[ allData$`CH 2` != 'Time out', ]
  
  # convert CH 1 and CH 2 readings to numerics
  allData$`CH 1` = as.numeric( allData$`CH 1` )
  allData$`CH 2` = as.numeric( allData$`CH 2` )
  
  nrowBefore = nrow(allData)
  # Remove any rows with any NA in it
  allData = na.omit(allData)
  nrowAfter = nrow(allData)
  
  cat ("Removed ", nrowBefore-nrowAfter , " NA rows from the pressure data\n")
  
  # for testing if something funky is causing an error in a row/rows for epoch time gen
  # for ( row in 1:nrow(allData) ) { 
  #   dateTime = paste0( allData$Data[row], " ", allData$Time[row] )
  #   #cat (row, " (No. = ", allData$`No.`[row],") : ", dateTime, "\n")
  #   as.POSIXct(dateTime )
  # }
  
  # create a new EPOCH_TIME column from the incorrectly spelled 'Data' column and Time columns
  # This may be used for joining to methane data later
  allData$EPOCH_TIME = as.numeric( as.POSIXct( paste0( allData$Data, " ", allData$Time)  )  )
    
  # create a differential column too
  # !!! Note - that this assumes input 1 (P1) is outside building and maps to column 'CH 1'
  #                          and input 2 (P2) is inside building and maps to column 'CH 2'
  allData$outside_minus_inside = allData$`CH 1` - allData$`CH 2`
  
  # sort it in time order
  sortedSelectedData = allData[ order(allData$EPOCH_TIME),]
  
  # constrain down to the time range from the experiment...
  startTime = as.numeric(experiment$`Start time (epoch seconds)`)
  if ( ! is.null(startTimeAdjustment)) { 
    cat("\t!! Adjusting start time by ", startTimeAdjustment, "seconds\n")
    startTime = startTime + startTimeAdjustment
  }
  
  # (Use limitDataToFirstNSeconds to overide the end time ie make it larger)
  # TODO check this works here for pressure data too
  endTime = as.numeric(experiment$`End time (epoch seconds)`) 
  
  cat ("\tFiltering pressure data to experiment's start/end time range  ", startTime, " to ", endTime, "\n" )
  filteredData = sortedSelectedData[ which( sortedSelectedData$EPOCH_TIME >= startTime & sortedSelectedData$EPOCH_TIME <= endTime ), ]
  
  rowCount = nrow(filteredData)
  if ( rowCount == 0 ) { 
    cat ("\t!!!", rowCount, "rows of data to process - quitting\n")
    stop_quietly()
  }
  else {
    cat ("\t",rowCount,"rows of data to process\n")
  }
  
  # load any barometerplus data in too
  filename = paste0(pressureDataFolder,"/barometerplus.csv")
  if ( file.exists( filename ) ) {
    
    # load the data
    pressureData = read_csv( filename, skip = 0, trim_ws = T) 
    
    # create an EPOCH_TIME column to merge with later
    pressureData$EPOCH_TIME = as.numeric( as.POSIXct( strptime( pressureData$TIME, '%b %e, %Y at %I:%M %p') )  )
    
    # create a differential column
    bplusBaseline = pressureData$`RESULT (hPa)`[1]
    pressureData$bplusDiff = (pressureData$`RESULT (hPa)` - pressureData$`RESULT (hPa)`[1])/100
    
    # merge the data
    filteredData = merge(filteredData, pressureData, by = "EPOCH_TIME", all=T )
    
  }
  
  return (filteredData)
}

# --------------------------------------------------
fitted_change_rate = function( methaneData, experimentName, fitWithTimeLimit) {
  
  # CH4 over time (ie EPOCH_TIME) can be modeled with the following exponential equation:
  #   
  #    CH4 = CH4max(1-e^(kt)) 
  #        = CH4max - CH4max * e^(kt)
  #
  # To use linear modeling to fit this exponential curve, take log of both sides :
  # 
  #    First re-arrange 
  #         CH4 = CH4max - CH4max*e^(kt) 
  #         CH4max - CH4 = CH4max*e^(kt)
  #
  #    Take logs of both sides
  #  
  #          log ( CH4max - CH4 ) = log( CH4max ) + log(e^(kt)) 
  #          log ( CH4max - CH4 ) = log( CH4max ) + kt  (where k is a constant that the modelling will yield)
  #
  #    For linear modeling: log( CH4max - CH4 ) ~ t 
  #    
  # This function's first derivative at t=0 gives the rate of change of CH4 at t=0 which will be used
  # to calculate the flux. The derivative is : -CH4max * k * e^(kt). At t=0, this is -CH4max * k * e^0 = -CH4max * k
  #    
  # That's ideal, but in the measured data, need to allow for changes in x and y and a better function to fit with is
  #
  #     CH4 = theta - alpha.e^(k*t) (with derivative at t=0 of -alpha*k)
  # 
  # So the overall approach is 
  #   1. use a linear model to get a good first estimate to k 
  #   2. then use k as part of fitting the better function with non linear least squares 
  
  # make an interim plot of all data in time range for debugging if necessary
  # Note 1/6/2023 updated to draw a plot for final paper
  if ( ! is.null( debugPlot )) {
    par(mar = c(5.1, 4.5, 4.1, 2.1 ) ) # bottom, left, top, and right
    plot( methaneData$EPOCH_TIME, 
          methaneData$CH4, 
          type='l',  
          xlab="Time (sec)",
          ylab=expression(Air ~CH[4] ~ concentration ~ (ppmv)),
          ylim=c(2.0,4) , xlim = c(0,8500), 
          bty = "l", cex.lab = 1.2
    )
    rect(0,   2, 1400,4, col= rgb(0,0,1,alpha=0.2), border=NA)
    rect(1401,2, 3615,4, col= rgb(0,1,0,alpha=0.2), border=NA)
    rect(3616,2, 8499,4, col= rgb(1,1,0,alpha=0.2), border=NA)
    lines( methaneData$EPOCH_TIME, methaneData$CH4, type='l') # draw over to get line on top of boxes
    
    mtext("All data debug plot")
   
    abline(h=seq(0,120,10),lty = "dashed", col = "gray" )
  }
  
  # truncate for now while experimenting with dev
  if ( ! is.null(limitDataToFirstNSeconds)) {
    cat ("\t!! Fitting will be done with just first ", limitDataToFirstNSeconds, " seconds of data\n")
    selectedData = methaneData[ which( methaneData$EPOCH_TIME <= limitDataToFirstNSeconds ), ] 
    methaneData = selectedData
  }
  else if ( ! is.null(fitWithTimeLimit)) {
    cat ("\t!! Fitting will be done with just first ", fitWithTimeLimit, " seconds of data\n")
    selectedData = methaneData[ which( methaneData$EPOCH_TIME <= fitWithTimeLimit ), ] 
    methaneData = selectedData 
  }
  else {
    cat ("\tINFO Fitting with all data (", methaneData$EPOCH_TIME[nrow(methaneData)], "seconds)")
  }
  
  cat("\tTime range covered by data: ", tail(methaneData,1)$EPOCH_TIME, "seconds\n")
  
  # make an interim plot of limited data in time range for debugging if necessary
  if ( ! is.null( debugPlot )) {
    
    plot( selectedData$EPOCH_TIME,
          selectedData$CH4,
          type='l',
          xlab="Time (seconds)",
          ylab="CH4 concentration (ppmv)"
    )
    mtext(paste0("Limited selection debug plot for experiment ", experimentName, "\n") )
    
    opar=par()
    par(mar = c(6, 4.5, 4.2, 2.1) ) # bottom, left, top, and right
    # Updated Feb 2023 to put data/times on x-axis
    theData = methaneData;
    theData$dateTime = as.POSIXct(theData$original_EPOCH_TIME, origin = "1970-01-01")
    debugYMax = max(theData$CH4) + 0.5
    plot( x = theData$dateTime,
          y = theData$CH4,
          type = 'l',
          las = 2 , #rotate x-axes labels
          ylim=c(0,debugYMax),
          ylab=expression(CH[4] ~ concentration~ (ppmv)),
          xaxt='n', xlab="")
    byStep = 30
    abline( v=seq(theData$original_EPOCH_TIME[1], theData$original_EPOCH_TIME[ nrow(theData)], byStep),lty = "dashed", col = "gray")
    abline( h=seq(0,debugYMax,0.5), lty = "dashed", col = "gray")
    axis.POSIXct(side = 1,
                 at=seq(min(theData$dateTime), max(theData$dateTime), by=byStep), # by=30 seconds
                 format="%b %d %H:%M:%S",
                 las=2,
                 cex.lab = .75, cex.axis=0.75)
    
    mtext(paste0("Limited selection debug plot for experiment ", experimentName, "\n") )
    par(opar)

  }

  # first do a linear fitting of CH4 = CH4max - CH4max*e^(kt) 
  # Take log of both sides and rearrange : log( CH4max - CH4 ) ~ t   i.e. is log( ch4max-ch4) predicted by time linearly
  cat ("\tInitial linear fitting ...\n")
  ch4max = max( methaneData$CH4 ) * 1.1 # initial estimate 
  model.linear = lm ( log( ch4max - methaneData$CH4 ) ~ methaneData$EPOCH_TIME, data = methaneData )
  alpha.0 = -exp(coef(model.linear)[1]) # intercept
  beta.0  = coef(model.linear)[2] # k
  
  # Add some weighting so that the fit goes through the start point
  wts = rep(1, nrow(methaneData) )
  wts[1] = originWeight  

  # Here's the exp. fit to the ideal exp. rise equation - it's not as good as the refined one later
  # basicExpModel = nls( methaneData$CH4 ~  ch4max*( 1 - exp(k*methaneData$EPOCH_TIME) + theta ),  
  #                 data = methaneData, 
  #                 start = list( theta=ch4max, k=beta.0 ),
  #                 weights = wts, 
  #                 control=nls.control(maxiter = nlsIterations)  # default is 50 - I override - 100 has been good
  # )
  # predictedCH4 = predict(basicExpModel, list(x = methaneData$EPOCH_TIME))
  # lines( x = methaneData$EPOCH_TIME, y = predictedCH4,col = 'red', lwd = 3)
  # residualsSum = sum(residuals(basicExpModel)) 
  
  cat ("\tNon-linear fitting ...\n")
  start = list(alpha = alpha.0, beta = beta.0, theta = ch4max)
  
  model.exponential = nls( methaneData$CH4 ~ alpha * exp(beta * methaneData$EPOCH_TIME) + theta , 
               data = methaneData, 
               start = start,
               weights = wts, 
               control=nls.control(maxiter = nlsIterations)  # default is 50 - I override - 100 has been good
             )
  residualsSum = sum(residuals(model.exponential))
  
  # plot the data that was used for modeling in black
  if ( is.null(dontPlotModel )) { 
    
     lines( x = methaneData$EPOCH_TIME,
         y = methaneData$CH4,
         type = 'l'
     )
  }  
  
  alpha = coefficients(model.exponential)[["alpha"]]
  beta  = coefficients(model.exponential)[["beta"]]
  theta = coefficients(model.exponential)[["theta"]] # this is the modeled steady state - an area of further exploration 
  
  cat("\t\t alpha, beta, theta =", -alpha,",", beta, ",", theta, "(",paste0 ("y=",theta, alpha, "exp(",sprintf("%.8f",beta),"x)"), ")" )
  
  # the slope at t=0 is alpha*beta 
  
  predictedCH4 = predict(model.exponential, list(x = methaneData$EPOCH_TIME))
  
  if ( is.null(dontPlotModel )) { 
     lines( x = methaneData$EPOCH_TIME, 
         y = predictedCH4, 
         col = 'blue', 
         lwd = 3)
  }
  
  cat ("\n\tCalculating gradient of first derivative of fitted exponential rise model : \n")
  
  y0 = predictedCH4[1]
  CH4ppmPerSecond = alpha*beta
  
  cat ("\t\tGradient, ie rate of change, is", CH4ppmPerSecond , " CH4 ppmv / second")
  
  # add line for calculated tangent at t0
  if ( is.null(dontPlotModel )) { 
     abline( y0, CH4ppmPerSecond, col='blue', lty=2)  
  }
  
  # add the fitted steady state asymptote line too
  abline( h=theta, lty='dashed', col="orange") 
  
  # add test value of the fitted steady state asymptote line too
  text( tail(methaneData,1)$EPOCH_TIME,theta,paste0(round(theta,2)), col="orange", pos=1)
  
  # add legend
  if ( is.null(plotCO2) ) {
    
    # todo/old
    # Optionally add CO2 measurements on a second axis
  } 
  
  else {
   
    par(new = TRUE) 
    plot(x = methaneData$EPOCH_TIME,
         y = methaneData$CO2,
         type='l',
         col = "orange",              
         axes = FALSE, 
         xlab = "", 
         ylab = "")
    
    axis(side = 4, at = pretty(range(methaneData$CO2))) 
    mtext("CO2 ppmv", side = 4, line = 3)
    
  }
  
  # return the rate of change of CH4 per second as modeled,  the modeled steady state, and this steady state divided by fitted exponent
  return ( list("CH4ppmPerSecond"=CH4ppmPerSecond, "steadyState"=theta, "k"=beta )  ) 
  
}

# --------------------------------------------------
calculate_flux_using_simple_slope = function( fromTime, toTime, methaneData, adjustedVolume ) {

  # filter down data to time bounded range EPOCH_TIME : [ fromTime : toTime ] 
  selectedData = methaneData[ which( methaneData$EPOCH_TIME >= fromTime  & methaneData$EPOCH_TIME <= toTime ), ]
  
  # calculate the PPM difference between time zero and simpledMethod_firstNSeconds ie x2 - x1
  ppmdiff = tail( selectedData,1 )$CH4 - head( selectedData,1 )$CH4 
  
  # Calculate the time difference in seconds, eg it might be 599.112, not quite 600
  # Also since EPOCH_TIME was adjusted to start at 0, this time difference is just the last EPOCH_TIME in the filtered selected data
  # But I'm leaving it spelled out so it looks more like x2 - x1, and in case in future the time shift is removed.
  simple_timeDiff = tail(selectedData,1)$EPOCH_TIME - selectedData$EPOCH_TIME[1] 
  
  # calculate the rate of change per second of CH4 over the time range
  simple_CH4ppmChangePerSecond = ( tail(selectedData,1)$CH4 - selectedData$CH4[1]) / simple_timeDiff 
  simple_CH4ppmPerHour = simple_CH4ppmChangePerSecond * 3600  # CH4 ppm per hour
  simple_CH4partsPerHour = simple_CH4ppmPerHour / 1000000 # CH4 parts per hour
  simple_CH4partsPerDay = simple_CH4partsPerHour * 24 # CH4 parts per day
  simple_CH4cubicFeetPerDay = simple_CH4partsPerDay * adjustedVolume

  return( list("flux"=simple_CH4cubicFeetPerDay, "rate"=simple_CH4ppmChangePerSecond, "data"=selectedData ) )
}

# --------------------------------------------------
calculate_flux = function( experiment, methaneData, paletteIndex, experimentName, fitWithTimeLimit ) {
  
  # calculates RCM flux for the experiment
  
  adjustedVolume = as.numeric(experiment$`Adjusted volume (Vadj)`)
  
  if ( fluxCalculationMethod == 'fitted') {
    
    # First make a copy of the data unshifted for pressure related stuff later
    originalMethaneData = methaneData
    
    # shift epoch time to its on the scale 0:...
    methaneData$original_EPOCH_TIME = methaneData$EPOCH_TIME
    methaneData$EPOCH_TIME = methaneData$EPOCH_TIME - head( methaneData, 1)$EPOCH_TIME
    
    # for fiddling

    #yMin = 2.0; yMax = 4.5
    #xMin = 0; xMax = 15000
    #yMin = 2.15; yMax = 2.5; xMin = 0; xMax = 800
    
    yMin = 2; yMax = 3.2; xMin = 0; xMax = 800
    ylim=c(2.0,4); xlim = c(0,800)
    entireDatasetColor = "gray"
    
    opar=par()
    par(mar = c(5.1, 5, 4.2, 2.1) ) # bottom, left, top, and right
    
    # create a plot that shows the entire experiment dataset
    plot( x = methaneData$EPOCH_TIME, 
          y = methaneData$CH4, 
          type = 'l',  
          xlab = "Time (sec)",
          cex.lab=1.2, # axes titles size
          cex.axis=1, # y-axis labels size
          cex.names=1, # x-axis labels size
          ylab = expression(Air ~CH[4] ~ concentration ~ (ppmv)),
          col = entireDatasetColor
          #,ylim=c(yMin, yMax)# , xlim=c(xMin, xMax)  # for fiddling
    )
    
    # add grid lines
    if ( !  is.null(figure7)  ) {
       abline(h = seq(0,max(methaneData$CH4), GridSpacingHorizontal), lty = "dashed", col = "gray") # horizontal grid
       abline(v = seq(0,tail(methaneData,1)$EPOCH_TIME, GridSpacingVertical ), lty = "dashed", col = "gray") # vertical grid
    }

    if ( isTRUE(dontModel )) { 
       stop_quietly()
    }
    
    # fit data ie fitted tangent calculation
    fittedValues = fitted_change_rate( methaneData, experimentName, fitWithTimeLimit ) 
    CH4ppmChangePerSecond = fittedValues$CH4ppmPerSecond # CH4 ppm per second from fitting
    CH4ppmPerHour = CH4ppmChangePerSecond * 3600  # CH4 ppm per hour
    CH4partsPerHour = CH4ppmPerHour / 1000000 # CH4 parts per hour
    CH4partsPerDay = CH4partsPerHour * 24 # CH4 parts per day
    CH4cubicFeetPerDay = CH4partsPerDay * adjustedVolume
    
    # Calculation using simple method
    
    # set up the time bounds to use in filtering down the data to use in the simple calculation
    fromTime = 0
    toTime = simpledMethod_firstNSeconds
    
    cat ("\n\tCalculating simple flux\n")
    calculated_flux = calculate_flux_using_simple_slope ( fromTime, toTime, methaneData, adjustedVolume )
    simple_CH4cubicFeetPerDay = calculated_flux$flux
    simple_CH4ppmChangePerSecond = calculated_flux$rate
    cat ("\t\tCalculating simple rate of change:",simple_CH4ppmChangePerSecond, " CH4 ppmv / second\n")
    selectedData = calculated_flux$data
    
    # add line for calculated tangent using simple method
    # TODO figure out why when using plotCO2 = TRUE, this line doesn't plot - prob'y to do with par() creating a new plot :|
    simple_color = "chartreuse3"
    simple_linewidth = 2
    if ( ! is.null(dontPlotModel) ) { 
      if ( ! paperPlots ) {
         simple_linewidth = 3
         # plot two green points
         points( 0, head(methaneData,1)$CH4, bg=simple_color, col=simple_color, pch=21, cex=2.5)
         points( tail(selectedData,1)$EPOCH_TIME, tail(selectedData,1)$CH4 , bg=simple_color, col=simple_color, pch=21, cex=2.5)
      }
    }
    
    # plot simple fitted line too
    if ( plotSimpleFit ) {
       abline( head(methaneData,1)$CH4 , simple_CH4ppmChangePerSecond, col=simple_color, lty="dashed", lwd=simple_linewidth)
    }
    
    if ( paperPlots ) { # this should really be in analysis
      varyTimeSpan = 1
      varyVolume = 1
      
      # show the effects of the time span used on calculations
      if ( varyTimeSpan ) {
        
        # first, simple slope ...
        timeSpansInMinutes = seq(5,60,5) #c( 5, 10, 15, 20, 25, 30 )
        cat ("\n\nTimespan (min),", paste0(timeSpansInMinutes, collapse=","), "\n", "Simple slope flux,")
        
        for ( timeSpan in timeSpansInMinutes) {
          timeSpanSeconds = timeSpan * 60
          calculated_flux = calculate_flux_using_simple_slope ( 0, timeSpanSeconds, methaneData, adjustedVolume )
          paper_simple_CH4cubicFeetPerDay = calculated_flux$flux
          paper_simple_CH4ppmChangePerSecond = calculated_flux$rate
          cat (paper_simple_CH4cubicFeetPerDay,",")
          x = 6375
          y = ( paper_simple_CH4ppmChangePerSecond * x ) + head(methaneData,1)$CH4 - 0.05
          abline( head(methaneData,1)$CH4 , paper_simple_CH4ppmChangePerSecond, col="orange", lty="dashed", lwd=1)
          text( x, y, timeSpanSeconds, xpd=NA, cex=.6)
        }
        cat ("\n")
        mtext("Effects of varying time span on simple slope calculation method")
        lines(x = methaneData$EPOCH_TIME, y = methaneData$CH4 ) # do this to make the line black again
        
        # and now fitted tangent ...
        timeSpansInSeconds = seq(1000,13000,1000) 
        calculatedFittedFluxes = c()
        for ( timeSpan in timeSpansInSeconds) {
          fitWithTimeLimit = timeSpan
          # fit data ie fitted tangent calculation
          fittedValues = fitted_change_rate( methaneData, experimentName, fitWithTimeLimit ) 
          CH4ppmChangePerSecond = fittedValues$CH4ppmPerSecond # CH4 ppm per second from fitting
          CH4ppmPerHour = CH4ppmChangePerSecond * 3600  # CH4 ppm per hour
          CH4partsPerHour = CH4ppmPerHour / 1000000 # CH4 parts per hour
          CH4partsPerDay = CH4partsPerHour * 24 # CH4 parts per day
          CH4cubicFeetPerDay = CH4partsPerDay * adjustedVolume
          calculatedFittedFluxes = append(calculatedFittedFluxes,CH4cubicFeetPerDay )
          x = 6000
          y = (paper_simple_CH4ppmChangePerSecond * 6000) + head(methaneData,1)$CH4
          cat("\nx,y=", x, ",", y , "[",paper_simple_CH4ppmChangePerSecond,"\n")
        }
        cat ("\n\nTimespan (sec),", paste0(timeSpansInSeconds, collapse=","), "\n")
        cat("Fitted tangent flux,", paste0(calculatedFittedFluxes, collapse=",") )
      }
      
      # show the effects of error in adjusted volume calculating the simple slope  
      if ( varyVolume ) {
        adjustedVolumeErrors = seq( 0.5, 1.5, 0.1)  # 50% to 150% error in steps of 10%
        cat ("\n\nError, simple slope flux :", adjustedVolumeErrors, "\n")
        for ( errorAmount in adjustedVolumeErrors) {
          adjustedVolumeWithError =  adjustedVolume * errorAmount
          calculated_flux = calculate_flux_using_simple_slope ( 0, 600, methaneData, adjustedVolumeWithError )
          paper_simple_CH4cubicFeetPerDay = calculated_flux$flux
          paper_simple_CH4ppmChangePerSecond = calculated_flux$rate
          cat (errorAmount, ",",paper_simple_CH4cubicFeetPerDay,",",paper_simple_CH4ppmChangePerSecond,"\n")
          abline( head(methaneData,1)$CH4 , paper_simple_CH4ppmChangePerSecond, col="orange", lty="dashed", lwd=1)
        }
        cat ("\n")
      }
      
    } 
    
    # for simple slope example plot 
    if ( ! is.null(figure7)  ) {
       abline( v=seq(0,800,200), lty = "dashed", col = "gray")
       abline( h=seq(2.15,2.5,0.1),lty = "dashed", col = "gray") 
       legend( x="bottomright",
          legend = c( expression(Logged ~ CH[4] ~ concentration ~ data), "Calculated slope"),
            col  =   c("black", simple_color), lty  =   c("solid","dashed"),
            lwd = c( 1, 2),
            cex  = 1, 
            bg = "white"
      )
    }
    
    if ( ! is.null(figure8) ) {
      abline( v=seq(0,15000,5000), lty = "dashed", col = "gray")
      abline( h=seq(2.5,5,0.5),lty = "dashed", col = "gray") 
    }
    
    if ( processPressureData ) {
       # shift the pressure data so it'll be aligned in time with the methane data
       startTimeDiff = pressureData$EPOCH_TIME[1] - originalMethaneData$EPOCH_TIME[1]
       pressureData$EPOCH_TIME = pressureData$EPOCH_TIME - pressureData$EPOCH_TIME[1] + startTimeDiff
       pressureData$outside_minus_inside = rescale(pressureData$outside_minus_inside, to = c(4.3,4.35) )
       
       # to plot this on the same graph, the data needs rescaling
       lines( x=pressureData$EPOCH_TIME, 
             y=pressureData$outside_minus_inside,
             type='l', 
             col="orange", 
             xlab="", ylab=""
       )
    }
    
    # add a plot legend
    if ( is.null(plotCO2)  ) {
      if (  plotLegend == 1) {
        if ( plotSimpleFit == 1 ) { 
           legendPosition="bottomright"
           if ( experiment$`Experiment name` == 'R10') { legendPosition="topleft" }
           legend( x=legendPosition, #x = "topleft", #x = "bottomright",
              y = (tail(methaneData,1)$CH4 + head(methaneData,1)$CH4 ) / 2,
              legend = c("All measured data", "Selected modeling range", "Fitted curve", "Fitted curve tangent at t=0", "Simple slope at t0", paste("Fitted steady state",round(fittedValues$steadyState,2), "ppmv")  ),
              col  =   c("gray",              "black",                "blue",         "blue",                       simple_color,                     "orange"), 
              lty  =   c("solid",             "solid",                "solid",        "dashed",                     "dashed",                         "dashed"), 
              cex  = 1, 
              bg = "white"
           )
        }
        else { 
          legend( x="bottomright",
                  y = (tail(methaneData,1)$CH4 + head(methaneData,1)$CH4 ) / 2,
                  legend = c("All measured data", "Selected modeling range", "Fitted curve", "Fitted curve tangent at t=0"  ),
                  col  =   c("gray",              "black",                "blue",         "blue"                        ), 
                  lty  =   c("solid",             "solid",                "solid",        "dashed"                     ), 
                  lwd = c(2,2,1,1), # black shows up as gray unless set lwd to 2 here
                  cex  = 0.8, 
                  bg = "white"
          )
        }
      }
    }
    else {
      legend( x = "bottomright", # need to use bottomright else the legend doesn't show - poss to do with earlier call to par(new=TRUE)
              y = (tail(methaneData,1)$CH4 + head(methaneData,1)$CH4 ) / 2,
              legend = c("All measured data", "Selected modeling range", "Fitted curve", "Fitted curve tangent t=0", "CO2"),
              col  =   c("gray",              "black",          "blue",               "green",             "orange"), 
              lty=1, 
              cex=0.8
      )
    }
    
    cat ( "\n\tSimple flux =", round(simple_CH4cubicFeetPerDay,4), " (cu ft CH4 / day)",
          "\n\tFitted flux =", round(CH4cubicFeetPerDay,4), " (cu ft CH4 / day)",
          "\n\tSteady state =", round(fittedValues$steadyState,4), " (cu ft CH4 / day)",
          "\n\tSteady state divided by fitted exponent (k) =", fittedValues$steadyState / fittedValues$k, 
          "\n\t\t[", round(simple_CH4cubicFeetPerDay,4), "," , round(CH4cubicFeetPerDay,4), ",", round(fittedValues$steadyState,4), "]",
         "\n")
    
    
    return ( list(
                   "fittedFlux" = CH4cubicFeetPerDay,
                   "simpleFlux" = simple_CH4cubicFeetPerDay,
                   "steadyState" = fittedValues$steadyState,
                   "steadyOverK" = fittedValues$steadyState / fittedValues$k )
    )
    
  }
  else { 
    cat ("Unrecognized flux calc method - only one method supported : 'fitted' - quitting\n")  
    stop_quietly()
  }
}

# --------------------------------------------------
process_methane_and_pressure_data = function( methaneTimeSeriesData, experiment ) {
  
  # load the pressure data - this will also constrain it to the time range from the experiment  
  pressureData = load_pressure_data( experiment ) 
  
  # first do some sanity checks ... alert if there's no overlap 
  if ( pressureData$EPOCH_TIME[1] > methaneTimeSeriesData$EPOCH_TIME[nrow(methaneTimeSeriesData)]  ) {
    cat ("ERROR: pressure data is all after methane data in time\n")
    stop_quietly()
  }
  if ( pressureData$EPOCH_TIME[ nrow(pressureData)] < methaneTimeSeriesData$EPOCH_TIME[1] ) {
    cat ("ERROR: pressure data is all before methane data in time\n")
    stop_quietly()
  }
  
  # want to make a composite plot with CH4 on top, pressure below (based in pressure1 experiment)
  # rescale CH4 and pressure
  
  upperLimit = round(max(methaneTimeSeriesData$CH4)+.5)
  middleLimit = upperLimit/2
  lowerRange = c(0,middleLimit)
  upperRange = c(middleLimit, upperLimit )
  ch4range = c( middleLimit * 1.05, upperLimit ) # add a bit of padding
  

  methaneTimeSeriesData$CH4rescaled = rescale(methaneTimeSeriesData$CH4, to = ch4range )
  methaneTimeSeriesData$dateTime = as.POSIXct(methaneTimeSeriesData$EPOCH_TIME, origin = "1970-01-01")
  
  pressureData$CH1Rescaled  = rescale(pressureData$`CH 1`, to=lowerRange )
  pressureData$CH2Rescaled  = rescale(pressureData$`CH 2`, to=lowerRange )
  pressureData$diffRescaled = rescale(pressureData$outside_minus_inside, to=lowerRange )
  
  # smooth the pressure data so its easier to view and compare
  smoothedCH1scaled = supsmu( x = pressureData$EPOCH_TIME, y = pressureData$CH1Rescaled )
  smoothedCH2scaled = supsmu( x = pressureData$EPOCH_TIME, y = pressureData$CH2Rescaled )
  smoothedDiffscaled = supsmu( x = pressureData$EPOCH_TIME, y = pressureData$diffRescaled )
  
  smoothedCH1 = supsmu( x= pressureData$EPOCH_TIME, y=pressureData$`CH 1`)

  # create a simple plot of the CH4 data
  par(mar = c(7.5, 4.1, 3.5, 3.5) ) # bottom, left, top, and right
  
  plot( 
        x = methaneTimeSeriesData$dateTime,
        y = methaneTimeSeriesData$CH4rescaled,
        type = 'l',
        las = 2 , #rotate x-axes labels
        ylim=c(0,upperLimit),
        yaxt='n',ylab="",
        xaxt='n', xlab="")
  
  # add smoothed pressure data
  lines( smoothedCH1scaled, type = 'l', col="red")
  lines( smoothedCH2scaled, type = 'l', col="blue")
  lines( smoothedDiffscaled, type = 'l', col="green")
  
  # draw the center divider
  abline(h=middleLimit)
  
  # add some hour dividers
  abline( v=seq(methaneTimeSeriesData$EPOCH_TIME[1], methaneTimeSeriesData$EPOCH_TIME[ nrow(methaneTimeSeriesData)], 3600),
          lty = "dashed", col = "gray")
  
  axis.POSIXct(side = 1, 
               at=seq(min(methaneTimeSeriesData$dateTime), max(methaneTimeSeriesData$dateTime), by="hour"), 
               format="%b %d %H:%M:%S", 
               las=2,
               cex.lab = 0.9, 
               cex.axis= 0.9)
  
  # alternative better plot...
  methaneTimeSeriesData$EPOCH_TIME = round( methaneTimeSeriesData$EPOCH_TIME )
  mergedData = merge(methaneTimeSeriesData, pressureData, by = "EPOCH_TIME", all=T )
  
  smoothedCH1 = supsmu( x = mergedData$EPOCH_TIME, y = mergedData$`CH 1`)
  df = data.frame( smoothedCH1$x, smoothedCH1$y)
  names(df) = c("EPOCH_TIME", "smoothedCH1")
  mergedData = merge(mergedData, df, by = "EPOCH_TIME", all=T )
  
  smoothedCH2 = supsmu( x = mergedData$EPOCH_TIME, y = mergedData$`CH 2`)
  df = data.frame( smoothedCH2$x, smoothedCH2$y)
  names(df) = c("EPOCH_TIME", "smoothedCH2")
  mergedData = merge(mergedData, df, by = "EPOCH_TIME", all=T )
  
  smoothedDiff = supsmu( x = mergedData$EPOCH_TIME, y = mergedData$outside_minus_inside)
  df = data.frame( smoothedDiff$x, smoothedDiff$y)
  names(df) = c("EPOCH_TIME", "smoothedDiff")
  mergedData = merge(mergedData, df, by = "EPOCH_TIME", all=T )
  
  # remove NA rows that have been added from merging
  nrowBefore = nrow(mergedData)
  mergedData = na.omit(mergedData)
  nrowAfter = nrow(mergedData)
  cat ("Removed ", nrowBefore-nrowAfter , " NA rows from the merged data\n")
  
  par( mar=c(7, 4.4, 4.4, 6) ) #bottom, left, top, and right

  # plot the CH4 on one axis
  plot( x= mergedData$dateTime,
        y= mergedData$CH4, 
        type='l', 
        las=2, 
        col="black",
        xlab="", xaxt='n', 
        ylab=expression(Air~ CH[4]~concentration~(ppmv) ),
        yaxt='n',
        lwd=2.0)

  axis.POSIXct(side = 1, 
               at=seq(min(mergedData$dateTime), max(mergedData$dateTime), by=14400), 
               format="%b %d %H:%M:%S", 
               las=2,
               cex.lab = 0.9, 
               cex.axis = 0.9)
  
  # add the y-axes labels
  axis(2) # this adds the left y-axis with automatically populated values from the data
  
  # plot the raw and smoothed CH 1 pressure
  par(new=T)
  
  ylimits = c( 
    min( min( mergedData$`CH 2`, na.rm=TRUE), min( mergedData$`CH 1`, na.rm = T)),
    max( max( mergedData$`CH 2`, na.rm=TRUE), max( mergedData$`CH 1`, na.rm = T))
    )
  
  if ( experiment$`Experiment name` == 'pressure5') { 
    ylimits = c(-0.05, 0.2)
  }
  
  if ( experiment$`Experiment name` == 'mantest4') { 
    ylimits = c(-0.03, 0.2)
  }
  
  # transparency alpha (smaller => fainter)
  rawPressureDataAlpha = 0.075
  # outside : red
  plot( mergedData$dateTime, mergedData$`CH 1`,      
        col=rgb(red=0.5,green=0,blue=0,alpha=rawPressureDataAlpha), 
        type='l', 
        axes=F, 
        xlab="", 
        ylab="",
        ylim=ylimits )
  
  lines( mergedData$dateTime, mergedData$smoothedCH1, col="red",  type='l')
  
  # inside : blue
  lines( mergedData$dateTime, mergedData$`CH 2`,      col=rgb(red=0,green=0,blue=0.5,alpha=rawPressureDataAlpha), type='l' ) #, axes=F, xlab="", ylab="")
  lines( mergedData$dateTime, mergedData$smoothedCH2, col="blue",  type='l')
  
  # outside - inside : green
  lines( mergedData$dateTime, mergedData$smoothedDiff, col="green",  type='l')
  
  axis(4, col.axis = "orange")
  mtext("Pressure differentials (millibar)", side = 4, line =3, col="orange" )
  
  # grid lines
  abline( v=seq(methaneTimeSeriesData$EPOCH_TIME[1], methaneTimeSeriesData$EPOCH_TIME[ nrow(methaneTimeSeriesData)], 14400), lty = "dashed", col = "gray")
  abline( h=seq(-.2,.2, 0.05), lty="dashed", col="orange")
  
  legend( x = 'top', #'bottom', #"topleft", #x = "bottomright",
          legend = c(expression(Basement~air~CH[4]~concentration),   "Outside pressure change (smoothed)", "Basement pressure change (smoothed)", "Outside-inside change difference (smoothed)"  ),
          col  =   c("black", "red",     "blue",   "green" ),
          lwd  =   c(2,1,1,1 ),
          cex  = 0.8, 
          bg = "white")

  mtext( paste0("Pressure test name: ", experimentNameFilter ,"\n") )
  points(mergedData$EPOCH_TIME, mergedData$bplusDiff, col="red")
 
  stop_quietly()
  
  
}

# --------------------------------------------------
process_experiments_data = function( experiments ) {
  
  # processes each experiment :
  
  # create a blank graph for adding lines to later
  if ( 0 ) { 
    plot.new( )
    plotTitle = paste0("Flux method: ",fluxCalculationMethod, ", calculation time span: first ", simpledMethod_firstNSeconds, " seconds")
    plot( 1, main = plotTitle,
          type="n", xlab="Time (seconds)", ylab="CH4 (ppmv)", 
          xlim=c(0,simpledMethod_firstNSeconds + extraSpace ), ylim=c(2,2.4),
          yaxt='n'
    )
  }
  
  # add some results columns
  experiments[ , 'calculatedFlux_fitted'] = 0
  experiments[ , 'calculatedFlux_simple'] = 0
  experiments[ , 'calculatedSteadyState'] = 0
  experiments[ , 'steadyStateOverK'] = 0
  
  for (experimentNumber in 1:nrow(experiments)) {
    experimentName = as.character(experiments[experimentNumber,"Experiment name"])
    
    if ( experimentName == "STOP") {
      stop_quietly()
    }
    
    fitWithTimeLimit = NULL
    
    if ( as.character( experiments[experimentNumber,]$`How much time to fit with (sec)`) != "all") {
      fitWithTimeLimit = as.integer(experiments[experimentNumber,]$`How much time to fit with (sec)` )
    }
      
    if ( ! is.null(experimentNameFilter)) {
      if ( experimentName != experimentNameFilter) { next }  
    }
    cat("Experiment :", experimentName, "\n")
    
    methaneTimeSeriesData = load_methane_data( experiments[experimentNumber,], times=NULL )
    cat ("INFO : ", round( nrow(methaneTimeSeriesData) / ( methaneTimeSeriesData$EPOCH_TIME[nrow(methaneTimeSeriesData)] - methaneTimeSeriesData$EPOCH_TIME[1] ), 2) , " measurements logged per second\n" )
    
    if ( processPressureData ) {
    
      process_methane_and_pressure_data( methaneTimeSeriesData, experiments[experimentNumber,] )
      
    }
    
    # calculate the flux for the experiment
    calculatedFlux = calculate_flux( experiments[experimentNumber,] , methaneTimeSeriesData, experimentNumber, experimentName, fitWithTimeLimit)
    experiments[experimentNumber,]$calculatedFlux_fitted = calculatedFlux$fittedFlux
    experiments[experimentNumber,]$calculatedFlux_simple = calculatedFlux$simpleFlux
    experiments[experimentNumber,]$calculatedSteadyState = calculatedFlux$steadyState
    experiments[experimentNumber,]$steadyStateOverK = calculatedFlux$steadyOverK

    mtext( paste0("Experiment ", experimentName, " RCM calculated CH4 emissions (cu ft/day)\n",
                  "Fitted tangent: ",  round(calculatedFlux$fittedFlux,3), ", ",
                  "Simple slope: ", round(calculatedFlux$simpleFlux,3), "\n"),
                  #"Steady state: ", round(calculatedFlux$steadyState,2),"\n" ),
      adj=0, cex=.85
    )
    
  }
  
  # write a results spreadsheet
  cat ("Writing results.xlsx\n")
  write_xlsx(experiments,"results.xlsx")

}


# --------------------------------------------------
Regressionp = function (modelobject) {
  # retrieves F statistic p-value from linear model
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)}



# --------------------------------------------------
analyze_leaks = function ( leaksData )  {
  
  # convert to numerical data
  leaksData$`Size ppm` = as.numeric(leaksData$`Size ppm` )
  leaksData$`Home age (years)` = as.numeric(leaksData$`Home age (years)` )
  
  leaksData = leaksData[order(leaksData$`Size ppm`, decreasing=T),]
  leaksData$leaksDataLogPPM = log( leaksData$`Size ppm` )
  
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  
  floors = sort(unique( leaksData$Floor ))
  colorPalette = c("yellow", "red", "green", "blue", "black")
  leaksData$colors = colorPalette[ match(leaksData$Floor, floors) ]
  
  
  opar=par()
  par( mfrow=c(1,2), mai= c(1.1, 1, 0.5, 0.3) )
  
  #Leak sizes categorized by floor 
  thePlot = barplot( 
    main="A. Leak sizes categorized by floor",adj=0, cex.main=1,
    leaksData$`Size ppm`, 
    col = leaksData$colors,
    ylim=c(0, max(leaksData$`Size ppm`)+2000),
    xlab=paste0("                           Individual leaks (n=", nrow(leaksData),")" ),
    ylab=expression("                    "~Leak~air~CH[4]~concentration~(ppmv)),
    )
  box()
  abline(h = seq(0,14000, 1000), lty = "dashed", col = "gray")
  legend( "topright",
          legend=c(floors),
          col=colorPalette,
          fill=colorPalette,
          ncol=2,
          cex=0.8,
          title="Floor") 

  # histogram to see if there might be a hint of a long tail
  theHist = hist( leaksData$`Size ppm`, 
                  xlab=expression("               "~Leaks~air~CH[4]~concentrations~(ppm)), 
                  ylab="                                      Density", 
                  main=paste0("B. Histogram of leaks sizes"),adj=0, cex.main=1,
                  col="cadetblue2",
                  ylim=c(0,95),
                  cex.axis=0.9, # y-axis labels size
                  cex.names=0.9 # x-axis labels size
  )
  box()
  text(x=theHist$mids, y=theHist$counts, labels=theHist$counts ,cex=.7,pos=3)
  cat ( "Largest leak PPMV was", max(leaksData$`Size ppm`) , "ppmv and was responsible for", 
        round( 100 * max(leaksData$`Size ppm` / sum( leaksData$`Size ppm`),0) ), "% of the total leaks ppm (",sum( leaksData$`Size ppm`) ,"ppmv) ",
        "or,",  round(max(leaksData$`Size ppm`)/sd(leaksData$`Size ppm`),1) , "standard deviations above the mean"
        )
  
  sources = sort(unique( leaksData$`Leak source` ))
  colorPalette = brewer.pal(n = length( unique( leaksData$`Leak source`) ) , name = 'Set2' )
  colorPalette = c( "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
  leaksData$colors = colorPalette[ match(leaksData$`Leak source`, sources) ]
  thePlot = barplot( 
    main="A. Individual leak sources",
    border=T,
    leaksData$`Size ppm`, 
    col = leaksData$colors,
    ylim=c(0, max(leaksData$`Size ppm`)+2000),
    xlab=paste0("Individual leaks (n=",  nrow(leaksData),")"),
    ylab=expression(Leak~air~CH[4]~concentration~(ppmv))
  )
  box()
  legend( "topright",
          legend=c(sources),
          col=colorPalette,
          fill=colorPalette,
          ncol=2,
          title="Leak source"
          ,cex=1
          ) 
 
  colorPalette = c( "#8DA0CB" , "#FC8D62", "#A6D854", "#E78AC3", "#66C2A5" ) # needed to shuffle the colors to get the to match previous plot
  # and a chart showing leak sources
  thePlot = ggplot(leaksData, aes(x=reorder(`Leak source`,`Leak source`, function(x)-length(x)))) +
    ggtitle("") +
    geom_bar( fill=colorPalette ) +
    xlab("") +
    ylab("Number of leaks") +
    theme( panel.background = element_rect(fill = "white",colour = "black", size = 1, linetype = "solid"),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # turn off grid
           legend.position = c(.1, .8), # legend topright
           legend.key = element_blank(), # make the backgrounds of the legend items not colored (gray by default)
           legend.background = element_rect( fill="white", size=0.5, linetype="solid", colour ="gray"), # legend border and fill
           plot.title = element_text(hjust = 0.5), # justify title
           plot.subtitle = element_text(hjust = 0.5), # justify subtitle
           axis.title = element_text( color="black", size=17), #, face="bold") # axes labels - note with expression(), need to use bold() inside it
           axis.text.x=element_text( size=14, color="black"),  #  x-axis labels
           axis.text.y=element_text(size=14, color="black"),
           )
  print(thePlot)
  
  opar=par()
  par( mfrow=c(1,2), mai= c(.6, 1, 0.5, 0.5) )
  
  theData = table(leaksData$`Dwelling type`, leaksData$Floor )
  # plot out the number of leaks by floor
  barplot( theData,
           main="A. Number of found leaks by floor & building type", adj=0, cex.main=1,
           xlab="",
           ylab="               Number of found leaks",
           col=c("#993404", "#FFFFCC"),
           legend.text=rownames(theData),
           ylim=c(0, 80),
           border = "black",
           axis.lty=1, # add the x-axis line
           args.legend=list(title="Building type", cex=1),
           cex.axis=1.3, # y-axis labels size
           cex.names=1.3, # x-axis labels size
           cex.lab=1.5
           )

  box() # add a border around the chart
  
  
  theData = table(leaksData$`Dwelling type`, leaksData$`Before or after meter` )
  # plot out the number of leaks by floor
  theBarPlot = barplot( theData,
           main="B. Leak locations relative to natural gas meter & building type",  adj=0,cex.main=1,
           xlab="",
           ylab="",
           col=c("#993404", "#FFFFCC"),
           legend.text=rownames(theData),
           ylim=c(0, 80),
           border = "black",
           axis.lty=1, # add the x-axis line
           args.legend=list(title="Building type", cex=1),
           cex.axis=1.3, # y-axis labels size
           cex.names=1.3 # x-axis labels size
  )
  box()
  par(opar)
  
  # any relationship between home age and number of leaks ?
  theData = aggregate(  rep(1,length(leaksData$`Home age (years)`)), by=list(leaksData$`Home age (years)`)  , sum)
  theModel = lm( `Group.1` ~ x, data=theData)
  plot( theData ,
        main="Leak count by building age",
        xlab="Building age (years)",
        ylab="Number of leaks")
  box()
  theStats = paste0("R-squared: ", round(summary(theModel)$r.squared,3),", n=",nrow(theData),", p=",signif(Regressionp(theModel), digits=3) )
  mtext( theStats  )
  
  cat (theStats,"\n")
  
  # what about sum of leakage by year ?
  leaksData$`Size ppm` = as.numeric(leaksData$`Size ppm`)
  theData = aggregate(leaksData$`Size ppm`, list(leaksData$`Home age (years)`), FUN=sum)
  theModel = lm( `Group.1` ~ x, data=theData)
  plot( theData,
        main="Sum of leaks PPMV by building age",
        xlab="Building age",
        ylab="Sum of leaks PPMV")
  theStats = paste0("R-squared: ", round(summary(theModel)$r.squared,3),", n=",nrow(theData),", p=",signif(Regressionp(theModel), digits=3) )
  mtext( theStats  )
  cat (theStats,"\n")
  
  return()
}

# --------------------------------------------------
analyze_varying = function ( experimentData ) {
  # do some analysis of how varying timespan, data range and volume effect emission calculations
  
  # 1. RCM.  Data : all RCM experiments 
  #  a) simple slope : varying time span
  #  b) fitted tangent : varying data time range 
  #  c) simple slope : varying volume
  #  d) fitted tangent : varying volume
  # 2. effects on control test : converging or diverging from actual test flux (enough data except for R3)
  # 3. use Dom expt 
 
  theData = experimentData[experimentData$`Experiment type` == "BCM",]
  theData = theData %>% drop_na(`Experiment name`) # drop NA rows
  theData = theData %>% drop_na(`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`) # use only expts where flux was calculated
  
  # Prep for fitted tangent vary testing
  for ( row in 1:nrow(theData )) {
    if ( theData$"How much time to fit with (sec)"[row] == 'all' ) { 
      theData$"How much time to fit with (sec)"[row] = theData$"Max data (sec)"[row]
    }
  }
  theData$`How much time to fit with (sec)` = as.numeric(theData$`How much time to fit with (sec)`)
  
  
  # ==========  1a  =========  
if ( 1 ) {
  
  opar=par()
  par( mfrow=c(1,2), mai= c(1.1, 1.2, 0.5, 0.3) )
  
  
  title = "1 a) Analyzing effects of varying time span on RCM simple slope calculations"
  title = "A. Simple slope\n"
  from=5; to=60; step=5
  timeSpansInMinutes = seq(from, to, step ) # 5,10,15,...60
  # create empty results plot for the composite line chart (bad cos it implies values between)
  plot( x=timeSpansInMinutes,
        xaxt='n',
        type='n', 
        xlab = "Time span (mins)", 
        cex.lab=1.3, # x+y lab size
        cex.axis=1.2, # y-axis labels size
        cex.names=1.2,
        ylab = expression(Calculated~CH[4]~emissions~(ft^3~day^-1)),
        ylim=c(0,.65), 
        xlim=c(from,to)
  )
  mtext(title,adj=0, cex=1.3)
  axis( 1, at=seq(from, to, by=step), las=2)
  abline(h=seq(0,1,0.1), lty = "dashed", col = "lightgray")
  
  # all results frame - each column is an experiment, each row is a time span
  resFrame1A = data.frame(matrix(nrow = length(timeSpansInMinutes) , ncol=0 ) )

  for (experimentNumber in 1:nrow(theData)) {
    
    experimentName = as.character( theData[experimentNumber,"Experiment name"] )
    adjustedVolume = as.numeric( theData$`Adjusted volume (Vadj)`[experimentNumber] )
    cat (title, " ", experimentName,"\n")
    methaneData = load_methane_data( theData[experimentNumber,], times = NULL ) 
    methaneData$EPOCH_TIME = methaneData$EPOCH_TIME - head( methaneData, 1)$EPOCH_TIME
    fluxes=c() # calculated fluxes results
    for ( timeSpan in timeSpansInMinutes) {
      fluxes = append(fluxes, calculate_flux_using_simple_slope ( 0, (timeSpan * 60) , methaneData, adjustedVolume )$flux )
    }
    
    lines( timeSpansInMinutes, y=fluxes, type='b' )
    cat ("Timespan (min),", paste0(timeSpansInMinutes, collapse=","), "\n")
    cat ("fluxes,", paste0(fluxes, collapse=","), "\n")
    
    resFrame1A[ experimentName ] = fluxes
    #resFrame[ nrow(resFrame) +1 , ] = c(experimentName, fluxes) # this might be useful for a composite boxplot later perhaps
    
  }

  # if need this - make individual plots for the appendix
  if ( 0 ) {
     for ( col in 1:col(resFrame1A)) {
       experimentName = names(resFrame1A)[col]
       plot( x=timeSpansInMinutes, y=resFrame1A[,col], 
             xlab="Time span (mins)", 
             #ylab = expression(Air ~CH[4] ~ concentration ~ (ppm)),
             ylab = expression(Calculated~CH[4]~emissions~(ft^3~day^-1)),
             xaxt='n' )
       axis( 1, at=seq(from, to, by=step), las=2)
       mtext(paste0(title,": ", experimentName, "\n"))
     }
  } 
  
  # create a composite boxplot of the results
  # TODO not sure a boxplot makes that much sense - perhaps use scatter plot with error bars https://r-coder.com/scatter-plot-r/ or just a scatter plot by category
  
  if ( 0 ) {
     boxplot(resFrame1A, xlab = "Experiment", 
             ylab = expression(Calculated~CH[4]~emissions~(ft^3~day^-1)),
             las=2,
             cex.lab=1.3, # x+y lab size
             cex.axis=1.2, # y-axis labels size
             cex.names=1.2)
     abline(h=seq(0,0.6,0.05), lty = "dashed", col = "gray") # add some grid lines
     mtext( paste0(title, "\n"), cex=1.3)
  }

  # ==========  1b  =========  
  

  title = "1 b) Analyzing effects of varying data range used in RCM fitted tangent calculations"
  title = "B. Fitted tangent\n"
  # For some experiments, not all of the experimental data collected can be used eg E13 where there's a spike.
  # The 'How much time to fit with (sec)' reflects what I think the max amount to safely fit with is (and that doesn't break nls() )
  
  from=1500; to=22000; step=2000
  dataRanges = seq(from, to, step ) 
  # create empty results plot
  plot( x=dataRanges,
        xaxt='n',
        type='n', 
        xlab = "Time span (sec)", 
        cex.lab=1.3, # x+y lab size
        cex.axis=1.2, # y-axis labels size
        cex.names=1.2,
        ylab="",
        ylim=c(0,.65), 
        xlim=c(from,to)
  )
  mtext(title,adj=0,cex=1.3)
  
  axis( 1, at=seq(from, to, by=step), las=2)
  abline(h=seq(0,1,0.1), lty = "dashed", col = "lightgray")
  # all results frame - each column is an experiment, each row is a time span
  resFrame1B = data.frame(matrix(nrow = length(dataRanges) , ncol=0 ) )
  for (experimentNumber in 1:nrow(theData)) {
    
    experimentName = as.character( theData[experimentNumber,"Experiment name"] )
    
    #if ( experimentName != "R3") { next }
    # Note that if dataRanges = 1500,... , R3 will be all NA's since 1200 was the max time to use for fitting that one
    
    adjustedVolume = as.numeric( theData$`Adjusted volume (Vadj)`[experimentNumber] )
    cat (title," ", experimentName,"\n")
    methaneData = load_methane_data( theData[experimentNumber,], times = NULL ) 
    methaneData$EPOCH_TIME = methaneData$EPOCH_TIME - head( methaneData, 1)$EPOCH_TIME
   
    fluxes=c() # calculated fluxes results
    for ( timeRange in dataRanges) {
      # don't try to fit beyond max usable data range and avoid nls fitting issues
      if ( timeRange > theData[experimentNumber,"How much time to fit with (sec)"] ) { 
        CH4cubicFeetPerDay = NA 
      }
      else { 
        fitWithTimeLimit = timeRange 
        # fit data ie fitted tangent calculation
        fittedValues = fitted_change_rate( methaneData, experimentName, fitWithTimeLimit ) 
        CH4ppmChangePerSecond = fittedValues$CH4ppmPerSecond # CH4 ppm per second from fitting
        CH4ppmPerHour = CH4ppmChangePerSecond * 3600  # CH4 ppm per hour
        CH4partsPerHour = CH4ppmPerHour / 1000000 # CH4 parts per hour
        CH4partsPerDay = CH4partsPerHour * 24 # CH4 parts per day
        CH4cubicFeetPerDay = CH4partsPerDay * adjustedVolume
      }
      fluxes = append(fluxes, CH4cubicFeetPerDay )
    }
    
    lines( dataRanges, y=fluxes, type='b' )
    cat ("Timespan (min),", paste0(dataRanges, collapse=","), "\n")
    cat ("fluxes,", paste0(fluxes, collapse=","), "\n")
    
    resFrame1B[ experimentName ] = fluxes
    #resFrame[ nrow(resFrame) +1 , ] = c(experimentName, fluxes) # this might be useful for a composite boxplot later perhaps
  
  }
  
  # Remove any columns that are all NA - which is R3
  resFrame1B = resFrame1B[,colSums(is.na(resFrame1B))<nrow(resFrame1B)]
  mtext("R3 removed because time to fit that was < initial data step of 1500", side=1, line =5)
  
  # also make individual plots for the appendix if needed
  if ( 0 ) {
     for ( col in 1:ncol(resFrame1B)) {
       if ( ! all(is.na(resFrame1B[,col])) ) { 
          experimentName = names(resFrame1B)[col]
          plot( x=dataRanges, y=resFrame1B[,col], 
                xlab="Data ranges (sec)", 
                ylab = expression(Calculated~CH[4]~emissions~(ft^3~day^-1)),
                xaxt='n' )
          axis( 1, at=seq(from, to, by=step), las=2)
          mtext(paste0(title,": ", experimentName, "\n"))
       }
       else { cat( paste0( "!! No results for experiment ", experimentName))   }
     }
  }
  
  # create a composite boxplot of the results
  boxplot(resFrame1A, xlab = "Experiment", 
          ylab = expression(Calculated~CH[4]~emissions~(ft^3~day^-1)),
          las=2)
  abline(h=seq(0,0.6,0.05), lty = "dashed", col = "gray") # add some grid lines
  mtext("A. Simple slope\n", adj=0)
  boxplot(resFrame1B, xlab = "Experiment", ylab = "", las=2)
  abline(h=seq(0,0.6,0.05), lty = "dashed", col = "gray") # add some grid lines
  mtext("A. Fitted tangent\n", adj=0)
  
  par(opar)

}
  
  #    # ==========  1c  =========  
  # this isn't necessary because flux = rate * day * volume / 1,000,000 : IE flux is directly proportional to volume 
if ( 0 ) { 
  title = "1 c) Analyzing effects of varying volume used in RCM simple slope calculations (time span = 600 s)"
  from= 50; to=150 ; step=10 # %
  percentVariations = seq(from, to, step ) 
  # create empty results plot
  plot( x=percentVariations,
        xaxt='n',
        type='n', 
        xlab = "% volume difference from measured", 
        ylab = expression(Calculated~CH[4]~emissions~(ft^3~day^-1)),
        ylim=c(0,0.8), 
        xlim=c(from,to),
        main = title
  )
  axis( 1, at=seq(from, to, by=step), las=2)
  abline(h=seq(0,1,0.1), lty = "dashed", col = "gray")
  
  # all results frame - each column is an experiment, each row is a time span
  resFrame = data.frame(matrix(nrow = length(percentVariations) , ncol=0 ) )

  for (experimentNumber in 1:nrow(theData)) {
    
    experimentName = as.character( theData[experimentNumber,"Experiment name"] )
    
    cat (title, " ", experimentName,"\n")
    methaneData = load_methane_data( theData[experimentNumber,], times = NULL ) 
    methaneData$EPOCH_TIME = methaneData$EPOCH_TIME - head( methaneData, 1)$EPOCH_TIME
    fluxes=c() # calculated fluxes results
    
    for ( percent in percentVariations) {
      adjustedVolume = as.numeric( theData$`Adjusted volume (Vadj)`[experimentNumber] ) * ( percent/100 )
      fluxes = append(fluxes, calculate_flux_using_simple_slope ( 0, 600 , methaneData, adjustedVolume )$flux )
    }
    
    lines( percentVariations, y=fluxes )
    cat ("Timespan (min),", paste0(percentVariations, collapse=","), "\n")
    cat ("fluxes,", paste0(fluxes, collapse=","), "\n")
    
    resFrame[ experimentName ] = fluxes
    #resFrame[ nrow(resFrame) +1 , ] = c(experimentName, fluxes) # this might be useful for a composite boxplot later perhaps
    
  }
  
  # create a composite boxplot of the results
  boxplot(resFrame, xlab = "Experiment", 
          ylab = expression(Calculated~CH[4]~emissions~(ft^3~day^-1)), 
          las=2,
          cex.lab=1.3, # x+y lab size
          cex.axis=1.2, # y-axis labels size
          cex.names=1.2)
  abline(h=seq(0,0.8,0.1), lty = "dashed", col = "gray") # add some grid lines
  mtext( paste0(title, "\n"), cex=1.3)

}
  
  # =====  2. effects of time range or time span on control test : converging or diverging from actual test flux ? (see if enough data)
  # 
  # Using control test Control12062022-2, vary time span for simple slope and see if converge or diverge from actual flux
  title = "2 : effects of time span on control test calculated using simple slope method"
  title = "A. Simple slope"
  experimentName = "R8_RCM_9" 
  actualFlux = 1.025800707 
  experimentData = load_experiments_spreadsheet(dataFile, workSheet, expectedColumns , skipRows, FALSE )
  theData = experimentData[experimentData$`Experiment name` == experimentName,]
  adjustedVolume = as.numeric( theData$`Adjusted volume (Vadj)`[1] )
  cat (title, ", control experiment ", experimentName,"\n")
  methaneData = load_methane_data( theData[1,], times = NULL )
  methaneData$EPOCH_TIME = methaneData$EPOCH_TIME - head( methaneData, 1)$EPOCH_TIME
  simpleSlopeFluxes = c()
  fittedTangentFluxes = c()
  
  from=1; to=28; step=1 # limited with control test which was about 30 mins long
  timeSpansInMinutes = seq(from, to, step ) 
  
  for ( timeSpan in timeSpansInMinutes) {
      simpleSlopeFluxes = append(simpleSlopeFluxes, calculate_flux_using_simple_slope ( 0, (timeSpan * 60) , methaneData, adjustedVolume )$flux )
  }
  opar=par()
  par( mfrow=c(1,2), mai= c(1, 1, 0.5, 0.3) ) # side by side plots, margins to make them closer together,   # bottom, left, top and right
  parSidebySide=par()
  plot( x=timeSpansInMinutes,
        y=simpleSlopeFluxes,
        xlab = "Time span (mins)", 
        cex.lab=1.3, # xlab size
        cex.axis=1.2, # y-axis labels size
        cex.names=1.2, # x-axis labels size
        ylab = expression(Calculated~CH[4]~emissions~(ft^3~day^-1)),
        ylim=c( 0.85, 1.05), 
        xlim=c(from,to),
        xaxt='n'
  )
  axis( 1, at=seq(from, to, by=step), las=2)
  abline(h=seq( 0.85, 1.05,0.05), lty = "dashed", col = "gray")
  mtext( paste0( title,"\n"), adj=0, cex=1.3)
  
  abline(h=actualFlux,   lty = "dashed", col = "orange", lwd=3)
  
  text(x=20, y=1.02, col="orange", paste0("Actual flux : ", round(actualFlux,3)) ,cex=1.1 ) 
  
  
  title = "2 : effects of time span on control test calculated using fitted tangent method"
  title = "B. Fitted tangent"
  from=100; to=1700; step=100
  dataRanges = seq(from, to, step ) 
  for ( timeRange in dataRanges) {
    fitWithTimeLimit = timeRange 
    fittedValues = fitted_change_rate( methaneData, experimentName, fitWithTimeLimit ) 
    CH4ppmChangePerSecond = fittedValues$CH4ppmPerSecond # CH4 ppm per second from fitting
    CH4ppmPerHour = CH4ppmChangePerSecond * 3600  # CH4 ppm per hour
    CH4partsPerHour = CH4ppmPerHour / 1000000 # CH4 parts per hour
    CH4partsPerDay = CH4partsPerHour * 24 # CH4 parts per day
    CH4cubicFeetPerDay = CH4partsPerDay * adjustedVolume
    fittedTangentFluxes = append(fittedTangentFluxes, CH4cubicFeetPerDay  )
  }
  plot( x=dataRanges,
        y=fittedTangentFluxes,
        xlab = "Time span (sec)", 
        cex.lab=1.3, # x+y lab size
        cex.axis=1.2, # y-axis labels size
        cex.names=1.2, # x-axis labels size
        ylab="",
        ylim=c( 0.85, 1.05), 
        xlim=c(from,to),
        xaxt='n'
  )
  axis( 1, at=seq(from, to, by=step), las=2)
  abline(h=seq( 0.85, 1.05,0.05), lty = "dashed", col = "gray")
  
  mtext(paste0( title,"\n"), adj=0, cex=1.3)
  abline(h=actualFlux,   lty = "dashed", col = "orange", lwd=3)
  text(x=1200, y=1.02, col="orange", paste0("Actual flux : ", round(actualFlux,3)) ,cex=1.1  ) 
  
  cat ("\nTimespan (min),", paste0(timeSpansInMinutes, collapse=","), "\n")
  cat ("simpleSlopeFluxes,", paste0(simpleSlopeFluxes, collapse=","), "\n")
  
  cat ("\nData ranges (min),", paste0(dataRanges, collapse=","), "\n")
  cat ("fittedTangentFluxes,", paste0(fittedTangentFluxes, collapse=","), "\n")

  par(opar)

}

# --------------------------------------------------
analyze_ambients = function ( experimentData ) {
  
  # Ambient concentrations by floor - all buildings
  
  colNames = c("Floor", "Min", "Max", "Mean", "Median", "n")
  thePlots = list()
  theTitles = c()
  plotNum = 1
  for ( buildingType in c("all", "single", "multi")) {
    
    if ( buildingType == "all") {
      theData = experimentData # all data
      theTitle = "A. Single-family and multi-family homes"
      plotTitle = expression( Ambient ~ air ~ CH[4] ~ concentrations ~ "in" ~ single ~ and ~ multi ~ family ~ homes)
      yAxisLabel = expression( Average~ambient~air~ CH[4]~concentration~(ppmv) )
      thePadding = ""
    }
    else if ( buildingType == "single") {
      theData = experimentData[experimentData$`Single or multi family home` == "single",]
      theTitle = "B. Single-family homes"
      plotTitle = expression(Ambient ~ air ~ CH[4] ~ concentrations ~ "in" ~ single ~ family ~ homes)
      yAxisLabel = NULL
      thePadding = ""
    }
    else if ( buildingType == "multi") {
      theData = experimentData[experimentData$`Single or multi family home` == "multi",]
      theTitle = "C. Multi-family homes"
      plotTitle = expression(Ambient ~ air ~ CH[4] ~ concentrations ~ "in" ~ multi ~ family ~ homes)
      yAxisLabel = NULL
      thePadding = "  "
    }
    
    theTitles = append(theTitles, theTitle)
    
    ambientConcAllTypes = data.frame( matrix( nrow=5, ncol=length(colNames )), stringsAsFactors=FALSE ) 
    colnames(ambientConcAllTypes) = colNames;
    n = length(which(!is.na( theData$`Basement ambient pre-flush CH4` )))
    ambientConcAllTypes[1,] = c(paste0("Basement",thePadding," (n=",n,")" ), min(theData$`Basement ambient pre-flush CH4`, na.rm=T), max(theData$`Basement ambient pre-flush CH4`, na.rm=T), mean(theData$`Basement ambient pre-flush CH4`, na.rm=T), median(theData$`Basement ambient pre-flush CH4`, na.rm=T) , n )
    n = length(which(!is.na( theData$`1st floor CH4 concentration` )))
    ambientConcAllTypes[2,] = c(paste0("Floor 1 (n=",n,")") ,  min(theData$`1st floor CH4 concentration`, na.rm=T),    max(theData$`1st floor CH4 concentration`, na.rm=T),    mean(theData$`1st floor CH4 concentration`, na.rm=T),    median(theData$`1st floor CH4 concentration`, na.rm=T),    n  )
    n = length(which(!is.na( theData$`2nd floor CH4 concentration` )))
    ambientConcAllTypes[3,] = c(paste0("Floor 2 (n=",n,")"),  min(theData$`2nd floor CH4 concentration`, na.rm=T),    max(theData$`2nd floor CH4 concentration`, na.rm=T),    mean(theData$`2nd floor CH4 concentration`, na.rm=T),    median(theData$`2nd floor CH4 concentration`, na.rm=T),     n  )
    n = length(which(!is.na( theData$`3rd floor CH4 concentration` )))
    ambientConcAllTypes[4,] = c(paste0("Floor 3 (n=",n,")"),  min(theData$`3rd floor CH4 concentration`, na.rm=T),    max(theData$`3rd floor CH4 concentration`, na.rm=T),    mean(theData$`3rd floor CH4 concentration`, na.rm=T),    median(theData$`3rd floor CH4 concentration`, na.rm=T),     n  )
    n = length(which(!is.na( theData$`4th floor CH4 concentration` )))
    ambientConcAllTypes[5,] = c(paste0("Floor 4 (n=",n,")"),  min(theData$`4th floor CH4 concentration`, na.rm=T),    max(theData$`4th floor CH4 concentration`, na.rm=T),    mean(theData$`4th floor CH4 concentration`, na.rm=T),    median(theData$`4th floor CH4 concentration`, na.rm=T) ,    n  )
    ambientConcAllTypes$Min = as.numeric(ambientConcAllTypes$Min )
    ambientConcAllTypes$Max = as.numeric(ambientConcAllTypes$Max )                           
    ambientConcAllTypes$Mean = as.numeric(ambientConcAllTypes$Mean )                           
    ambientConcAllTypes$Median = as.numeric(ambientConcAllTypes$Median ) 
    
    # replace any Inf/-Inf/NaN values to NA to prevent ggplot plotting them
    ambientConcAllTypes[sapply(ambientConcAllTypes, is.infinite)] = NA
    ambientConcAllTypes[sapply(ambientConcAllTypes, is.nan)] = NA
    
    plotSubtitle=NULL
  
    thePlot = ggplot() + 
      ggtitle( label=" ") +
      geom_point( size=2, data = ambientConcAllTypes, aes( x=Floor, y=`Min`,  color="Min" ) ) + # 
      geom_point( size=2, data = ambientConcAllTypes, aes( x=Floor, y=`Max`,  color="Max" ) ) +   # 
      geom_point( size=2, data = ambientConcAllTypes, aes( x=Floor, y=Mean,   color="Mean" ) )  + # 
      geom_point( size=2, data = ambientConcAllTypes, aes( x=Floor, y=Median, color="Median" ) ) + # 
      scale_color_manual(name = NULL,   # no legend title
                         values = c( "Min" = "blue", "Max" = "red" , "Mean" = "orange", "Median"="green"), # control colors of points
                         labels = c( "Min", "Max", "Mean", "Median"),
                         breaks = c( "Min", "Max", "Mean", "Median") # this seems to override alphabetical ordering of legend ?
      ) + # control legend labels
      labs( x=NULL, y=yAxisLabel) +
      scale_y_continuous(minor_breaks = seq(0, 20, 1),  # increase y-axis grid lines frequency 
                         limits=c(1,20) # and y-limits
                         ) + 
      theme( axis.text.x=element_text(angle = -90, hjust = 0, color="black", size=12),  # rotate and color the x-axis labels
             axis.text.y=element_text( color="black", size=11), # y-axes tick labels text 
             panel.background = element_rect(fill = "white",colour = "black", size = 1, linetype = "solid"),
             panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
             legend.position = c(.2, .8), # legend topright
             legend.key = element_blank(), # make the backgrounds of the legend items not colored (gray by default)
             legend.background = element_rect( fill="white", size=0.5, linetype="solid", colour ="gray"), # legend border and fill
             legend.text = element_text(size=11),
             plot.title = element_text(hjust = 0.5, face="plain"), # justify title
             plot.subtitle = element_text(hjust = 0.5, face="plain"), # justify subtitle
             axis.title = element_text( color="black", face="plain"), # axess labels - note with expression(), need to use bold() inside it to make this work
             axis.title.y = element_text(size = 13) # y-axis title
      )

    cat(theTitle, "\n"); print( ambientConcAllTypes); cat("\n")
    cat("\tAverage min:", mean(ambientConcAllTypes$Min,na.rm=T), ", average max:", mean(ambientConcAllTypes$Max,na.rm=T),"\n\n")

    thePlots[[plotNum]] =  thePlot
    plotNum = plotNum + 1
  }
  print( plot_grid( thePlots[[1]], thePlots[[2]], thePlots[[3]], ncol=3, labels=theTitles, label_size=12, hjust=-0.2, label_fontface="plain") )
  
}
  
  

# --------------------------------------------------
analyze_flushes = function ( experimentData )  {

  # get some stats for all RCM experiments where had sufficient data
  theData = experimentData[experimentData$`use in overall RCM flush analysis` == "yes",]
  theData$reldiffPostFlushandOutdoor = 100 * ( theData$`CH4 at end of flush (ppm)` / theData$`Outside CH4`) 
  cat ("RCM flushes of experiments ALL where had enough data (n=",nrow(theData),")\n")
  cat ("  Durations : ", min(theData$`total flush duration`), " - " ,max(theData$`total flush duration`) ," min\n")
  cat ("                Mean:", mean( theData$`total flush duration` ), ", median:", median( theData$`total flush duration` ) , "\n")
  cat ("RCM post flush ch4 conc:\n")
  cat ("   Mean : ", mean( theData$reldiffPostFlushandOutdoor), ", median:", median(theData$reldiffPostFlushandOutdoor), " %\n")
  
  
  # ====== first do a composite plot of CH4 concentration over time for experiments which were full-flushed without being preceded by simple flushing =======
  # filter down to full-only flushes (ie not preceded by simple SRCM flushes)
  theData = experimentData[experimentData$`flush type (combination means simple followed by full flush; fullonly means just full flush not preceded by simple)` == "fullonly",]
  theData = theData %>% drop_na(`Experiment name`) # drop NA rows
  theData = theData %>% drop_na(`Basement full flush start epoch time`) # drop any rows that don't have a time here eg E3.1
  
  theData$reldiffPostFlushandOutdoor = 100 * ( theData$`CH4 at end of flush (ppm)` / theData$`Outside CH4`) 
  theData$fullFlushDuration = (theData$`Basement full flush end epoch time` - theData$`Basement full flush start epoch time`) / 60 # mins
  cat ("RCM flushes of experiments not preceded by SRCM (n=",nrow(theData),")\n")
  cat ("  Durations : ", min(theData$fullFlushDuration), " - " ,max(theData$fullFlushDuration) ," min\n")
  cat ("                Mean:", mean( theData$fullFlushDuration ), ", median:", median( theData$fullFlushDuration ) , "\n")
  cat ("RCM post flush ch4 conc:\n")
  cat ("   Mean : ", mean( theData$reldiffPostFlushandOutdoor), ", median:", median(theData$reldiffPostFlushandOutdoor), " %\n")
  
  opar=par()
  par(mar = c(5.1, 4.5, 4.2, 2.1) ) # bottom, left, top, and right
  
  # create empty plot
  plot( x=seq(0,4000,1), 
        type='n', 
        xlab = "Time (sec)", 
        ylab = expression(Air ~CH[4] ~ concentration ~ (ppmv)),
        ylim=c(2,5),
        cex.lab=1.2, # xlab size
        cex.axis=1, # y-axis labels size
        cex.names=1, # x-axis labels size
        main = "RCM flushes"
  )
  
  colors = brewer.pal(n = nrow(theData), name = 'Set2' )
  enames = c()
  
  for (experimentNumber in 1:nrow(theData)) {
    
    experimentName = as.character(theData[experimentNumber,"Experiment name"])

    cat("Experiment : ",experimentName ) 
    enames = append(enames, experimentName)
    startTime = theData$`Basement full flush start epoch time`[experimentNumber]
    endTime = theData$`Basement full flush end epoch time`[experimentNumber]
    methaneTimeSeriesData = load_methane_data( theData[experimentNumber,], times = c( startTime, endTime ) ) 
                                                          
    # shift epoch time to its on the scale 0:...
    methaneTimeSeriesData$EPOCH_TIME = methaneTimeSeriesData$EPOCH_TIME - head( methaneTimeSeriesData, 1)$EPOCH_TIME
    lines( x = methaneTimeSeriesData$EPOCH_TIME, 
           y = methaneTimeSeriesData$CH4, 
           type = 'l',  
           col=colors[experimentNumber]
    )
    
    # how long did it take to get down to within some percentage of outdoor ambient (Outside CH4) ?
    smoothed = supsmu( x = methaneTimeSeriesData$EPOCH_TIME, y = methaneTimeSeriesData$CH4 )
    lines( smoothed, lty = "dashed", col="black") #colors[experimentNumber] ) 
    
  }
  
  legend( x="topright",
          legend = enames,
          col  =   colors, 
          cex  = 0.9, 
          lty=1,
          lwd=2,
          bg = "white",
          title="Experiments")
  
  # ====== now do a composite plot of CH4 concentration over time during simple flushes =======
  theData = experimentData[experimentData$`bag samples done same day or different day to BCM` == "same",]
  theData = theData %>% drop_na(`Experiment name`) # drop NA rows
  theData = theData %>% drop_na(`Simple flush start epoch time`) # drop any rows that don't have a time here eg E3.1
  
  theData$reldiffPostFlushandOutdoor = 100 * ( theData$`basement ambient post-simple flush CH4 ppm` / theData$`Outside CH4`) 
  cat ("SRCM flushes (n=",nrow(theData),")\n")
  cat ("  Durations : ", min(theData$`Simple flush duration (min)`), " - " ,max(theData$`Simple flush duration (min)`) ," min\n")
  cat ("                Mean:", mean( theData$`Simple flush duration (min)` ), ", median:", median( theData$`Simple flush duration (min)`) , "\n")
  cat ("SRCM post flush ch4 conc:\n")
  cat ("   Mean : ", mean( theData$reldiffPostFlushandOutdoor), ", median:", median(theData$reldiffPostFlushandOutdoor), " %\n")
  
  # create empty plot
  plot( x=seq(0,3750,1), 
        type='n', 
        xlab = "Time (sec)", 
        ylab = expression(Air ~CH[4] ~ concentration ~ (ppmv)),
        ylim=c(2,5.5),
        main = "SRCM flushes",
        cex.lab=1.2, # xlab size
        cex.axis=1, # y-axis labels size
        cex.names=1, # x-axis labels size
  )
  
  colors= c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#cccccc','#b15928')
  enames = c()
  
  for (experimentNumber in 1:nrow(theData)) {
    
    experimentName = as.character(theData[experimentNumber,"Experiment name"])
    
    cat("Experiment : ",experimentName ) 
    enames = append(enames, experimentName)
    startTime = theData$`Simple flush start epoch time`[experimentNumber]
    endTime = startTime + ( theData$`Simple flush duration (min)`[experimentNumber] * 60 )
    methaneTimeSeriesData = load_methane_data( theData[experimentNumber,], times = c( startTime, endTime ) ) 
    
    # shift epoch time to its on the scale 0:...
    methaneTimeSeriesData$EPOCH_TIME = methaneTimeSeriesData$EPOCH_TIME - head( methaneTimeSeriesData, 1)$EPOCH_TIME
    lines( x = methaneTimeSeriesData$EPOCH_TIME, 
           y = methaneTimeSeriesData$CH4, 
           type = 'l',  
           col=colors[experimentNumber]
           )
    smoothed = supsmu( x = methaneTimeSeriesData$EPOCH_TIME, y = methaneTimeSeriesData$CH4 )
    lines( smoothed, lty = "dashed", col="black") #colors[experimentNumber] ) 
  }
 
  legend( x="topright",
          legend = enames,
          col  =   colors, 
          cex  = 0.9, 
          lty=1,
          lwd=2,
          bg = "white",
          title="Experiments")

}

# --------------------------------------------------
analyze_RCM_control_tests = function ( controlTestsData , yLims, yLab, theTitle) {
  
  controlTestsData$`Control test flux` = as.numeric(controlTestsData$`Control test flux`)
  controlTestsData$`Calculated emissions using simple slope` = as.numeric(controlTestsData$`Calculated emissions using simple slope`)
  controlTestsData$`Calculated emissions using fitted tangent` = as.numeric( controlTestsData$`Calculated emissions using fitted tangent`)
  # I use the flux values from my spreadsheet here because in some tests I needed to adjust for existing leak flux
  # so this routine creates some basic final stats and plots them
  
  controlTestsData$simpleSlopeFluxRelDiff = controlTestsData$`Calculated emissions using simple slope` / controlTestsData$`Control test flux`
  controlTestsData$fittedTangentFluxRelDiff = controlTestsData$`Calculated emissions using fitted tangent` / controlTestsData$`Control test flux`
  
  simpleSlopeFluxRelDiffMean = 100-round(100*mean(controlTestsData$simpleSlopeFluxRelDiff),3)
  fittedTangentFluxRelDiffMean = 100-round(100*mean(controlTestsData$fittedTangentFluxRelDiff),3)
  
  theStats = paste0("Emissions calculated using simple slope and fitted tangent were on average within ", simpleSlopeFluxRelDiffMean ,"% and ", fittedTangentFluxRelDiffMean, "% respectively of control tests (n=", nrow(controlTestsData),")\n")
  cat(theStats)
 
  thePlot = ggplot() + 
   
   ggtitle( theTitle ) + 
   
   # adding color= into aes() will create a legend. To waste 4 hours, figure out how to change shapes in the legend. Gave up on that one for now
   geom_point( size=3, data = controlTestsData, aes( x=`Experiment name`, y=`Control test flux`, color="Control" ) ) + # actual control flux
   geom_point( size=2, data = controlTestsData, aes( x=`Experiment name`, y=`Calculated emissions using simple slope`,  color="RCM using simple slope" ) ) +   # flux using simple slope
   geom_point( size=2, data = controlTestsData, aes( x=`Experiment name`, y=`Calculated emissions using fitted tangent`,  color="RCM using fitted tangent" ) )  + # flux using fitted tangent
   scale_color_manual(name = "Daily emissions", # no legend title
                      values = c( "Control" = "green", "RCM using simple slope" = "orange", "RCM using fitted tangent" = "blue"), # control colors of points
                      labels = c( "Control", "RCM using simple slope", "RCM using fitted tangent"),
                      breaks = c( "Control", "RCM using simple slope", "RCM using fitted tangent")
                      ) + # control legend labels
   
   labs( x="", y=yLab) +
   ylim( yLims ) +
   theme( axis.text.x=element_text(angle = -90, hjust = 0, color="black", size=11),  # rotate and color the x-axis labels
          axis.text.y=element_text( color="black", size=11), # y-axes tick text color
          panel.background = element_rect(fill = "white",colour = "black", size = 1, linetype = "solid"),
          #panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "gray"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # turn off grid
          legend.text=element_text(size=11),
          legend.position = c(.75, .8), # legend topright
          legend.key = element_blank(), # make the backgrounds of the legend items not colored (gray by default)
          legend.background = element_rect( fill="white", size=0.5, linetype="solid", colour ="gray"), # legend border and fill
          plot.title = element_text(hjust = 0.5), # justify title
          axis.title = element_text( color="black", face="bold"), # axes labels - note with expression(), need to use bold() inside it to make this work
          axis.title.y = element_text(size = 13), # y-axis title
          axis.title.x = element_text(size = 13) # y-axis title
          )
 
 
  return ( thePlot )
 
 
}

# --------------------------------------------------
analyze_SRCM_control_tests = function ( controlTestsData ) {

  controlTestsData$`bag 1 ch4 ppm` = as.numeric( controlTestsData$`bag 1 ch4 ppm`)
  controlTestsData$`bag 2 ch4 ppm` = as.numeric( controlTestsData$`bag 2 ch4 ppm`)
  controlTestsData$`time span` = as.numeric( controlTestsData$`time span`)
  controlTestsData$`adjusted basement volume (Vadj)` = as.numeric( controlTestsData$`adjusted basement volume (Vadj)` )
  controlTestsData$`simplified volume (Vsim)` = as.numeric( controlTestsData$`simplified volume (Vsim)` )
  controlTestsData$`Control test flux` = as.numeric( controlTestsData$`Control test flux` )

  controlTestsData$ch4ppmPerSec = (controlTestsData$`bag 2 ch4 ppm` - controlTestsData$`bag 1 ch4 ppm`) / controlTestsData$`time span`
  controlTestsData$ch4ppmPerDay = controlTestsData$ch4ppmPerSec * 60 * 60 * 24 
  
  controlTestsData$fluxUsingVadj = ( controlTestsData$ch4ppmPerDay * controlTestsData$`adjusted basement volume (Vadj)` ) / 1000000
  controlTestsData$fluxUsingVsim = ( controlTestsData$ch4ppmPerDay * controlTestsData$`simplified volume (Vsim)` ) / 1000000
  
  controlTestsData$fluxUsingVadjRelDiff = controlTestsData$fluxUsingVadj / controlTestsData$`Control test flux`
  controlTestsData$fluxUsingVsimRelDiff = controlTestsData$fluxUsingVsim / controlTestsData$`Control test flux`
  
  # calc some basic stats
  fluxUsingVadjRelDiffmean = 100-round(100*mean(controlTestsData$fluxUsingVadjRelDiff),0)
  fluxUsingVsimRelDiffmean = 100-round(100*mean(controlTestsData$fluxUsingVsimRelDiff),0)
  theStats = paste0("Emissions calculated using Vadj and Vsim were on average within ", fluxUsingVadjRelDiffmean ,"% and ", fluxUsingVsimRelDiffmean, "% respectively of control tests (n=", nrow(controlTestsData),")")
  cat(theStats)
  
  thePlot = ggplot() + 
    ggtitle( "Bag method control tests" ) + 
    
    # adding color= into aes() will create a legend. To waste 4 hours, figure out how to change shapes in the legend. Gave up on that one for now
    geom_point( size=3, data = controlTestsData, aes( x=`Experiment name`, y=`Control test flux`, color="Control" ) ) + # actual control flux
    geom_point( size=2, data = controlTestsData, aes( x=`Experiment name`, y=fluxUsingVadj,  color="Bag method using Vadj" ) ) +   # flux using vadj
    geom_point( size=2, data = controlTestsData, aes( x=`Experiment name`, y=fluxUsingVsim,  color="Bag method using Vsim" ) )  + # flux using vsim

    scale_color_manual(name = "Daily emissions", # no legend title
                       values = c( "Control" = "green", "Bag method using Vadj" = "orange", "Bag method using Vsim" = "blue"), # control colors of points
                       labels = c( "Control", "Bag method using Vadj", "Bag method using Vsim"),
                       breaks = c( "Control", "Bag method using Vadj", "Bag method using Vsim")
                       ) + # control legend labels
    
    labs( x="", y=expression( Daily~CH[4]~emissions ~ (ft^3~day^-1) ) ) +
    
    theme( axis.text.x=element_text(angle = -90, hjust = 0, color="black", size=11),  # rotate and color the x-axis labels
           axis.text.y=element_text( color="black", size=11), # y-axes tick text color
           panel.background = element_rect(fill = "white",colour = "black", size = 1, linetype = "solid"),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # turn off grid
           legend.text=element_text(size=11),
           legend.position = c(.15, .8), # legend topright
           legend.key = element_blank(), # make the backgrounds of the legend items not colored (gray by default)
           legend.background = element_rect( fill="white", size=0.5, linetype="solid", colour ="gray"), # legend border and fill
           plot.title = element_text(hjust = 0.5), # justify title
           axis.title = element_text( color="black", face="bold"), # axes labels - note with expression(), need to use bold() inside it to make this work
           axis.title.y = element_text(size = 13), # y-axis title
           axis.title.x = element_text(size = 13) # y-axis title
           )

  return ( thePlot )
  
}

# --------------------------------------------------
analyze_data = function ( experimentData )  {

  # todo - import the data in way that type coercion isn't necessary.
  
  # Get some stats on differences between outside and basement ambient CH4 concentrations
  experimentData = experimentData %>% drop_na(`include in analysis`) # drop NA rows

  experimentData$`Outside CH4` = as.numeric(experimentData$`Outside CH4`)
  experimentData$`Basement ambient pre-flush CH4` = as.numeric(experimentData$`Basement ambient pre-flush CH4`)
  
  experimentData$BasementOutsideConcDiff = experimentData$`Basement ambient pre-flush CH4` - experimentData$`Outside CH4`
  theData = experimentData %>% drop_na(`Experiment name`)
  theData = theData[ order(theData$BasementOutsideConcDiff, decreasing=T),]
  barplot( theData$BasementOutsideConcDiff, 
           xlab="Experiment locations",
           ylab=expression(Air ~ CH[4] ~ concentration ~ difference ~ (ppmv) ),
           names.arg = theData$`Experiment name`, 
           las=2,
           ylim=c(0,5),
           axis.lty=1, # add the x-axis line
           col="white",
           cex.axis=0.8, # y-axis labels size
           cex.names=0.8 # x-axis labels size
  )
  box()
  
  mtext("Differences between outdoor and basement air CH4 concentrations\n")
  mtext( paste0( 
                "Average ", round(mean(theData$BasementOutsideConcDiff),3),", ",
                "median ", round(median(theData$BasementOutsideConcDiff),3), ", ",
                "min ", round(min(theData$BasementOutsideConcDiff),3), ", ",
                "max ", round(max(theData$BasementOutsideConcDiff),3), " ppmv"),
                side=1,line=4, cex=0.85
  ) 
  cat("Mean outdoor CH4:", round(mean(theData$`Outside CH4`),2),"(n=",nrow(theData),")\n")
  cat("Median outdoor CH4:", round(median(theData$`Outside CH4`),2),"(n=",nrow(theData),")\n")
  
  cat("Mean basement air CH4 concentration:", round(mean(theData$`Basement ambient pre-flush CH4`),2),"(n=",nrow(theData),")\n")
  cat("Median basement air CH4 concentration:", round(median(theData$`Basement ambient pre-flush CH4`),2),"(n=",nrow(theData),")\n")
  
  # filter down to things only required for analysis
  experimentData = experimentData[experimentData$`include in analysis` == "yes",]
  experimentData = experimentData %>% drop_na(`include in analysis`) # drop NA rows
  
  # convert to numerical data
  experimentData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)` = as.numeric( experimentData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`)
  experimentData$`Modeled calculated CH4 flux (cu ft / day)` = as.numeric( experimentData$`Modeled calculated CH4 flux (cu ft / day)`)
  experimentData$`bag samples flux using adjusted volume` = as.numeric( experimentData$`bag samples flux using adjusted volume`)
  experimentData$`simplified bag samples flux using simplified volume` = as.numeric( experimentData$`simplified bag samples flux using simplified volume`)
  experimentData$`basement ambient post-simple flush CH4 ppm` = as.numeric( experimentData$`basement ambient post-simple flush CH4 ppm`)
  experimentData$`CH4 at end of flush (ppm)` = as.numeric(experimentData$`CH4 at end of flush (ppm)`)
  experimentData$`Outside CH4` = as.numeric(experimentData$`Outside CH4`)
  experimentData$`Basement ambient pre-flush CH4` = as.numeric(experimentData$`Basement ambient pre-flush CH4`)
  experimentData$`Total ppm of all basement leaks` = as.numeric( experimentData$`Total ppm of all basement leaks` )
  experimentData$`Simple flush duration (min)` = as.numeric(experimentData$`Simple flush duration (min)`)
  experimentData$`sample bag 2 fill seconds after bag 1` = as.numeric(experimentData$`sample bag 2 fill seconds after bag 1`)
  experimentData$`Adjusted volume (Vadj)` = as.numeric(experimentData$`Adjusted volume (Vadj)`)
  experimentData$`simplified volume (cu ft)` = as.numeric( experimentData$`simplified volume (cu ft)`)
  experimentData$`Home age (years)` = as.numeric(experimentData$`Home age (years)`)
  experimentData$`Basement ambient pre-flush CH4` = as.numeric( experimentData$`Basement ambient pre-flush CH4` )
  experimentData$`1st floor CH4 concentration` = as.numeric( experimentData$`1st floor CH4 concentration` )
  experimentData$`2nd floor CH4 concentration` = as.numeric( experimentData$`2nd floor CH4 concentration` )
  experimentData$`3rd floor CH4 concentration` = as.numeric( experimentData$`3rd floor CH4 concentration` )
  experimentData$`4th floor CH4 concentration` = as.numeric( experimentData$`4th floor CH4 concentration` )
  experimentData$`Volume adjustment` = as.numeric(experimentData$`Volume adjustment`)
  experimentData$`Start time (epoch seconds)` = as.numeric(experimentData$`Start time (epoch seconds)`)
  experimentData$`End time (epoch seconds)` = as.numeric(experimentData$`End time (epoch seconds)`)
  experimentData$`Basement full flush start epoch time` = as.numeric(experimentData$`Basement full flush start epoch time`)
  experimentData$`Basement full flush end epoch time` = as.numeric(experimentData$`Basement full flush end epoch time`)
  experimentData$`Simple flush start epoch time` = as.numeric(experimentData$`Simple flush start epoch time`)
  #experimentData$`Simple flush duration (min)` = as.numeric(experimentData$`Simple flush duration (min)`)
  experimentData$`Max data (sec)` = as.numeric(experimentData$`Max data (sec)`)
  experimentData$`total flush duration` = as.numeric(experimentData$`total flush duration`)

  # do some analysis of how varying timespan, data range and volume effect emission calculations
  if ( analyzeVarying ) { analyze_varying( experimentData ) }
  
  # do flushing analysis
  if ( analyzeFlushes ) {analyze_flushes( experimentData )}
  
  # do analysis of ambient floor measures
  if ( analyzeAmbients ) { analyze_ambients(experimentData )}
  
  # quick hist showing the home age distribution
  par(originalPar)
  theHist = hist(experimentData$`Home age (years)`,
       main = "Histogram of building age\n",
       xlab = "Age (years)" ,
       cex.lab = 1.2,
       cex.axis=1.2, 
       cex.names=1.2
       )
  box()
  mtext( paste0("n = ", sum( theHist$counts),"\n" ))
 
  par(mar = c(5.1, 4.7, 4.1, 2.1 ) ) # bottom, left, top, and right
  
  # and some info about the volume adjustments
  experimentData$vapercent = experimentData$`Volume adjustment`*100
  theData = experimentData[ , c("vapercent","Experiment name"  )]
  theData = theData %>% drop_na(vapercent)
  theData = theData[order(theData$vapercent, decreasing=T),] 
  thePlot= barplot( 
           theData$vapercent,
           xlab="Experiment locations",
           ylab="Volume adjustment %",
           ylim=c(0,20),
           main="Volume adjustments",
           names=theData$`Experiment name`,
           col="white",
           axis.lty=1, # add the tics and an x-axis line
           cex.lab = 1.2,
           cex.axis=1, # y-axis labels size
           cex.names=1, # x-axis labels size
           las=2 # rotate x-axis labels
           )
  box()
  mtext(   paste0( "Mean:",  round(mean(experimentData$vapercent, na.rm=T),1), "%, ",
                   "Median:", round(median(experimentData$vapercent, na.rm=T),1),"%, ",
                   "Min - Max : " ,round(min(experimentData$vapercent, na.rm=T),1),"-",round(max(experimentData$vapercent, na.rm=T),1), ", n=", nrow(experimentData) ),
    side=1, line =4, cex=.85)
  
  # filter out non RCM data
  experimentData = experimentData[experimentData$`Experiment type` == "BCM",]
  
  # Get some stats on duration of experiments
  experimentData$BCMduration = experimentData$`End time (epoch seconds)` - experimentData$`Start time (epoch seconds)`
  cat( "Concentration data was logged for between",
       round(min(experimentData$BCMduration,na.rm=T) / 60), "mins and ",
       round(max(experimentData$BCMduration,na.rm=T) / 60), 
       "(average: " , round(mean(experimentData$BCMduration, na.rm=T) / 60), "mins ", 
       "median: ", round(median(experimentData$BCMduration,na.rm=T) / 60), "mins)\n")
       
  # more stats on differences between outside and basement ambient CH4 concentrations - this time on where RCM was done
  experimentData$BasementOutsideConcDiff = experimentData$`Basement ambient pre-flush CH4` - experimentData$`Outside CH4`
  theData = experimentData %>% drop_na(`Experiment name`)
  theData = theData[ order(theData$BasementOutsideConcDiff, decreasing=T),]
  barplot( theData$BasementOutsideConcDiff, 
      xlab="Experiments",
      ylab=expression(Air ~ CH[4] ~ concentration ~ difference ~ (ppmv) ),
      names.arg = theData$`Experiment name`, 
      las=2,
      ylim=c(0,5),
      axis.lty=1, # add the x-axis line
      col="white",
      cex.axis=0.8, # y-axis labels size
      cex.names=0.8 # x-axis labels size
  )
  
  abline( h = seq(0,5,.5), lty = "dashed", col = "lightgray")
  mtext( paste0("Difference between unflushed basement and outdoors air CH4 concentrations\n",
              "Average ", round(mean(theData$BasementOutsideConcDiff),1),", ",
              "median ", round(median(theData$BasementOutsideConcDiff),1), ", ",
              "min ", round(min(theData$BasementOutsideConcDiff),1), ", ",
              "max ", round(max(theData$BasementOutsideConcDiff),1), " ppmv\n"
         )
  )
  
    
  # RCM - simple slope 
  # remove any rows where simple flux calc is not a number (which will be NA from the previous as.numeric conversion)
  theData = experimentData %>% drop_na(`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`)
  
  
  fluxColumn = "Simple calculated CH4 flux (cu ft / day) (600 seconds span)"
  
  # order it
  orderedData = theData[ order(theData[[fluxColumn]], decreasing=T),]
  
  # plot it - for reporting back, if a name is set, I'll highlight that bar and remove the experiment names from the plot
  if ( ! is.null(reportBackName )) {
     # remove 'R6' experiment
     orderedData = orderedData[orderedData$`Experiment name` != 'R6',] # 
     cols = c("lightgray","deepskyblue")
     pos = orderedData$`Experiment name` == reportBackName
     par(mar = c(7, 4, 2, 2) + 2) 
     thePlot = barplot( 
       as.numeric(orderedData[[fluxColumn]]), 
       ylab = expression(Daily~CH[4]~emissions ~ (ft^3~day^-1)),
       main = "Emissions from your basement relative to other participants",
       col = cols[pos+1],
       ylim=c(0,0.6),
       las=2 # rotate x-axis labels
    )
  }
  else {
    par(mar = c(7, 4, 2, 2) + 2) 
    thePlot = barplot( 
      ylab = expression(Daily~CH[4]~emissions~(ft^3~day^-1)),
      xlab = "Experiments",
      as.numeric(orderedData[[fluxColumn]]), 
      names.arg=c(orderedData$`Experiment name`),  # x-axis labels
      ylim=c(0,0.6),
      las=2, # rotate x-axis labels
      axis.lty=1, # add the x-axis line
      col="lightgoldenrodyellow",
      cex.axis=0.8, # y-axis labels size
      cex.names=0.8 # x-axis labels size
    )
  }
  mtext("RCM 'simple slope' daily emissions")
  
  # horizontal grid
  abline(h = seq(0, 0.5, 0.1),  lty = "dashed", col = "gray")
  
  averageFlux = mean(theData[[fluxColumn]])
  minFlux = min(theData[[fluxColumn]])
  maxFlux = max(theData[[fluxColumn]])
  medianFlux = median(theData[[fluxColumn]])
  totalFlux = sum(theData[[fluxColumn]])
  
  mtext(paste0(
               "Average ", round(averageFlux,2), ", ",
               "Median ",  round(medianFlux,2), ", ", 
               "Min ",     round(minFlux,2), ", ",
               "Max ",     round(maxFlux,2), ",", 
               "Total ",    round(totalFlux,2) 
              ),
        side =1 , # underneath
        line =5
  )
  
  ssFluxes = theData[[fluxColumn]]
  # and a quick histogram to see if there might be a hint of a long tail
  theHist = hist( theData[[fluxColumn]], 
        xlab=fluxColumn, 
        ylab="Density", 
        main=paste0("Histogram of emissions calculated using simple slope"),
        ylim=c(0,10))
  
  abline(h = seq(0,max(theHist$counts)+1, 1), lty = "dashed", col = "gray")
  lines(density(theData[[fluxColumn]]), lwd = 1, col = "chocolate3", lty="dashed")
  cat ("Using simple slope calculation, the largest calculated emission (", orderedData$`Experiment name`[1],") was responsible for ", 
       round(100*max(theData[[fluxColumn]])/sum(theData[[fluxColumn]]),4), "% of total flux from all experiments",
       "or,",  round(max(theData[[fluxColumn]])/sd(theData[[fluxColumn]]),1) , "standard deviations above the mean\n\n")
  
  # RCM - fitted tangent 
  fluxColumn = "Modeled calculated CH4 flux (cu ft / day)"
  
  # order it
  orderedData = theData[ order(theData[[fluxColumn]], decreasing=T),]
  
  par(mar = c(7, 4, 2, 2) + 2) 
  thePlot = barplot( 
    as.numeric(orderedData[[fluxColumn]]), 
    names.arg=c(orderedData$`Experiment name`),  # x-axis labels
    ylab = expression(Daily~CH[4]~emissions~(ft^3~day^-1)),
    xlab = "Experiments",
    ylim=c(0,0.6),
    las=2, # rotate x-axis labels
    axis.lty=1, # add the x-axis line
    col="lightgoldenrodyellow",
    cex.axis=0.8, # y-axis labels size
    cex.names=0.8 # x-axis labels size
  )
  mtext("RCM 'fitted tangent' daily emissions")
  # horizontal grid
  abline(h = seq(0,0.5, 0.1), lty = "dashed", col = "gray")
  
  averageFlux = mean(theData[[fluxColumn]])
  minFlux = min(theData[[fluxColumn]])
  maxFlux = max(theData[[fluxColumn]])
  medianFlux = median(theData[[fluxColumn]])
  totalFlux = sum(theData[[fluxColumn]])
  
  mtext(paste0("Average ", round(averageFlux,2), ", ",
                "Median ",  round(medianFlux,2), ",", 
               "Min ",     round(minFlux,2), ", ",
               "Max ",     round(maxFlux,2), ",", 
               "Total ",    round(totalFlux,2)),
        side =1 , # underneath
        line =5
               
       )
  ftFluxes = theData[[fluxColumn]]
  
  # and a quick histogram to see if there might be a hint of a long tail
  theHist = hist( theData[[fluxColumn]], 
        xlab=fluxColumn, 
        ylab="Density", 
        main=paste0("Histogram of emissions calculated using fitted tangent"),
        ylim=c(0,10)
        )
  
  abline(h = seq(0,max(theHist$counts)+1, 1), lty = "dashed", col = "gray")
  lines(density(theData[[fluxColumn]]), lwd = 1, col = "chocolate3", lty="dashed")
  cat ("Using fitted tangent calculation, the largest calculated emission (", orderedData$`Experiment name`[1],") was responsible for ", 
       round(100*max(theData[[fluxColumn]])/sum(theData[[fluxColumn]]),4), "% of total flux from all experiments",
       "or,",  round(max(theData[[fluxColumn]])/sd(theData[[fluxColumn]]),1) , "standard deviations above the mean\n\n")

  # plot a combo histogram
  theBreaks = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55)
  ssHist = hist(ssFluxes, plot=F, breaks=theBreaks)  # using just a # for breaks doesn't appear to gaurantee the same # of breaks
  ftHist = hist(ftFluxes, plot=F, breaks=theBreaks)  # so I use an explicit vector of breaks
 
  
  par( mfrow=c(1,2), mai= c(1, .8, 0.5, 0.3) ) # # side by side plots, margins to make them closer together,   # bottom, left, top and right
  parSidebySide=par()
  
  # plot simple slope fluxes
  plot( ssHist,
        col=colorsRCM[1],
        ylab="Density",
        xlab=NULL,
        main=NULL,
        ylim=c(0,6),
        xlim=c(0,.55),
        xaxt='n' # plot the x-axis labels after
        )
  mtext("Emissions calculated using simple slope")
  axis( side=1, at=seq(0,0.6, 0.05), labels=seq(0,0.6, 0.05) )
  
  # add the fitted tangent fluxes
  plot( ftHist,
        col=colorsRCM[1],
        ylab=NULL,
        xlab=NULL,
        main=NULL,
        ylim=c(0,6), 
        xlim=c(0,.55),
        xaxt='n' # plot the x-axis labels after
  )
  mtext("Emissions calculated using fitted tangent")
  axis( side=1, at=seq(0,0.6, 0.05), labels=seq(0,0.6, 0.05) )
  par( originalPar )
  mtext( expression(Calculated~emissions~(ft^3~day^-1)), side=1, line=4)
  
  # after trying multhist and ggplot, I ended up with this simple approach to plotting the histogram results side by side
  par(mar=c(7, 4.1, 4.1, 2.1)) # bottom, left, top, right
  value_matrix = matrix(nrow = 2, ncol = 11)
  value_matrix[1,] = ssHist$counts
  value_matrix[2,] = ftHist$counts
  binNumbers=seq(0.05,0.55,0.05)
  binNames = c( "0 - 0.05", "0.05 - 0.1","0.1 - 0.15","0.15 - 0.2","0.2 - 0.25","0.25 - 0.3","0.3 - 0.35","0.35 - 0.4","0.4 - 0.45","0.45 - 0.5","0.5 - 0.55")
  thePlot = barplot(value_matrix, 
                    beside = TRUE,
                    col = colorsRCM, 
                    main = "Histogram of RCM emissions comparing calculation methods",
                    ylim=c(0,6),
                    xlab="",
                    ylab = "Density",
                    axis.lty=1,
                    cex.axis=1,
                    cex.lab=1.2, # xlab size
                    cex.names=1.1 # x-axis labels size
  )
  
  # thePlot object returned from barplot will has x-coords of each bar so I use them to plot the bin labels
  axis( side=1, 
        at=(thePlot[1,] + thePlot[2,]) / 2 , 
        labels=binNames,
        cex.axis=1,
        cex.names=1,
        las=2
        )
  
  legend( x="topright",
          title="Calculation method",
          legend = c("Simple slope", "Fitted tangent"),
          fill = colorsRCM,
          bg = "white",
          cex=1
  )
  mtext(side=1, 
        line=6, 
        expression(Calculated~emissions~(ft^3~day^-1)) ,
        cex=1.15)
  
  x=1
  par( originalPar )
  
  
  # correlation between both methods
  # ie how well is the simple flux predicted by the modeled flux
  theModel = lm( `Simple calculated CH4 flux (cu ft / day) (600 seconds span)` ~ `Modeled calculated CH4 flux (cu ft / day)`, data=theData)

  # Compare both calculation method results
  
  # create an % difference of modeled to simple
  par(mar = c(7, 4, 2, 2) + 3) 
  
  theData$fluxCalcsPercentDiff = round(  100 * ( ( theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)` / theData$`Modeled calculated CH4 flux (cu ft / day)`) - 1), 0)
  
  cat ( "Calculated RCM daily emissions ranged between  ", 
        round( min( theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`, theData$`Modeled calculated CH4 flux (cu ft / day)` ),2),
        " and ", 
        round(max( theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`, theData$`Modeled calculated CH4 flux (cu ft / day)` ),2),
        " cu ft / day\n"
  )
  cat ( "Mean RCM fitted tangent daily emissions: ", round( mean( theData$`Modeled calculated CH4 flux (cu ft / day)`) , 2), " cu ft /day, n=", nrow(theData),"\n" )
  cat ( "Mean RCM simple slope daily emissions: ", round( mean( theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`) , 2), " cu ft /day, n=",nrow(theData) ,"\n" )

        
  theData = theData[ order(theData$fluxCalcsPercentDiff, decreasing=T), ]
  
  value_matrix = matrix(nrow = 2, ncol = nrow(theData))
  value_matrix[1,] = theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`
  value_matrix[2,] = theData$`Modeled calculated CH4 flux (cu ft / day)`
  
  thePlot = barplot(value_matrix, 
                    names.arg = theData$`Experiment name`, 
                    beside = TRUE, 
                    col = colorsRCM, #c("peachpuff", "skyblue"), 
                    main = "RCM daily emissions",
                    ylim=c(0,0.6),
                    #xlab="Experiments",
                    xlab="Experiment locations",
                    ylab = expression(Daily~CH[4]~emissions~(ft^3~day^-1)),
                    las=2, # rotate x-axis labels
                    cex.lab=1, # xlab size
                    axis.lty=1, # add the x-axis line
                    cex.axis=.9, # y-axis labels size
                    cex.names=.9  # , add=T # x-axis labels size
          )
  
  # for now just plot again to put gridlines under. I tried panel.first, or plot empty, abline, plot etc and wasted too much time again
  thePlot = barplot(value_matrix, 
                    names.arg = theData$`Experiment name`, 
                    beside = TRUE, 
                    col = colorsRCM, #c("peachpuff", "skyblue"), 
                    main = "RCM daily emissions",
                    ylim=c(0,0.6),
                    #xlab="Experiments",
                    xlab="Experiment locations",
                    ylab = expression(Daily~CH[4]~emissions~(ft^3~day^-1)),
                    las=2, # rotate x-axis labels
                    cex.lab=1, # xlab size
                    axis.lty=1, # add the x-axis line
                    cex.axis=.9, # y-axis labels size
                    cex.names=.9, # x-axis labels size
                    add = TRUE
  )
  
  legend( x="topright",
          title="Calculation method",
          legend = c("Simple slope", "Fitted tangent"),
          fill = colorsRCM, #c("peachpuff", "skyblue"),
          bg = "white",
          cex=0.9
  )
  
  # add the % differences above the grouped bars 
  text( x = apply(thePlot,2,mean),   # thePlot contains x coords for each bar pair - this finds midpoint of each bar pair
        y = apply(value_matrix, 2, max) + 0.025,  # pick the highest y value for the pair, and add a fixed amount - this makes the labels consistent spatially
        labels = paste0(theData$fluxCalcsPercentDiff,"%"), 
        cex=0.9 )
  
  
  
  # add a subtitle with some stats
  mtext(  paste0( 
    "Mean difference: ", round( mean(theData$fluxCalcsPercentDiff), 0),   "%, ",
    "Median difference: ", round( median(theData$fluxCalcsPercentDiff), 0), "%, ",
    "Min difference: ",     round( min(theData$fluxCalcsPercentDiff), 0),    "%, ",
    "Max difference: ",     round( max(theData$fluxCalcsPercentDiff), 0), "%",
    "\nR-squared: ", round(summary(theModel)$r.squared,3),", p=",signif(Regressionp(theModel), digits=3),
    " (Ordered by difference in descending order)"),
    side =1 , line =5 , cex=.8 

  )
  
  # and a histogram of the differences percentages
  theHist = hist( theData$fluxCalcsPercentDiff, 
                  xlab="Difference %", 
                  ylab="Density", 
                  main=paste0("Histogram of % differences between simple slope and fitted tangent calcs"),
                  breaks = 10
                  ,ylim=c(0,12), xlim=c(0,250)
  )
  
  # Bag samples
  # create a plot of calculated fluxes from bag sampling method with adjusted volume , compared to simplified volume
  # first drop any rows where bag samples were not taken
  theData = theData %>% drop_na(`simplified bag samples flux using simplified volume`)
  
  # some basic stats about bag sampling
  bagSamplingBasics = theData[theData$`bag samples done same day or different day to BCM`=="same",]
  bagSamplingBasics$flushedToOutsidePercentDifference = round( 100 * (bagSamplingBasics$`basement ambient post-simple flush CH4 ppm`-bagSamplingBasics$`Outside CH4`) / bagSamplingBasics$`Outside CH4`, 0)
  cat("ASK was performed on the same day ahead of BCM in ", nrow(bagSamplingBasics), " of the experiments\n",
      "Post-simple flushing (2 box fans) basement CH4 concentrations ranged from ", min(bagSamplingBasics$flushedToOutsidePercentDifference),"to",max(bagSamplingBasics$flushedToOutsidePercentDifference),"% difference to ambient outside CH4 concentrations taken at the start of the experiment.\n",
      "The median difference was ", round(median(bagSamplingBasics$flushedToOutsidePercentDifference),1),"% with average of ",round(mean(bagSamplingBasics$flushedToOutsidePercentDifference),1),"%\n",
      "Simple flushing durations ranged from ", min(bagSamplingBasics$`Simple flush duration (min)`)," to ", max(bagSamplingBasics$`Simple flush duration (min)`), " minutes, with average ", round(mean(bagSamplingBasics$`Simple flush duration (min)`),0)," and median ",round(median(bagSamplingBasics$`Simple flush duration (min)`),0),"\n",
      "Time between bag samples : average: ", round(mean(bagSamplingBasics$`sample bag 2 fill seconds after bag 1`),0)," sec, median ", round(median(bagSamplingBasics$`sample bag 2 fill seconds after bag 1`),0), " sec\n"
      )
  bagSamplingBasics = theData[theData$`bag samples done same day or different day to BCM`=="different",]
  bagSamplingBasics$flushedToOutsidePercentDifference = round( 100 * (bagSamplingBasics$`basement ambient post-simple flush CH4 ppm`-bagSamplingBasics$`Outside CH4`) / bagSamplingBasics$`Outside CH4`, 0)
  cat("\nASK was performed on a different day to BCM in ", nrow(bagSamplingBasics), " of the experiments\n",
      "Post-simple flushing (2 box fans) basement CH4 concentrations were not taken\n",
      "Simple flushing durations ranged from ", min(bagSamplingBasics$`Simple flush duration (min)`)," to ", max(bagSamplingBasics$`Simple flush duration (min)`), " minutes, with average ", round(mean(bagSamplingBasics$`Simple flush duration (min)`),0)," and median ",round(median(bagSamplingBasics$`Simple flush duration (min)`),0),"\n",
      "Time between bag samples : average: ", round(mean(bagSamplingBasics$`sample bag 2 fill seconds after bag 1`),0)," sec, median ", round(median(bagSamplingBasics$`sample bag 2 fill seconds after bag 1`),0), " sec\n"
  )
  
  # calculate differences %'s ( simple flux differences are the same as calculated vol differences since flux is directly prop. to vol used )
  theData$bagSampleFluxCalcsPercentDiff = round( 100 * ( (theData$`simplified bag samples flux using simplified volume` / theData$`bag samples flux using adjusted volume`) -1 ), 0)

  cat ( "Calculated SRCM daily emissions ranged between  ", 
        round( min( theData$`simplified bag samples flux using simplified volume`, theData$`bag samples flux using adjusted volume` ),2),
        " and ", 
        round(max( theData$`simplified bag samples flux using simplified volume`, theData$`bag samples flux using adjusted volume` ) ,2),
        " cu ft / day\n"
  )
  cat ( "Mean SRCM Vsim daily emissions: ",       round( mean( theData$`simplified bag samples flux using simplified volume`) , 2), " cu ft /day, n=", nrow(theData),"\n" )
  cat ( "Mean SRCM Vadj slope daily emissions: ", round( mean( theData$`bag samples flux using adjusted volume`) , 2), " cu ft /day, n=",nrow(theData) ,"\n" )
  
    
  theData = theData[ order(theData$bagSampleFluxCalcsPercentDiff, decreasing=T), ] 
  theModel = lm( `simplified bag samples flux using simplified volume` ~ `bag samples flux using adjusted volume`, data=theData)
  
  # create an % difference of bag sampling fluxes using adjusted and simplified volumes
  value_matrix = matrix(nrow = 2, ncol = nrow(theData))
  value_matrix[1,] = theData$`simplified bag samples flux using simplified volume`
  value_matrix[2,] = theData$`bag samples flux using adjusted volume`
  
  thePlot = barplot(value_matrix, 
                    names.arg = theData$`Experiment name`, 
                    beside = TRUE, 
                    col = colorsSRCM, #c("darkcyan", "darkgoldenrod1"), 
                    #legend.text = c("Simplified volume (Vsim)", "Adjusted volume (Vadj)"),
                    main = "SRCM daily emissions",
                    ylim=c(0,0.6),
                    ylab = expression(Daily~CH[4]~emissions ~ (ft^3~day^-1)),
                    #xlab = "Experiments",
                    xlab = "Experiment locations",
                    #args.legend = list( cex=0.8 ), # scale legend text
                    cex.lab=1, # xlab size
                    axis.lty=1, # add the tics and an x-axis line
                    cex.axis=.9, # y-axis labels size
                    cex.names=.9, # x-axis labels size
                    las=2 # rotate x-axis labels
  )
  
  # same dumb approac to put gridline under ...
  thePlot = barplot(value_matrix, 
                    names.arg = theData$`Experiment name`, 
                    beside = TRUE, 
                    col = colorsSRCM, #c("darkcyan", "darkgoldenrod1"),  
                    main = "SRCM daily emissions",
                    ylim=c(0,0.6),
                    ylab = expression(Daily~CH[4]~emissions ~ (ft^3~day^-1)),
                    xlab = "Experiment locations",
                    axis.lty=1, # add the tics and an x-axis line
                    cex.lab=1, # xlab size
                    cex.axis=.9, # y-axis labels size
                    cex.names=.9, # x-axis labels size
                    las=2, # rotate x-axis labels
                    add=TRUE
  )
  legend( x="topright",
          title="Volume method",
          legend = c(expression(Simplified~volume~(V[sim])), expression(Adjusted~volume~(V[adj])) ),
          fill = colorsSRCM,
          bg = "white",
          cex=0.9
  )
  
  # add the % differences above the grouped bars 
  text( x = apply(thePlot,2,mean),   # thePlot contains x coords for each bar pair - this finds midpoint of each bar pair
        y = apply(value_matrix, 2, max) + 0.025,  # pick the highest y value for the pair, and add a fixed amount - this makes the labels consistent spatially
        labels = paste0(theData$bagSampleFluxCalcsPercentDiff,"%"), 
        cex=0.9 )
  
  # add a subtitle with some stats
  mtext(  paste0(
                  "Average difference: ", round( mean(theData$bagSampleFluxCalcsPercentDiff), 0),   "%, ",
                  "Median difference: ", round( median(theData$bagSampleFluxCalcsPercentDiff), 0), "%, ",
                  "Min difference: ",     round( min(theData$bagSampleFluxCalcsPercentDiff), 0),    "%, ",
                  "Max difference: ",     round( max(theData$bagSampleFluxCalcsPercentDiff), 0), "%",
                  "\nR-squared: ", round(summary(theModel)$r.squared,3),", p=",signif(Regressionp(theModel), digits=3),
                  " (Ordered by % difference, descending)" ),
          side=1, line = 5, cex=.8 
  )
  
  # combo histogram
  vsimFluxes = theData$`simplified bag samples flux using simplified volume`
  vadjFluxes = theData$`bag samples flux using adjusted volume`
  cat ("SRCM VsimFluxes mean / median :", mean(vsimFluxes),"/", median(vsimFluxes), "(n=",length(vsimFluxes),")\n")
  cat ("SRCM VadjFluxes mean / median :", mean(vadjFluxes),"/", median(vadjFluxes), "(n=",length(vadjFluxes),")\n")
  
  vsimHist = hist(vsimFluxes, plot=F, breaks=10)
  vadjHist = hist(vadjFluxes, plot=F, breaks=10)

  earlierPar = par()
  par(parSidebySide)
  
  # plot simple slope fluxes
  plot( vsimHist,
        col=colorsSRCM[1],#col="#0000ff66",
        ylab="Density",
        xlab=NULL,
        main=NULL,
        ylim=c(0,6),
        xlim=c(0,.55),
        xaxt='n' # plot the x-axis labels after
  )
  mtext( expression(Emissions ~ calculated ~ using ~ V[sim]) )
  axis( side=1, at=seq(0,0.6, 0.05), labels=seq(0,0.6, 0.05) )
  
  # add the fitted tangent fluxes
  plot( vadjHist,
        col=colorsSRCM[1],#col="#0000ff66",
        ylab=NULL,
        xlab=NULL,
        main=NULL,
        ylim=c(0,6), 
        xlim=c(0,.55),
        xaxt='n' # plot the x-axis labels after
  )
  mtext( expression(Emissions ~ calculated ~ using ~ V[adj]) )
  axis( side=1, at=seq(0,0.6, 0.05), labels=seq(0,0.6, 0.05) )
  par( originalPar )
  mtext( expression(Calculated~emissions~(ft^3~day^-1)), side=1, line=4)
  
  par(mar=c(7, 4.1, 4.1, 2.1)) # bottom, left, top, right
  value_matrix = matrix(nrow = 2, ncol = 11)
  value_matrix[1,] = vsimHist$counts
  value_matrix[2,] = vadjHist$counts
  binNumbers=seq(0.05,0.55,0.05)
  binNames = c( "0 - 0.05", "0.05 - 0.1","0.1 - 0.15","0.15 - 0.2","0.2 - 0.25","0.25 - 0.3","0.3 - 0.35","0.35 - 0.4","0.4 - 0.45","0.45 - 0.5","0.5 - 0.55")
  thePlot = barplot(value_matrix, 
                    beside = TRUE,
                    col = colorsSRCM, 
                    main = "Histogram of SRCM emissions comparing volumes used",
                    ylim=c(0,6),
                    xlab="",
                    ylab = "Density",
                    cex.axis=1,
                    cex.lab=1.2, # xlab size
                    cex.names=1.1, # x-axis labels size
                    axis.lty=1 # add the x-axis line
  )
  
  # thePlot object returned from barplot will has x-coords of each bar so I use them to plot the bin labels
  axis( side=1, at=(thePlot[1,] + thePlot[2,]) / 2 , labels=binNames, las=2, cex.axis=1, cex.names=1 )
  
  legend( x="topright",
          title="Volume method",
          legend = c(expression(Simplified~volume~(V[sim])), expression(Adjusted~volume~(V[adj]))),
          fill = colorsSRCM,
          bg = "white",
          cex=1
  )
  
  mtext(side=1, line=6, expression(Calculated~emissions~(ft^3~day^-1)), cex=1.15)
  
  par( originalPar )
  
  # Compare bag (simplified volume) flux with simple slope flux
  # calculate differences %'s
  theData$bagSampleVersusSimpleSlopeFluxCalcsPercentDiff =   round( 100 * ( (theData$`simplified bag samples flux using simplified volume` / theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`) - 1) , 0)
  theData$bagSampleVersusFittedTangentFluxCalcsPercentDiff = round( 100 * ( (theData$`simplified bag samples flux using simplified volume` / theData$`Modeled calculated CH4 flux (cu ft / day)`) - 1) , 0)
  
  # also look at the absolute differences
  theData$bagSampleVersusSimpleSlopeFluxCalcsAbsoluteDiff = abs( theData$`simplified bag samples flux using simplified volume` - theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)` )
  
  
  theData = theData[ order(theData$bagSampleVersusSimpleSlopeFluxCalcsPercentDiff, decreasing=T), ] 
  theModel = lm( `simplified bag samples flux using simplified volume` ~ `Simple calculated CH4 flux (cu ft / day) (600 seconds span)`, data=theData)
  
  # create a plot of % difference of bag sampling fluxes with simple slope fluxes
  value_matrix = matrix(nrow = 2, ncol = nrow(theData))
  value_matrix[1,] = theData$`simplified bag samples flux using simplified volume`
  value_matrix[2,] = theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`
  
  par(mar = c(6, 5, 4.1, 2.1 ) ) # bottom, left, top, and right  
  thePlot = barplot(value_matrix, 
                    names.arg = theData$`Experiment name`, 
                    beside = TRUE, 
                    col = c(colorsSRCM[1],colorsRCM[1]),# c("red", "blue"), 
                    legend.text = c(expression(Bag~method~(using~V[sim])), "RCM (using simple slope)"),
                    args.legend = list( x="topright", cex=0.9 , title="Calculation method" ),
                    main=NULL,
                    ylim=c(0,0.6),
                    ylab = expression(Daily~CH[4]~emissions ~ (ft^3~day^-1)),
                    #xlab = "Experiments",
                    xlab = "Experiment locations",
                    axis.lty=1, # add the tics and an x-axis line
                    cex.axis=1, # y-axis labels size
                    cex.names=1, # x-axis labels size
                    cex.lab=1.2, # xlab size
                    las=2 # rotate x-axis labels,
  )

  # no comment...
  thePlot = barplot(value_matrix, 
                    names.arg = theData$`Experiment name`, 
                    beside = TRUE, 
                    col = c(colorsSRCM[1],colorsRCM[1]),# c("red", "blue"),  
                    legend.text = c(expression(Bag~method~(using~V[sim])), "RCM (using simple slope)"),
                    args.legend = list( x="topright", cex=0.9 , title="Method" ),
                    main=NULL,
                    ylim=c(0,0.6),
                    ylab = expression(Daily~CH[4]~emissions ~ (ft^3~day^-1)),
                    xlab = "Experiment locations",
                    axis.lty=1, # add the tics and an x-axis line
                    cex.axis=1, # y-axis labels size
                    cex.names=1, # x-axis labels size
                    cex.lab=1.2, # xlab size
                    las=2, # rotate x-axis labels
                    add=T
  )
  
  # add the % differences above the grouped bars 
  text( x = apply(thePlot,2,mean),   # thePlot contains x coords for each bar pair - this finds midpoint of each bar pair
        y = apply(value_matrix, 2, max) + 0.025,  # pick the highest y value for the pair, and add a fixed amount - this makes the labels consistent spatially
        labels = paste0(theData$bagSampleVersusSimpleSlopeFluxCalcsPercentDiff,"%"),
        cex=1 )
  
  mtext ( expression(Comparing~bag~method~(using~V[sim])~and~RCM~(using~simple~slope)~daily~emissions~calculations), line=1 )
  # add a subtitle with some stats
  mtext(   paste0(
    "Average difference: ", round( mean(theData$bagSampleVersusSimpleSlopeFluxCalcsPercentDiff), 0),   "%, ",
    "Median difference: ", round( median(theData$bagSampleVersusSimpleSlopeFluxCalcsPercentDiff), 0), "%, ",
    "Min difference: ",     round( min(theData$bagSampleVersusSimpleSlopeFluxCalcsPercentDiff), 0),    "%, ",
    "Max difference: ",     round( max(theData$bagSampleVersusSimpleSlopeFluxCalcsPercentDiff), 0), "%",
    "\nR-squared: ", round(summary(theModel)$r.squared,3),", p=",signif(Regressionp(theModel), digits=3),
    " (Ordered by % difference, descending)"
  ), side=1, line = 5, cex=.8 )
  
  # create a plot of the the absolute differences between bag sample and simple slope fluxes
  thePlot = barplot(theData$bagSampleVersusSimpleSlopeFluxCalcsAbsoluteDiff, 
                    names.arg = theData$`Experiment name`, 
                    beside = TRUE, 
                    args.legend = list( x="topleft" ),
                    main = "Absolute differences between ASK fluxes and BCM Simple Slope fluxes",
                    ylim=c(0,0.1),
                    ylab = "CH4 Flux difference (cu ft / day)",
                    las=2 # rotate x-axis labels
  )
  abline(h = seq(0,0.1, 0.01), lty = "dashed", col = "gray")
  
  # So how does a scatter plot of the relative difference between the bag sampling and simple slope fluxes vary with size of the flux (as calculated with simple slope)
  # ie does for example largest flux mean less error
  if ( 0 ) {
     thePlot = plot( main = "Exploring relative difference between SRCM & RCM emissions, and emission size",
                  
                  x = theData$`Modeled calculated CH4 flux (cu ft / day)`, # RCM fitted tangent
                  y = theData$bagSampleVersusFittedTangentFluxCalcsPercentDiff,
                  xlab = expression(RCM ~ fitted ~ tangent ~ calculated ~ flux ~ (ft^3~day^-1) ),
                  ylab = "SRCM / RCM fitted (%)",
                  axis.lty=1, # add the tics and an x-axis line
                  cex.axis=0.8, # y-axis labels size
                  cex.names=0.8, # x-axis labels size
                  xlim = c(0,0.6),
                  ylim = c(-100,400),
                  pch=21, col="black", bg="red"
     )
     abline(h=0, lty="dashed", col="gray")      
  }
  
  
  # xyplot makes it 'easier' to add fitted curve 
  thePlotFT = xyplot( 
    #main = "Relative difference between SRCM & RCM (fitted tangent) emissions, and emission size",
    main = "B. Fitted tangent",
    theData$bagSampleVersusFittedTangentFluxCalcsPercentDiff ~ theData$`Modeled calculated CH4 flux (cu ft / day)` ,
          # these 3 lines add a nice colored loess fit curve
          #type=c("smooth", "p"), 
          #key=list(corner=c(.95,.05),lines=list(col=c("orange"), lty=c("dashed"), lwd=2),text=list(c("Smoothed fit (loess)"))),
          #col.line = "orange", lty='dashed', lwd=2, # controls fitted line      
          type=c("p"),
          scales = list(tck = c(1,0), x=list(cex=1), y=list(cex=1)), # turn off tick marks on top and right borders, and scale labels
          par.settings = list(plot.symbol = list(pch = 19,col=c("red"))), # adjust color of points
          xlab = list( expression(RCM ~ fitted ~ tangent ~ calculated ~ daily ~ emissions ~ (ft^3~day^-1) ), fontsize=13),
          ylab = "",
          xlim = c(0,0.52),
          ylim = c(-150,420),
          panel=function(...) {  # equiv to abline h=0 sort of thing
              panel.xyplot(...)
              panel.grid(h=-1,v=-1)
              panel.abline(h=0, col="gray", lty='dashed', lwd=2)
              ltext(
                x=theData$`Modeled calculated CH4 flux (cu ft / day)`,
                y=theData$bagSampleVersusFittedTangentFluxCalcsPercentDiff,
                #labels=paste0(theData$`Experiment name`, " " , theData$bagSampleVersusFittedTangentFluxCalcsPercentDiff),
                labels=theData$`Experiment name`,
                pos=1,
                cex=0.9
              )
          }
    )

  # look at the error plot but comparing RCM simple slope
  thePlotSS = xyplot( 
    #main = "Relative difference between SRCM & RCM (simple slope) emissions, and emission size",
    main = "A. Simple slope",
    theData$bagSampleVersusSimpleSlopeFluxCalcsPercentDiff ~ theData$`Modeled calculated CH4 flux (cu ft / day)` ,
    # these 3 lines add a nice colored loess fit curve
    #type=c("smooth", "p"),  
    #key=list(corner=c(.95,.05), lines=list(col=c("blue"), lty=c("dashed"), lwd=2), text=list(c("Smoothed fit (loess)"))),
    #col.line = "blue", lty='dashed', lwd=2, # controls fitted line
    type=c("p"),
    scales = list(tck = c(1,0), x=list(cex=1), y=list(cex=1)), # turn off tick marks on top and right borders , and scale labels
    par.settings = list(plot.symbol = list(pch = 19,col=c("red"))), # adjust color of points
    xlab = list( expression(RCM ~ simple ~ slope ~ calculated ~ daily ~ emissions ~ (ft^3~day^-1) ), fontsize=13),
    ylab=list("Bag method emissions divided by RCM emissions (%)", fontsize=13),
    xlim = c(0,0.52),
    ylim = c(-100,150),
    panel=function(...) {  # equiv to abline h=0 sort of thing
      panel.xyplot(...)
      panel.grid(h=-1,v=-1)
      panel.abline(h=0, col="gray", lty='dashed', lwd=2)
      ltext(
        x=theData$`Modeled calculated CH4 flux (cu ft / day)`,
        y=theData$bagSampleVersusSimpleSlopeFluxCalcsPercentDiff,
        labels=theData$`Experiment name`,
        pos=1,
        cex=0.9
      )
    }
  )
  
  print( plot_grid( thePlotSS, thePlotFT, ncol=2, labels=NULL, label_size=10, hjust=-0.2) )

  # Exploring simple air flushing - how does simple flushing compare with thorough RCM flushing 
  # calculate differences %'s
  # room ambient post-simple flush CH4 ppm,  CH4 at end of flush (ppm)
  theData = experimentData %>% drop_na(`basement ambient post-simple flush CH4 ppm`)
  
  theData$simpleVersusThoroughFlushingCH4 = round(  100 * ( ( theData$`basement ambient post-simple flush CH4 ppm` / theData$`CH4 at end of flush (ppm)`) -1) , 0)
  
  theData = theData[ order(theData$simpleVersusThoroughFlushingCH4, decreasing=T), ] 
  theModel = lm( `basement ambient post-simple flush CH4 ppm` ~ `CH4 at end of flush (ppm)`, data=theData)
  
  # create a plot of % difference of bag sampling fluxes with simple slope fluxes
  value_matrix = matrix(nrow = 2, ncol = nrow(theData))
  value_matrix[1,] = theData$`basement ambient post-simple flush CH4 ppm`
  value_matrix[2,] = theData$`CH4 at end of flush (ppm)`
  thePlot = barplot(value_matrix, 
                    ylab = expression(Post~-~flush~air ~CH[4] ~ concentration ~ (ppmv)),
                    xlab = "Experiment",
                    names.arg = theData$`Experiment name`, 
                    beside = TRUE,
                    col = c("coral", "cornflowerblue"), 
                    legend.text = c("SRCM simple flush", "RCM flush"),
                    args.legend = list( x="bottomright", cex=0.8  ),
                    main = expression(Comparing~post~-~flush~air ~CH[4] ~ concentrations ~ (ppmv)~between~RCM~and~SRCM),
                    ylim=c(0, max(theData$`basement ambient post-simple flush CH4 ppm`)+0.5),
                    axis.lty=1, # add the tics and an x-axis line
                    cex.axis=0.8, # y-axis labels size
                    cex.names=0.8, # x-axis labels size
                    las=2 # rotate x-axis labels
  )
  # horizontal grid
  abline(h = seq(0,max(theData$`basement ambient post-simple flush CH4 ppm`)+0.3, 0.5), lty = "dashed", col = "gray")
  
  # add the % differences above the grouped bars 
  text( x= thePlot[1,],
        y = theData$`basement ambient post-simple flush CH4 ppm`+0.1, 
        labels = paste0("  ", theData$simpleVersusThoroughFlushingCH4,"%"), 
        cex=0.7)
  
  # add a subtitle with some stats
  mtext( paste0(
    "Average difference: ", round( mean(theData$simpleVersusThoroughFlushingCH4), 0),   "%, ",
    "Median difference: ", round( median(theData$simpleVersusThoroughFlushingCH4), 0), "%, ",
    "Min difference: ",     round( min(theData$simpleVersusThoroughFlushingCH4), 0),    "%, ",
    "Max difference: ",     round( max(theData$simpleVersusThoroughFlushingCH4), 0), "%",
    "\nR-squared: ", round(summary(theModel)$r.squared,3),", p=",signif(Regressionp(theModel), digits=3),
    " (Ordered by % difference, descending)"
  ),  side=1, line = 5, cex=.8 )
  
  
  # Comparing BCM post flush CH4 with outdoor ambient CH4 for just those experiments where only full flush was done, and not preceded by simple flush
  theData = experimentData %>% drop_na(`CH4 at end of flush (ppm)`)
  theData = theData %>% drop_na(`Outside CH4`)
  theData = theData[theData$`flush type (combination means simple followed by full flush; fullonly means just full flush not preceded by simple)` == "fullonly",]
  theData$relativeBCMtoOutsideFlush = round(  (theData$`CH4 at end of flush (ppm)` / theData$`Outside CH4`) * 100 , 0)
  theData$absoluteBCMtoOutsideFlush = theData$`CH4 at end of flush (ppm)` - theData$`Outside CH4`
  cat ("\n\nComparing BCM post flush CH4 with outdoor ambient CH4 for just those experiments where only full flush was done, and not preceded by simple flush:\n")
  
  theRelSummary = paste0(
    "\tRelative difference: ", round( min(theData$relativeBCMtoOutsideFlush), 0),  " to ", round( max(theData$relativeBCMtoOutsideFlush), 0), " , ", 
    "Average : ", round( mean(theData$relativeBCMtoOutsideFlush), 0),  " ",
    "Median : ", round( median(theData$relativeBCMtoOutsideFlush), 0), " %\n"
  )
  
  theAbsSummary = paste0(
    "\tAbsolute difference: ", min(theData$absoluteBCMtoOutsideFlush),  " to ", max(theData$absoluteBCMtoOutsideFlush), " , ", 
    "Median : ", median(theData$absoluteBCMtoOutsideFlush)," ",
    "Average : ", mean(theData$absoluteBCMtoOutsideFlush), " ppmv\n"
  )
  
  cat (theRelSummary, theAbsSummary)
  # don't need a graph for now
  
  # Comparing RCM post flush CH4 with outdoor ambient CH4 
  # NOTE this is regardless of whether flushing was just full flush, and/or simple followed by full flush
  theData = experimentData %>% drop_na(`CH4 at end of flush (ppm)`)
  theData = theData %>% drop_na(`Outside CH4`)
  theData$outsideVersusBCMflushedCH4 = round(  (theData$`CH4 at end of flush (ppm)` / theData$`Outside CH4`) * 100 , 0) # relative measure
  theData$abs_outsideVersusBCMflushedCH4 = theData$`CH4 at end of flush (ppm)` - theData$`Outside CH4` # absolute measure
  
  theData = theData[ order(theData$outsideVersusBCMflushedCH4, decreasing=T), ] 
  theModel = lm( `CH4 at end of flush (ppm)` ~ `Outside CH4`, data=theData) 
     
  theSummary = paste0(
       "Median difference: ", round( median(theData$outsideVersusBCMflushedCH4), 0), "%, ",
       "Average difference: ", round( mean(theData$outsideVersusBCMflushedCH4), 0),   "%, ",
       "Min difference: ",     round( min(theData$outsideVersusBCMflushedCH4), 0),    "%, ",
       "Max difference: ",     round( max(theData$outsideVersusBCMflushedCH4), 0), "%",
       "\nR-squared: ", round(summary(theModel)$r.squared,3),", p=",signif(Regressionp(theModel), digits=3),
       " (Ordered by % difference, descending)"
     )
     
  value_matrix = matrix(nrow = 2, ncol = nrow(theData))
  value_matrix[1,] = theData$`CH4 at end of flush (ppm)`
  value_matrix[2,] = theData$`Outside CH4`
  thePlot = barplot(value_matrix, 
                    names.arg = theData$`Experiment name`, 
                    beside = TRUE, 
                    col = c("blue", "yellow"), 
                    legend.text = c("Basement CH4 ffter BCM flush", "Outside CH4"),
                    args.legend=list(cex=0.8),
                    main = "Comparing outside CH4 with basement CH4 after BCM flushing",
                    ylim=c(0, max(theData$`CH4 at end of flush (ppm)`)+0.5),
                    ylab = "CH4 ppmv",
                    las=2 # rotate x-axis labels
  )
  abline(h = seq(0,max(theData$`CH4 at end of flush (ppm)`)+0.3, 0.5), lty = "dashed", col = "gray")
  text( x= thePlot[1,],
        y = theData$`CH4 at end of flush (ppm)`+0.1, 
        labels = paste0("  ", theData$outsideVersusBCMflushedCH4,"%"), 
        cex=0.7)
  mtext(  cex=0.9, theSummary)

  # Exploring how basement pre-flush CH4 concentration might relate to RCM calculated fitted tangent flux
  # variables : Simple calculated CH4 flux (cu ft / day) (600 seconds span), Basement ambient pre-flush CH4
  # no - variables are Modeled calculated CH4 flux (cu ft / day) and Basement ambient pre-flush CH4
  theData = experimentData %>% drop_na(`Modeled calculated CH4 flux (cu ft / day)`)
  theData = theData %>% drop_na(`Basement ambient pre-flush CH4`)

  # explore linear correlation
  theModel = lm( `Modeled calculated CH4 flux (cu ft / day)` ~ `Basement ambient pre-flush CH4`, data = theData)
  theModelR2 = round(summary(theModel)$r.squared,3)
  theTitle = expression("Basement pre-flush ambient air" ~ CH[4] ~"concentration and emissions calculated using RCM fitted tangent")
  theTitle = "B. Fitted tangent"
  theSubtitle = expr( R^2==!!round(summary(theModel)$r.squared,3)~ ", "~ n==!!nrow(theData)~ ", "~ p==!!signif(Regressionp(theModel), digits=3))
  
  thePlot = plot( x = theData$`Basement ambient pre-flush CH4`,
                  y = theData$`Modeled calculated CH4 flux (cu ft / day)`,
                  main = theTitle,
                  xlab = expression(Unflushed~basement ~ ambient~air~CH[4]~concentration~(ppm)),
                  ylab = expression( RCM~titted ~ tangent ~ daily~CH[4]~emissions ~ (ft^3~day^-1) ) , 
                  ylim=c(0,max(theData$`Modeled calculated CH4 flux (cu ft / day)`+0.1)),
                  xlim=c(min(theData$`Basement ambient pre-flush CH4`), max(theData$`Basement ambient pre-flush CH4`)+0.5),
                  pch=21, col="blue", bg="lightblue"
  )
  abline(v = seq(0, 7, 0.5), lty = "dashed", col = "gray")
  abline(h = seq(0,max(theData$`Basement ambient pre-flush CH4`)+0.5, .05), lty = "dashed", col = "gray")
  mtext( theSubtitle )
  abline( theModel, col="green") # plot fit line

  thePlotAmbFT = ggplot( theData, 
                         aes(`Basement ambient pre-flush CH4`, `Modeled calculated CH4 flux (cu ft / day)`, label=`Experiment name`) ) + 
    ggtitle( label = theTitle, subtitle = theSubtitle ) + 
    geom_point( size=2 ) + 
    geom_text(hjust=0.7, vjust=-0.5, size=4 ) +
    labs( x=expression( Unflushed~basement ~ ambient~air~CH[4]~concentration~(ppmv)), y="")+
          #y=expression( Daily~CH[4]~emissions~calculated~using~RCM~fitted~tangent~ (ft^3~day^-1) )) +  # x/y-axis labels
    geom_smooth( method=lm) +
    ylim(-.05,.5) +
    theme( axis.text.x=element_text( size=12),
           axis.text.y=element_text( color="black", size=12 ), # y-axes tick text color
           panel.background = element_rect(fill = "white",colour = "black", size = 1, linetype = "solid"),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # turn off grid
           legend.key = element_blank(), # make the backgrounds of the legend items not colored (gray by default)
           legend.background = element_rect( fill="white", size=0.5, linetype="solid", colour ="gray"), # legend border and fill
           plot.title = element_text(hjust = 0, size = 16), # justify title
           plot.subtitle = element_text(hjust = 0, size=14), # justify subtitle
           axis.title = element_text( color="black", face="bold"), # axes labels - note with expression(), need to use bold() inside it to make this work
           axis.title.y = element_text(size = 13), # y-axis title
           axis.title.x = element_text(size = 14)
    )

  theModel = lm( `Simple calculated CH4 flux (cu ft / day) (600 seconds span)` ~ `Basement ambient pre-flush CH4`, data = theData)
  theModelR2 = round(summary(theModel)$r.squared,3)
  #theTitle = expression("Basement pre-flush ambient air" ~ CH[4] ~"concentration and emissions calculated using RCM simple slope")
  theTitle = "A. Simple slope"
  theSubtitle = expr( R^2==!!round(summary(theModel)$r.squared,3)~ ", "~ n==!!nrow(theData)~ ", "~ p==!!signif(Regressionp(theModel), digits=3))
  thePlotAmbSS = ggplot( theData, aes(`Basement ambient pre-flush CH4`, `Simple calculated CH4 flux (cu ft / day) (600 seconds span)`, label=`Experiment name`) ) + 
    ggtitle( label = theTitle, subtitle = theSubtitle ) + 
    geom_point( size=2 ) + 
    geom_text(hjust=0.7, vjust=-0.5, size=4 ) +
    labs( x=expression( Unflushed~basement ~ ambient~air~CH[4]~concentration~(ppmv)), 
          y=expression( Daily~CH[4]~emissions~calculated~using~RCM~simple~slope~ (ft^3~day^-1) )) +  # x/y-axis labels
    geom_smooth( method=lm) +
    ylim(-.05,.5) +
    theme( axis.text.x=element_text( size=12),
           axis.text.y=element_text( color="black", size=12), # y-axes tick text color
           panel.background = element_rect(fill = "white",colour = "black", size = 1, linetype = "solid"),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # turn off grid
           legend.key = element_blank(), # make the backgrounds of the legend items not colored (gray by default)
           legend.background = element_rect( fill="white", size=0.5, linetype="solid", colour ="gray"), # legend border and fill
           plot.title = element_text(hjust = 0, size = 16), # justify title
           plot.subtitle = element_text(hjust = 0, size=14), # justify subtitle
           axis.title = element_text( color="black", face="bold"), # axes labels - note with expression(), need to use bold() inside it to make this work
           axis.title.y = element_text(size = 13), # y-axis title
           axis.title.x = element_text(size = 14)
    )
  
  opar=par()
  par( mfrow=c(1,2), mai= c(1, .8, 0.5, 0.3) ) # # side by side plots, margins to make them closer together,   # bottom, left, top and right
  print( plot_grid( thePlotAmbSS, thePlotAmbFT, ncol=2, labels=NULL, label_size=14, hjust=-0.2, label_fontface="plain") )
  par(opar)
  
  
  
  

  # explore log correlation
  # theModel = lm( `Simple calculated CH4 flux (cu ft / day) (600 seconds span)` ~ log(`Basement ambient pre-flush CH4`), data = theData)
  # thePlot = plot( x = log(theData$`Basement ambient pre-flush CH4`),
  #                 y = theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)` ,
  #                 main = "Does log of pre-flush ambient CH4 and predict Simple Slope calculated flux",
  #                 xlab = "Log pre-flush ambient CH4 concentration (ppm)",
  #                 ylab = "Simple slope calculated daily flux (cu ft / day)",
  #                 #ylim=c(0,max(theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`+0.1)),
  #                 #xlim=c(min(theData$`Basement ambient pre-flush CH4`), max(theData$`Basement ambient pre-flush CH4`)+0.5),
  #                 pch=21, col="blue", bg="lightblue"
  # )
  # abline(v = seq(0, 7, 0.5), lty = "dashed", col = "gray"); 
  # abline(h = seq(-4,-0.5, .5), lty = "dashed", col = "gray")
  # mtext( paste0("R-squared: ", round(summary(theModel)$r.squared,3),", n=",nrow(theData),", p=",signif(Regressionp(theModel), digits=3) ) )
  # abline( theModel, col="green")
  
  
  # Exploring how basement total leaks ppm might relate to calculated RCM fitted tangent flux
  # variables : 
  #    Total ppm of all basement leaks : this is a sum per basement of CGI measured leaks ppm
  #    Modeled calculated CH4 flux (cu ft / day) : RCM fitted tangent calculated emissions estimate per basement
  # explore linear correlation
  theData = experimentData %>% drop_na(`Modeled calculated CH4 flux (cu ft / day)`)
  theData = theData %>% drop_na(`Total ppm of all basement leaks`)
  theModel = lm( `Modeled calculated CH4 flux (cu ft / day)` ~ `Total ppm of all basement leaks`, data = theData)
  
  theTitle = "Sum of basements’ leaks as quantified with the CGI and RCM fitted tangent calculated daily emissions"
  theTitle = "A. Total CGI-measured basement leakage vs emissions"
  theSubtitle = expr( R^2==!!round(summary(theModel)$r.squared,3)~ ", "~ n==!!nrow(theData)~ ", "~ p==!!signif(Regressionp(theModel), digits=3))
  
  thePlot = plot( y = theData$`Modeled calculated CH4 flux (cu ft / day)`,
                  x = theData$`Total ppm of all basement leaks`,
                  main = "Did total ppmv of all basement leaks predict RCM fitted tangent flux?",
                  xlab = "Sum basement ppmv leaks readings",
                  ylab=expression(Daily~CH[4]~emissions~calculated~using~fitted~tangent~ (ft^3~day^-1) ),
                  ylim=c(0,max(theData$`Modeled calculated CH4 flux (cu ft / day)`+0.1)),
                  pch=21, col="blue", bg="pink"
  )
  abline(h = seq(0,0.65, .05), lty = "dashed", col = "gray")
  abline(v = seq(0, 7000, 500), lty = "dashed", col = "gray")
  mtext( paste0("R-squared: ", round(summary(theModel)$r.squared,3),", n=",nrow(theData),", p=",signif(Regressionp(theModel), digits=3) ) )
  abline( theModel, col="green")
  
  # better plot with confidence bands
  thePlotBLFT = ggplot( theData, aes(`Total ppm of all basement leaks`, `Modeled calculated CH4 flux (cu ft / day)`, label=`Experiment name` ) ) + 
    ggtitle( label = theTitle, subtitle = theSubtitle ) + 
    geom_point( size=2 ) + 
    geom_text(hjust=0.7, vjust=-0.5, size=4 ) +
    labs( x=expression("Sum of CGI quantified basement leaks (ppmv)"),
          y=expression( Daily~CH[4]~emissions~calculated~using~RCM~fitted~tangent~ (ft^3~day^-1) ) ) +  # x/y-axis labels
    geom_smooth( method=lm) +
    theme( 
      axis.text.y=element_text( color="black"), # y-axes tick text color
      panel.background = element_rect(fill = "white",colour = "black", size = 1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "gray"),
      panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "gray"),
      legend.key = element_blank(), # make the backgrounds of the legend items not colored (gray by default)
      legend.background = element_rect( fill="white", size=0.5, linetype="solid", colour ="gray"), # legend border and fill
      plot.title = element_text(hjust = 0), # justify title
      plot.subtitle = element_text(hjust = 0), # justify subtitle
      axis.title = element_text( color="black", face="bold") # axes labels - note with expression(), need to use bold() inside it to make this work
    )

  #skipping log exploration for now - was in but I removed it
  
  # How about does sum leakage per cu ft of adjusted basement volume correlating predict the RCM fitted tangent flux ?
  theData$sumLeakagePPMperCubicFoot = theData$`Total ppm of all basement leaks`/ theData$`Adjusted volume (Vadj)`
  theModel = lm( `Modeled calculated CH4 flux (cu ft / day)` ~ sumLeakagePPMperCubicFoot , data = theData)
  
  theTitle = expression(Basement~leakage~ft^-3 ~ and ~emissions~calculated~with~RCM~fitted~tangent)
  theTitle = "Did CGI-measured leakage per cubic foot predict calculated emissions?"
  theTitle = "B. Total CGI-measured basement leakage per cubic foot vs emissions"
  theSubtitle = expr( R^2==!!round(summary(theModel)$r.squared,3)~ ", "~ n==!!nrow(theData)~ ", "~ p==!!signif(Regressionp(theModel), digits=3))
  
  thePlot = plot( x = theData$sumLeakagePPMperCubicFoot,
                  y = theData$`Modeled calculated CH4 flux (cu ft / day)`,
                  main = theTitle,
                  xlab = "Sum basement leaks ppmv per Vadj",
                  ylab = expression(Daily~CH[4]~RCM~fitted~tangent~emissions~ (ft^3~day^-1)),
                  xlim=c(0, max(theData$sumLeakagePPMperCubicFoot)),
                  ylim=c(0,max(theData$`Modeled calculated CH4 flux (cu ft / day)`+0.1)),
                  pch=21, col="black", bg="green"
  )
  abline(h = seq(0, 0.6,0.05), lty = "dashed", col = "gray")
  abline(v = seq(0, 2.5, 0.25), lty = "dashed", col = "gray")
  mtext( theSubtitle )
  abline( theModel, col="green")
  
  
  thePlotBLV = ggplot( theData, aes(sumLeakagePPMperCubicFoot, `Modeled calculated CH4 flux (cu ft / day)`, label=`Experiment name` ) ) + 
    ggtitle( label = theTitle, subtitle = theSubtitle ) + 
    geom_point( size=2 ) + 
    geom_text(hjust=0.7, vjust=-0.5, size=4 ) +
    labs( x=expression( Total~basement~leakage~per~V[adj]~(ppm~ft^-3)), y="") +
          #y=expression(Daily~CH[4]~emissions~calculated~using~RCM~fitted~tangent~ (ft^3~day^-1) )  ) +  # x/y-axis labels
    geom_smooth( method=lm) +
    theme(
      axis.text.y=element_text( color="black"), # y-axes tick text color
      panel.background = element_rect(fill = "white",colour = "black", size = 1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "gray"),
      panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "gray"),
      legend.key = element_blank(), # make the backgrounds of the legend items not colored (gray by default)
      legend.background = element_rect( fill="white", size=0.5, linetype="solid", colour ="gray"), # legend border and fill
      plot.title = element_text(hjust = 0), # justify title
      plot.subtitle = element_text(hjust = 0), # justify subtitle
      axis.title = element_text( color="black", face="bold") # axes labels - note with expression(), need to use bold() inside it to make this work
    )

  print( plot_grid( thePlotBLFT, thePlotBLV, ncol=2, labels=NULL, label_size=10, hjust=-0.2) )
  
  # How did home age predict the simple slope flux ?
  theData = experimentData %>% drop_na(`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`)
  theData = theData %>% drop_na(`Home age (years)`)

  theModel = lm( `Simple calculated CH4 flux (cu ft / day) (600 seconds span)` ~ `Home age (years)` , data = theData)
  thePlot = plot( x = theData$`Home age (years)`,
                  y = theData$`Simple calculated CH4 flux (cu ft / day) (600 seconds span)`,
                  main = "Did age of building predict simple slope flux?",
                  xlab = "Building age (years)",
                  ylab = "Simple slope calculated daily flux (cu ft / day)",
                  pch=21, col="black", bg="orange"
  )
  abline(h = seq(0, 0.6,0.05), lty = "dashed", col = "gray")
  abline(v = seq(0, 200, 25), lty = "dashed", col = "gray")
  mtext( paste0("R-squared: ", round(summary(theModel)$r.squared,3),", n=",nrow(theData),", p=",signif(Regressionp(theModel), digits=3) ) )
  abline( theModel , col="green")

}


# =================================================================================
if ( ! interactive() ) {
  process_cli_options() 
}

if ( processExperiments ) {
  
  workSheet = "Flux measurements"
  skipRows = 2
  expectedColumns = c( 
    "Experiment name", # an identifier useful for when plotting etc
    "Data files", # a list of data folder(s)
    "Start time (epoch seconds)", # time of when basement was closed up and ramp-up started
    "End time (epoch seconds)", # time when closed down the experiment
    "Adjusted volume (Vadj)", # adjusted basement volume
    "How much time to fit with (sec)" # if a number, use that much time in seconds from the start to fit with
  )
  
  # read in the experiments data spreadsheet and get the relevant bits for processing
  experimentData = load_experiments_spreadsheet(dataFile, workSheet, expectedColumns , skipRows, TRUE )
  process_experiments_data( experimentData )
  
} 

if ( doAnalysis ) { 
  
  if (  analyzeControls  == 1 ) {
    
    # analyze RCM control tests
    workSheet = "RCM control tests"
    skipRows = 3
    expectedColumns = c(
      "Experiment name",
      "Control test name",
      "Control test flux",
      "Calculated emissions using simple slope",
      "Calculated emissions using fitted tangent"
    )
    controlTestsData = load_experiments_spreadsheet(dataFile, workSheet, expectedColumns , skipRows , FALSE)
    
    thePlotRCM1 = analyze_RCM_control_tests(controlTestsData, 
                                            c(0,210), 
                                            expression(Daily~CH[4]~emissions ~ (ft^3~day^-1) ),
                                            "A. RCM control tests results")
    
    thePlotRCM2 = analyze_RCM_control_tests(controlTestsData, 
                                            c(0,6),
                                            "", 
                                            "B. RCM control tests results (y-axis zoom)") 
    
    print( plot_grid( thePlotRCM1, thePlotRCM2, ncol=2, labels=NULL, label_size=10, hjust=-0.2) )
    
    
    
    # analyze SRCM control tests
    workSheet = "SRCM control tests"
    skipRows = 2
    expectedColumns = c( 
      "Experiment name",
      "Control test name",
      "Control test flux",
      "adjusted basement volume (Vadj)",
      "simplified volume (Vsim)",
      "bag 1 ch4 ppm",
      "bag 2 ch4 ppm",
      "time span"
      
    )
    controlTestsData = load_experiments_spreadsheet(dataFile, workSheet, expectedColumns , skipRows , FALSE)
    thePlotSRCM = analyze_SRCM_control_tests(controlTestsData)
    print( thePlotSRCM )
    x=1
  
  }
  
  if (  analyzeLeaks  == 1 ) {
    # analyze leaks
    workSheet = "Leaks"
    skipRows = 0
    expectedColumns = c( 
      "Experiment name",
      "Floor",
      "Size ppm",
      "Before or after meter",
      "Home age (years)",
      "Dwelling type",
      "Leak source"
    )
    leaksData = load_experiments_spreadsheet(dataFile, workSheet, expectedColumns , skipRows, FALSE )
    analyze_leaks( leaksData )
  }
  
    # analyzed processed data - populate the results first before doing analysis
    workSheet = "Flux measurements"
    skipRows = 2
    expectedColumns = c( 
      "include in analysis",
      "Experiment name", # an identifier useful for when plotting etc
      "Experiment type", # BCM will be filtered out
      "Single or multi family home",
      "Basement empty volume (cu ft) (Vempty)", 
      "Volume adjustment",
      "simplified volume (cu ft)",
      "Adjusted volume (Vadj)", # adjusted basement volume
      "Simple calculated CH4 flux (cu ft / day) (600 seconds span)", 
      "Modeled calculated CH4 flux (cu ft / day)",
      "Modeled Steady state CH4 (ppm)",
      "Outside CH4",
      "Basement ambient pre-flush CH4",
      "CH4 at end of flush (ppm)",
      "Simple flush duration (min)",
      "basement ambient post-simple flush CH4 ppm",
      "sample bag 2 fill seconds after bag 1",
      "bag 1 CH4 (ppm)",
      "bag 2 CH4 (ppm)",
      "include in vol adj averaging",
      #"Volume adjustment",
      "bag samples flux using adjusted volume",
      "simplified bag samples flux using simplified volume",
      "Total ppm of all basement leaks",
      "bag samples done same day or different day to BCM",
      "Home age (years)",
      "1st floor CH4 concentration",
      "2nd floor CH4 concentration",
      "3rd floor CH4 concentration",
      "4th floor CH4 concentration",
      "Start time (epoch seconds)",
      "End time (epoch seconds)",
      "flush type (combination means simple followed by full flush; fullonly means just full flush not preceded by simple)",
      "Basement full flush start epoch time",
      "Basement full flush end epoch time",
      "Simple flush start epoch time",
      #"Simple flush duration (min)",
      "Data files",
      #"bag samples done same day or different day to BCM",
      "Experiment name original",
      "How much time to fit with (sec)",
      "Max data (sec)",
      "use in overall RCM flush analysis",
      "total flush duration"
      
    )
    experimentData = load_experiments_spreadsheet(dataFile, workSheet, expectedColumns , skipRows, FALSE )
    
    # filter down to just BCM rows (some of these will have be na.omit'd already)
    analyze_data( experimentData )
  
}
  

cat ("Done.\n")
