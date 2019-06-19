#' @title checkFertilityEvent_latlon
#'
#' @description \code{checkFertilityEvent_latlon} Checks for fertility event
#'   relevant to Hybrid Rice breeding using aWhere's API based on latitude &
#'   longitude
#'
#' @details
#'
#' This function generates frequency by time information related to a specific
#' threshold being exceeded.  It does this by pulling all required weather data
#' via the aWhere API, subsets the data for only the requested years, and then
#' calculates the frequency of the threshold veing exceeded for each day of the
#' year.  It then outputs that information in a variety of formats as specified
#' by the user.  This function only considers maxTemp
#'
#' Default units are returned by the API. Latitude and longitude must be in
#' decimal degrees.
#'
#'
#' @param - latitude: the latitude of the requested location (double)
#' @param - longitude: the longitude of the requested locations (double)
#' @param - threshold: vector of the the temperature values that must be
#'   exceeded.  Will be plotted on same figure (double)
#' @param - numConsecutiveDaysToCheck: number of consecutive days the threshold
#'   must be exceeded to count as fertility.  The final day of the period will
#'   be conisdered the fertility event.  Default value is 1 (optional - double)
#' @param - period_start: character string of the first day for which you want
#'   to retrieve data, in the form without including the year: MM-DD
#' @param - period_end: character string of the last day for which you want to
#'   retrieve data, in the form without including the year: MM-DD
#' @param - yearsToInclude: vector of years to use for calculating frequency of
#'   fertility events.  Default is all years from 2006 to present (optional -
#'   double)
#' @param - smoothPlot: Smooth the plot (optional - Boolean)
#' @param - saveCSV: complete path and filename for output of saved csv. Any
#'   deviation away from '' will be treated as valid entry (optional - string)
#' @param - saveFigure: complete path and filename for output of saved figure.
#'   Any deviation away from '' will be treated as valid entry (optional -
#'   string)
#' @param - figureDims: Vector where 1st element is figureWidth, 2nd element is
#'   figure height, 3rd element is the units of width/height.  Valid entries for
#'   3rd element (units) is "in","cm","mm". (optional - c(numeric,numeric,string))
#'
#' @import aWhereAPI
#' @import lubridate
#' @import data.table
#' @import ggplot2
#'
#' @return List.  Element 1 is the full datset queried from API with chance of
#'   fertility appended.  Element 2 is the handle for figure created
#'
#'
#' @examples
#' \dontrun{checkFertilityEvent_latlon(latitude = 39.8282
#'                                     ,longitude = -95.5795
#'                                     ,threshold = c(18,20,22)
#'                                     ,numConsecutiveDaysToCheck = 2
#'                                     ,period_start = '04-28'
#'                                     ,period_end = '08-01')}

#' @export


checkFertilityEvent_latlon <- function(latitude
                                       ,longitude
                                       ,threshold
                                       ,numConsecutiveDaysToCheck = 1
                                       ,period_start
                                       ,period_end
                                       ,yearsToInclude = seq(2008,as.numeric(strsplit(as.character(Sys.Date()),split = '-')[[1]][1]),1)
                                       ,smoothPlot = FALSE
                                       ,saveCSV = ''
                                       ,saveFigure = ''
                                       ,figureDims = c(6,3.38,'in')) {

  periodStart <- strsplit(period_start,'-','-',fixed = TRUE)[[1]]
  periodEnd   <- strsplit(period_end,'-','-',fixed = TRUE)[[1]]

  if (period_end[1] <= period_start[1]) {
    wrapsNewYear <- TRUE
  } else {
    wrapsNewYear <- FALSE
  }

  returnedData <- list()

  for (x in 1:length(yearsToInclude)) {
    cat(paste0('Determining API Calls for year ',yearsToInclude[x],'\n'))

    #Breaking it up this way will minimize the data transfer size
    if (wrapsNewYear == FALSE) {
      currentStartDate <- paste0(yearsToInclude[x],'-',periodStart[1],'-',periodStart[2])
      currentEndDate   <- paste0(yearsToInclude[x],'-',periodEnd[1],'-',periodEnd[2])
    } else {
      currentStartDate <- paste0(yearsToInclude[x],'-',periodStart[1],'-',periodStart[2])
      currentEndDate   <- paste0(yearsToInclude[x]+ 1,'-',periodEnd[1],'-',periodEnd[2])
    }

    if (numConsecutiveDaysToCheck > 1) {
      currentStartDate <- as.character(ymd(currentStartDate) - numConsecutiveDaysToCheck + 1)
    }

    if (currentStartDate > (Sys.Date()-1)) {
      cat(paste0('    No API calls for year ',yearsToInclude[x],'\n'))
      next
    }

    if (currentEndDate > Sys.Date()) {
      currentEndDate <- as.character(Sys.Date()-1)
    }

    seqDateLength <- length(seq(lubridate::ymd(currentStartDate),lubridate::ymd(currentEndDate),1))

    cat(paste0('    Requesting Data between ',currentStartDate,' and ',currentEndDate,'\n'))

    suppressWarnings(returnedData[[length(returnedData) +1]] <- as.data.table(aWhereAPI::daily_observed_latlng(latitude
                                                                                                              ,longitude
                                                                                                              ,currentStartDate
                                                                                                              ,currentEndDate
                                                                                                              ,propertiesToInclude = c('temperatures'
                                                                                                                                       ,'precipitation'))))

    returnedData[[length(returnedData)]][,precipitation.amount := round(precipitation.amount,2)]



    returnedData[[length(returnedData)]][,seqDatePosition := seq(1,.N,1)]

    returnedData[[length(returnedData)]][,paste0(c('temperatures.max_roll_'
                                                   ,'temperatures.min_roll_'
                                                   ,'precipitation.amount_roll_')
                                                 ,numConsecutiveDaysToCheck) := list(round(zoo::rollapply(data = temperatures.max
                                                                                                          ,width = numConsecutiveDaysToCheck
                                                                                                          ,FUN = mean
                                                                                                          ,na.rm = TRUE
                                                                                                          ,align = 'right'
                                                                                                          ,fill = NA),2)
                                                                                     ,round(zoo::rollapply(data = temperatures.min
                                                                                                           ,width = numConsecutiveDaysToCheck
                                                                                                           ,FUN = mean
                                                                                                           ,na.rm = TRUE
                                                                                                           ,align = 'right'
                                                                                                           ,fill = NA),2)
                                                                                     ,round(zoo::rollapply(data = precipitation.amount
                                                                                                           ,width = numConsecutiveDaysToCheck
                                                                                                           ,FUN = sum
                                                                                                           ,na.rm = TRUE
                                                                                                           ,align = 'right'
                                                                                                           ,fill = NA),2))]
  }

  returnedData <- data.table::as.data.table(do.call("rbind", returnedData))

  setkey(returnedData,date)

  returnedData[,month     := lubridate::month(date)]
  returnedData[,day       := lubridate::day(date)]
  returnedData[,dayOfYear := lubridate::yday(date)]
  returnedData[,monthDayString := paste0(month,'-',day)]

  #Drob weirdness related to leap years
  returnedData <- returnedData[!(month == 2 & day == 29) & dayOfYear <= 365,]

  returnedData[,c('temperatures.max_avg'
                  ,'temperatures.min_avg'
                  ,'precipitation.amount_avg') := list(round(mean(temperatures.max,na.rm = TRUE),2)
                                                      ,round(mean(temperatures.min,na.rm = TRUE),2)
                                                      ,round(mean(precipitation.amount,na.rm = TRUE),2)),by = 'monthDayString']

  thresholdString <- c()

  for (z in 1:length(threshold)) {
    currentthreshold <- threshold[z]

    returnedData[,exceedthreshold := 0]
    returnedData[,exceedthreshold_0 := 0]


    returnedData[((temperatures.max + temperatures.min)/2) < currentthreshold,exceedthreshold_0 := 1]

    fertilityString <- paste0('returnedData[exceedthreshold_0 == 1')

    if (numConsecutiveDaysToCheck > 1) {

      fertilityString <- paste0(fertilityString,' & ')

      for (y in 2:numConsecutiveDaysToCheck) {
        #we are using lag here to recreat silverlight behavior where the fertility value of sequence was reported for the last day of the sequence
        eval(parse(text = paste0('returnedData[,exceedthreshold_',y-1,' := shift(exceedthreshold_0,n = y-1,type = \'lag\')]')))

        fertilityString <- paste0(fertilityString,'exceedthreshold_',y-1,' == 1')

        if (y < numConsecutiveDaysToCheck) {
          fertilityString <- paste0(fertilityString,' & ')
        } else {
          fertilityString <- paste0(fertilityString,', exceedthreshold := 1]')
        }

      }
    } else {
      fertilityString <- paste0(fertilityString,', exceedthreshold := 1]')
    }

    eval(parse(text = fertilityString))

    returnedData[,grep('exceedthreshold_',colnames(returnedData),fixed = TRUE,value = TRUE) := NULL]

    eval(parse(text = paste0('returnedData[,freqOfFertilityEvent_',currentthreshold,' := round(mean(exceedthreshold,na.rm = TRUE),2),by = \'dayOfYear\']')))

    thresholdString <- c(thresholdString,paste0('freqOfFertilityEvent_',currentthreshold))

  }

  eval(parse(text = paste0('dataToPlot <- unique(unique(returnedData[,list(dayOfYear,seqDatePosition,monthDayString,',paste0(thresholdString,collapse = ','),')],by = \'dayOfYear\'),by = \'monthDayString\')')))
  setkey(dataToPlot,seqDatePosition)

  #converting to long format to handle plotting multiple thresholds at once
  dataToPlot <- data.table::melt(dataToPlot,id.vars = c('dayOfYear','seqDatePosition','monthDayString'),measure.vars = thresholdString)


  #About 10 tick labels is max for legible plot in typical visualization window
  sizeBreaks <- ceiling(nrow(dataToPlot)/10)/length(threshold)


  if (smoothPlot == FALSE) {

    fertilityPlot <- ggplot(dataToPlot[seqDatePosition >= numConsecutiveDaysToCheck,]
                            ,aes(x = seqDatePosition, y = value, group = variable, colour = variable)) +
                      coord_cartesian(ylim = c(0,1)) +
                      geom_point() +
                      geom_line() +
                      scale_x_continuous(breaks = unique(dataToPlot[,list(seqDatePosition,monthDayString)])[seq(numConsecutiveDaysToCheck,.N,sizeBreaks),seqDatePosition]
                                         ,labels= unique(dataToPlot[,list(seqDatePosition,monthDayString)])[seq(numConsecutiveDaysToCheck,.N,sizeBreaks),monthDayString]) +
                      ggtitle(label = paste0('Fertility Frequency')
                              ,subtitle = paste0('Latitude: ',latitude,' Longitude: ',longitude,'\nNumber Sequential Days: ',numConsecutiveDaysToCheck,'\nYears Included: ',paste0(gsub('20',"'",yearsToInclude),collapse = ', '))) +
                      ylab('Frequency of Fertility Event') +
                      xlab('Date') +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                      scale_color_discrete(name = 'Fertility\nthreshold',labels = paste0(threshold,'°C'))
  } else {

    fertilityPlot <- ggplot(dataToPlot[seqDatePosition >= numConsecutiveDaysToCheck,]
                            ,aes(x = seqDatePosition, y = value, colour = variable)) +
                      coord_cartesian(ylim = c(0,1)) +
                      geom_point() +
                      geom_smooth(span = .3, se = FALSE) +
                      scale_x_continuous(breaks = unique(dataToPlot[,list(seqDatePosition,monthDayString)])[seq(numConsecutiveDaysToCheck,.N,sizeBreaks),seqDatePosition]
                                         ,labels= unique(dataToPlot[,list(seqDatePosition,monthDayString)])[seq(numConsecutiveDaysToCheck,.N,sizeBreaks),monthDayString]) +
                      ggtitle(label = paste0('Fertility Frequency')
                              ,subtitle = paste0('Latitude: ',latitude,' Longitude: ',longitude,'\nNumber Sequential Days: ',numConsecutiveDaysToCheck,'\nYears Included: ',paste0(gsub('20',"'",yearsToInclude),collapse = ', '))) +
                      ylab('Frequency of Fertility Event') +
                      xlab('Date') +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                      scale_color_discrete(name = 'Fertility\nthreshold',labels = paste0(threshold,'°C'))
  }


  print(fertilityPlot)

  if (saveCSV != '') {
    write.csv(returnedData,file = saveCSV,row.names = FALSE)
  }

  if (saveFigure != '') {
    ggsave(filename = saveFigure
           ,plot = fertilityPlot
           ,width = as.numeric(figureDims[1])
           ,height = as.numeric(figureDims[2])
           ,units = figureDims[3])

  }

  datesToInclude <- unique(returnedData[,monthDayString])[-(1:(numConsecutiveDaysToCheck-1))]

  returnedData[,c('month','day','dayOfYear','exceedthreshold','seqDatePosition') := NULL]

  setcolorder(returnedData,c('latitude'
                             ,'longitude'
                             ,'monthDayString'
                             ,'date'
                             ,'temperatures.max'
                             ,'temperatures.min'
                             ,'precipitation.amount'
                             ,grep(pattern = 'freqOfFertilityEvent',x = colnames(returnedData),value = TRUE)
                             ,'temperatures.max_avg'
                             ,'temperatures.min_avg'
                             ,'precipitation.amount_avg'
                             ,paste0(c('temperatures.max_roll_'
                                       ,'temperatures.min_roll_'
                                       ,'precipitation.amount_roll_')
                                     ,numConsecutiveDaysToCheck)))

  setnames(returnedData,c('monthDayString'),c('monthDay'))

  return(list(returnedData[monthDay %in% datesToInclude],fertilityPlot))

}

