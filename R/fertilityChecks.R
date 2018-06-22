#' @title checkFertilityEvent_latlon
#'
#' @description \code{checkFertilityEvent_latlon} Checks for fertility event
#'   relevant to Hybrid Rice breeding using aWhere's API based on latitude &
#'   longitude
#'
#' @details
#'
#' This function generates frequency by time information related to a specific
#' threshhold being exceeded.  It does this by pulling all required weather data
#' via the aWhere API, subsets the data for only the requested years, and then
#' calculates the frequency of the threshhold veing exceeded for each day of the
#' year.  It then outputs that information in a variety of formats as specified
#' by the user.  This function only considers maxTemp
#'
#' Default units are returned by the API. Latitude and longitude must be in
#' decimal degrees.
#'
#'
#' @references NEED TO ADD
#'
#' @param - latitude: the latitude of the requested location (double)
#' @param - longitude: the longitude of the requested locations (double)
#' @param - threshhold: the temperature value that must be exceeded (double)
#' @param - numConsecutiveDaysToCheck: number of consecutive days the threshhold
#'   must be exceeded to count as fertility.  The final day of the period will
#'   be conisdered the fertility event.  Default value is 1 (optional - double)
#' @param - period_start: character string of the first day for which you want
#'   to retrieve data, in the form without including the year: MM-DD
#' @param - period_end: character string of the last day for which you want to
#'   retrieve data, in the form without including the year: MM-DD
#' @param - yearsToInclude: vector of years to use for calculating frequency of
#'   fertility events.  Default is all years from 2006 to present (optional -
#'   doubble)
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
#'                                     ,threshhold = 20
#'                                     ,numConsecutiveDaysToCheck = 2
#'                                     ,period_start = '04-28'
#'                                     ,period_end = '08-01')}

#' @export


checkFertilityEvent_latlon <- function(latitude
                                       ,longitude
                                       ,threshhold
                                       ,numConsecutiveDaysToCheck = 1
                                       ,period_start
                                       ,period_end
                                       ,yearsToInclude = seq(2006,as.numeric(strsplit(as.character(Sys.Date()),split = '-')[[1]][1]),1)
                                       ,smoothPlot = FALSE
                                       ,saveFigure = ''
                                       ,saveCSV = '') {

  periodStart <- strsplit(period_start,'-','-',fixed = TRUE)[[1]]
  periodEnd   <- strsplit(period_end,'-','-',fixed = TRUE)[[1]]

  if (period_end[1] <= period_start[1]) {
    wrapsNewYear <- TRUE
  } else {
    wrapsNewYear <- FALSE
  }

  returnedData <- list()

  for (x in 1:length(yearsToInclude)) {
    cat(paste0('Requesting Data for Year ',yearsToInclude[x],'\n'))

    #Breaking it up this way will minimize the data transfer size
    if (wrapsNewYear == FALSE) {
      currentStartDate <- paste0(yearsToInclude[x],'-',periodStart[1],'-',periodStart[2])
      currentEndDate   <- paste0(yearsToInclude[x],'-',periodEnd[1],'-',periodEnd[2])

      if (currentStartDate > (Sys.Date()-1)) {
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
                                                                                                                ,currentEndDate)))

      returnedData[[length(returnedData)]][,seqDatePosition := seq(1,seqDateLength,1)]
    } else {
      currentStartDate <- paste0(yearsToInclude[x],'-',periodStart[1],'-',periodStart[2])
      currentEndDate   <- paste0(yearsToInclude[x],'-12-31')

      if (currentStartDate > (Sys.Date()-1)) {
        next
      }

      if (currentEndDate > (Sys.Date()-1)) {
        currentEndDate <- as.character(Sys.Date()-1)
      }

      seqDateLength_1 <- length(seq(lubridate::ymd(currentStartDate),lubridate::ymd(currentEndDate),1))

      cat(paste0('     Requesting Data between ',currentStartDate,' and ',currentEndDate,'\n'))

      suppressWarnings(returnedData[[length(returnedData) +1]] <- as.data.table(aWhereAPI::daily_observed_latlng(latitude
                                                                                                                ,longitude
                                                                                                                ,currentStartDate
                                                                                                                ,currentEndDate)))

      returnedData[[length(returnedData)]][,seqDatePosition := seq(1,seqDateLength_1,1)]


      currentStartDate <- paste0(yearsToInclude[x],'-01-01')
      currentEndDate   <- paste0(yearsToInclude[x],'-',periodEnd[1],'-',periodEnd[2])

      if (currentStartDate > (Sys.Date()-1)) {
        next
      }

      if (currentEndDate > (Sys.Date()-1)) {
        currentEndDate <- as.character(Sys.Date()-1)
      }

      seqDateLength_2 <- length(seq(lubridate::ymd(currentStartDate),lubridate::ymd(currentEndDate),1))

      cat(paste0('     Requesting Data between ',currentStartDate,' and ',currentEndDate,'\n'))

      suppressWarnings(returnedData[[length(returnedData) +1]] <- as.data.table(aWhereAPI::daily_observed_latlng(latitude
                                                                                                                ,longitude
                                                                                                                ,currentStartDate
                                                                                                                ,currentEndDate)))

      returnedData[[length(returnedData)]][,seqDatePosition := seq(seqDateLength_1+1,seqDateLength_1 + seqDateLength_2,1)]
    }
  }

  returnedData <- data.table::as.data.table(do.call("rbind", returnedData))

  setkey(returnedData,date)


  returnedData[,month     := lubridate::month(date)]
  returnedData[,day       := lubridate::day(date)]
  returnedData[,dayOfYear := lubridate::yday(date)]
  returnedData[,monthDayString := paste0(month,'-',day)]

  #Drob weirdness related to leap years
  returnedData <- returnedData[!(month == 2 & day == 29) & dayOfYear <= 365,]

  returnedData[,exceedThreshhold := 0]
  returnedData[,exceedThreshhold_0 := 0]
  returnedData[((temperatures.max + temperatures.min)/2) < threshhold,exceedThreshhold_0 := 1]

  fertilityString <- paste0('returnedData[exceedThreshhold_0 == 1')

  if (numConsecutiveDaysToCheck > 1) {

    fertilityString <- paste0(fertilityString,' & ')

    for (y in 2:numConsecutiveDaysToCheck) {
      #we are using lag here to recreat silverlight behavior where the fertility value of sequence was reported for the last day of the sequence
      eval(parse(text = paste0('returnedData[,exceedThreshhold_',y-1,' := shift(exceedThreshhold_0,n = y-1,type = \'lag\')]')))

      fertilityString <- paste0(fertilityString,'exceedThreshhold_',y-1,' == 1')

      if (y < numConsecutiveDaysToCheck) {
        fertilityString <- paste0(fertilityString,' & ')
      } else {
        fertilityString <- paste0(fertilityString,', exceedThreshhold := 1]')
      }

    }
  } else {
    fertilityString <- paste0(fertilityString,', exceedThreshhold := 1]')
  }

  eval(parse(text = fertilityString))

  returnedData[,grep('exceedThreshhold_',colnames(returnedData),fixed = TRUE,value = TRUE) := NULL]


  returnedData[,freqOfFertilityEvent := mean(exceedThreshhold,na.rm = TRUE),by = 'dayOfYear']

  dataToPlot <- unique(unique(returnedData[,list(dayOfYear,seqDatePosition,monthDayString,freqOfFertilityEvent)],by = 'dayOfYear'),by = 'monthDayString')
  setkey(dataToPlot,seqDatePosition)

  #About 10 tick labels is max for legible plot in typical visualization window
  sizeBreaks <- ceiling(nrow(dataToPlot)/10)


  if (smoothPlot == FALSE) {

    fertilityPlot <- ggplot(dataToPlot, aes(seqDatePosition, freqOfFertilityEvent)) +
                      coord_cartesian(ylim = c(0,1)) +
                      geom_point() +
                      geom_line() +
                      scale_x_continuous(breaks = dataToPlot[seq(1,.N,sizeBreaks),seqDatePosition],labels= dataToPlot[seq(1,.N,sizeBreaks),monthDayString]) +
                      ggtitle(label = paste0('Fertility Frequency'),subtitle = paste0('Latitude: ',latitude,' Longitude: ',longitude,'\nFertility Threshhold:',threshhold,'\nNumber Sequential Days: ',numConsecutiveDaysToCheck,'\nYears Included: ',paste0(gsub('20',"'",yearsToInclude),collapse = ', '))) +
                      ylab('Frequency of Fertility Event') +
                      xlab('Date') +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {

    fertilityPlot <- ggplot(dataToPlot, aes(seqDatePosition, freqOfFertilityEvent)) +
                      coord_cartesian(ylim = c(0,1)) +
                      geom_point() +
                      geom_smooth(span = .3, se = FALSE) +
                      scale_x_continuous(breaks = dataToPlot[seq(1,.N,sizeBreaks),seqDatePosition],labels= dataToPlot[seq(1,.N,sizeBreaks),monthDayString]) +
                      ggtitle(label = paste0('Fertility Frequency'),subtitle = paste0('Latitude: ',latitude,' Longitude: ',longitude,'\nFertility Threshhold:',threshhold,'\nNumber Sequential Days: ',numConsecutiveDaysToCheck,'\nYears Included: ',paste0(gsub('20',"'",yearsToInclude),collapse = ', '))) +
                      ylab('Frequency of Fertility Event') +
                      xlab('Date') +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }


  print(fertilityPlot)

  if (saveCSV != '') {
    write.csv(returnedData,file = saveCSV,row.names = FALSE)
  }

  if (saveFigure != '') {
    ggsave(saveFigure,fertilityPlot)
  }

  returnedData[,c('month','day','dayOfYear','exceedThreshhold') := NULL]

  return(list(returnedData,fertilityPlot))

}

