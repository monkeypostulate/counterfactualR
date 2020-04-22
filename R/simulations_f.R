# #####################################################################
# Title:  Simuation functions
#  Author: Abel Camacho Guardian
# Date: 25.10.2019
# Version: 1
# Comments: None
#
# #####################################################################




# #####################################################################
#  Function: week_trend
#' @name week_trend
#' @title Simulate weekdays
#' @description Calculate counterfactual under different scenarios.
#'
#' @examples
#' ## Example
#'week_trend(weekday='Friday',baseline=21:27)
#'week_trend(weekday=as.Date('2019-10-25),21:27)
#'week_trend(weekday=6,baseline=21:27)
#'

#' @param weekday Day of the week as Date, word in English or number:
#' @param baseline A vector of values per day
week_trend<-function(weekday,baseline){

  if(is.Date(weekday))
    weekday<-wday(weekday)

  if(is.character(weekday))
    weekday<-tolower(weekday)


  week_days<-c('monday','tuesday','wednesday','thursday','friday','saturday','sunday')


  if(weekday %in% 1:7){
    weekday<-week_days[weekday]
  }

  if(! weekday %in% week_days){
    stop('Enter a valid week of the day')
  }
  weekday_trends<-switch(weekday,
                         'monday'=baseline[1],
                         'tuesday'=baseline[2],
                         'wednesday'=baseline[3],
                         'thursday'=baseline[4],
                         'friday'=baseline[5],
                         'saturday'=baseline[6],
                         'sunday'=baseline[7]
  )
  return(weekday_trends)
}



# #####################################################################
#  Function: week_trend
#' @name month_trend
#' @title Month trend
#' @description Calculate counterfactual under different scenarios.
#'
#' @examples
#' ## Example
#'month_trend(month='October',
#'            baseline=50:62)
#'month_trend(month=10,
#'            baseline=50:62)
#'month_trend(month=as.Date('2019-10-25'),
#'            baseline=50:62)


#' @param weekday Day of the week as Date, word in English or number:
#' @param baseline A vector of values per day
month_trend<-function(month,baseline){

  if(is.Date(month))
    month<-month(month)

  if(is.character(month))
    month<-tolower(month)


  months.eng<-c('january','february','march','april','may','june','july',
                'august','september','october','november','december')

  if(month %in% 1:12){
    month<-months.eng[month]
  }

  if(! month %in% months.eng){
    stop('Enter a valid month')
  }

  baseline_trend<-switch(month,
                      'january'=baseline[1],
                      'february'=baseline[2],
                      'march'=baseline[3],
                      'april'=baseline[4],
                      'may'=baseline[5],
                      'june'=baseline[6],
                      'july'=baseline[7],
                      'august'=baseline[8],
                      'september'=baseline[9],
                      'october'=baseline[10],
                      'november'=baseline[11],
                      'december'=baseline[12],

  )
  return(baseline_trend)
}



# #####################################################################
# Function: Simulate trend with lag effects
#' @name simulate_sales_lag
#' @title Simulate trends with lag effects
#' @description Simulate any trend with seasonal trends at the week and month level,
#'  and with lag effects.
#' @examples

#'sales_data2<-simulate_sales_lag(time_period=seq(as.Date('2019-01-01'),as.Date('2019-06-01'),1),
#'                                baseline_month=c(0,0,5,-5,-10,-9,8,5,0,-8,0,-10),
#'                                baseline_week=c(60,55,50,49,45,15,0),
#'                                lag_variables=list('1'=0.3,'7'=-0.2))
#'
#'plot(sales_data2$date,sales_data2$total, type='l')

simulate_sales_lag<-function(time_period,
                             baseline_week=c(0,0,5,-5,-10,-9,8,5,0,-8,0,-10),
                             baseline_month=c(60,55,50,49,45,15,0),
                             lag_variables=list('1'=0.5,'7'=-0.2)){

  n.days<-length(time_period)
  lags<-as.integer(names(lag_variables))
  lag_values<-as.numeric(lag_variables)

  data_output<-data.frame(matrix(0, nrow=n.days, ncol=2))
  names(data_output)<-c('date','total')

  data_output$date<-time_period

  data_output<-data_output%>%
    mutate(wday=factor(wday(date)),
           month=factor(month(date))
    )



  for(k in 1:n.days){
    data_output$total[k]<-month_trend(data_output$date[k],baseline=baseline_month)

    data_output$total[k]<-week_trend(data_output$date[k],baseline=baseline_week)+
      data_output$total[k]+sample(-1:1,1, replace=T)



    for(l in 1:length(lags)){
      if(k-lags[l]>0)
      {
        lag<-k-lags[l]
        data_output$total[k]<-data_output$total[k]+
          lag_values[l]*data_output$total[k-lags[l]]
      }
    }


  }


  data_output<-data_output%>%
    mutate(
      wday=factor(wday),
      month=factor(month),
      lag_1=lag(total,1),
      lag_7=lag(total,7)
    )


  # Output
  return(data_output)

}




# ###########################
# Media uplift
# ###########################


media_uplift<-function(x,al=1/4,be=3.5,ga=0.002){
  output<-be*(x^(al))-be*ga*x^(2*al)
  return(output)
}


