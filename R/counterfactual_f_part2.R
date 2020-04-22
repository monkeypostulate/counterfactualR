

# #####################################################################
#
#' @name marketing_effects
#' @title Estimate effects of different variables
#' @description
#' @export
#'


##  change function to aggregate_effects
marketing_effects<-function(data_input,
                            scenarios)
{

  for(i in 2:length(scenarios))
  {
    data_input[,scenarios[i]]<-
      apply(data_input[,scenarios[i:(i-1)]],1,bounded.f)
    data_input[,paste0('delta_',scenarios[i])]<-
      apply(data_input[,scenarios[i:(i-1)]],1,substract)
    data_input[,paste0('perc_',scenarios[i])]<-
      apply(data_input[,scenarios[i:(i-1)]],1,percentage)

  }

  return(data_input)
}



# #####################################################################
# Simulate Scenarios
#' @name simul_scenarios
#' @title  Simulate scenarios
#' @description
#' @export
#'
simul_scenarios<-function(data_input,model,scenario){

  data_input$date<-as.Date(data_input$date)
  scenario$date<-as.Date(scenario$date)

  columns.names<-names(scenario)[-1]
  names(scenario)[-1]<-sapply(names(scenario)[-1],paste0,'1')

  start.date.campaign<-min(scenario$date)
  end.date.campaign<-max(scenario$date)
  chosen_period<-start.date.campaign+seq(0,end.date.campaign-start.date.campaign)


  data_input<-data_input%>%
    left_join(scenario,
              by='date')


  for(temp.col in columns.names){
    temp2.col<-paste0(temp.col,'1')
    data_input[,temp.col]<-apply(data_input[,c(temp.col,temp2.col)],1,replacement)
    data_input<-data_input%>%
      select(-temp2.col)
  }

  data_output<-data_input%>%
    counterfactual(model,chosen_period,alpha=0)

  return(data_output)
}



# #####################################################################
#
mix_change<-function(data_input,
                     columns,
                     baseline_period,
                     counterfactual_period){

  data_transf<-pivot(data_input,columns,columns)
  data_temp<-data_input%>%
    select(date,predictions)

  names(data_temp)[2]<-'predictions'

  data_ouput<-counterfactual_mix(data_transf,
                                 data_temp,
                                 'group',
                                 baseline_period,
                                 counterfactual_period)
  return(data_ouput)
}




# #####################################################################
#  Function: Calculate Mix
#' @title Calculate Mix
#' @name variable_mix
#' @title
#' @description
#' @export

#' @examples
#'
#' data("Sales")
#' period_mix<-as.Date('2019-01-03')+seq(0,13)
#' Sales%>%
#'  filter(year(date)==2019 & week(date) %in% 1:5)%>%
#'  variable_mix(chosen_period=period_mix,target_var='SALES_CHANNEL')
#'


#' @param data_input A tibble table with a column date
#' having time information and a column  target_var grouping the entries.
#' If no target_var is provided, then specify the name of target_var.
#' @param chosen_period Period where the mix is calculated
#' @param target_var Variable having group information


variable_mix<-function(data_input,chosen_period=NA, target_var=NA){

  # check if target_variable was provided.
  if(!is.na(target_var))
    data_input$TARGET_VAR<-data_input[,target_var]

  if(!is.na(chosen_period[1]))
    data_input<-data_input%>%
      filter(date %in%chosen_period)


  data_output<-data_input%>%
    group_by(TARGET_VAR)%>%
    summarise(N=n())%>%
    ungroup()%>%
    arrange(N) %>%
    filter(N>3)%>%
    mutate(mix=N/sum(N))%>%
    dplyr::select(TARGET_VAR,N, mix)

  return(data_output)
  #' @return variable mix returns a summary a pivot table with the mix total
}


# #####################################################################
#
# Data
#
# #####################################################################

#' This is data to be included in my package
#'
#' @name sales
#' @docType data
NULL


#' This is data to be included in my package
#'
#' @name sales2
#' @docType data
NULL

