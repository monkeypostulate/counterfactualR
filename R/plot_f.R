# #####################################################################
# Polygon Plot
polygon_trend<-function(data){

  names(data)<-c('dates','y','groups')

  all.groups<-as.character(unique(data[,'groups']))

  polygon_data<-list()



  polygon_data[[1]]<-
    data[data$groups==all.groups[1],]

  first.row<-data.frame('dates'=min(data[data$groups==all.groups[1],'dates']),
                        y=0)
  last.row<-data.frame('dates'=max(data[data$groups==all.groups[1],'dates']),
                       y=0)

  polygon_data[[1]]<-rbind(first.row,polygon_data[[1]][,c('dates','y')])
  polygon_data[[1]]<-rbind(polygon_data[[1]][,c('dates','y')],last.row)
  polygon_data[[1]]<-cbind(polygon_data[[1]],all.groups[1])

  names(polygon_data[[1]])[3]<-'groups'


  polygon_output<-polygon_data[[1]]



  for(i in 2:length(all.groups)){
    polygon_data[[i]]<-data%>%
      group_by(dates)%>%
      filter(groups %in% all.groups[1:i-1])%>%
      summarise(y=sum(y))%>%
      ungroup()


    temp_data<-data%>%
      group_by(dates)%>%
      filter(groups %in% all.groups[1:i])%>%
      summarise(y=sum(y))%>%
      ungroup()%>%
      arrange(desc(dates))


    polygon_data[[i]]<-rbind(polygon_data[[i]],temp_data)

    polygon_data[[i]]<-cbind(polygon_data[[i]],all.groups[i])
    names(polygon_data[[i]])[3]<-'groups'


    polygon_output<-rbind(polygon_output,polygon_data[[i]])

  }




  # #############################
  # Create plot function
  # #############################
  g_plot<-ggplot()

  for(i in 1:length(all.groups)){
    g_plot<-g_plot+
      geom_polygon(data=polygon_data[[i]],aes(x=dates,y=y,fill=groups))
  }






  return(g_plot)

}



# #####################################################################
# Data transformation for Tableau




sign_cf <- function(data_input,
                    counterfactual_period,
                    day_before = -7) {
  period_before_cf <- min(counterfactual_period) + seq(day_before, 0)
  period_analysis <- c(period_before_cf, counterfactual_period)
  data_input <- data_input %>%
    mutate(
      counterfactual = ifelse(delta < 0, counterfactual + delta, counterfactual),
      sign = ifelse(delta < 0, "negative", "positive"),
      delta = (delta)
    ) %>%
    filter(date %in% period_analysis)



  data_output <- rbind(
    data_input %>%
      mutate(
        total = actual,
        type = "actual",
        sign = "zero"
      ) %>%
      filter(date %in% period_analysis) %>%
      select(date, group, type, total, sign),
    data_input %>%
      mutate(
        total = counterfactual,
        type = "counterfactual",
        sign = "zero"
      ) %>%
      select(date, group, type, total, sign),
    data_input %>%
      filter(date %in% period_analysis) %>%
      mutate(
        total = abs(delta),
        type = "delta"
      ) %>%
      select(date, group, type, total, sign)
  )

  return(data_output)
}

