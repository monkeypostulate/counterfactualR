# #####################################################################
# Title:  Counterfactual functions
#  Author: Abel Camacho Guardian
# Date: 25.10.2019
# Version: 1
# Comments: None
#
# #####################################################################



# #####################################################################
#  Function: estim_counterfactual
#' @name estim_counterfactual
#' @title Calculate counterfactual
#' @description  Simulate counterfactual scenarios.
#' @usage estim_counterfactual(data,
#'                            model,
#'                            period,
#'                            alpha)
#'
#' @examples
#' ## Calculate counterfactual scenarios under different scenarios
#' data("sales_data")
#' sales_model1 <- lm(total ~
#' wday +
#'   month +
#'   adj_med_spend,
#' data = sales_data %>%
#'   filter(date < as.Date("2018-07-01"))
#' )
#'
#' start_campaign <- as.Date("2018-07-01")
#' end_campaign <- as.Date("2018-07-25")
#' chosen_period <- start_campaign + seq(0, end_campaign - start_campaign)
#'
#' ## Scenario with no Media Spend
#' sales_data %>%
#'   mutate(adj_med_spend = min(adj_med_spend)) %>%
#'   estim_counterfactual(sales_model1, chosen_period) %>%
#'   sum()
#'
#' ## Sales under the scenario that we haven't done any marketing campaign
#' sales_data %>%
#'   estim_counterfactual(sales_model1, chosen_period) %>%
#'   sum()
#' @param data_input The data containing the variables in the model,
#' and a named column date with date time data.
#' If no dates column is provided, then the first row is assumed to be a date.
#' @param chosen_model A model for which scenarios are simulated
#' @param chosen_period The period when the  counterfactual scenario is simulated.
#' The period covered by data_input should cover the chosen_period.
#' @param alpha Optionally, the lag effect between (0,1). If it is not specified, it is set to zero.
# Columns with lag information should be named as lag_i with i being a number greater than zero.


#' @details  Most scenarios are constructed with historical actuals
#  and counterfactuls (what if). The what if is specified in the period defined by chosen_period.
#' @export
estim_counterfactual <- function(data_input,
                                 chosen_model,
                                 chosen_period,
                                 alpha = 0,
                                 model='ols') {
  if (is.character(alpha)) {
    stop("alpha most be a numeric vector")
  }

  if (max(alpha) > 1 | alpha < 0) {
    warning("alpha must be a vector between with entries in [0,1]")
  }

  # If no dates column is provided, then the first row is assumed to have dates.
  if (!"dates" %in% names(data_input)) {
    names(data_input)[1] <- "date"
  }

  data_input <- data_input %>%
    arrange(date)

  # get all the lags variables
  lag_variables <- as.numeric(substring(names(data_input)[substr(names(data_input), 1, 4) == "lag_"], 5, 7))

  if (length(alpha) == 1) {
    alpha <- rep(alpha, length(lag_variables))
  }

  counterfactual_values <- rep(0, length(chosen_period))
  s <- 1
  for (t in chosen_period) {
    if (model %in% c("ols", "poisson")) {
      pred.y <- predict(chosen_model,
        type = "response",
        newdata = data_input %>%
          filter(date == t)
      )
    }
    if (model %in% c("bayesian")) {
      pred.y <- predict(selecte_model,
        type = "response",
        newdata = data_input %>%
          filter(date == t))$fit
    }



    counterfactual_values[s] <- pred.y
    k2 <- 1
    for (k in lag_variables) {
      data_input[data_input$date == t + k, paste0("lag_", k)] <-
        alpha[k2] * pred.y + (1 - alpha[k2]) * data_input[data_input$date == t + k, paste0("lag_", k)]
      k2 <- k2 + 1
    }
    s <- s + 1
  }

  #' @return  Return counterfactual values
  return(counterfactual_values)
}


# #####################################################################
# Simulate Mix
#' @name counterfactual_mix
#' @title Counterfactual Mix
#' @description For a given variable and scenario,it simulates a counterfactual mix,
#'  and it estimates the differeence between the actual mix and the counterfactual mix.
#'
#' @examples
#' ## Calculate counterfactual scenarios under different scenarios
#'
#' data("sales_data")
#'
#' sales_model1 <- lm(total ~
#' wday +
#'   month,
#' data = sales_data %>%
#'   filter(date < as.Date("2018-07-01"))
#' )
#'
#' start_campaign <- as.Date("2018-07-01")
#' end_campaign <- as.Date("2018-07-25")
#' campaign_period <- start_campaign + seq(0, end_campaign - start_campaign)
#' baseline_period <- min(chosen_period) + seq(-14, 0, 1)
#'
#'
#' cf_scenario1 <- sales_data %>%
#'   mutate(adj_med_spend = min(adj_med_spend)) %>%
#'   estim_counterfactual(sales_model1, chosen_period)
#' cf <- data.frame("date" = chosen_period, "counterfactual" = cf_scenario1)
#'
#' #
#' sales_data <- sales_data %>%
#'   left_join(cf, on = "date")
#'
#'
#' delta_shops <- sales_data %>%
#'   mutate(predictions = counterfactual) %>%
#'   counterfactual_mix(
#'     col_names = c("x", "y"),
#'     group_names = c("Shop x", "Shop y"),
#'     baseline_period = baseline_period,
#'     counterfactual_period = campaign_period
#'   )
#'
#' delta_shops %>%
#'   filter(date %in% campaign_period) %>%
#'   group_by(group) %>%
#'   summarise(
#'     actual = sum(actual),
#'     counterfactual = sum(counterfactual),
#'     delta = sum(delta),
#'     delta_days = sum(delta) / length(campaign_period),
#'     delta_perc = 100 * sum(delta) / sum(counterfactual)
#'   )
#' @param data_input baseline and counterfactuals are provided in the data
#' @param col_name Model used to construct senarios
#' @param group_names The period when the counterfactual scenario is calculated.
#' @param baseline_period The period where the baseline mix is assumed
#' @param counterfactual_period the period where the counterfactuals are simulated
# Columns with lag information should be named as lag_i with i being a number greater than zero.
#' @export 


counterfactual_mix <- function(data_input,
                               col_names,
                               group_names,
                               baseline_period,
                               counterfactual_period) {
  actual_mix <- data_input %>%
    pivot(
      columns = col_names,
      groups = group_names
    ) %>%
    rename(actual = total)

  # counterfactual SCENARIO
  cf.sales <- data_input %>%
    mutate(predictions = counterfactual) %>%
    select(date, predictions)

  # Estimate baseline Mix
  baseline_mix <- actual_mix %>%
    filter(date %in% baseline_period) %>%
    group_by(group) %>%
    summarise(actual = sum(actual)) %>%
    ungroup()
  TOTAL <- sum(baseline_mix$actual)

  baseline_mix <- baseline_mix %>%
    mutate(
      perc_mix = actual / TOTAL
    )

  # Estimate Counterfactual mix
  sales_info <- merge(cf.sales, baseline_mix)
  sales_info <- sales_info %>%
    mutate(counterfactual = predictions * perc_mix) %>%
    select(date, group, counterfactual)

  # Combine Actual and Counterfactual scenario
  output <- actual_mix %>%
    left_join(sales_info,
      on = c("date", "group")
    )
  # Counterfactual equal to actuals in dates not in the campaign period
  output <- output %>%
    mutate(
      counterfactual = ifelse(date %in% counterfactual_period,
        ifelse(counterfactual > 0, counterfactual, 0),
        actual
      ),
      delta = actual - counterfactual
    )


  # Final format
  output <- output %>%
    select(date, group, actual, counterfactual, delta)
  #' @return Return Counterfactual mix
  return(output)
}
