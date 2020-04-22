
# #####################################################################
# data transformation

#' @name pivot
#' @title  Pivot Table
#' @description Data transformation
#'
#' @examples
#' ## Calculate counterfactual scenarios under different scenarios
#'
#' data("sales_data")
#' sales_data %>%
#'   pivot(c("cs1", "cs2"), c("cs1", "cs2")) %>%
#'   filter(date %in% c(baseline_period, campaign_period)) %>%
#'   ggplot() +
#'   geom_line(aes(x = date, y = total, col = group)) +
#'   geom_vline(xintercept = min(campaign_period))
#' @param data_input Data
#' @param columns Columns representing different values
#' @param groups Name of the groups
#'
pivot <- function(data_input, columns, groups) {
  data_ouput <- cbind(data_input[, c("date", columns[1])], "group" = groups[1])
  names(data_ouput)[2] <- "total"
  for (i in 2:length(columns)) {
    data_temp <- cbind(data_input[, c("date", columns[i])], "group" = groups[i])
    names(data_temp)[2] <- "total"
    data_ouput <- rbind(data_ouput, data_temp)
  }
  
  data_ouput[, "group"] <- factor(data_ouput[, "group"], levels = groups)
  
  return(data_ouput)
}



# ####################################################
# Data transformation
# ####################################################


data_model <- function(data_input,
                       column,
                       chosen_groups = NULL,
                       drop_na = T,
                       prefix = "col_") {
  data_input$chosen_col <- data_input[, column]
  
  if (length(chosen_groups) == 0) {
    if (drop_na) {
      chosen_groups <- unique(data_input$chosen_col)[!is.na(unique(data_input$chosen_col))]
    } else {
      chosen_groups <- unique(data_input$chosen_col)
    }
  }
  
  
  for (l in 1:length(chosen_groups)) {
    data_temp <- data_input %>%
      filter(chosen_col == chosen_groups[l]) %>%
      group_by(date) %>%
      summarise(total = n()) %>%
      mutate(total = replace_na(total, 0))
    
    if (l == 1) {
      data_output <- data_temp
    } else {
      data_output <- data_output %>%
        full_join(data_temp,
                  by = "date"
        ) %>%
        mutate(total = replace_na(total, 0))
    }
    
    for (i in 2:length(names(data_output))) {
      data_output[is.na(data_output[, i]), names(data_output)[i]] <- 0
    }
    
    names(data_output)[length(data_output)] <- paste0(prefix, tolower(chosen_groups[l]))
  }
  
  
  return(data_output)
}




##########################################################################################################
# Detect source of deviations

source_deviation <- function(
  data_input,
  model_baseline,
  baseline_mix,
  chosen_period,
  attributes,
  thresholds = -10) {
  if (length(thresholds) == 1) {
    thresholds <- c(-abs(thresholds), abs(thresholds))
  }
  
  cf <- data_input %>% estim_counterfactual(
    model_baseline,
    chosen_period,
    0
  )
  cf <- data.frame(
    "date" = chosen_period,
    "counterfactual" = cf
  )
  data_input <- data_input %>%
    left_join(cf,
              by = "date"
    )
  data_input <- data_input %>%
    mutate(
      counterfactual = ifelse(!is.na(counterfactual),
                              counterfactual,
                              total
      ),
      counterfactual = ifelse(counterfactual < 0, 0, counterfactual),
      delta = total - counterfactual
    )
  
  
  data_temp <- data_input %>%
    mutate(predictions = counterfactual) %>%
    counterfactual_mix(
      col_names = attributes[[1]],
      group_names = attributes[[1]],
      baseline_period = baseline_mix,
      counterfactual_period = chosen_period
    ) %>%
    filter(date %in% chosen_period)
  
  
  for (l in 2:length(attributes)) {
    data_temp <- rbind(
      data_temp,
      data_input %>%
        mutate(predictions = counterfactual) %>%
        counterfactual_mix(
          col_names = attributes[[l]],
          group_names = attributes[[l]],
          baseline_period = baseline_mix,
          counterfactual_period = chosen_period
        ) %>%
        filter(date %in% chosen_period)
    )
  }
  
  data_output <- data_temp %>%
    filter(delta > thresholds[2] | delta < thresholds[1]) %>%
    arrange(desc(abs(delta)))
  
  
  return(data_output)
}
