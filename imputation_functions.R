# Functions used for imputation.R

# delete random obs
# function takes data with missings, a vector containing the variables
# that should get missings and a vector containing the percentages of
# values that should get deleted
# gives back a list containing a list of rows with missing values, the 
# dataset with artificial missings and the dataset with no missings
delete_obs <- function(data, var_with_missings, percentage){ # delete random obs
  
  data_no_NA <- na.omit(data)
  data_missings <- data_no_NA
  n_row <- nrow(data_no_NA)
  length_var <- length(var_with_missings)
  
  # introduce random NAs
  rows <- mapply(sample, rep(n_row, length_var), percentage * n_row)
  for (i in 1:length_var) {
    data_missings[rows[[i]], var_with_missings[i]] <- NA
  }
  return(list(rows = rows, data_missings = data_missings,
              data_no_NA = data_no_NA))
}

# function takes data with artifical missings, data with no missings,
# list of rows with missings, a vector containing the continous variables
# and m = 5 the number of imputations
# function returns plot of imputed vs true values
mice_with_diagnostic <- function(data_missings, data_no_NA, rows, var_continuous,
                        m = 5, ...) { # imputes and gives diagnostic plot

  n <- nrow(data_no_NA)
  
  # impute
  mice_missings <- mice(data_missings, m, ...)
  data_imputed_list <- complete(mice_missings, "all")
  
  # make one df
  data_imputed <- data.frame()
  for (i in 1:m) {
    data_imputed_i <- data_imputed_list[[i]] %>%
      cbind(number = i)
    data_imputed <- rbind(data_imputed, data_imputed_i)
  }
  
  # plot
  par(mfrow = c(1, 3))
  for (i in 1:length(var_continuous)) {
    if (m != 5) warning("not the right rows selected")
    rows_new <- c(rows[[i]], rows[[i]] + n_row, rows[[i]] + 
                    2 * n_row, rows[[i]] + 3 * n_row,
                  rows[[i]] + 4 * n_row)
    x <- data_no_NA[rows[[i]], var_continuous[i]] %>%
      rep(times = m)
    y <- data_imputed[rows_new, c(var_continuous[i], "number")]
    plot(x, y[, 1], col = alpha(y[, 2]), 
         xlab = "true values",
         ylab = "imputed values")
    abline(0, 1)
    title(var_continuous[i])
  }
}

get_evaluation <- function(imp_eval,
                           start_continuous = 1, end_continuous = 3, 
                           start_categorial = 4, end_categorial = 5) { 
  # gives back the correlation and 
  # percentage of right imputed used to 
  # evaluate the diff methods
  
  # function takes mids object containing the imputation, numeric values to
  # specify where continous and categorical variables are in df
  # returns vector of mean results
  
  foreach(i = rows, j = var) %do% {
    data_no_NA[i,j]
  } -> true_value
  
  correlation_vec <- c()
  percentage_vec <- c()
  
  for (imputation_number in 1:5) {
    foreach(i = rows, j = var) %do% {
      complete(imp_eval, imputation_number)[i, j]
    } -> imp_eval_value
    
    # continuous
    for (i in start_continuous:end_continuous) {
      test <- cor.test(true_value[[i]], imp_eval_value[[i]])
      correlation <- round(test$estimate, 2)
      p_value <- round(test$p.value, 4)
      correlation_vec <- c(correlation_vec, correlation)
    }
    # categorical
    for (i in start_categorial:end_categorial) {
      table <- table(true_value[[i]], imp_eval_value[[i]])
      right <- sum(diag(table))
      total <- sum(table)
      percentage <- round(100 * right/total, 2)
      percentage_vec <- c(percentage_vec, percentage)
    } 
  }
  correlation_df <- data.frame(matrix(correlation_vec, ncol = length(start_continuous:end_continuous),
                              byrow = TRUE))
  percentage_df <- data.frame(matrix(percentage_vec, ncol = length(start_categorial:end_categorial),
                               byrow = TRUE))
  results_df <- cbind(correlation_df, percentage_df)
  names(results_df) <- var
  print(results_df)
  return(apply(results_df, 2, mean))
}
