# Functions used for imputation.R

delete_obs <- function(data, var_w_missings, perc){ # delete random obs
  
  data_no_NA <- na.omit(data)
  data_missings <- data_no_NA
  n <- nrow(data_no_NA)
  n_var <- length(var_w_missings)
  
  # introduce random NAs
  rows <- mapply(sample, rep(n, n_var), perc*n)
  for (i in 1:n_var) {
    data_missings[rows[[i]], var_w_missings[i]] <- NA
  }
  return(list(rows = rows, data_missings = data_missings,
              data_no_NA = data_no_NA))
}

mice_w_diag <- function(data_missings, data_no_NA, rows, var_cont, var_cat,
                        m = 5, ...) { # imputes and gives diagnostic plot
  n <- nrow(data_no_NA)
  
  # impute
  mice_missings <- mice(data_missings, m, ...)
  data_imputed_l <- complete(mice_missings, "all")
  
  # make one df
  data_imputed <- data.frame()
  for(i in 1:m){
    data_imputed_i <- data_imputed_l[[i]] %>%
      cbind(number = i)
    data_imputed <- rbind(data_imputed, data_imputed_i)
  }
  
  # plot
  par(mfrow = c(1, 3))
  for(i in 1:length(var_cont)){
    if(m!=5) warning("not the right rows selected")
    rows_new <- c(rows[[i]],rows[[i]]+n, rows[[i]]+2*n, rows[[i]]+3*n,
                  rows[[i]]+4*n)
    x <- data_no_NA[rows[[i]], var_cont[i]] %>%
      rep(times = m)
    y <- data_imputed[rows_new, c(var_cont[i], "number")]
    plot(x, y[,1], col = alpha(y[,2]), 
         xlab = "true values",
         ylab = "imputed values")
    abline(0, 1)
    title(var_cont[i])
  }
}

get_evaluation <- function(imp_eval,
                           start_cont = 1, end_cont = 3, start_cat = 4,
                           end_cat = 5) { # gives back the correlation and 
  # percentage of right imputed used to 
  # evaluate the diff methods
  foreach(i = rows, j = var) %do% {
    data_no_NA[i,j]
  } -> true_val
  
  cor_vec <- c()
  perc_vec <- c()
  
  for(imputation_number in 1:5){
    foreach(i = rows, j = var) %do% {
      complete(imp_eval, imputation_number)[i,j]
    } -> imp_eval_val
    
    # continuous
    for(i in start_cont:end_cont){
      test <- cor.test(true_val[[i]], imp_eval_val[[i]])
      cor <- round(test$estimate, 2)
      p_val <- round(test$p.value, 4)
      cor_vec <- c(cor_vec, cor)
    }
    # categorical
    for(i in start_cat:end_cat){
      table <- table(true_val[[i]], imp_eval_val[[i]])
      right <- sum(diag(table))
      total <- sum(table)
      perc <- round(100*right/total, 2)
      perc_vec <- c(perc_vec, perc)
    } 
  }
  cor_df <- data.frame(matrix(cor_vec, ncol = length(start_cont:end_cont),
                              byrow = TRUE))
  perc_df <- data.frame(matrix(perc_vec, ncol = length(start_cat:end_cat),
                               byrow = TRUE))
  results_df <- cbind(cor_df, perc_df)
  names(results_df) <- var
  print(results_df)
  return(apply(results_df, 2, mean))
}