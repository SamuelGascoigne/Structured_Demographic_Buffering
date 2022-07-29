# calculate stochastic growth rate given a list of matrices
# this code was adapted from the popbio package
#
calculate_stochastic_growth_rate <- function(matrices, end_time = 1000, seed = 1234){
  
  set.seed(seed)
  
  dimension <- nrow(matrices[[1]])
  
  matrix_number <- length(matrices)
  
  
  n0 <- stats::runif(dimension)
  
  r <- numeric(end_time)
  
  
  for (t in 1:end_time)
  {
    
    mat <- sample(1:matrix_number, 1)
    
    A <- matrices[[mat]]
    
    n0 <- A %*% n0
    
    N <- sum(n0)
    
    r[t] <- log(N)
    
    n0 <- n0 / N
    
  }
  
  stoch_lambda <- exp(mean(r))
  
  return(stoch_lambda)
  
}


# calculate the mean, variance and coefficient of variation for each vital rate
# in a stochastic list of matrices

K_kernel_variance_perturbation <- function(K_kernel_list, pert_value = 1e-6, verbose = FALSE){
  
  
  
  ptm <- proc.time()
  
  dimension <- nrow(K_kernel_list[[1]])
  
  true_lambda <- calculate_stochastic_growth_rate(K_kernel_list)
  
  
  
  lambda_pert_matrix <- matrix(NA, nrow = dimension, ncol = dimension)
  
  elasticity_matrix <- matrix(NA, nrow = dimension, ncol = dimension)
  
  
  var_matrix <- parameter_matrices(K_kernel_list)$var_matrix
  
  new_var_matrix <- var_matrix
  
  count <- 1
  
  total <- dimension^2
  
  name <- deparse(substitute(K_kernel_list))
  
  
  for(row in 1:dimension){
    
    for(column in 1:dimension){
      
      K_record_pert <- K_kernel_list
      
      record_values <- c()
      
      for(kernel in 1:length(K_record_pert)){
        
        
        record_values[kernel] <- K_record_pert[[kernel]][row, column]
        
      }  
      
      # calculate parameters (mean and variance) for the element across all kernels
      
      mean_value <- mean(record_values)
      
      variance_value <- var(record_values)
      
      
      # generate array of values that is standardize to a mean of zero
      
      standardized_vector <- record_values - mean_value
      
      
      # generate perturbation value vector
      
      perturbation_vector <- standardized_vector * pert_value
      
      
      
      # calculate variance of new vector
      
      new_vector <- record_values + perturbation_vector
      
      new_var <- var(new_vector)
      
      
      
      for(kernel in 1:length(K_record_pert)){
        
        
        K_record_pert[[kernel]][row, column] <- K_record_pert[[kernel]][row, column] + perturbation_vector[kernel]
        
      }  
      
      
      if(verbose == TRUE){
        
        
        if(count == 1 || count %% 100 == 0 || count == total){
          
          name <- deparse(substitute(K_kernel_list))
          
          percentage <- (count/total) * 100
          
          run_time <- proc.time() - ptm
          
          run_time_message <- round(run_time["elapsed"])
          
          run_time_message <- seconds_to_period(run_time_message)
          
          
          message(name, " K-kernel perturbation: ", count, "/", total, " (", 
                  percentage, "%, ", run_time_message, ")")
          
        }
        
      }
      
      lambda_pert_value <- calculate_stochastic_growth_rate(K_record_pert)
      
      lambda_pert_matrix[row, column] <- lambda_pert_value
      
      #new_var_matrix <- parameter_matrices(K_record_pert)$var_matrix
      
      
      # elasticity_matrix[row, column] <- (log(lambda_pert_value) - log(true_lambda)) / log(pert_value)
      
      elasticity_matrix[row, column] <-  (var_matrix[row, column]/true_lambda)* ((lambda_pert_value - true_lambda)/ (new_var - var_matrix[row, column]))
      
      count <- count + 1
      
    }
    
  }
  
  return(elasticity_matrix)
  
  
}




# calculate the elasticity and sensitivity for the P and F subkernels

subkernel_variance_perturbations <- function(P_kernel_list, F_kernel_list, K_kernel_list = NA, pert = 1e-6){
  
  ptm <- proc.time()
  
  
  if(is.na(K_kernel_list) == TRUE){
    
    # initialize K_kernel_list to have the same structure as a subkernel
    
    K_kernel_list <- P_kernel_list
    
    for(mat in 1:length(P_kernel_list)){
      
      K_kernel_list[[mat]] <- P_kernel_list[[mat]] + F_kernel_list[[mat]]
      
    }
  }
  
  
  
  
  
  
  dimension <- nrow(K_kernel_list[[1]])
  
  true_lambda <- calculate_stochastic_growth_rate(K_kernel_list)
  
  pert_value <- pert
  
  
  lambda_pert_matrix <- matrix(NA, nrow = dimension, ncol = dimension)
  
  P_elasticity_matrix <- matrix(NA, nrow = dimension, ncol = dimension)
  
  F_elasticity_matrix <- matrix(NA, nrow = dimension, ncol = dimension)
  
  
  count <- 1
  
  total <- dimension^2
  
  name <- deparse(substitute(P_kernel_list))
  
  
  K_record_pert <- K_kernel_list
  
  for(row in 1:dimension){
    
    for(column in 1:dimension){
      
      P_record_pert <- P_kernel_list
      
      record_values <- c()
      
      for(kernel in 1:length(K_record_pert)){
        
        
        record_values[kernel] <- P_record_pert[[kernel]][row, column]
        
      }  
      
      # calculate parameters (mean and variance) for the element across all kernels
      
      mean_value <- mean(record_values)
      
      variance_value <- var(record_values)
      
      
      # generate array of values that is standardize to a mean of zero
      
      standardized_vector <- record_values - mean_value
      
      
      # generate perturbation value vector
      
      perturbation_vector <- standardized_vector * pert_value
      
      
      
      for(kernel in 1:length(K_record_pert)){
        
        
        P_record_pert[[kernel]][row, column] <- P_record_pert[[kernel]][row, column] + perturbation_vector[kernel]
        
      }
      
      if(count == 1 || count %% 100 == 0 || count == total){
        
        percentage <- (count/total) * 100
        
        run_time <- proc.time() - ptm
        
        run_time_message <- round(run_time["elapsed"])
        
        run_time_message <- seconds_to_period(run_time_message)
        
        message(name, " P-kernel perturbation: ", count, "/", total, " (", 
                percentage, "%, ", run_time_message, ")")
        
      }
      
      for(mat in 1:length(P_kernel_list)){
        
        K_record_pert[[mat]] <- P_record_pert[[mat]] + F_kernel_list[[mat]]
        
      }
      
      
      
      lambda_pert_value <- calculate_stochastic_growth_rate(K_record_pert)
      
      lambda_pert_matrix[row, column] <- lambda_pert_value
      
      P_elasticity_matrix[row, column] <- (log(lambda_pert_value) - log(true_lambda)) / log(pert_value)
      
      count <- count + 1
      
    }
    
  }
  
  
  
  count <- 1
  
  name <- deparse(substitute(F_kernel_list))
  
  
  K_record_pert <- K_kernel_list
  
  
  for(row in 1:dimension){
    
    for(column in 1:dimension){
      
      F_record_pert <- F_kernel_list
      
      record_values <- c()
      
      
      for(kernel in 1:length(K_record_pert)){
        
        
        record_values[kernel] <- F_record_pert[[kernel]][row, column]
        
      }  
      
      # calculate parameters (mean and variance) for the element across all kernels
      
      mean_value <- mean(record_values)
      
      variance_value <- var(record_values)
      
      
      # generate array of values that is standardize to a mean of zero
      
      standardized_vector <- record_values - mean_value
      
      
      # generate perturbation value vector
      
      perturbation_vector <- standardized_vector * pert_value
      
      
      
      for(kernel in 1:length(K_record_pert)){
        
        
        F_record_pert[[kernel]][row, column] <- F_record_pert[[kernel]][row, column] + perturbation_vector[kernel]
        
      }
      
      if(count == 1 || count %% 100 == 0 || count == total){
        
        percentage <- (count/total) * 100
        
        run_time <- proc.time() - ptm
        
        run_time_message <- round(run_time["elapsed"])
        
        run_time_message <- seconds_to_period(run_time_message)
        
        message(name, " F-kernel perturbation: ", count, "/", total, " (", 
                percentage, "%, ", run_time_message, ")")
        
      }
      
      for(mat in 1:length(P_kernel_list)){
        
        K_record_pert[[mat]] <- F_record_pert[[mat]] + P_kernel_list[[mat]]
        
      }
      
      
      
      lambda_pert_value <- calculate_stochastic_growth_rate(K_record_pert)
      
      lambda_pert_matrix[row, column] <- lambda_pert_value
      
      F_elasticity_matrix[row, column] <- (log(lambda_pert_value) - log(true_lambda)) / log(pert_value)
      
      count <- count + 1
      
    }
    
  }
  
  
  
  output <- list("P_elasticity_matrix" = P_elasticity_matrix,
                 "F_elasticity_matrix" = F_elasticity_matrix)
  
  
  return(output)
  
  
}




iterate_stage_distribution <- function(K_kernel_list, distribution_number = 1000, simple = TRUE){
  
  
  seed_vector <- c(1:distribution_number)
  
  stage_distribution_list <- list()
  
  number_of_stages <- nrow(K_kernel_list[[1]])
  
  
  for(seed in 1:length(seed_vector)){
    
    set.seed(seed_vector[seed])
    
    n0 <- runif(number_of_stages)
    
    n0 <- n0/sum(n0)
    
    n0_matrix <- matrix(data = NA, nrow = number_of_stages, ncol = length(K_kernel_list)+1)
    
    n0_matrix[,1] <- n0
    
    
    for(kernel in 1:length(K_kernel_list)){
      
      n0 <- K_kernel_list[[kernel]] %*% n0
      
      n0 <- n0/sum(n0)
      
      n0_matrix[, kernel+1] <- n0
      
    }
    
    stage_distribution <- rowMeans(n0_matrix)
    
    
    stage_distribution_list[[seed]] <- stage_distribution
    
    
  }
  
  seed <- rep(seed_vector, each = length(K_kernel_list))
  
  stage <- rep(c(1:number_of_stages), times = length(K_kernel_list))
  
  stage_distribution_df <- ldply(stage_distribution_list, data.frame)
  
  names(stage_distribution_df)[1] <- "frequency"
  
  stage_distribution_df <- cbind(seed, stage, stage_distribution_df)
  
  if(simple == FALSE){
    
    output <- list("stage_distribution_df" = stage_distribution_df,
                   "stage_distribution_list" = stage_distribution_list)
    
  } else {
    
    frequency <- stage_distribution_df |> 
      group_by(stage) |> 
      dplyr::summarise(frequency = mean(frequency))
    
    output <- frequency
    
    
  }
  
  
  return(output)
  
}

# calculate the mean, variance and coefficient of variation for each vital rate
# in a stochastic list of matrices

parameter_matrices <- function(matrices){
  
  dimension <- nrow(matrices[[1]])
  
  omnibus_matrix <- matrix(0, nrow = length(matrices), ncol = dimension^2)
  
  for(mat_num in 1:length(matrices)){
    
    omnibus_matrix[mat_num, ] <- as.vector(matrices[[mat_num]])
    
  }
  
  mean_matrix <- matrix(colSums(omnibus_matrix)/length(matrices),
                        nrow = dimension, ncol = dimension)
  
  sd_vector <- c()
  
  for(col in 1:ncol(omnibus_matrix)){
    
    sd_vector[col] <- sd(omnibus_matrix[,col])
    
  }
  
  var_matrix <- matrix(sd_vector^2, 
                       nrow = dimension, ncol = dimension)
  
  cv_matrix <- sqrt(var_matrix)/mean_matrix
  
  
  output <- list("mean_matrix" = mean_matrix,
                 "var_matrix" = var_matrix,
                 "cv_matrix" = cv_matrix)
  
  return(output)
  
}


