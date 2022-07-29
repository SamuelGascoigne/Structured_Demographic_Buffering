
simulate_environment <- function(mean, 
                                 standard_dev, 
                                 autocorrelation_coefficient, 
                                 duration,
                                 seed = 1234){
  
  set.seed(seed)
  
  # generate n+2 standard normal variates
  E = rnorm(duration+2) 
  
  
  Y = numeric(duration) # initialize the vector of interest
  Y[1] = E[3] + autocorrelation_coefficient*E[2]
  Y[2] = E[4] + autocorrelation_coefficient*Y[1]
  for (i in 3:duration) Y[i] = E[i+2] + autocorrelation_coefficient*Y[i-1] 
  
  resources <- standard_dev*Y/sd(Y)
  resources <- (resources-mean(resources)) + mean
  
  return(resources)
}

sample_env <- function(env_data, index){
  
  as.list(env_data[index, ]) |> 
    setNames(names(env_data))
  
}

kernel_extraction <- function(ipm_array){
  
  
  # initialize lists to store P, F and K kernels
  
  P_record <- list()
  
  F_record <- list()
  
  K_record <- list()
  
  
  # use a counter to index the position of added matrices
  
  count <- 1
  
  iteration_number <- dim(ipm_array)[3]
  
  # run through all matrices in the ipm_array and add them to the kernel lists
  
  for(matrix in 1:iteration_number){
    
    if(matrix %% 2 == 1){
      
      P_record[[count]] <- ipm_array[,,matrix]
      
    } else if(matrix %% 2 == 0){
      
      F_record[[count]] <- ipm_array[,,matrix]
      
      K_record[[count]] <- ipm_array[,,matrix-1] +ipm_array[,,matrix]
      
      count <- count + 1
      
    }
    
  }
  
  
  
  output <- list("K_record" = K_record,
                 "P_record" = P_record,
                 "F_record" = F_record)
  
  return(output)
  
  
}

Berberis_thunbergii_kernels <- function(cv, autocorrelation, seed = 1234, iteration_number = 1000){
  
  
  
  species <- pdb$Metadata$species_accepted[pdb$Metadata$ipm_id == "aaaa59"]
  
  proto_ipm   <- pdb_make_proto_ipm(pdb, 
                                    ipm_id = "aaaa59",
                                    det_stoch = "stoch",
                                    kern_param = "param")
  
  
  PAR_mean <- 0
  
  PAR_standard_dev <- cv * 1.5
  
  use_PAR <- simulate_environment(mean = PAR_mean, 
                                  standard_dev = PAR_standard_dev, 
                                  autocorrelation_coefficient = autocorrelation, 
                                  duration = iteration_number,
                                  seed = seed)
  
  
  Ni_mean <- 0
  
  Ni_standard_dev <- cv * 1.5
  
  use_Ni <- simulate_environment(mean = Ni_mean, 
                                 standard_dev = Ni_standard_dev, 
                                 autocorrelation_coefficient = autocorrelation, 
                                 duration = iteration_number,
                                 seed = seed + 1)
  
  
  pH_mean <- 0
  
  pH_standard_dev <- cv * 1.5
  
  use_pH <- simulate_environment(mean = pH_mean, 
                                 standard_dev = pH_standard_dev, 
                                 autocorrelation_coefficient = autocorrelation, 
                                 duration = iteration_number,
                                 seed = seed + 2)
  
  mt_warm_month_mean <- 0
  
  mt_warm_month_standard_dev <- cv * 1.5
  
  use_mt_warm_month <- simulate_environment(mean = mt_warm_month_mean, 
                                            standard_dev = mt_warm_month_standard_dev, 
                                            autocorrelation_coefficient = autocorrelation, 
                                            duration = iteration_number,
                                            seed = seed + 3)
  
  
  mp_may_mean <- 0
  
  mp_may_standard_dev <- cv * 1.5
  
  use_mp_may <- simulate_environment(mean = mp_may_mean, 
                                     standard_dev = mp_may_standard_dev, 
                                     autocorrelation_coefficient = autocorrelation, 
                                     duration = iteration_number,
                                     seed = seed + 4)
  
  
  
  use_env_data <- data.frame(PAR = use_PAR,
                             Ni = use_Ni,
                             pH = use_pH,
                             mt_warm_month = use_mt_warm_month,
                             mp_may = use_mp_may)
  
  
  
  proto_ipm$aaaa59$env_state <- lapply(
    proto_ipm$aaaa59$env_state,
    function(x) return(NA))
  
  proto_ipm$aaaa59 <- define_env_state(
    proto_ipm$aaaa59,
    env_vars = sample_env(use_env_data, index = t),
    data_list = list(use_env_data = use_env_data,
                     sample_env = sample_env))
  
  args  <-list(
    
    # The names in the outermost list should be ipm_id's
    
    aaaa59 = list(
      
      # The names in the inner list should be arguments to make_ipm()
      
      iterate         = TRUE,
      iterations      = iteration_number,
      return_sub_kernels = TRUE
      
    )
  )
  
  ipms <- pdb_make_ipm(proto_ipm, addl_args = args)
  
  
  # convert the subkernels into an array of matrices
  
  Berberis_thunbergii_ipm_array <- array(unlist(ipms$aaaa59$sub_kernels), dim = c(50, 50, iteration_number*2))
  
  output <- kernel_extraction(Berberis_thunbergii_ipm_array)
  
  
  return(output)
  
  
}

Calathea_crotalifera_kernels <- function(cv, autocorrelation, seed = 1234, iteration_number = 1000){
  
  
  
  species <- pdb$Metadata$species_accepted[pdb$Metadata$ipm_id == "aaaa15"]
  
  proto_ipm   <- pdb_make_proto_ipm(pdb, 
                                    ipm_id = "aaaa15",
                                    det_stoch = "stoch",
                                    kern_param = "param")
  
  j_mean <- 3
  
  j_standard_dev <- cv * 1.4
  
  use_j <- simulate_environment(mean = j_mean, 
                                standard_dev = j_standard_dev, 
                                autocorrelation_coefficient = autocorrelation, 
                                duration = iteration_number,
                                seed = seed)
  
  a_mean <- 6
  
  a_standard_dev <- cv * 0.8
  
  use_a <- simulate_environment(mean = a_mean, 
                                standard_dev = a_standard_dev, 
                                autocorrelation_coefficient = autocorrelation, 
                                duration = iteration_number,
                                seed = seed + 1)
  
  
  
  use_env_data <- data.frame(j = use_j, A_max = use_a)
  
  
  
  proto_ipm$aaaa15$env_state <- lapply(
    proto_ipm$aaaa15$env_state,
    function(x) return(NA))
  
  proto_ipm$aaaa15 <- define_env_state(
    proto_ipm$aaaa15,
    env_vars = sample_env(use_env_data, index = t),
    data_list = list(use_env_data = use_env_data,
                     sample_env = sample_env))
  
  args  <-list(
    
    # The names in the outermost list should be ipm_id's
    
    aaaa15 = list(
      
      # The names in the inner list should be arguments to make_ipm()
      
      iterate         = TRUE,
      iterations      = iteration_number,
      return_sub_kernels = TRUE
      
    )
  )
  
  ipms <- pdb_make_ipm(proto_ipm, addl_args = args)
  
  
  # convert the subkernels into an array of matrices
  
  Calathea_crotalifera_ipm_array <- array(unlist(ipms$aaaa15$sub_kernels), dim = c(50, 50, iteration_number*2))
  
  output <- kernel_extraction(Calathea_crotalifera_ipm_array)
  
  
  
  return(output)
  
  
}

Heliconia_tortuosa_kernels <- function(cv, autocorrelation, seed = 1234, iteration_number = 1000){
  
  
  
  species <- pdb$Metadata$species_accepted[pdb$Metadata$ipm_id == "aaaa16"]
  
  proto_ipm   <- pdb_make_proto_ipm(pdb, 
                                    ipm_id = "aaaa16",
                                    det_stoch = "stoch",
                                    kern_param = "param")
  
  j_mean <- 3
  
  j_standard_dev <- cv * 1.4
  
  use_j <- simulate_environment(mean = j_mean, 
                                standard_dev = j_standard_dev, 
                                autocorrelation_coefficient = autocorrelation, 
                                duration = iteration_number, 
                                seed = seed)
  
  
  a_mean <- 6.5
  
  a_standard_dev <- cv * 0.8654937
  
  use_a <- simulate_environment(mean = a_mean, 
                                standard_dev = a_standard_dev, 
                                autocorrelation_coefficient = autocorrelation, 
                                duration = iteration_number, 
                                seed = seed + 1)
  
  
  
  use_env_data <- data.frame(j = use_j, A_max = use_a)
  
  
  
  proto_ipm$aaaa16$env_state <- lapply(
    proto_ipm$aaaa16$env_state,
    function(x) return(NA))
  
  proto_ipm$aaaa16 <- define_env_state(
    proto_ipm$aaaa16,
    env_vars = sample_env(use_env_data, index = t),
    data_list = list(use_env_data = use_env_data,
                     sample_env = sample_env))
  
  args  <-list(
    
    # The names in the outermost list should be ipm_id's
    
    aaaa16 = list(
      
      # The names in the inner list should be arguments to make_ipm()
      
      iterate         = TRUE,
      iterations      = iteration_number,
      return_sub_kernels = TRUE
      
    )
  )
  
  ipms <- pdb_make_ipm(proto_ipm, addl_args = args)
  
  
  # convert the subkernels into an array of matrices
  
  Heliconia_tortuosa_ipm_array <- array(unlist(ipms$aaaa16$sub_kernels), dim = c(50, 50, iteration_number*2))
  
  output <- kernel_extraction(Heliconia_tortuosa_ipm_array)
  
  return(output)
  
  
}


