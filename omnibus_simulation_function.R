omnibus_simulation <- function(Genus_species_vector, 
                                    variance_vector, 
                                    autocorrelation_vector,
                                    seed_vector = 1234,
                                    iteration_number = 1000,
                                    start_pert = 200,
                                    Haridas_method = FALSE,
                                    save_iterations = TRUE,
                                    delete_iterations = TRUE,
                                    save_after = TRUE,
                                    simulation_file_name = NA){
  
  
  # Start a timer to record the length of the simulation.
  
  ptm <- proc.time()
  
  
  
  # Calculate the number of simulations that will take place in the simulation.
  
  number_of_simulations <- length(seed_vector) * length(variance_vector) * length(autocorrelation_vector) * length(Genus_species)
  
  
  
  # Create vectors and lists to store data throughout the simulation
  
  elapsed_time_vec <- c() # Time for each simulation within the omnibus simulation.
  
  if(Haridas_method == TRUE){
    
    elasticity_values_list <- list() # Matrices of elasticity values calculated via Haridas et al. (2009).
    
  }
  
  simulation_elasticity_values_list <- list() # Matrices of elasticity values calculated via simulation.
  
  simulation_P_elasticity_values_list <- list() # Matrices of P kernel elasticity values calculated via simulation.
  
  simulation_F_elasticity_values_list <- list() # Matrices of F kernel elasticity values calculated via simulation.
  
  stage_dist_list <- list() # Average stage distribution vectors.
  
  seed_bank <- c() # Seed values
  
  variance <- c() # Variance values.
  
  autocorrelation <- c() # Autocorrelation values.
  
  Genus_species <- c() # Genus and species.
  
  stochastic_lambda <- c() # Stochastic lambda.
  
  if(Haridas_method == TRUE){
    
    buffering <- c() # Summation of stochastic elasticities of variance using Haridas et al. (2009).
    
    buffering_SSD <- c() # Summation of stochastic elasticities of variance using Haridas et al. (2009) corrected for stage distribution.
    
  }
  
  simulation_buffering <- c() # Summation of stochastic elasticities of variance using simulation.
  
  simulation_buffering_SSD <- c() # Summation of stochastic elasticities of variance using simulation corrected for stage distribution.
  
  simulation_P_buffering <- c() # Summation of stochastic elasticities of P subkernel variance using simulation.
  
  simulation_P_buffering_SSD <- c() # Summation of stochastic elasticities of P subkernel variance using simulation corrected for stage distribution.
  
  simulation_F_buffering <- c() # Summation of stochastic elasticities of F subkernel variance using simulation.
  
  simulation_F_buffering_SSD <- c() # Summation of stochastic elasticities of F subkernel variance using simulation corrected for stage distribution.
  
  if(Haridas_method == TRUE){
    
    avg_stage_buffering <- c() # # Average stage weighted by the summation of stochastic elasticities of variance using Haridas et al. (2009).
    
    avg_stage_buffering_SSD <- c() # # Average stage weighted by the summation of stochastic elasticities of variance using Haridas et al. (2009) corrected for stage distribution.
    
  }
  
  avg_stage_simulation_buffering <- c() # Average stage weighted by the summation of stochastic elasticities of variance using simulation.
  
  avg_stage_simulation_buffering_SSD <- c() # Average stage weighted by the summation of stochastic elasticities of variance using simulation corrected for stage distribution.
  
  avg_stage_simulation_P_buffering <- c() # Average stage weighted by the summation of stochastic elasticities of P subkernel variance using simulation.
  
  avg_stage_simulation_P_buffering_SSD <- c() # Average stage weighted by the summation of stochastic elasticities of P subkernel variance using simulation corrected for stage distribution.
  
  avg_stage_simulation_F_buffering <- c() # Average stage weighted by the summation of stochastic elasticities of F subkernel variance using simulation.
  
  avg_stage_simulation_F_buffering_SSD <- c() # Average stage weighted by the summation of stochastic elasticities of F subkernel variance using simulation corrected for stage distribution.
  
  
  
  
  
  # Initialize a counter that will allow for the easy addition of data to 
  # the above vectors and lists throughout the nested for loops.
  
  count <- 1
  
  
  
  # Create a string that contains the start time of the simulation.
  
  start_time <- str_replace_all(Sys.time(), ":", ".")
  
  
  
  # Define the name of the file that will store iterations of the final output as the simulation runs.
  
  temp_file_name <-paste("data/saving_iterations", sep = "")
  
  
  
  # Create the temporary storage file if save_iterations is true and the file does not already exist.
  
  if(save_iterations == TRUE && file.exists(temp_file_name) != TRUE){
    
    dir.create(temp_file_name)
    
  } 
  
  
  
  # This series of nested for loops runs through all species in the genus_species vector as well as all
  # values in the variance and autocorrelation vectors in a full-factorial manner. Simply put, all species 
  # are exposed to all possible combinations of autocorrelation and variance from the autocorrelation and 
  # variance vectors.
  
  for(seedling in 1:length(seed_vector)){
    
    for(species in 1:length(Genus_species_vector)){
      
      for(var in 1:length(variance_vector)){
        
        for(auto in 1: length(autocorrelation_vector)){
          
          
          # Print a message saying what number simulation out of the total number of simulations to be run.
          
          message("\nRunning simulation ", count, " out of ", number_of_simulations, ".")
          
          
          
          # Record the time in order to calculate the length of this iteration of the nested for loops.
          
          ptm_single <- proc.time()
          
          
          
          # Construct the K, P and F kernels for the species specified.
          # Each species has its own kernel construction function. This may be refactored in the future.
          
          if(Genus_species_vector[species] == "Berberis_thunbergii" || Genus_species_vector[species] == "Berberis thunbergii"){
            
            simulation_ipms <- Berberis_thunbergii_kernels(cv = variance_vector[var], 
                                                           autocorrelation = autocorrelation_vector[auto],
                                                           seed = seed_vector[seedling],
                                                           iteration_number = iteration_number)
            
          } else if(Genus_species_vector[species] == "Calathea_crotalifera" || Genus_species_vector[species] == "Calathea crotalifera"){
            
            simulation_ipms <- Calathea_crotalifera_kernels(cv = variance_vector[var], 
                                                            autocorrelation = autocorrelation_vector[auto],
                                                            seed = seed_vector[seedling],
                                                            iteration_number = iteration_number)
            
          } else if(Genus_species_vector[species] == "Heliconia_tortuosa" || Genus_species_vector[species] == "Heliconia tortuosa"){
            
            simulation_ipms <- Heliconia_tortuosa_kernels(cv = variance_vector[var], 
                                                          autocorrelation = autocorrelation_vector[auto],
                                                          seed = seed_vector[seedling],
                                                          iteration_number = iteration_number)
          } else{
            
            # An error message that indicates there is an issue with the species defined in Genus_species_vector
            # and escapes the omnibus_simulation function.
            
            message("Genus_species_vector was entered incorrectly.")
            
            return() # This return call escapes the simulation.
            
          }
          
          
          
          # Compute the stage distribution for each iteration of the K kernels.
          
          stage_dist_complex <- iterate_stage_distribution(simulation_ipms$K_record, simple = FALSE)$stage_distribution_list
          
          
          
          # Compute the average stage distribution across the simulation.
          
          stage_dist_simple <- iterate_stage_distribution(simulation_ipms$K_record, simple = TRUE)
          
          
          
          # Next, we compute stochastic elasticities as per Haridas et al. (2009).
          
          # Compute stochastic elasticities.
          # This function is from the popdemo package.
          
          if(Haridas_method == TRUE){
            
            K_perturbation <- perturb_stochastic(simulation_ipms$K_record [start_pert:iteration_number], stage_dist_complex[start_pert:iteration_number])
            
            
            
            # Extract the stochastic elasticities of variance from K_perturbation.
            
            elasticity_values <- K_perturbation$E_sigma
            
            
            
            # Sum the columns in the elasticity values matrix.
            # This represents the contribution of vital rate variance on population growth structured by stage.
            
            elasticity_values_colSums <- colSums(elasticity_values)
            
          }
          
          
          
          # Now we calculate stochastic elasticities of variance via simulation.
          
          # Calculate stochastic elasticities of variance for the K-kernel.
          
          simulation_elasticity_values <- K_kernel_variance_perturbation(simulation_ipms$K_record[start_pert:iteration_number], pert_value = 1e-6)
          
          
          
          # Calculate stochastic elasticities of variance for the subkernels.
          
          simulation_subkernel_elasticity_values <- subkernel_variance_perturbations(simulation_ipms$P_record[start_pert:iteration_number],
                                                                                     simulation_ipms$F_record[start_pert:iteration_number])
          
          
          # Sum the columns in the elasticity values matrix.
          # This represents the contribution of vital rate variance on population growth structured by stage.
          
          simulation_elasticity_values_colSums <- colSums(simulation_elasticity_values)
          
          
          
          # After calculating all of those values, let's store them in the vectors and lists we created at the beginning of the function.
          
          
          if(Haridas_method == TRUE){
            
            elasticity_values_list[[count]] <- elasticity_values # Stochastic elasticities of variance via Haridas et al. (2009).
            
          }
          
          simulation_elasticity_values_list[[count]] <- simulation_elasticity_values # Stochastic elasticities of variance via simulation.
          
          simulation_P_elasticity_values_list[[count]] <- simulation_subkernel_elasticity_values$P_elasticity_matrix
          
          simulation_F_elasticity_values_list[[count]] <- simulation_subkernel_elasticity_values$F_elasticity_matrix
          
          stage_dist_list[[count]] <- stage_dist_simple # Average stage distribution.
          
          seed_bank[count] <- seed_vector[seedling] # Seed value.
          
          variance[count] <- variance_vector[var] # Variance value.
          
          autocorrelation[count] <- autocorrelation_vector[auto] # Autocorrelation value.
          
          Genus_species[count] <- Genus_species_vector[species] # Genus and species.
          
          stochastic_lambda[count] <- calculate_stochastic_growth_rate(simulation_ipms$K_record, 
                                                                       end_time = length(simulation_ipms$K_record)) # Stochastic lambda.
          
          
          
          # Calculating buffering values.
          
          if(Haridas_method == TRUE){
            
            buffering[count] <- sum(elasticity_values) # Summation of stochastic elasticities of variance via Haridas et al. (2009).
            
            buffering_SSD[count] <- sum(colSums(elasticity_values) * stage_dist_simple$frequency) # Summation of stochastic elasticities of variance using Haridas et al. (2009) corrected for stage distribution.
            
          }
          
          simulation_buffering[count] <- sum(simulation_elasticity_values) # Summation of stochastic elasticities of variance using simulation.
          
          simulation_buffering_SSD[count] <- sum(colSums(simulation_elasticity_values) * stage_dist_simple$frequency) # Summation of stochastic elasticities of variance using simulation corrected for stage distribution.
          
          simulation_P_buffering[count] <- sum(simulation_subkernel_elasticity_values$P_elasticity_matrix) # Summation of stochastic elasticities of P subkernel variance using simulation.
          
          simulation_P_buffering_SSD[count] <- sum(colSums(simulation_subkernel_elasticity_values$P_elasticity_matrix) * stage_dist_simple$frequency) # Summation of stochastic elasticities of P subkernel variance using simulation corrected for stage distribution.
          
          simulation_F_buffering[count] <- sum(simulation_subkernel_elasticity_values$F_elasticity_matrix) # Summation of stochastic elasticities of F subkernel variance using simulation.
          
          simulation_F_buffering_SSD[count] <- sum(colSums(simulation_subkernel_elasticity_values$F_elasticity_matrix) * stage_dist_simple$frequency) # Summation of stochastic elasticities of F subkernel variance using simulation corrected for stage distribution.
          
          
          
          # Calculating the average stage of maximal buffering.
          
          stages_vector <- seq(from = 0, to = 1, length.out = ncol(simulation_elasticity_values)) # First set-up a vector that will represent the increments (i.e., proxy for the meshpoints) in the kernels.
          
          if(Haridas_method == TRUE){
            
            avg_stage_buffering[count] <- sum(colSums(elasticity_values) * stages_vector)/sum(colSums(elasticity_values))
            
            avg_stage_buffering_SSD[count] <- sum((colSums(elasticity_values) * stage_dist_simple$frequency) * stages_vector)/sum(colSums(elasticity_values) * stage_dist_simple$frequency)
            
          }
          
          avg_stage_simulation_buffering[count] <- sum(colSums(simulation_elasticity_values) * stages_vector)/sum(colSums(simulation_elasticity_values))
          
          avg_stage_simulation_buffering_SSD[count] <- sum((colSums(simulation_elasticity_values) * stage_dist_simple$frequency) * stages_vector)/sum(colSums(simulation_elasticity_values) * stage_dist_simple$frequency)
          
          avg_stage_simulation_P_buffering[count] <- sum(colSums(simulation_subkernel_elasticity_values$P_elasticity_matrix) * stages_vector)/sum(colSums(simulation_subkernel_elasticity_values$P_elasticity_matrix))
          
          avg_stage_simulation_P_buffering_SSD[count] <- sum((colSums(simulation_subkernel_elasticity_values$P_elasticity_matrix) * stage_dist_simple$frequency) * stages_vector)/sum(colSums(simulation_subkernel_elasticity_values$P_elasticity_matrix) * stage_dist_simple$frequency) 
          
          avg_stage_simulation_F_buffering[count] <- sum(colSums(simulation_subkernel_elasticity_values$F_elasticity_matrix) * stages_vector)/sum(colSums(simulation_subkernel_elasticity_values$F_elasticity_matrix)) 
          
          avg_stage_simulation_F_buffering_SSD[count] <- sum((colSums(simulation_subkernel_elasticity_values$F_elasticity_matrix) * stage_dist_simple$frequency) * stages_vector)/sum(colSums(simulation_subkernel_elasticity_values$F_elasticity_matrix) * stage_dist_simple$frequency) 
          
          
          
          # Now, we calculate how long that iteration of the nested for loops took.
          
          
          # Calculate the elapsed time.
          
          elapsed_time <- (proc.time() - ptm)["elapsed"]
          
          
          
          # Store the elapsed time.
          
          elapsed_time_vec[count] <- (proc.time() - ptm_single)["elapsed"]
          
          
          
          # Print a message with how long the simulation took.
          
          message("This simulation took ", round(elapsed_time_vec[count]/60, digits = 1), " minutes.")
          
          
          
          # If the simulation is not on its last iteration, print how much longer the simulation is expected to take.
          
          if(count < number_of_simulations){
            
            message("Expected time to completion ", round(mean(elapsed_time_vec/60) * (number_of_simulations - count)), " minutes.\n")
            
            
          }
          
          
          # Save the iteration, along with all previous iterations, to the temporary storage file.
          
          if(save_iterations == TRUE && count %% 10 == 0){
            
            
            
            
            # Create a vector of all the files in the temporary save folder.
            
            saving_iterations_files <-  dir("data/saving_iterations")
            
            
            # Identify which files contain the simulation start time in their name.
            
            files_to_remove <-  saving_iterations_files[grepl(start_time, saving_iterations_files, fixed=TRUE)]
            
            
            # Add the directory of the files in question.
            
            files_to_remove <- paste0("data/saving_iterations/", files_to_remove)
            
            
            # Remove the files with simulation start time in their name.
            
            unlink(files_to_remove)
            
            
            
            file_name = paste(temp_file_name, "/iteration_", as.character(count), " ", start_time, ".RData", sep = "")
            
            if(Haridas_method == TRUE){
              
              df_iteration <- data.frame(cbind(Genus_species,
                                               variance, 
                                               autocorrelation, 
                                               stochastic_lambda, 
                                               buffering, 
                                               buffering_SSD, 
                                               simulation_buffering, 
                                               simulation_buffering_SSD,
                                               simulation_P_buffering, 
                                               simulation_P_buffering_SSD,
                                               simulation_F_buffering, 
                                               simulation_F_buffering_SSD,
                                               avg_stage_buffering, 
                                               avg_stage_buffering_SSD, 
                                               avg_stage_simulation_buffering, 
                                               avg_stage_simulation_buffering_SSD,
                                               avg_stage_simulation_P_buffering, 
                                               avg_stage_simulation_P_buffering_SSD,
                                               avg_stage_simulation_F_buffering, 
                                               avg_stage_simulation_F_buffering_SSD,
                                               seed_bank,
                                               iteration_number))
              
              
              output_temp <- list("df" = df,
                                  "elasticity_values_list" = elasticity_values_list,
                                  "simulation_elasticity_values_list" = simulation_elasticity_values_list,
                                  "simulation_P_elasticity_values_list" = simulation_P_elasticity_values_list,
                                  "simulation_F_elasticity_values_list" = simulation_F_elasticity_values_list,
                                  "stage_dist_list" = stage_dist_list,
                                  "Genus_species" = Genus_species,
                                  "variance" = variance,
                                  "autocorrelation" = autocorrelation,
                                  "buffering" = buffering, 
                                  "buffering_SSD" = buffering_SSD, 
                                  "simulation_buffering" = simulation_buffering, 
                                  "simulation_buffering_SSD" = simulation_buffering_SSD,
                                  "simulation_P_buffering" = simulation_P_buffering, 
                                  "simulation_P_buffering_SSD" = simulation_P_buffering_SSD,
                                  "simulation_F_buffering" = simulation_F_buffering, 
                                  "simulation_F_buffering_SSD" = simulation_F_buffering_SSD, 
                                  "avg_stage_buffering" = avg_stage_buffering, 
                                  "avg_stage_buffering_SSD" = avg_stage_buffering_SSD, 
                                  "avg_stage_simulation_buffering" = avg_stage_simulation_buffering, 
                                  "avg_stage_simulation_buffering_SSD" = avg_stage_simulation_buffering_SSD,
                                  "avg_stage_simulation_P_buffering" = avg_stage_simulation_P_buffering, 
                                  "avg_stage_simulation_P_buffering_SSD" = avg_stage_simulation_P_buffering_SSD,
                                  "avg_stage_simulation_F_buffering" = avg_stage_simulation_F_buffering, 
                                  "avg_stage_simulation_F_buffering_SSD" = avg_stage_simulation_F_buffering_SSD, 
                                  "seed_bank" = seed_bank,
                                  "iteration_number" = iteration_number)
              
            } else {
              
              df_iteration <- data.frame(cbind(Genus_species,
                                               variance, 
                                               autocorrelation, 
                                               stochastic_lambda,
                                               simulation_buffering, 
                                               simulation_buffering_SSD,
                                               simulation_P_buffering, 
                                               simulation_P_buffering_SSD,
                                               simulation_F_buffering, 
                                               simulation_F_buffering_SSD,
                                               avg_stage_simulation_buffering, 
                                               avg_stage_simulation_buffering_SSD,
                                               avg_stage_simulation_P_buffering, 
                                               avg_stage_simulation_P_buffering_SSD,
                                               avg_stage_simulation_F_buffering, 
                                               avg_stage_simulation_F_buffering_SSD,
                                               seed_bank,
                                               iteration_number))
              
              
              output_temp <- list("df" = df,
                                  "simulation_elasticity_values_list" = simulation_elasticity_values_list,
                                  "simulation_P_elasticity_values_list" = simulation_P_elasticity_values_list,
                                  "simulation_F_elasticity_values_list" = simulation_F_elasticity_values_list,
                                  "stage_dist_list" = stage_dist_list,
                                  "Genus_species" = Genus_species,
                                  "variance" = variance,
                                  "autocorrelation" = autocorrelation,
                                  "simulation_buffering" = simulation_buffering, 
                                  "simulation_buffering_SSD" = simulation_buffering_SSD,
                                  "simulation_P_buffering" = simulation_P_buffering, 
                                  "simulation_P_buffering_SSD" = simulation_P_buffering_SSD,
                                  "simulation_F_buffering" = simulation_F_buffering, 
                                  "simulation_F_buffering_SSD" = simulation_F_buffering_SSD, 
                                  "avg_stage_simulation_buffering" = avg_stage_simulation_buffering, 
                                  "avg_stage_simulation_buffering_SSD" = avg_stage_simulation_buffering_SSD,
                                  "avg_stage_simulation_P_buffering" = avg_stage_simulation_P_buffering, 
                                  "avg_stage_simulation_P_buffering_SSD" = avg_stage_simulation_P_buffering_SSD,
                                  "avg_stage_simulation_F_buffering" = avg_stage_simulation_F_buffering, 
                                  "avg_stage_simulation_F_buffering_SSD" = avg_stage_simulation_F_buffering_SSD, 
                                  "seed_bank" = seed_bank,
                                  "iteration_number" = iteration_number)
              
            }
            
            save(output_temp, file = file_name)
            
          }
          
          
          # Increase count by one in preparation for the next pass through the nested for loops.
          
          count <- count + 1
          
        }
        
      }
      
    }
    
  }
  
  
  # Build a dataframe with all of the non-matrix/vectorised output.
  
  if(Haridas_method == TRUE){
    
    df <- data.frame(cbind(Genus_species,
                           variance, 
                           autocorrelation, 
                           stochastic_lambda, 
                           buffering, 
                           buffering_SSD, 
                           simulation_buffering, 
                           simulation_buffering_SSD,
                           simulation_P_buffering, 
                           simulation_P_buffering_SSD,
                           simulation_F_buffering, 
                           simulation_F_buffering_SSD,
                           avg_stage_buffering, 
                           avg_stage_buffering_SSD, 
                           avg_stage_simulation_buffering, 
                           avg_stage_simulation_buffering_SSD,
                           avg_stage_simulation_P_buffering, 
                           avg_stage_simulation_P_buffering_SSD,
                           avg_stage_simulation_F_buffering, 
                           avg_stage_simulation_F_buffering_SSD,
                           seed_bank,
                           iteration_number))
    
  } else {
    
    df <- data.frame(cbind(Genus_species,
                           variance, 
                           autocorrelation, 
                           stochastic_lambda, 
                           simulation_buffering, 
                           simulation_buffering_SSD,
                           simulation_P_buffering, 
                           simulation_P_buffering_SSD,
                           simulation_F_buffering, 
                           simulation_F_buffering_SSD,
                           avg_stage_simulation_buffering, 
                           avg_stage_simulation_buffering_SSD,
                           avg_stage_simulation_P_buffering, 
                           avg_stage_simulation_P_buffering_SSD,
                           avg_stage_simulation_F_buffering, 
                           avg_stage_simulation_F_buffering_SSD,
                           seed_bank,
                           iteration_number))
    
  }
  
  # Create an output object.
  # The output is a list containing the just constructed data frame along with all other outputs.
  
  if(Haridas_method == TRUE){
    
    output <- list("df" = df,
                   "elasticity_values_list" = elasticity_values_list,
                   "simulation_elasticity_values_list" = simulation_elasticity_values_list,
                   "simulation_P_elasticity_values_list" = simulation_P_elasticity_values_list,
                   "simulation_F_elasticity_values_list" = simulation_F_elasticity_values_list,
                   "stage_dist_list" = stage_dist_list,
                   "Genus_species" = Genus_species,
                   "variance" = variance,
                   "autocorrelation" = autocorrelation,
                   "buffering" = buffering, 
                   "buffering_SSD" = buffering_SSD, 
                   "simulation_buffering" = simulation_buffering, 
                   "simulation_buffering_SSD" = simulation_buffering_SSD,
                   "simulation_P_buffering" = simulation_P_buffering, 
                   "simulation_P_buffering_SSD" = simulation_P_buffering_SSD,
                   "simulation_F_buffering" = simulation_F_buffering, 
                   "simulation_F_buffering_SSD" = simulation_F_buffering_SSD, 
                   "avg_stage_buffering" = avg_stage_buffering, 
                   "avg_stage_buffering_SSD" = avg_stage_buffering_SSD, 
                   "avg_stage_simulation_buffering" = avg_stage_simulation_buffering, 
                   "avg_stage_simulation_buffering_SSD" = avg_stage_simulation_buffering_SSD,
                   "avg_stage_simulation_P_buffering" = avg_stage_simulation_P_buffering, 
                   "avg_stage_simulation_P_buffering_SSD" = avg_stage_simulation_P_buffering_SSD,
                   "avg_stage_simulation_F_buffering" = avg_stage_simulation_F_buffering, 
                   "avg_stage_simulation_F_buffering_SSD" = avg_stage_simulation_F_buffering_SSD, 
                   "seed_bank" = seed_bank,
                   "iteration_number" = iteration_number)
    
  } else {
    
    output <- list("df" = df,
                   "simulation_elasticity_values_list" = simulation_elasticity_values_list,
                   "simulation_P_elasticity_values_list" = simulation_P_elasticity_values_list,
                   "simulation_F_elasticity_values_list" = simulation_F_elasticity_values_list,
                   "stage_dist_list" = stage_dist_list,
                   "Genus_species" = Genus_species,
                   "variance" = variance,
                   "autocorrelation" = autocorrelation,
                   "simulation_buffering" = simulation_buffering, 
                   "simulation_buffering_SSD" = simulation_buffering_SSD,
                   "simulation_P_buffering" = simulation_P_buffering, 
                   "simulation_P_buffering_SSD" = simulation_P_buffering_SSD,
                   "simulation_F_buffering" = simulation_F_buffering, 
                   "simulation_F_buffering_SSD" = simulation_F_buffering_SSD, 
                   "avg_stage_simulation_buffering" = avg_stage_simulation_buffering, 
                   "avg_stage_simulation_buffering_SSD" = avg_stage_simulation_buffering_SSD,
                   "avg_stage_simulation_P_buffering" = avg_stage_simulation_P_buffering, 
                   "avg_stage_simulation_P_buffering_SSD" = avg_stage_simulation_P_buffering_SSD,
                   "avg_stage_simulation_F_buffering" = avg_stage_simulation_F_buffering, 
                   "avg_stage_simulation_F_buffering_SSD" = avg_stage_simulation_F_buffering_SSD, 
                   "seed_bank" = seed_bank,
                   "iteration_number" = iteration_number)
    
  }
  
  
  # Save the output to the data folder within the directory and print a message detailing the name of the file.
  
  if(save_after == TRUE){
    
    if(is.character(simulation_file_name) == TRUE){
      
      # If the desired file name is prespecified, use that as the file name when saving the output.
      
      output_file_name <-paste("data/",simulation_file_name, ".RData", sep = "")
      
      save(output, file = output_file_name)
      
      message("\nAll simulation results are saved in the data folder under the name ",
              output_file_name, ".\n")
      
    } else {
      
      # If there is no desired file name, the default name includes the date and time of the start of the simulation.
      
      current_time <- str_replace_all(Sys.time(), ":", ".")
      
      output_file_name <-paste("data/simulation ", current_time, ".RData", sep = "")
      
      save(output, file = output_file_name)
      
      message("\nAll simulation results are saved in the data folder under the name ",
              output_file_name, ".\n")
      
      
    }
    
  }
  
  
  # After saving the output, delete all files that were used as temporary files for this simulation.
  
  if(save_iterations == TRUE && delete_iterations == TRUE){
    
    # Create a vector of all the files in the temporary save folder.
    
    saving_iterations_files <-  dir("data/saving_iterations")
    
    
    # Identify which files contain the simulation start time in their name.
    
    files_to_remove <-  saving_iterations_files[grepl(start_time, saving_iterations_files, fixed=TRUE)]
    
    
    # Add the directory of the files in question.
    
    files_to_remove <- paste0("data/saving_iterations/", files_to_remove)
    
    
    
    # Remove the files with simulation start time in their name.
    
    unlink(files_to_remove)
    
  }
  
  
  # Calculate the length of time the simulation took to complete.
  
  elapsed_time <- proc.time() - ptm
  
  
  
  # Print a message indicating how long the simulation took for how many simulations.
  
  message("\nThis set of ", number_of_simulations, " simulations took ", 
          trunc(elapsed_time["elapsed"]/60), " minutes.\n")
  
  
  
  # Return the output for use in the current R environment.
  
  return(output)
  
}