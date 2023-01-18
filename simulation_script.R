# Testing

source("packages_and_setup.R")
source("analysis_functions.R")
source("individual_population_simulation_functions.R")
source("omnibus_simulation_function.R")


Genus_species <- c("Berberis_thunbergii", 
                   #"Heliconia_tortuosa", 
                   #"Calathea_crotalifera"
                   ) 
 
 
# variance_values <- seq(from = 0.9, to = 1.1, length.out = 15)
#  
# autocorrelation_values <- seq(from = -0.95, to = 0.95, length.out = 15)
# 
# beta_simulation <- omnibus_simulation(Genus_species, variance_values, autocorrelation_values)
# 
# beta_simulation$df

Genus_species <- c("Berberis_thunbergii") 

variance_values <- seq(from = 0.9, to = 1.1, length.out = 35)

autocorrelation_values <- seq(from = -0.95, to = 0.95, length.out = 35)

seed_vec <- c(1, 2, 3, 4, 5)

beta_simulation <- omnibus_simulation(Genus_species, 
                                      variance_values, 
                                      autocorrelation_values,
                                      seed_vector = seed_vec)

beta_simulation$df
