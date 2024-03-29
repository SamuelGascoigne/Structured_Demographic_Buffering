source("packages_and_setup.R")
source("analysis_functions.R")
source("individual_population_simulation_functions.R")
source("omnibus_simulation_function.R")


Genus_species <- c("Berberis_thunbergii", 
                   "Heliconia_tortuosa", 
                   "Calathea_crotalifera") 

 
variance_values <- seq(from = 0.9, to = 1.1, length.out = 15)
 
autocorrelation_values <- seq(from = -0.8, to = 0.8, length.out = 15)

beta_simulation <- omnibus_simulation(Genus_species, variance_values, autocorrelation_values)

beta_simulation$df


