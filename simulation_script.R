# Testing

source("packages_and_setup.R")
source("analysis_functions.R")
source("individual_population_simulation_functions.R")
source("omnibus_simulation_function.R")


Genus_species <- c("Berberis_thunbergii", 
                   "Heliconia_tortuosa", 
                   "Calathea_crotalifera") 
 
 
variance_values <- seq(from = 0.9, to = 1.1, length.out = 1)
 
autocorrelation_values <- seq(from = -0.95, to = 0.95, length.out = 1)

beta_simulation <- omnibus_simulation(Genus_species, variance_values, autocorrelation_values)

beta_simulation$df


