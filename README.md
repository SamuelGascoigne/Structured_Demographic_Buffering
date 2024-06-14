## Structured Demographic Buffering

### Brief overview of the project

This repository stores the code for the manuscript ___Structured demographic buffering: A framework to explore the environment drivers and demographic mechanisms underlying demographic buffering___ - for which there is a preprint accessible [here](https://www.biorxiv.org/content/10.1101/2023.07.20.549848v1.abstract). 

The purpose of this project was to identify the independent roles of environmental variance and autocorrelation on demographic buffering using stochastic-parameter integral projection models (IPMs). In turn, these scripts set-up IPMs and run simulations to explore the relationship(s) between environment, population and vital rate structure on buffering (*i.e.*, the summation of stochastic elasticities of population growth rate with respect to vital rate variance).


### Brief overview of the code structure

The simulation upon which the results are based is quite beefy. In turn, the output of the simulation is stored in the data file. To generate these data, the files:

- *packages_and_setup.R*
- *analysis_functions.R*
- *individual_population_simulation_functions.R*
- *omnibus_simulation_function.R*

are fed into the simulation script aptly named *simulation_script.R*. 

Subsequent analysis and data visualization is performed in *linear_models.R* and *visualization_script.R*. All analysis (*e.g.*, model comparison) can be found in *linear_models.R*. These analyses also include the partitioning of variance across environmental variables (*e.g.*, mean temperature) and the construction of pie charts that show this variance partitioning. The pie charts pertaining to individual figures can be identified either by using the code outline or the figure names (*e.g.*, the pie chart for Figure 3b is named Fig3b_pie). The code for the scatter plots, heat maps and density plots can be found in *visualization_script.R* - which has the same outline and naming scheme as *linear_models.R*.

### Brief over view of the data

The data generated from this simulation, and used in the analysis, can be found in the data folder under the name *simulation 2023-03-01 08.57.26.RData*. The naming convention (*i.e*, *simulation DATE TIME.RData*) is soft coded into the script to allow for the efficient and descriptive saving of simulation outputs. Specifically, see lines 111-123, 361-492 and 619-677 of   *omnibus_simulation_function.R* to see (1) how intermediate realizations of the data file are temporarily stored and (2) the final output file is saved at the end of the simulation.

The data allows for the accessing of simulation outputs in the form of a list or a dataframe. In both cases, the following table comprises the simulation output variable names and their corresponding definitions.

| **Simulation output**  |  **Definition**  |
| ------------- | ------------- |
| *Genus_species*  | The genus and species of the simulated population (*i.e.*, *Berberis thunbergii*, *Calathea crotalifera* or *Heliconia tortuosa*)  |
| *variance*  | The proportional variance used in the simulation  |
| *autocorrelation*  | The autocorrelation value used in the simulation  |
| *stochastic_lambda*  | The stochastic population growth associated with the simulation  |
| *simulation_buffering*  | The numerically quantified summation of stochastic elasticities of variance  |
| *simulation_buffering_SSD*  | The weighted sum of stochastic elasticities of variance, weighted by the stable/average stage distribution   |
| *simulation_P_buffering*  | The numerically quantified summation of stochastic elasticities of variance associated with rates of progression (*i.e.* the **P**-kernel)  |
| *simulation_P_buffering_SSD*  | The weighted sum of stochastic elasticities of variance of the **P**-kernel, weighted by the stable/average stage distribution |
| *simulation_F_buffering*  | The numerically quantified summation of stochastic elasticities of variance associated with rates of fertility (*i.e.* the **F**-kernel)  |
| *simulation_F_buffering_SSD*  | The weighted sum of stochastic elasticities of variance of the **F**-kernel, weighted by the stable/average stage distribution |
| *avg_stage_simulation_buffering*  | The centre of the distribution of stochastic elasticities of  variance across ontogeny  |
| *avg_stage_simulation_buffering_SSD*  | The centre of the distribution of stochastic elasticities of variance, weighted by the stable/average stage distribution, across ontogeny  |
| *avg_stage_simulation_P_buffering*  | The centre of the distribution of stochastic elasticities of variance associated with the **P**-kernel across ontogeny  |
| *avg_stage_simulation_P_buffering_SSD*  | The centre of the distribution of stochastic elasticities of variance associated with the **P**-kernel, weighted by the stable/average stage distribution, across ontogeny  |
| *avg_stage_simulation_F_buffering*  | The centre of the distribution of stochastic elasticities of variance associated with the **F**-kernel across ontogeny  |
| *avg_stage_simulation_F_buffering_SSD*  | The centre of the distribution of stochastic elasticities of variance associated with the **F**-kernel, weighted by the stable/average stage distribution, across ontogeny  |
| *seed_bank*  | The seed used for the simulation |
| *iteration_number*  | The number of timesteps the population was projected through in the simulated stochastic environment  |

