## Structured Demographic Buffering

---

### Brief overview of the project

This repository stores the code for the manuscript ___Structured demographic buffering: A framework to explore the environment drivers and demographic mechanisms underlying demographic buffering___ - for which there is a preprint accessible [here](https://www.biorxiv.org/content/10.1101/2023.07.20.549848v1.abstract). 

The purpose of this project is to identify the independent roles of enviornmental variance and autocorrelation on demographic buffering using stochastic-parameter integral projection models (IPMs). In turn, these scripts set-up IPMs and run simulations to explore the relationship(s) between environment, population and vital rate structure on buffering (i.e., the summation of stochastic elasticities of lambda with respect to vital rate variance).


---


### Brief overview of the code structure

Th simulation upon which the results are based is quite beefy. In turn, the output of the simulation is stored in the data file. To generate these data, the files:

- *packages_and_setup.R*
- *analysis_functions.R*
- *individual_population_simulation_functions.R*
- *omnibus_simulation_function.R*

are fed into the simulation script aptly named *simulation_script.R*. 

Subsequent analysis and data visualization is performed in the *visualization_script.R* and *linear_models.R*.
