library(Rpadrino) # for padrino
library(tidyverse) # for object manipulation and graphing
library(PKNCA) # for geometric mean and variance calculations
library(reshape2) # for coercing matrices into dataframes
library(showtext) # for change font in ggplot
library(viridis) # for viridis colours in ggplot
library(lubridate) # for time stamping functions
library(popbio) # for some demographic rate calculations
library(ggpubr) # for ggarrange()
library(plyr) # for converting a list object into a dataframe via (ldply())
library(scales) # for muted colours in graphs (muted())
library(Rage) # for the stochastic elasticities of variance calculation

# font_add_google("Montserrat", "Montserrat") # import the Montserrat font that is used for ggplot graphics

showtext_auto() # default text attributes to use showtext and thus allow Montserrat to be called in a ggplot function

# download the Padrino database

pdb <- pdb_download(save = FALSE) 