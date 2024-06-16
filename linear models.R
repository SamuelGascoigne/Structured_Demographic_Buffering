# Set-up -----


# Clear the environment

rm(list = ls())


# Import libraries

library(ggplot2)
library(tidyverse)


# Import data

load("data/simulation 2023-03-01 08.57.26.RData")


# Tidy data

output_df <- output$df |> 
  mutate(autocorrelation = as.numeric(autocorrelation)) |> 
  mutate(variance = as.numeric(variance)) |> 
  mutate(simulation_buffering = as.numeric(simulation_buffering)) |> 
  mutate(stochastic_lambda = as.numeric(stochastic_lambda))


# Make species specific datasets for convenience

Bt_df <- output_df %>% 
  filter(Genus_species == "Berberis_thunbergii")

Cc_df <- output_df %>% 
  filter(Genus_species == "Calathea_crotalifera")

Ht_df <- output_df %>% 
  filter(Genus_species == "Heliconia_tortuosa")


# Figure 1 ----

## Figure 1c ----

### Build template ----

# Create Data
template_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(1,
          1,
          1,
          1)
)

### Pie chart ----

Fig1c_pie <- ggplot(template_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels




# Figure 2 ----

## Figure 2a ----


# Berberis thunbergii buffering figure

### Analysis ----

Bt_simulation_buffering_model_1 <- lm(simulation_buffering ~ variance * autocorrelation,
                                   data = Bt_df)

Bt_simulation_buffering_model_2 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2),
                                   data = Bt_df)

Bt_simulation_buffering_model_3 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                   data = Bt_df)

Bt_simulation_buffering_model_4 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2),
                                   data = Bt_df)

Bt_simulation_buffering_model_5 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                   data = Bt_df)

Bt_simulation_buffering_model_6 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                   data = Bt_df)

Bt_simulation_buffering_model_7 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                      data = Bt_df)

Bt_simulation_buffering_model_8 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                      data = Bt_df)

Bt_simulation_buffering_model_9 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Bt_df)

AIC(Bt_simulation_buffering_model_1, 
    Bt_simulation_buffering_model_2, 
    Bt_simulation_buffering_model_3,
    Bt_simulation_buffering_model_4, 
    Bt_simulation_buffering_model_5, 
    Bt_simulation_buffering_model_6,
    Bt_simulation_buffering_model_7, 
    Bt_simulation_buffering_model_8, 
    Bt_simulation_buffering_model_9) # Model 6 has the lowest AIC

summary(Bt_simulation_buffering_model_6)


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Bt_simulation_buffering_anova <- anova(Bt_simulation_buffering_model_6)

Bt_simulation_buffering_total_SS <- sum(Bt_simulation_buffering_anova["Sum Sq"])

Bt_simulation_buffering_autocorrelation_SS <- Bt_simulation_buffering_anova["autocorrelation", "Sum Sq"] + Bt_simulation_buffering_anova["I(autocorrelation^2)", "Sum Sq"] + Bt_simulation_buffering_anova["I(autocorrelation^3)", "Sum Sq"]

Bt_simulation_buffering_variance_SS <- Bt_simulation_buffering_anova["variance", "Sum Sq"] + Bt_simulation_buffering_anova["I(variance^2)", "Sum Sq"]

Bt_simulation_buffering_interaction_SS <- Bt_simulation_buffering_anova["variance:autocorrelation", "Sum Sq"]

Bt_simulation_buffering_autocorrelation_contribution <- Bt_simulation_buffering_autocorrelation_SS / Bt_simulation_buffering_total_SS

Bt_simulation_buffering_variance_contribution <- Bt_simulation_buffering_variance_SS / Bt_simulation_buffering_total_SS

Bt_simulation_buffering_interaction_contribution <- Bt_simulation_buffering_interaction_SS / Bt_simulation_buffering_total_SS

Bt_simulation_buffering_residual_contribution <- 1 - (Bt_simulation_buffering_autocorrelation_contribution + Bt_simulation_buffering_variance_contribution + Bt_simulation_buffering_interaction_contribution)



# Build a pie chart displaying the contributions.

### Pie chart ----

# Create Data
Bt_simulation_buffering_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Bt_simulation_buffering_autocorrelation_contribution,
          Bt_simulation_buffering_variance_contribution,
          Bt_simulation_buffering_interaction_contribution,
          Bt_simulation_buffering_residual_contribution)
)


Fig2a_pie <- ggplot(Bt_simulation_buffering_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5,  alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels



# Calathea crotalifera buffering figure

## Figure 2b ----

### Analysis ----

Cc_simulation_buffering_model_1 <- lm(simulation_buffering ~ variance * autocorrelation,
                                      data = Cc_df)

Cc_simulation_buffering_model_2 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2),
                                      data = Cc_df)

Cc_simulation_buffering_model_3 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Cc_df)

Cc_simulation_buffering_model_4 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2),
                                      data = Cc_df)

Cc_simulation_buffering_model_5 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                      data = Cc_df)

Cc_simulation_buffering_model_6 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Cc_df)

Cc_simulation_buffering_model_7 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                      data = Cc_df)

Cc_simulation_buffering_model_8 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                      data = Cc_df)

Cc_simulation_buffering_model_9 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Cc_df)

AIC(Cc_simulation_buffering_model_1, 
    Cc_simulation_buffering_model_2, 
    Cc_simulation_buffering_model_3,
    Cc_simulation_buffering_model_4, 
    Cc_simulation_buffering_model_5, 
    Cc_simulation_buffering_model_6,
    Cc_simulation_buffering_model_7, 
    Cc_simulation_buffering_model_8, 
    Cc_simulation_buffering_model_9) # Model 6 has the lowest AIC

summary(Cc_simulation_buffering_model_6)


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Cc_simulation_buffering_anova <- anova(Cc_simulation_buffering_model_6)

Cc_simulation_buffering_total_SS <- sum(Cc_simulation_buffering_anova["Sum Sq"])

Cc_simulation_buffering_autocorrelation_SS <- Cc_simulation_buffering_anova["autocorrelation", "Sum Sq"] + Cc_simulation_buffering_anova["I(autocorrelation^2)", "Sum Sq"] + Cc_simulation_buffering_anova["I(autocorrelation^3)", "Sum Sq"]

Cc_simulation_buffering_variance_SS <- Cc_simulation_buffering_anova["variance", "Sum Sq"] + Cc_simulation_buffering_anova["I(variance^2)", "Sum Sq"]

Cc_simulation_buffering_interaction_SS <- Cc_simulation_buffering_anova["variance:autocorrelation", "Sum Sq"]

Cc_simulation_buffering_autocorrelation_contribution <- Cc_simulation_buffering_autocorrelation_SS / Cc_simulation_buffering_total_SS

Cc_simulation_buffering_variance_contribution <- Cc_simulation_buffering_variance_SS / Cc_simulation_buffering_total_SS

Cc_simulation_buffering_interaction_contribution <- Cc_simulation_buffering_interaction_SS / Cc_simulation_buffering_total_SS

Cc_simulation_buffering_residual_contribution <- 1 - (Cc_simulation_buffering_autocorrelation_contribution + Cc_simulation_buffering_variance_contribution + Cc_simulation_buffering_interaction_contribution)


### Pie chart ----

# Create Data
Cc_simulation_buffering_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Cc_simulation_buffering_autocorrelation_contribution,
          Cc_simulation_buffering_variance_contribution,
          Cc_simulation_buffering_interaction_contribution,
          Cc_simulation_buffering_residual_contribution)
)



Fig2b_pie <- ggplot(Cc_simulation_buffering_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels


## Figure 2c ----

### Analysis ----

# Heliconia tortuosa buffering figure

Ht_simulation_buffering_model_1 <- lm(simulation_buffering ~ variance * autocorrelation,
                                      data = Ht_df)

Ht_simulation_buffering_model_2 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2),
                                      data = Ht_df)

Ht_simulation_buffering_model_3 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Ht_df)

Ht_simulation_buffering_model_4 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2),
                                      data = Ht_df)

Ht_simulation_buffering_model_5 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                      data = Ht_df)

Ht_simulation_buffering_model_6 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Ht_df)

Ht_simulation_buffering_model_7 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                      data = Ht_df)

Ht_simulation_buffering_model_8 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                      data = Ht_df)

Ht_simulation_buffering_model_9 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Ht_df)

AIC(Ht_simulation_buffering_model_1, 
    Ht_simulation_buffering_model_2, 
    Ht_simulation_buffering_model_3,
    Ht_simulation_buffering_model_4, 
    Ht_simulation_buffering_model_5, 
    Ht_simulation_buffering_model_6,
    Ht_simulation_buffering_model_7, 
    Ht_simulation_buffering_model_8, 
    Ht_simulation_buffering_model_9) # Model 6 has the lowest AIC

summary(Ht_simulation_buffering_model_6)


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Ht_simulation_buffering_anova <- anova(Ht_simulation_buffering_model_6)

Ht_simulation_buffering_total_SS <- sum(Ht_simulation_buffering_anova["Sum Sq"])

Ht_simulation_buffering_autocorrelation_SS <- Ht_simulation_buffering_anova["autocorrelation", "Sum Sq"] + Ht_simulation_buffering_anova["I(autocorrelation^2)", "Sum Sq"] + Ht_simulation_buffering_anova["I(autocorrelation^3)", "Sum Sq"]

Ht_simulation_buffering_variance_SS <- Ht_simulation_buffering_anova["variance", "Sum Sq"] + Ht_simulation_buffering_anova["I(variance^2)", "Sum Sq"]

Ht_simulation_buffering_interaction_SS <- Ht_simulation_buffering_anova["variance:autocorrelation", "Sum Sq"]

Ht_simulation_buffering_autocorrelation_contribution <- Ht_simulation_buffering_autocorrelation_SS / Ht_simulation_buffering_total_SS

Ht_simulation_buffering_variance_contribution <- Ht_simulation_buffering_variance_SS / Ht_simulation_buffering_total_SS

Ht_simulation_buffering_interaction_contribution <- Ht_simulation_buffering_interaction_SS / Ht_simulation_buffering_total_SS

Ht_simulation_buffering_residual_contribution <- 1 - (Ht_simulation_buffering_autocorrelation_contribution + Ht_simulation_buffering_variance_contribution + Ht_simulation_buffering_interaction_contribution)


### Pie chart ----

# Create Data
Ht_simulation_buffering_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Ht_simulation_buffering_autocorrelation_contribution,
          Ht_simulation_buffering_variance_contribution,
          Ht_simulation_buffering_interaction_contribution,
          Ht_simulation_buffering_residual_contribution)
)

Fig2c_pie <- ggplot(Ht_simulation_buffering_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels




# Figure 3 ----
 

## Deviance-based approach ----


# First for Berberis thunbergii

### Figure 3b ----

#### Analysis ----


Bt_df$scaled_simulation_buffering <- scale(Bt_df$simulation_buffering)

Bt_df$scaled_simulation_buffering_SSD <- scale(as.numeric(Bt_df$simulation_buffering_SSD))

Bt_df$scaled_simulation_buffering_SSD_residuals <- Bt_df$scaled_simulation_buffering_SSD - Bt_df$scaled_simulation_buffering


Bt_scaled_residuals_model_1 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation,
                                      data = Bt_df)

Bt_scaled_residuals_model_2 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(autocorrelation^2),
                                      data = Bt_df)

Bt_scaled_residuals_model_3 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Bt_df)

Bt_scaled_residuals_model_4 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2),
                                      data = Bt_df)

Bt_scaled_residuals_model_5 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                      data = Bt_df)

Bt_scaled_residuals_model_6 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Bt_df)

Bt_scaled_residuals_model_7 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                      data = Bt_df)

Bt_scaled_residuals_model_8 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                      data = Bt_df)

Bt_scaled_residuals_model_9 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Bt_df)

AIC(Bt_scaled_residuals_model_1, 
    Bt_scaled_residuals_model_2, 
    Bt_scaled_residuals_model_3,
    Bt_scaled_residuals_model_4, 
    Bt_scaled_residuals_model_5, 
    Bt_scaled_residuals_model_6,
    Bt_scaled_residuals_model_7, 
    Bt_scaled_residuals_model_8, 
    Bt_scaled_residuals_model_9) # Model 6 has the lowest AIC

summary(Bt_scaled_residuals_model_6) 


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Bt_scaled_residuals_anova <- anova(Bt_scaled_residuals_model_6)

Bt_scaled_residuals_total_SS <- sum(Bt_scaled_residuals_anova["Sum Sq"])

Bt_scaled_residuals_autocorrelation_SS <- Bt_scaled_residuals_anova["autocorrelation", "Sum Sq"] + Bt_scaled_residuals_anova["I(autocorrelation^2)", "Sum Sq"] + Bt_scaled_residuals_anova["I(autocorrelation^3)", "Sum Sq"]

Bt_scaled_residuals_variance_SS <- Bt_scaled_residuals_anova["variance", "Sum Sq"] + Bt_scaled_residuals_anova["I(variance^2)", "Sum Sq"]

Bt_scaled_residuals_interaction_SS <- Bt_scaled_residuals_anova["variance:autocorrelation", "Sum Sq"]

Bt_scaled_residuals_autocorrelation_contribution <- Bt_scaled_residuals_autocorrelation_SS / Bt_scaled_residuals_total_SS

Bt_scaled_residuals_variance_contribution <- Bt_scaled_residuals_variance_SS / Bt_scaled_residuals_total_SS

Bt_scaled_residuals_interaction_contribution <- Bt_scaled_residuals_interaction_SS / Bt_scaled_residuals_total_SS

Bt_scaled_residuals_residual_contribution <- 1 - (Bt_scaled_residuals_autocorrelation_contribution + Bt_scaled_residuals_variance_contribution + Bt_scaled_residuals_interaction_SS)



#### Pie chart ----

# Create Data
Bt_scaled_residuals_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Bt_scaled_residuals_autocorrelation_contribution,
          Bt_scaled_residuals_variance_contribution,
          Bt_scaled_residuals_interaction_contribution,
          Bt_scaled_residuals_residual_contribution)
)



Fig3b_pie <- ggplot(Bt_scaled_residuals_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels




# Second for Calathea crotalifera

### Figure 3e ----

#### Analysis ----


Cc_df$scaled_simulation_buffering <- scale(Cc_df$simulation_buffering)

Cc_df$scaled_simulation_buffering_SSD <- scale(as.numeric(Cc_df$simulation_buffering_SSD))

Cc_df$scaled_simulation_buffering_SSD_residuals <- Cc_df$scaled_simulation_buffering_SSD - Cc_df$scaled_simulation_buffering


Cc_scaled_residuals_model_1 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation,
                                  data = Cc_df)

Cc_scaled_residuals_model_2 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(autocorrelation^2),
                                  data = Cc_df)

Cc_scaled_residuals_model_3 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                  data = Cc_df)

Cc_scaled_residuals_model_4 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2),
                                  data = Cc_df)

Cc_scaled_residuals_model_5 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                  data = Cc_df)

Cc_scaled_residuals_model_6 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                  data = Cc_df)

Cc_scaled_residuals_model_7 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                  data = Cc_df)

Cc_scaled_residuals_model_8 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                  data = Cc_df)

Cc_scaled_residuals_model_9 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                  data = Cc_df)

AIC(Cc_scaled_residuals_model_1, 
    Cc_scaled_residuals_model_2, 
    Cc_scaled_residuals_model_3,
    Cc_scaled_residuals_model_4, 
    Cc_scaled_residuals_model_5, 
    Cc_scaled_residuals_model_6,
    Cc_scaled_residuals_model_7, 
    Cc_scaled_residuals_model_8, 
    Cc_scaled_residuals_model_9) # Model 6 has the lowest AIC

summary(Cc_scaled_residuals_model_6)


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Cc_scaled_residuals_anova <- anova(Cc_scaled_residuals_model_6)

Cc_scaled_residuals_total_SS <- sum(Cc_scaled_residuals_anova["Sum Sq"])

Cc_scaled_residuals_autocorrelation_SS <- Cc_scaled_residuals_anova["autocorrelation", "Sum Sq"] + Cc_scaled_residuals_anova["I(autocorrelation^2)", "Sum Sq"] + Cc_scaled_residuals_anova["I(autocorrelation^3)", "Sum Sq"]

Cc_scaled_residuals_variance_SS <- Cc_scaled_residuals_anova["variance", "Sum Sq"] + Cc_scaled_residuals_anova["I(variance^2)", "Sum Sq"]

Cc_scaled_residuals_interaction_SS <- Cc_scaled_residuals_anova["variance:autocorrelation", "Sum Sq"]

Cc_scaled_residuals_autocorrelation_contribution <- Cc_scaled_residuals_autocorrelation_SS / Cc_scaled_residuals_total_SS

Cc_scaled_residuals_variance_contribution <- Cc_scaled_residuals_variance_SS / Cc_scaled_residuals_total_SS

Cc_scaled_residuals_interaction_contribution <- Cc_scaled_residuals_interaction_SS / Cc_scaled_residuals_total_SS

Cc_scaled_residuals_residual_contribution <- 1 - (Cc_scaled_residuals_autocorrelation_contribution + Cc_scaled_residuals_variance_contribution + Cc_scaled_residuals_interaction_SS)


#### Pie chart ----

# Create Data
Cc_scaled_residuals_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Cc_scaled_residuals_autocorrelation_contribution,
          Cc_scaled_residuals_variance_contribution,
          Cc_scaled_residuals_interaction_contribution,
          Cc_scaled_residuals_residual_contribution)
)



Fig3e_pie <- ggplot(Cc_scaled_residuals_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels





# Third for Heliconia tortuosa

### Figure 3h ----

#### Analysis ----


Ht_df$scaled_simulation_buffering <- scale(Ht_df$simulation_buffering)

Ht_df$scaled_simulation_buffering_SSD <- scale(as.numeric(Ht_df$simulation_buffering_SSD))

Ht_df$scaled_simulation_buffering_SSD_residuals <- Ht_df$scaled_simulation_buffering_SSD - Ht_df$scaled_simulation_buffering


Ht_scaled_residuals_model_1 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation,
                                  data = Ht_df)

Ht_scaled_residuals_model_2 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(autocorrelation^2),
                                  data = Ht_df)

Ht_scaled_residuals_model_3 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                  data = Ht_df)

Ht_scaled_residuals_model_4 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2),
                                  data = Ht_df)

Ht_scaled_residuals_model_5 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                  data = Ht_df)

Ht_scaled_residuals_model_6 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                  data = Ht_df)

Ht_scaled_residuals_model_7 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                  data = Ht_df)

Ht_scaled_residuals_model_8 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                  data = Ht_df)

Ht_scaled_residuals_model_9 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                  data = Ht_df)

AIC(Ht_scaled_residuals_model_1, 
    Ht_scaled_residuals_model_2, 
    Ht_scaled_residuals_model_3,
    Ht_scaled_residuals_model_4, 
    Ht_scaled_residuals_model_5, 
    Ht_scaled_residuals_model_6,
    Ht_scaled_residuals_model_7, 
    Ht_scaled_residuals_model_8, 
    Ht_scaled_residuals_model_9) # Model 6 has the lowest AIC

summary(Ht_scaled_residuals_model_6) 


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Ht_scaled_residuals_anova <- anova(Ht_scaled_residuals_model_6)

Ht_scaled_residuals_total_SS <- sum(Ht_scaled_residuals_anova["Sum Sq"])

Ht_scaled_residuals_autocorrelation_SS <- Ht_scaled_residuals_anova["autocorrelation", "Sum Sq"] + Ht_scaled_residuals_anova["I(autocorrelation^2)", "Sum Sq"] + Ht_scaled_residuals_anova["I(autocorrelation^3)", "Sum Sq"]

Ht_scaled_residuals_variance_SS <- Ht_scaled_residuals_anova["variance", "Sum Sq"] + Ht_scaled_residuals_anova["I(variance^2)", "Sum Sq"]

Ht_scaled_residuals_interaction_SS <- Ht_scaled_residuals_anova["variance:autocorrelation", "Sum Sq"]

Ht_scaled_residuals_autocorrelation_contribution <- Ht_scaled_residuals_autocorrelation_SS / Ht_scaled_residuals_total_SS

Ht_scaled_residuals_variance_contribution <- Ht_scaled_residuals_variance_SS / Ht_scaled_residuals_total_SS

Ht_scaled_residuals_interaction_contribution <- Ht_scaled_residuals_interaction_SS / Ht_scaled_residuals_total_SS

Ht_scaled_residuals_residual_contribution <- 1 - (Ht_scaled_residuals_autocorrelation_contribution + Ht_scaled_residuals_variance_contribution + Ht_scaled_residuals_interaction_SS)


#### Pie chart ----

# Create Data
Ht_scaled_residuals_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Ht_scaled_residuals_autocorrelation_contribution,
          Ht_scaled_residuals_variance_contribution,
          Ht_scaled_residuals_interaction_contribution,
          Ht_scaled_residuals_residual_contribution)
)


Fig3h_pie <- ggplot(Ht_scaled_residuals_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels



## Estimate-based approach ----

# First Berberis thunbergii

### Figure 3c ----

#### Analysis ----

Bt_mean_buffered_stage_model_1 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation,
                                  data = Bt_df)

Bt_mean_buffered_stage_model_2 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(autocorrelation^2),
                                  data = Bt_df)

Bt_mean_buffered_stage_model_3 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                  data = Bt_df)

Bt_mean_buffered_stage_model_4 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2),
                                  data = Bt_df)

Bt_mean_buffered_stage_model_5 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                  data = Bt_df)

Bt_mean_buffered_stage_model_6 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                  data = Bt_df)

Bt_mean_buffered_stage_model_7 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                  data = Bt_df)

Bt_mean_buffered_stage_model_8 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                  data = Bt_df)

Bt_mean_buffered_stage_model_9 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                  data = Bt_df)

AIC(Bt_mean_buffered_stage_model_1, 
    Bt_mean_buffered_stage_model_2, 
    Bt_mean_buffered_stage_model_3,
    Bt_mean_buffered_stage_model_4, 
    Bt_mean_buffered_stage_model_5, 
    Bt_mean_buffered_stage_model_6,
    Bt_mean_buffered_stage_model_7, 
    Bt_mean_buffered_stage_model_8, 
    Bt_mean_buffered_stage_model_9) # Model 3 has the lowest AIC

summary(Bt_mean_buffered_stage_model_3) 


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Bt_mean_buffered_stage_anova <- anova(Bt_mean_buffered_stage_model_3)

Bt_mean_buffered_stage_total_SS <- sum(Bt_mean_buffered_stage_anova["Sum Sq"])

Bt_mean_buffered_stage_autocorrelation_SS <- Bt_mean_buffered_stage_anova["autocorrelation", "Sum Sq"] + Bt_mean_buffered_stage_anova["I(autocorrelation^2)", "Sum Sq"] + Bt_mean_buffered_stage_anova["I(autocorrelation^3)", "Sum Sq"]

Bt_mean_buffered_stage_variance_SS <- Bt_mean_buffered_stage_anova["variance", "Sum Sq"] 

Bt_mean_buffered_stage_interaction_SS <- Bt_mean_buffered_stage_anova["variance:autocorrelation", "Sum Sq"]

Bt_mean_buffered_stage_autocorrelation_contribution <- Bt_mean_buffered_stage_autocorrelation_SS / Bt_mean_buffered_stage_total_SS

Bt_mean_buffered_stage_variance_contribution <- Bt_mean_buffered_stage_variance_SS / Bt_mean_buffered_stage_total_SS

Bt_mean_buffered_stage_interaction_contribution <- Bt_mean_buffered_stage_interaction_SS / Bt_mean_buffered_stage_total_SS

Bt_mean_buffered_stage_residual_contribution <- 1 - (Bt_mean_buffered_stage_autocorrelation_contribution + Bt_mean_buffered_stage_variance_contribution + Bt_mean_buffered_stage_interaction_SS)


#### Pie chart ----

# Create Data
Bt_mean_buffered_stage_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Bt_mean_buffered_stage_autocorrelation_contribution,
          Bt_mean_buffered_stage_variance_contribution,
          Bt_mean_buffered_stage_interaction_contribution,
          Bt_mean_buffered_stage_residual_contribution)
)

Fig3c_pie <- ggplot(Bt_mean_buffered_stage_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels



# Second Calathea crotalifera

### Figure 3f ----

#### Analysis ----

Cc_mean_buffered_stage_model_1 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation,
                                     data = Cc_df)

Cc_mean_buffered_stage_model_2 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(autocorrelation^2),
                                     data = Cc_df)

Cc_mean_buffered_stage_model_3 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                     data = Cc_df)

Cc_mean_buffered_stage_model_4 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2),
                                     data = Cc_df)

Cc_mean_buffered_stage_model_5 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                     data = Cc_df)

Cc_mean_buffered_stage_model_6 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                     data = Cc_df)

Cc_mean_buffered_stage_model_7 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                     data = Cc_df)

Cc_mean_buffered_stage_model_8 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                     data = Cc_df)

Cc_mean_buffered_stage_model_9 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                     data = Cc_df)

AIC(Cc_mean_buffered_stage_model_1, 
    Cc_mean_buffered_stage_model_2, 
    Cc_mean_buffered_stage_model_3,
    Cc_mean_buffered_stage_model_4, 
    Cc_mean_buffered_stage_model_5, 
    Cc_mean_buffered_stage_model_6,
    Cc_mean_buffered_stage_model_7, 
    Cc_mean_buffered_stage_model_8, 
    Cc_mean_buffered_stage_model_9) # Model 3 has the lowest AIC

summary(Cc_mean_buffered_stage_model_3)


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Cc_mean_buffered_stage_anova <- anova(Cc_mean_buffered_stage_model_3)

Cc_mean_buffered_stage_total_SS <- sum(Cc_mean_buffered_stage_anova["Sum Sq"])

Cc_mean_buffered_stage_autocorrelation_SS <- Cc_mean_buffered_stage_anova["autocorrelation", "Sum Sq"] + Cc_mean_buffered_stage_anova["I(autocorrelation^2)", "Sum Sq"] + Cc_mean_buffered_stage_anova["I(autocorrelation^3)", "Sum Sq"]

Cc_mean_buffered_stage_variance_SS <- Cc_mean_buffered_stage_anova["variance", "Sum Sq"] 

Cc_mean_buffered_stage_interaction_SS <- Cc_mean_buffered_stage_anova["variance:autocorrelation", "Sum Sq"]

Cc_mean_buffered_stage_autocorrelation_contribution <- Cc_mean_buffered_stage_autocorrelation_SS / Cc_mean_buffered_stage_total_SS

Cc_mean_buffered_stage_variance_contribution <- Cc_mean_buffered_stage_variance_SS / Cc_mean_buffered_stage_total_SS

Cc_mean_buffered_stage_interaction_contribution <- Cc_mean_buffered_stage_interaction_SS / Cc_mean_buffered_stage_total_SS

Cc_mean_buffered_stage_residual_contribution <- 1 - (Cc_mean_buffered_stage_autocorrelation_contribution + Cc_mean_buffered_stage_variance_contribution + Cc_mean_buffered_stage_interaction_SS)


#### Pie chart ----

# Create Data
Cc_mean_buffered_stage_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Cc_mean_buffered_stage_autocorrelation_contribution,
          Cc_mean_buffered_stage_variance_contribution,
          Cc_mean_buffered_stage_interaction_contribution,
          Cc_mean_buffered_stage_residual_contribution)
)


Fig3f_pie <- ggplot(Cc_mean_buffered_stage_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels



# Third Heliconia tortuosa

### Figure 3i ----

#### Analysis ----

Ht_mean_buffered_stage_model_1 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation,
                                     data = Ht_df)

Ht_mean_buffered_stage_model_2 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(autocorrelation^2),
                                     data = Ht_df)

Ht_mean_buffered_stage_model_3 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                     data = Ht_df)

Ht_mean_buffered_stage_model_4 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2),
                                     data = Ht_df)

Ht_mean_buffered_stage_model_5 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                     data = Ht_df)

Ht_mean_buffered_stage_model_6 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                     data = Ht_df)

Ht_mean_buffered_stage_model_7 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                     data = Ht_df)

Ht_mean_buffered_stage_model_8 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                     data = Ht_df)

Ht_mean_buffered_stage_model_9 <- lm(avg_stage_simulation_buffering  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                     data = Ht_df)

AIC(Ht_mean_buffered_stage_model_1, 
    Ht_mean_buffered_stage_model_2, 
    Ht_mean_buffered_stage_model_3,
    Ht_mean_buffered_stage_model_4, 
    Ht_mean_buffered_stage_model_5, 
    Ht_mean_buffered_stage_model_6,
    Ht_mean_buffered_stage_model_7, 
    Ht_mean_buffered_stage_model_8, 
    Ht_mean_buffered_stage_model_9) # Model 3 has the lowest AIC

summary(Ht_mean_buffered_stage_model_3)


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Ht_mean_buffered_stage_anova <- anova(Ht_mean_buffered_stage_model_3)

Ht_mean_buffered_stage_total_SS <- sum(Ht_mean_buffered_stage_anova["Sum Sq"])

Ht_mean_buffered_stage_autocorrelation_SS <- Ht_mean_buffered_stage_anova["autocorrelation", "Sum Sq"] + Ht_mean_buffered_stage_anova["I(autocorrelation^2)", "Sum Sq"] + Ht_mean_buffered_stage_anova["I(autocorrelation^3)", "Sum Sq"]

Ht_mean_buffered_stage_variance_SS <- Ht_mean_buffered_stage_anova["variance", "Sum Sq"] 

Ht_mean_buffered_stage_interaction_SS <- Ht_mean_buffered_stage_anova["variance:autocorrelation", "Sum Sq"]

Ht_mean_buffered_stage_autocorrelation_contribution <- Ht_mean_buffered_stage_autocorrelation_SS / Ht_mean_buffered_stage_total_SS

Ht_mean_buffered_stage_variance_contribution <- Ht_mean_buffered_stage_variance_SS / Ht_mean_buffered_stage_total_SS

Ht_mean_buffered_stage_interaction_contribution <- Ht_mean_buffered_stage_interaction_SS / Ht_mean_buffered_stage_total_SS

Ht_mean_buffered_stage_residual_contribution <- 1 - (Ht_mean_buffered_stage_autocorrelation_contribution + Ht_mean_buffered_stage_variance_contribution + Ht_mean_buffered_stage_interaction_SS)


#### Pie chart ----

# Create Data
Ht_mean_buffered_stage_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Ht_mean_buffered_stage_autocorrelation_contribution,
          Ht_mean_buffered_stage_variance_contribution,
          Ht_mean_buffered_stage_interaction_contribution,
          Ht_mean_buffered_stage_residual_contribution)
)


Fig3i_pie <- ggplot(Ht_mean_buffered_stage_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels




# Figure 4 ----


## Figure 4a analysis ----

Bt_df$P_F_difference <- as.numeric(Bt_df$simulation_P_buffering) - as.numeric(Bt_df$simulation_F_buffering)

cor.test(x = Bt_df$P_F_difference,
         y = Bt_df$simulation_buffering, 
         method=c("pearson"))

Cc_df$P_F_difference <- as.numeric(Cc_df$simulation_P_buffering) - as.numeric(Cc_df$simulation_F_buffering)

cor.test(x = Cc_df$P_F_difference,
         y = Cc_df$simulation_buffering, 
         method=c("pearson"))


Ht_df$P_F_difference <- as.numeric(Ht_df$simulation_P_buffering) - as.numeric(Ht_df$simulation_F_buffering)

cor.test(x = Ht_df$P_F_difference,
         y = Ht_df$simulation_buffering, 
         method=c("pearson"))



# Now let's model the P/F sub-kernel contributions

# First Berberis thunbergii

## Figure 4b ----

### Analysis ----

Bt_df$P_F_difference <- as.numeric(Bt_df$simulation_P_buffering) - as.numeric(Bt_df$simulation_F_buffering)

Bt_P_F_difference_model_1 <- lm(P_F_difference  ~ variance * autocorrelation,
                                     data = Bt_df)

Bt_P_F_difference_model_2 <- lm(P_F_difference  ~ variance * autocorrelation + I(autocorrelation^2),
                                     data = Bt_df)

Bt_P_F_difference_model_3 <- lm(P_F_difference  ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                     data = Bt_df)

Bt_P_F_difference_model_4 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2),
                                     data = Bt_df)

Bt_P_F_difference_model_5 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                     data = Bt_df)

Bt_P_F_difference_model_6 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                     data = Bt_df)

Bt_P_F_difference_model_7 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                     data = Bt_df)

Bt_P_F_difference_model_8 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                     data = Bt_df)

Bt_P_F_difference_model_9 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                     data = Bt_df)

AIC(Bt_P_F_difference_model_1, 
    Bt_P_F_difference_model_2, 
    Bt_P_F_difference_model_3,
    Bt_P_F_difference_model_4, 
    Bt_P_F_difference_model_5, 
    Bt_P_F_difference_model_6,
    Bt_P_F_difference_model_7, 
    Bt_P_F_difference_model_8, 
    Bt_P_F_difference_model_9) # Model 6 has the lowest AIC

summary(Bt_P_F_difference_model_6) # Let's try without autocorrelation 


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Bt_P_F_difference_anova <- anova(Bt_P_F_difference_model_6)

Bt_P_F_difference_total_SS <- sum(Bt_P_F_difference_anova["Sum Sq"])

Bt_P_F_difference_autocorrelation_SS <- Bt_P_F_difference_anova["autocorrelation", "Sum Sq"] + Bt_P_F_difference_anova["I(autocorrelation^2)", "Sum Sq"] + Bt_P_F_difference_anova["I(autocorrelation^3)", "Sum Sq"]

Bt_P_F_difference_variance_SS <- Bt_P_F_difference_anova["variance", "Sum Sq"] + Bt_P_F_difference_anova["I(variance^2)", "Sum Sq"] 

Bt_P_F_difference_interaction_SS <- Bt_P_F_difference_anova["variance:autocorrelation", "Sum Sq"]

Bt_P_F_difference_autocorrelation_contribution <- Bt_P_F_difference_autocorrelation_SS / Bt_P_F_difference_total_SS

Bt_P_F_difference_variance_contribution <- Bt_P_F_difference_variance_SS / Bt_P_F_difference_total_SS

Bt_P_F_difference_interaction_contribution <- Bt_P_F_difference_interaction_SS / Bt_P_F_difference_total_SS

Bt_P_F_difference_residual_contribution <- 1 - (Bt_P_F_difference_autocorrelation_contribution + Bt_P_F_difference_variance_contribution + Bt_P_F_difference_interaction_SS)


### Pie chart ----

# Create Data
Bt_P_F_difference_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Bt_P_F_difference_autocorrelation_contribution,
          Bt_P_F_difference_variance_contribution,
          Bt_P_F_difference_interaction_contribution,
          Bt_P_F_difference_residual_contribution)
)


Fig4b_pie <- ggplot(Bt_P_F_difference_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels



# Second Calathea crotalifera

## Figure 4c ----

### Analysis ----

Cc_df$P_F_difference <- as.numeric(Cc_df$simulation_P_buffering) - as.numeric(Cc_df$simulation_F_buffering)

Cc_P_F_difference_model_1 <- lm(P_F_difference  ~ variance * autocorrelation,
                                data = Cc_df)

Cc_P_F_difference_model_2 <- lm(P_F_difference  ~ variance * autocorrelation + I(autocorrelation^2),
                                data = Cc_df)

Cc_P_F_difference_model_3 <- lm(P_F_difference  ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                data = Cc_df)

Cc_P_F_difference_model_4 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2),
                                data = Cc_df)

Cc_P_F_difference_model_5 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                data = Cc_df)

Cc_P_F_difference_model_6 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                data = Cc_df)

Cc_P_F_difference_model_7 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                data = Cc_df)

Cc_P_F_difference_model_8 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                data = Cc_df)

Cc_P_F_difference_model_9 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                data = Cc_df)

AIC(Cc_P_F_difference_model_1, 
    Cc_P_F_difference_model_2, 
    Cc_P_F_difference_model_3,
    Cc_P_F_difference_model_4, 
    Cc_P_F_difference_model_5, 
    Cc_P_F_difference_model_6,
    Cc_P_F_difference_model_7, 
    Cc_P_F_difference_model_8, 
    Cc_P_F_difference_model_9) # Model 6 has the lowest AIC

summary(Cc_P_F_difference_model_6) 


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Cc_P_F_difference_anova <- anova(Cc_P_F_difference_model_6)

Cc_P_F_difference_total_SS <- sum(Cc_P_F_difference_anova["Sum Sq"])

Cc_P_F_difference_autocorrelation_SS <- Cc_P_F_difference_anova["autocorrelation", "Sum Sq"] + Cc_P_F_difference_anova["I(autocorrelation^2)", "Sum Sq"] + Cc_P_F_difference_anova["I(autocorrelation^3)", "Sum Sq"]

Cc_P_F_difference_variance_SS <- Cc_P_F_difference_anova["variance", "Sum Sq"] + Cc_P_F_difference_anova["I(variance^2)", "Sum Sq"] 

Cc_P_F_difference_interaction_SS <- Cc_P_F_difference_anova["variance:autocorrelation", "Sum Sq"]

Cc_P_F_difference_autocorrelation_contribution <- Cc_P_F_difference_autocorrelation_SS / Cc_P_F_difference_total_SS

Cc_P_F_difference_variance_contribution <- Cc_P_F_difference_variance_SS / Cc_P_F_difference_total_SS

Cc_P_F_difference_interaction_contribution <- Cc_P_F_difference_interaction_SS / Cc_P_F_difference_total_SS

Cc_P_F_difference_residual_contribution <- 1 - (Cc_P_F_difference_autocorrelation_contribution + Cc_P_F_difference_variance_contribution + Cc_P_F_difference_interaction_SS)


### Pie chart ----

# Create Data
Cc_P_F_difference_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Cc_P_F_difference_autocorrelation_contribution,
          Cc_P_F_difference_variance_contribution,
          Cc_P_F_difference_interaction_contribution,
          Cc_P_F_difference_residual_contribution)
)


Fig4c_pie <- ggplot(Cc_P_F_difference_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels



# Third Heliconia tortuosa

## Figure 4d ----

### Analysis ----

Ht_df$P_F_difference <- as.numeric(Ht_df$simulation_P_buffering) - as.numeric(Ht_df$simulation_F_buffering)

Ht_P_F_difference_model_1 <- lm(P_F_difference  ~ variance * autocorrelation,
                                data = Ht_df)

Ht_P_F_difference_model_2 <- lm(P_F_difference  ~ variance * autocorrelation + I(autocorrelation^2),
                                data = Ht_df)

Ht_P_F_difference_model_3 <- lm(P_F_difference  ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                data = Ht_df)

Ht_P_F_difference_model_4 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2),
                                data = Ht_df)

Ht_P_F_difference_model_5 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                data = Ht_df)

Ht_P_F_difference_model_6 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                data = Ht_df)

Ht_P_F_difference_model_7 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                data = Ht_df)

Ht_P_F_difference_model_8 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                data = Ht_df)

Ht_P_F_difference_model_9 <- lm(P_F_difference  ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                data = Ht_df)

AIC(Ht_P_F_difference_model_1, 
    Ht_P_F_difference_model_2, 
    Ht_P_F_difference_model_3,
    Ht_P_F_difference_model_4, 
    Ht_P_F_difference_model_5, 
    Ht_P_F_difference_model_6,
    Ht_P_F_difference_model_7, 
    Ht_P_F_difference_model_8, 
    Ht_P_F_difference_model_9) # Model 6 has the lowest AIC

summary(Ht_P_F_difference_model_6) # Let's try without autocorrelation and variance and autocorrelation:variance


# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Ht_P_F_difference_anova <- anova(Ht_P_F_difference_model_6)

Ht_P_F_difference_total_SS <- sum(Ht_P_F_difference_anova["Sum Sq"])

Ht_P_F_difference_autocorrelation_SS <- Ht_P_F_difference_anova["autocorrelation", "Sum Sq"] + Ht_P_F_difference_anova["I(autocorrelation^2)", "Sum Sq"] + Ht_P_F_difference_anova["I(autocorrelation^3)", "Sum Sq"]

Ht_P_F_difference_variance_SS <- Ht_P_F_difference_anova["variance", "Sum Sq"] + Ht_P_F_difference_anova["I(variance^2)", "Sum Sq"] 

Ht_P_F_difference_interaction_SS <- Ht_P_F_difference_anova["variance:autocorrelation", "Sum Sq"]

Ht_P_F_difference_autocorrelation_contribution <- Ht_P_F_difference_autocorrelation_SS / Ht_P_F_difference_total_SS

Ht_P_F_difference_variance_contribution <- Ht_P_F_difference_variance_SS / Ht_P_F_difference_total_SS

Ht_P_F_difference_interaction_contribution <- Ht_P_F_difference_interaction_SS / Ht_P_F_difference_total_SS

Ht_P_F_difference_residual_contribution <- 1 - (Ht_P_F_difference_autocorrelation_contribution + Ht_P_F_difference_variance_contribution + Ht_P_F_difference_interaction_SS)


### Pie chart ----

# Create Data
Ht_P_F_difference_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "interaction",
          "residuals"),
  value=c(Ht_P_F_difference_autocorrelation_contribution,
          Ht_P_F_difference_variance_contribution,
          Ht_P_F_difference_interaction_contribution,
          Ht_P_F_difference_residual_contribution)
)

Fig4d_pie <- ggplot(Ht_P_F_difference_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels





