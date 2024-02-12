library(ggplot2)
library(tidyverse)

load("data/simulation 2023-03-01 08.57.26.RData")

output_df <- output$df |> 
  mutate(autocorrelation = as.numeric(autocorrelation)) |> 
  mutate(variance = as.numeric(variance)) |> 
  mutate(simulation_buffering = as.numeric(simulation_buffering)) |> 
  mutate(stochastic_lambda = as.numeric(stochastic_lambda))

# Build template pie chart

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

# Basic piechart

ggplot(template_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels


# Berberis thunbergii buffering figure

Bt_simulation_buffering_model_1 <- lm(simulation_buffering ~ variance * autocorrelation,
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_simulation_buffering_model_2 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_simulation_buffering_model_3 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_simulation_buffering_model_4 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_simulation_buffering_model_5 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_simulation_buffering_model_6 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_simulation_buffering_model_7 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                      data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_simulation_buffering_model_8 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                      data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_simulation_buffering_model_9 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

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

# Knock out autocorrelation to see if it improves the model

Bt_simulation_buffering_model_10 <- lm(simulation_buffering ~ variance + autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                                                          data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

AIC(Bt_simulation_buffering_model_6, 
    Bt_simulation_buffering_model_10) # Knocking out autocorrelation improved the model

summary(Bt_simulation_buffering_model_10)

# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Bt_simulation_buffering_anova <- anova(Bt_simulation_buffering_model_10)

Bt_simulation_buffering_total_SS <- sum(Bt_simulation_buffering_anova["Sum Sq"])

Bt_simulation_buffering_autocorrelation_SS <- Bt_simulation_buffering_anova["I(autocorrelation^2)", "Sum Sq"] + Bt_simulation_buffering_anova["I(autocorrelation^3)", "Sum Sq"]

Bt_simulation_buffering_variance_SS <- Bt_simulation_buffering_anova["variance", "Sum Sq"] + Bt_simulation_buffering_anova["I(variance^2)", "Sum Sq"]

Bt_simulation_buffering_interaction_SS <- Bt_simulation_buffering_anova["variance:autocorrelation", "Sum Sq"]

Bt_simulation_buffering_autocorrelation_contribution <- Bt_simulation_buffering_autocorrelation_SS / Bt_simulation_buffering_total_SS

Bt_simulation_buffering_variance_contribution <- Bt_simulation_buffering_variance_SS / Bt_simulation_buffering_total_SS

Bt_simulation_buffering_interaction_contribution <- Bt_simulation_buffering_interaction_SS / Bt_simulation_buffering_total_SS

Bt_simulation_buffering_residual_contribution <- 1 - (Bt_simulation_buffering_autocorrelation_contribution + Bt_simulation_buffering_variance_contribution + Bt_simulation_buffering_interaction_contribution)

# Build a pie chart displaying the contributions.

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

# Basic piechart

Bt_simulation_buffering_pie <- ggplot(Bt_simulation_buffering_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5,  alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels



# Calathea crotalifera buffering figure

Cc_simulation_buffering_model_1 <- lm(simulation_buffering ~ variance * autocorrelation,
                                      data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_2 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2),
                                      data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_3 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_4 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2),
                                      data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_5 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                      data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_6 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_7 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                      data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_8 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                      data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_9 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

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

# Knock out autocorrelation, variance, and variance:autocorrelation to see if it improves the model

Cc_simulation_buffering_model_10 <- lm(simulation_buffering ~ variance + autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_11 <- lm(simulation_buffering ~ autocorrelation + autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_12 <- lm(simulation_buffering ~ variance + autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_13 <- lm(simulation_buffering ~ autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_14 <- lm(simulation_buffering ~ variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_15 <- lm(simulation_buffering ~ autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_simulation_buffering_model_16 <- lm(simulation_buffering ~ I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

AIC(Cc_simulation_buffering_model_6, 
    Cc_simulation_buffering_model_10, 
    Cc_simulation_buffering_model_11,
    Cc_simulation_buffering_model_12, 
    Cc_simulation_buffering_model_13, 
    Cc_simulation_buffering_model_14,
    Cc_simulation_buffering_model_15, 
    Cc_simulation_buffering_model_16) # Model 13 has the lowest AIC

summary(Cc_simulation_buffering_model_13)

# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Cc_simulation_buffering_anova <- anova(Cc_simulation_buffering_model_13)

Cc_simulation_buffering_total_SS <- sum(Cc_simulation_buffering_anova["Sum Sq"])

Cc_simulation_buffering_autocorrelation_SS <- Cc_simulation_buffering_anova["I(autocorrelation^2)", "Sum Sq"] + Cc_simulation_buffering_anova["I(autocorrelation^3)", "Sum Sq"]

Cc_simulation_buffering_variance_SS <- Cc_simulation_buffering_anova["I(variance^2)", "Sum Sq"]

Cc_simulation_buffering_interaction_SS <- Cc_simulation_buffering_anova["autocorrelation:variance", "Sum Sq"]

Cc_simulation_buffering_autocorrelation_contribution <- Cc_simulation_buffering_autocorrelation_SS / Cc_simulation_buffering_total_SS

Cc_simulation_buffering_variance_contribution <- Cc_simulation_buffering_variance_SS / Cc_simulation_buffering_total_SS

Cc_simulation_buffering_interaction_contribution <- Cc_simulation_buffering_interaction_SS / Cc_simulation_buffering_total_SS

Cc_simulation_buffering_residual_contribution <- 1 - (Cc_simulation_buffering_autocorrelation_contribution + Cc_simulation_buffering_variance_contribution + Cc_simulation_buffering_interaction_contribution)

# Build a pie chart displaying the contributions.

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

# Basic piechart

ggplot(Cc_simulation_buffering_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels

# Heliconia tortuosa buffering figure

Ht_simulation_buffering_model_1 <- lm(simulation_buffering ~ variance * autocorrelation,
                                      data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_2 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2),
                                      data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_3 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_4 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2),
                                      data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_5 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2),
                                      data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_6 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_7 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3),
                                      data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_8 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2),
                                      data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_9 <- lm(simulation_buffering ~ variance * autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

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

# Knock out autocorrelation, variance, and variance:autocorrelation to see if it improves the model

Ht_simulation_buffering_model_10 <- lm(simulation_buffering ~ variance + autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_11 <- lm(simulation_buffering ~ autocorrelation + autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_12 <- lm(simulation_buffering ~ variance + autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_13 <- lm(simulation_buffering ~ autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_14 <- lm(simulation_buffering ~ variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_15 <- lm(simulation_buffering ~ autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_simulation_buffering_model_16 <- lm(simulation_buffering ~ I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                       data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

AIC(Ht_simulation_buffering_model_6, 
    Ht_simulation_buffering_model_10, 
    Ht_simulation_buffering_model_11,
    Ht_simulation_buffering_model_12, 
    Ht_simulation_buffering_model_13, 
    Ht_simulation_buffering_model_14,
    Ht_simulation_buffering_model_15, 
    Ht_simulation_buffering_model_16) # Model 14 has the lowest AIC

summary(Ht_simulation_buffering_model_14)

# Now calculate the variance explained by autocorrelation, variance and autocorrelation:variance

Ht_simulation_buffering_anova <- anova(Ht_simulation_buffering_model_14)

Ht_simulation_buffering_total_SS <- sum(Ht_simulation_buffering_anova["Sum Sq"])

Ht_simulation_buffering_autocorrelation_SS <- Ht_simulation_buffering_anova["I(autocorrelation^2)", "Sum Sq"] + Ht_simulation_buffering_anova["I(autocorrelation^3)", "Sum Sq"]

Ht_simulation_buffering_variance_SS <- Ht_simulation_buffering_anova["variance", "Sum Sq"] + Ht_simulation_buffering_anova["I(variance^2)", "Sum Sq"]

Ht_simulation_buffering_autocorrelation_contribution <- Ht_simulation_buffering_autocorrelation_SS / Ht_simulation_buffering_total_SS

Ht_simulation_buffering_variance_contribution <- Ht_simulation_buffering_variance_SS / Ht_simulation_buffering_total_SS

Ht_simulation_buffering_residual_contribution <- 1 - (Ht_simulation_buffering_autocorrelation_contribution + Ht_simulation_buffering_variance_contribution)

# Build a pie chart displaying the contributions.

# Create Data
Ht_simulation_buffering_df <- data.frame(
  group=c("autocorrelation",
          "variance",
          "residuals"),
  value=c(Ht_simulation_buffering_autocorrelation_contribution,
          Ht_simulation_buffering_variance_contribution,
          Ht_simulation_buffering_residual_contribution)
)

# Basic piechart

ggplot(Ht_simulation_buffering_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels



# Let's now model the impact of population structure figures


# First for Berberis thunbergii



Bt_df <- output_df %>% 
  filter(Genus_species == "Berberis_thunbergii")

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

summary(Bt_scaled_residuals_model_6) # All parameters are significant so we stay with Model 6

Bt_scaled_residuals_anova <- anova(Bt_scaled_residuals_model_6)

Bt_scaled_residuals_total_SS <- sum(Bt_scaled_residuals_anova["Sum Sq"])

Bt_scaled_residuals_autocorrelation_SS <- Bt_scaled_residuals_anova["autocorrelation", "Sum Sq"] + Bt_scaled_residuals_anova["I(autocorrelation^2)", "Sum Sq"] + Bt_scaled_residuals_anova["I(autocorrelation^3)", "Sum Sq"]

Bt_scaled_residuals_variance_SS <- Bt_scaled_residuals_anova["variance", "Sum Sq"] + Bt_scaled_residuals_anova["I(variance^2)", "Sum Sq"]

Bt_scaled_residuals_interaction_SS <- Bt_scaled_residuals_anova["variance:autocorrelation", "Sum Sq"]

Bt_scaled_residuals_autocorrelation_contribution <- Bt_scaled_residuals_autocorrelation_SS / Bt_scaled_residuals_total_SS

Bt_scaled_residuals_variance_contribution <- Bt_scaled_residuals_variance_SS / Bt_scaled_residuals_total_SS

Bt_scaled_residuals_interaction_contribution <- Bt_scaled_residuals_interaction_SS / Bt_scaled_residuals_total_SS

Bt_scaled_residuals_residual_contribution <- 1 - (Bt_scaled_residuals_autocorrelation_contribution + Bt_scaled_residuals_variance_contribution + Bt_scaled_residuals_interaction_SS)

# Build a pie chart displaying the contributions.

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

# Basic piechart

ggplot(Bt_scaled_residuals_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels




# Second for Calathea crotalifera



Cc_df <- output_df %>% 
  filter(Genus_species == "Calathea_crotalifera")

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

summary(Cc_scaled_residuals_model_6) # All parameters are significant so we stay with Model 6

Cc_scaled_residuals_anova <- anova(Cc_scaled_residuals_model_6)

Cc_scaled_residuals_total_SS <- sum(Cc_scaled_residuals_anova["Sum Sq"])

Cc_scaled_residuals_autocorrelation_SS <- Cc_scaled_residuals_anova["autocorrelation", "Sum Sq"] + Cc_scaled_residuals_anova["I(autocorrelation^2)", "Sum Sq"] + Cc_scaled_residuals_anova["I(autocorrelation^3)", "Sum Sq"]

Cc_scaled_residuals_variance_SS <- Cc_scaled_residuals_anova["variance", "Sum Sq"] + Cc_scaled_residuals_anova["I(variance^2)", "Sum Sq"]

Cc_scaled_residuals_interaction_SS <- Cc_scaled_residuals_anova["variance:autocorrelation", "Sum Sq"]

Cc_scaled_residuals_autocorrelation_contribution <- Cc_scaled_residuals_autocorrelation_SS / Cc_scaled_residuals_total_SS

Cc_scaled_residuals_variance_contribution <- Cc_scaled_residuals_variance_SS / Cc_scaled_residuals_total_SS

Cc_scaled_residuals_interaction_contribution <- Cc_scaled_residuals_interaction_SS / Cc_scaled_residuals_total_SS

Cc_scaled_residuals_residual_contribution <- 1 - (Cc_scaled_residuals_autocorrelation_contribution + Cc_scaled_residuals_variance_contribution + Cc_scaled_residuals_interaction_SS)

# Build a pie chart displaying the contributions.

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

# Basic piechart

ggplot(Cc_scaled_residuals_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels





# Third for Heliconia tortuosa



Ht_df <- output_df %>% 
  filter(Genus_species == "Heliconia_tortuosa")

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

summary(Ht_scaled_residuals_model_6) # Let's try without autocorrelation to see if it improves the model

Ht_scaled_residuals_model_10 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance + variance:autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                  data = Ht_df)

AIC(Ht_scaled_residuals_model_6,
    Ht_scaled_residuals_model_10) # Model 10 has the lowest AIC

summary(Ht_scaled_residuals_model_10) 

Ht_scaled_residuals_anova <- anova(Ht_scaled_residuals_model_10)

Ht_scaled_residuals_total_SS <- sum(Ht_scaled_residuals_anova["Sum Sq"])

Ht_scaled_residuals_autocorrelation_SS <- Ht_scaled_residuals_anova["I(autocorrelation^2)", "Sum Sq"] + Ht_scaled_residuals_anova["I(autocorrelation^3)", "Sum Sq"]

Ht_scaled_residuals_variance_SS <- Ht_scaled_residuals_anova["variance", "Sum Sq"] + Ht_scaled_residuals_anova["I(variance^2)", "Sum Sq"]

Ht_scaled_residuals_interaction_SS <- Ht_scaled_residuals_anova["variance:autocorrelation", "Sum Sq"]

Ht_scaled_residuals_autocorrelation_contribution <- Ht_scaled_residuals_autocorrelation_SS / Ht_scaled_residuals_total_SS

Ht_scaled_residuals_variance_contribution <- Ht_scaled_residuals_variance_SS / Ht_scaled_residuals_total_SS

Ht_scaled_residuals_interaction_contribution <- Ht_scaled_residuals_interaction_SS / Ht_scaled_residuals_total_SS

Ht_scaled_residuals_residual_contribution <- 1 - (Ht_scaled_residuals_autocorrelation_contribution + Ht_scaled_residuals_variance_contribution + Ht_scaled_residuals_interaction_SS)

# Build a pie chart displaying the contributions.

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

# Basic piechart

ggplot(Ht_scaled_residuals_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels



# Let's build the models for predicting buffering weight by stage distribution in response to autocorrelation, variance and autocorrelation:variance

# First Berberis thunbergii

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

summary(Bt_mean_buffered_stage_model_3) # Let's try without autocorrelation and autocorrelation:variance

Bt_mean_buffered_stage_model_10 <- lm(avg_stage_simulation_buffering  ~ variance + autocorrelation:variance + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                     data = Bt_df)

Bt_mean_buffered_stage_model_11 <- lm(avg_stage_simulation_buffering  ~ variance + autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Bt_df)

Bt_mean_buffered_stage_model_12 <- lm(avg_stage_simulation_buffering  ~ variance + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Bt_df)

AIC(Bt_mean_buffered_stage_model_3, 
    Bt_mean_buffered_stage_model_10, 
    Bt_mean_buffered_stage_model_11,
    Bt_mean_buffered_stage_model_12) # Model 3 has the lowest AIC

Bt_mean_buffered_stage_anova <- anova(Bt_mean_buffered_stage_model_3)

Bt_mean_buffered_stage_total_SS <- sum(Bt_mean_buffered_stage_anova["Sum Sq"])

Bt_mean_buffered_stage_autocorrelation_SS <- Bt_mean_buffered_stage_anova["autocorrelation", "Sum Sq"] + Bt_mean_buffered_stage_anova["I(autocorrelation^2)", "Sum Sq"] + Bt_mean_buffered_stage_anova["I(autocorrelation^3)", "Sum Sq"]

Bt_mean_buffered_stage_variance_SS <- Bt_mean_buffered_stage_anova["variance", "Sum Sq"] 

Bt_mean_buffered_stage_interaction_SS <- Bt_mean_buffered_stage_anova["variance:autocorrelation", "Sum Sq"]

Bt_mean_buffered_stage_autocorrelation_contribution <- Bt_mean_buffered_stage_autocorrelation_SS / Bt_mean_buffered_stage_total_SS

Bt_mean_buffered_stage_variance_contribution <- Bt_mean_buffered_stage_variance_SS / Bt_mean_buffered_stage_total_SS

Bt_mean_buffered_stage_interaction_contribution <- Bt_mean_buffered_stage_interaction_SS / Bt_mean_buffered_stage_total_SS

Bt_mean_buffered_stage_residual_contribution <- 1 - (Bt_mean_buffered_stage_autocorrelation_contribution + Bt_mean_buffered_stage_variance_contribution + Bt_mean_buffered_stage_interaction_SS)

# Build a pie chart displaying the contributions.

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

# Basic piechart

ggplot(Bt_mean_buffered_stage_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels

# Second Calathea crotalifera

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

summary(Cc_mean_buffered_stage_model_3) # Let's try without autocorrelation and autocorrelation:variance

Cc_mean_buffered_stage_model_10 <- lm(avg_stage_simulation_buffering  ~ variance + autocorrelation:variance + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Cc_df)

Cc_mean_buffered_stage_model_11 <- lm(avg_stage_simulation_buffering  ~ variance + autocorrelation + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Cc_df)

Cc_mean_buffered_stage_model_12 <- lm(avg_stage_simulation_buffering  ~ variance + I(variance^2) + I(variance^3) + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Cc_df)

AIC(Cc_mean_buffered_stage_model_3, 
    Cc_mean_buffered_stage_model_10, 
    Cc_mean_buffered_stage_model_11,
    Cc_mean_buffered_stage_model_12) # Model 3 has the lowest AIC

Cc_mean_buffered_stage_anova <- anova(Cc_mean_buffered_stage_model_3)

Cc_mean_buffered_stage_total_SS <- sum(Cc_mean_buffered_stage_anova["Sum Sq"])

Cc_mean_buffered_stage_autocorrelation_SS <- Cc_mean_buffered_stage_anova["autocorrelation", "Sum Sq"] + Cc_mean_buffered_stage_anova["I(autocorrelation^2)", "Sum Sq"] + Cc_mean_buffered_stage_anova["I(autocorrelation^3)", "Sum Sq"]

Cc_mean_buffered_stage_variance_SS <- Cc_mean_buffered_stage_anova["variance", "Sum Sq"] 

Cc_mean_buffered_stage_interaction_SS <- Cc_mean_buffered_stage_anova["variance:autocorrelation", "Sum Sq"]

Cc_mean_buffered_stage_autocorrelation_contribution <- Cc_mean_buffered_stage_autocorrelation_SS / Cc_mean_buffered_stage_total_SS

Cc_mean_buffered_stage_variance_contribution <- Cc_mean_buffered_stage_variance_SS / Cc_mean_buffered_stage_total_SS

Cc_mean_buffered_stage_interaction_contribution <- Cc_mean_buffered_stage_interaction_SS / Cc_mean_buffered_stage_total_SS

Cc_mean_buffered_stage_residual_contribution <- 1 - (Cc_mean_buffered_stage_autocorrelation_contribution + Cc_mean_buffered_stage_variance_contribution + Cc_mean_buffered_stage_interaction_SS)

# Build a pie chart displaying the contributions.

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

# Basic piechart

ggplot(Cc_mean_buffered_stage_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels


# Third Heliconia tortuosa

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

summary(Ht_mean_buffered_stage_model_3) # All parameters are significant so we stay with Model 3

Ht_mean_buffered_stage_anova <- anova(Ht_mean_buffered_stage_model_3)

Ht_mean_buffered_stage_total_SS <- sum(Ht_mean_buffered_stage_anova["Sum Sq"])

Ht_mean_buffered_stage_autocorrelation_SS <- Ht_mean_buffered_stage_anova["autocorrelation", "Sum Sq"] + Ht_mean_buffered_stage_anova["I(autocorrelation^2)", "Sum Sq"] + Ht_mean_buffered_stage_anova["I(autocorrelation^3)", "Sum Sq"]

Ht_mean_buffered_stage_variance_SS <- Ht_mean_buffered_stage_anova["variance", "Sum Sq"] 

Ht_mean_buffered_stage_interaction_SS <- Ht_mean_buffered_stage_anova["variance:autocorrelation", "Sum Sq"]

Ht_mean_buffered_stage_autocorrelation_contribution <- Ht_mean_buffered_stage_autocorrelation_SS / Ht_mean_buffered_stage_total_SS

Ht_mean_buffered_stage_variance_contribution <- Ht_mean_buffered_stage_variance_SS / Ht_mean_buffered_stage_total_SS

Ht_mean_buffered_stage_interaction_contribution <- Ht_mean_buffered_stage_interaction_SS / Ht_mean_buffered_stage_total_SS

Ht_mean_buffered_stage_residual_contribution <- 1 - (Ht_mean_buffered_stage_autocorrelation_contribution + Ht_mean_buffered_stage_variance_contribution + Ht_mean_buffered_stage_interaction_SS)

# Build a pie chart displaying the contributions.

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

# Basic piechart

ggplot(Ht_mean_buffered_stage_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels


# Now let's model the P/F sub-kernel contributions

# First Berberis thunbergii

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

Bt_P_F_difference_model_10 <- lm(P_F_difference  ~ variance + autocorrelation:variance + I(variance^2)  + I(autocorrelation^2) + I(autocorrelation^3),
                                      data = Bt_df)

AIC(Bt_P_F_difference_model_6, 
    Bt_P_F_difference_model_10) # Model 10 has the lowest AIC

summary(Bt_P_F_difference_model_10)

Bt_P_F_difference_anova <- anova(Bt_P_F_difference_model_10)

Bt_P_F_difference_total_SS <- sum(Bt_P_F_difference_anova["Sum Sq"])

Bt_P_F_difference_autocorrelation_SS <- Bt_P_F_difference_anova["I(autocorrelation^2)", "Sum Sq"] + Bt_P_F_difference_anova["I(autocorrelation^3)", "Sum Sq"]

Bt_P_F_difference_variance_SS <- Bt_P_F_difference_anova["variance", "Sum Sq"] + Bt_P_F_difference_anova["I(variance^2)", "Sum Sq"] 

Bt_P_F_difference_interaction_SS <- Bt_P_F_difference_anova["variance:autocorrelation", "Sum Sq"]

Bt_P_F_difference_autocorrelation_contribution <- Bt_P_F_difference_autocorrelation_SS / Bt_P_F_difference_total_SS

Bt_P_F_difference_variance_contribution <- Bt_P_F_difference_variance_SS / Bt_P_F_difference_total_SS

Bt_P_F_difference_interaction_contribution <- Bt_P_F_difference_interaction_SS / Bt_P_F_difference_total_SS

Bt_P_F_difference_residual_contribution <- 1 - (Bt_P_F_difference_autocorrelation_contribution + Bt_P_F_difference_variance_contribution + Bt_P_F_difference_interaction_SS)

# Build a pie chart displaying the contributions.

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

# Basic piechart

ggplot(Bt_P_F_difference_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels

# Second Calathea crotalifera

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

summary(Cc_P_F_difference_model_6) # Let's try without autocorrelation and variance and autocorrelation:variance

Cc_P_F_difference_model_10 <- lm(P_F_difference  ~ variance + autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Cc_df)

Cc_P_F_difference_model_11 <- lm(P_F_difference  ~ autocorrelation + autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Cc_df)

Cc_P_F_difference_model_12 <- lm(P_F_difference  ~ autocorrelation + variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Cc_df)

Cc_P_F_difference_model_13 <- lm(P_F_difference  ~ autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Cc_df)

Cc_P_F_difference_model_14 <- lm(P_F_difference  ~ variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Cc_df)

Cc_P_F_difference_model_15 <- lm(P_F_difference  ~ autocorrelation:variance  + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Cc_df)

Cc_P_F_difference_model_16 <- lm(P_F_difference  ~ I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Cc_df)


AIC(Cc_P_F_difference_model_6, 
    Cc_P_F_difference_model_10,
    Cc_P_F_difference_model_11,
    Cc_P_F_difference_model_12,
    Cc_P_F_difference_model_13,
    Cc_P_F_difference_model_14,
    Cc_P_F_difference_model_15,
    Cc_P_F_difference_model_16) # Model 15 has the lowest AIC

summary(Cc_P_F_difference_model_15) 

Cc_P_F_difference_anova <- anova(Cc_P_F_difference_model_15)

Cc_P_F_difference_total_SS <- sum(Cc_P_F_difference_anova["Sum Sq"])

Cc_P_F_difference_autocorrelation_SS <- Cc_P_F_difference_anova["I(autocorrelation^2)", "Sum Sq"] + Cc_P_F_difference_anova["I(autocorrelation^3)", "Sum Sq"]

Cc_P_F_difference_variance_SS <- Cc_P_F_difference_anova["I(variance^2)", "Sum Sq"] 

Cc_P_F_difference_interaction_SS <- Cc_P_F_difference_anova["autocorrelation:variance", "Sum Sq"]

Cc_P_F_difference_autocorrelation_contribution <- Cc_P_F_difference_autocorrelation_SS / Cc_P_F_difference_total_SS

Cc_P_F_difference_variance_contribution <- Cc_P_F_difference_variance_SS / Cc_P_F_difference_total_SS

Cc_P_F_difference_interaction_contribution <- Cc_P_F_difference_interaction_SS / Cc_P_F_difference_total_SS

Cc_P_F_difference_residual_contribution <- 1 - (Cc_P_F_difference_autocorrelation_contribution + Cc_P_F_difference_variance_contribution + Cc_P_F_difference_interaction_SS)

# Build a pie chart displaying the contributions.

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

# Basic piechart

ggplot(Cc_P_F_difference_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels


# Third Heliconia tortuosa

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

Ht_P_F_difference_model_10 <- lm(P_F_difference  ~ variance + autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Ht_df)

Ht_P_F_difference_model_11 <- lm(P_F_difference  ~ autocorrelation + autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Ht_df)

Ht_P_F_difference_model_12 <- lm(P_F_difference  ~ autocorrelation + variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Ht_df)

Ht_P_F_difference_model_13 <- lm(P_F_difference  ~ autocorrelation + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Ht_df)

Ht_P_F_difference_model_14 <- lm(P_F_difference  ~ variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Ht_df)

Ht_P_F_difference_model_15 <- lm(P_F_difference  ~ autocorrelation:variance + I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Ht_df)

Ht_P_F_difference_model_16 <- lm(P_F_difference  ~ I(variance^2) + I(autocorrelation^2) + I(autocorrelation^3),
                                 data = Ht_df)


AIC(Ht_P_F_difference_model_6, 
    Ht_P_F_difference_model_10,
    Ht_P_F_difference_model_11,
    Ht_P_F_difference_model_12,
    Ht_P_F_difference_model_13,
    Ht_P_F_difference_model_14,
    Ht_P_F_difference_model_15,
    Ht_P_F_difference_model_16) # Model 10 has the lowest AIC

summary(Ht_P_F_difference_model_10)

Ht_P_F_difference_anova <- anova(Ht_P_F_difference_model_10)

Ht_P_F_difference_total_SS <- sum(Ht_P_F_difference_anova["Sum Sq"])

Ht_P_F_difference_autocorrelation_SS <- Ht_P_F_difference_anova["I(autocorrelation^2)", "Sum Sq"] + Ht_P_F_difference_anova["I(autocorrelation^3)", "Sum Sq"]

Ht_P_F_difference_variance_SS <- Ht_P_F_difference_anova["variance", "Sum Sq"] + Ht_P_F_difference_anova["I(variance^2)", "Sum Sq"] 

Ht_P_F_difference_interaction_SS <- Ht_P_F_difference_anova["variance:autocorrelation", "Sum Sq"]

Ht_P_F_difference_autocorrelation_contribution <- Ht_P_F_difference_autocorrelation_SS / Ht_P_F_difference_total_SS

Ht_P_F_difference_variance_contribution <- Ht_P_F_difference_variance_SS / Ht_P_F_difference_total_SS

Ht_P_F_difference_interaction_contribution <- Ht_P_F_difference_interaction_SS / Ht_P_F_difference_total_SS

Ht_P_F_difference_residual_contribution <- 1 - (Ht_P_F_difference_autocorrelation_contribution + Ht_P_F_difference_variance_contribution + Ht_P_F_difference_interaction_SS)

# Build a pie chart displaying the contributions.

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

# Basic piechart

ggplot(Ht_P_F_difference_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="black", size = 1.5, alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("orange", "dark grey", "white", "dark blue")) +
  theme_void() # remove background, grid, numeric labels






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


