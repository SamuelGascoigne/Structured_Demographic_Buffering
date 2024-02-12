


load("data/simulation 2023-03-01 08.57.26.RData")

library(tidyverse)
library(metR)
library(ggallin)
library(rasterVis)
library(raster)
library(rgl)
library(sjPlot)
library(scales)
library(reshape2)
library(effectsize)

library(ggpubr)


# Let's make a plot to explain the plotting scheme

blank_matrix <- matrix(0, nrow = 15, ncol = 15)

blank_matrix_df <- blank_matrix %>% 
  melt()

blank_matrix %>% 
  melt() %>% 
  ggplot(aes(x = as.numeric(Var1), 
             y = as.numeric(Var2), 
             fill = as.numeric(value))) +
  geom_tile(colour = "light grey", 
            size = 0.5) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c()) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c()) +
  theme_minimal() +
  scale_fill_gradient2(low = "white",
                       mid = "white",
                       high = "white",
                       midpoint = 0,
                       breaks = c(0)) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=7.5, 
                xmax=8.5, 
                ymin=7.5, 
                ymax=8.5)) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)



demo_autocorrelation <- seq(from = -0.8, to = 0.8, length.out = 15)
demo_variance <- seq(from = 0.9, to = 1.1, length.out = 15)

demo_buffering_function <- function(auto, var){
  
  output <- 0.1*auto + -0.5*auto^3 + 1*var + 0.1*auto*var
  
  
}
set.seed(1234)
test_demo <- outer(demo_autocorrelation, demo_variance, demo_buffering_function)


test_demo_df <- test_demo %>% 
  melt() 

midpoint_demo <- test_demo_df$value[113]

test_demo %>% 
  melt() %>% 
  ggplot(aes(x = as.numeric(Var1), 
             y = as.numeric(Var2), 
             fill = as.numeric(value))) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c()) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c()) +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = midpoint_demo,
                       breaks = c(midpoint_demo)) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=7.5, 
                xmax=8.5, 
                ymin=7.5, 
                ymax=8.5)) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)






# Just the use of Berberis thunbergii and Calathea crotalifera


output_df <- output$df %>% 
  mutate(autocorrelation = as.numeric(autocorrelation)) %>% 
  mutate(variance = as.numeric(variance)) %>% 
  mutate(simulation_buffering = as.numeric(simulation_buffering)) %>% 
  mutate(stochastic_lambda = as.numeric(stochastic_lambda))


output_df %>% 
  filter(Genus_species != "Heliconia_tortuosa") %>% 
  ggplot(aes(x = as.numeric(stochastic_lambda), fill = Genus_species)) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             size = 0.5,
             colour = "dark red") +
  geom_density(alpha = 0.5,
               trim = TRUE,
               colour = "dark grey",
               size = 0.5) +
  geom_point(aes(y = 3), position = position_jitter(width = 0, height = 3),
             shape = 21) +
  scale_y_continuous(expand = c(0, 0.25)) +
  scale_x_continuous(limits = c(0.8, 1.4)) +
  scale_fill_manual(labels = c("Berberis thunbergii", "Calathea crotalifera"),
                    values = c("pink", "grey")) +
  labs(x = expression(lambda[s]), y = "Frequency", fill = "Species") +
  theme_minimal() +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)


output_df %>% 
  #filter(Genus_species != "Heliconia_tortuosa") %>% 
  ggplot(aes(x = as.numeric(stochastic_lambda), fill = Genus_species)) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             size = 1,
             colour = "dark red") +
  geom_density(alpha = 0.5,
               #trim = TRUE,
               colour = "black",
               size = 1) +
  # geom_point(aes(y = 3), position = position_jitter(width = 0, height = 3),
  #            shape = 21,
  #            alpha = 0.25) +
  scale_y_continuous(expand = c(0, 0.25)) +
  scale_x_continuous(limits = c(0.8, 1.4)) +
  scale_fill_manual(labels = c("Berberis thunbergii", "Calathea crotalifera", "Heliconia tortuosa"),
  values = c("#BD4E03", "#6785BB", "#686868")) +
  labs(x = expression(lambda[s]), y = "Frequency", fill = "Species") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)



Bt_buffering <- output_df %>% 
  filter(Genus_species == "Berberis_thunbergii") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(stochastic_lambda))) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0), breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = 1.37813145461294) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.114285714285714/2, 
                xmax=0.114285714285714/2, 
                ymin=((0.985714285714286+1)/2), 
                ymax=((1+1.01428571428571 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(lambda[s])) +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

Bt_stochastic_lambda_model_1 <- lm(stochastic_lambda ~ variance + autocorrelation,
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_stochastic_lambda_model_2 <- lm(stochastic_lambda ~ variance + autocorrelation + I(autocorrelation^2),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_stochastic_lambda_model_3 <- lm(stochastic_lambda ~ variance + autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))


summary(Bt_stochastic_lambda_model_1)
summary(Bt_stochastic_lambda_model_2)
summary(Bt_stochastic_lambda_model_3)

AIC(Bt_stochastic_lambda_model_1, Bt_stochastic_lambda_model_2, Bt_stochastic_lambda_model_3) # Evidence to take the quadratic model

plot_model(Bt_stochastic_lambda_model_2)

Cc_buffering <- output$df %>% 
  filter(Genus_species == "Calathea_crotalifera") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(stochastic_lambda))) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0), breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = 0.975827577300978) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.114285714285714/2, 
                xmax=0.114285714285714/2, 
                ymin=((0.985714285714286+1)/2), 
                ymax=((1+1.01428571428571 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression( lambda[s])) +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

Cc_stochastic_lambda_model_1 <- lm(stochastic_lambda ~ variance + autocorrelation,
                                   data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_stochastic_lambda_model_2 <- lm(stochastic_lambda ~ variance + autocorrelation + I(autocorrelation^2),
                                   data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_stochastic_lambda_model_3 <- lm(stochastic_lambda ~ variance + autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                   data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))


summary(Cc_stochastic_lambda_model_1)
summary(Cc_stochastic_lambda_model_2)
summary(Cc_stochastic_lambda_model_3)

AIC(Cc_stochastic_lambda_model_1, Cc_stochastic_lambda_model_2, Cc_stochastic_lambda_model_3) # Evidence to take the quadratic model

plot_model(Cc_stochastic_lambda_model_2)





Ht_buffering <- output$df %>% 
  filter(Genus_species == "Heliconia_tortuosa") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(stochastic_lambda))) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0), breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = 1.367447) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.114285714285714/2, 
                xmax=0.114285714285714/2, 
                ymin=((0.985714285714286+1)/2), 
                ymax=((1+1.01428571428571 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression( lambda[s])) +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

Ht_stochastic_lambda_model_1 <- lm(stochastic_lambda ~ variance + autocorrelation,
                                   data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_stochastic_lambda_model_2 <- lm(stochastic_lambda ~ variance + autocorrelation + I(autocorrelation^2),
                                   data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_stochastic_lambda_model_3 <- lm(stochastic_lambda ~ variance + autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                   data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))


summary(Ht_stochastic_lambda_model_1)
summary(Ht_stochastic_lambda_model_2)
summary(Ht_stochastic_lambda_model_3)

AIC(Ht_stochastic_lambda_model_1, Ht_stochastic_lambda_model_2, Ht_stochastic_lambda_model_3) # Evidence to take the quadratic model

plot_model(Cc_stochastic_lambda_model_2)



ggarrange(Bt_buffering, Cc_buffering, Ht_buffering)




# Some data cleaning

output_df <- output$df %>% 
  mutate(autocorrelation = as.numeric(autocorrelation)) %>% 
  mutate(variance = as.numeric(variance)) %>% 
  mutate(simulation_buffering = as.numeric(simulation_buffering)) %>% 
  mutate(stochastic_lambda = as.numeric(stochastic_lambda))

# First, let's make the figures for Berberis thunbergii.

output_df%>% 
  filter(Genus_species == "Berberis_thunbergii") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(simulation_buffering))) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0), breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.247412203841646) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.114285714285714/2, 
                xmax=0.114285714285714/2, 
                ymin=((0.985714285714286+1)/2), 
                ymax=((1+1.01428571428571 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

Bt_buffering_model_1 <- lm(simulation_buffering ~ variance * autocorrelation,
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_buffering_model_2 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_buffering_model_3 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_buffering_model_4 <- lm(simulation_buffering ~ variance + variance:autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                           data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))



summary(Bt_buffering_model_1)
summary(Bt_buffering_model_2)
summary(Bt_buffering_model_3)
summary(Bt_buffering_model_4)

AIC(Bt_buffering_model_1, Bt_buffering_model_2, Bt_buffering_model_3, Bt_buffering_model_4)

eta_squared(Bt_buffering_model_4)


output$df %>% 
  filter(Genus_species == "Calathea_crotalifera") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(simulation_buffering))) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0), breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.282909267143583) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.114285714285714/2, 
                xmax=0.114285714285714/2, 
                ymin=((0.985714285714286+1)/2), 
                ymax=((1+1.01428571428571 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

Cc_buffering_model_1 <- lm(simulation_buffering ~ variance * autocorrelation,
                           data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_buffering_model_2 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2),
                           data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_buffering_model_3 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                           data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

Cc_buffering_model_4 <- lm(simulation_buffering ~ variance + I(autocorrelation^2) + I(autocorrelation^3),
                           data = subset(output_df, output_df$Genus_species == "Calathea_crotalifera"))

summary(Cc_buffering_model_1)
summary(Cc_buffering_model_2)
summary(Cc_buffering_model_3)
summary(Cc_buffering_model_4)

AIC(Cc_buffering_model_1, Cc_buffering_model_2, Cc_buffering_model_3, Cc_buffering_model_4)

eta_squared(Cc_buffering_model_3)

output$df %>% 
  filter(Genus_species == "Heliconia_tortuosa") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(simulation_buffering))) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0), breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.034128153) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.114285714285714/2, 
                xmax=0.114285714285714/2, 
                ymin=((0.985714285714286+1)/2), 
                ymax=((1+1.01428571428571 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

Ht_buffering_model_1 <- lm(simulation_buffering ~ variance * autocorrelation,
                           data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_buffering_model_2 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2),
                           data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_buffering_model_3 <- lm(simulation_buffering ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                           data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

Ht_buffering_model_4 <- lm(simulation_buffering ~ variance + I(autocorrelation^2) + I(autocorrelation^3),
                           data = subset(output_df, output_df$Genus_species == "Heliconia_tortuosa"))

summary(Ht_buffering_model_1)
summary(Ht_buffering_model_2)
summary(Ht_buffering_model_3)
summary(Ht_buffering_model_4)

AIC(Ht_buffering_model_1, Ht_buffering_model_2, Ht_buffering_model_3, Ht_buffering_model_4)

eta_squared(Ht_buffering_model_4)


# Let's do the autocorrelation/population structure graphs

Bt_scaled_buffering <- output$df


output_df$scaled_simulation <- buffering

output_df %>% 
  group_by(Genus_species) %>% 
  add_column(scaled_simulation_buffering = scale(simulation_buffering))


output_df$scaled_simulation_buffering

Bt_df <- output_df %>% 
  filter(Genus_species == "Berberis_thunbergii")

Bt_df$scaled_simulation_buffering <- scale(Bt_df$simulation_buffering)

Bt_df$scaled_simulation_buffering_SSD <- scale(as.numeric(Bt_df$simulation_buffering_SSD))

Bt_scaled_residuals_1 <- Bt_df %>% 
  ggplot(aes(x = scaled_simulation_buffering,
             y = scaled_simulation_buffering_SSD)) +
  geom_abline(intercept = 0, 
              slope = 1,
              colour = "gold",
              linetype = "dashed",
              size = 1) +
  geom_point(colour = "dark grey",
             size = 1.5) +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

summary(lm(scaled_simulation_buffering_SSD ~ scaled_simulation_buffering, data = Bt_df))

Bt_df$scaled_simulation_buffering_SSD_residuals <- Bt_df$scaled_simulation_buffering_SSD - Bt_df$scaled_simulation_buffering

# Bt_df %>% 
#   ggplot(aes(x = simulation_buffering,
#              y = scaled_simulation_buffering_SSD_residuals,
#              colour = variance)) +
#   geom_abline(intercept = 0, 
#               slope = 0,
#               colour = "gold",
#               linetype = "dashed",
#               size = 1) +
#   geom_point() +
#   scale_colour_gradient2(low = "green",
#                          mid = "orange",
#                          high = "pink",
#                          midpoint = 1) +
#   theme_blank() +
#   theme(text = element_text(size = 12, family = c("Roboto")),
#         panel.background = element_rect(colour = "black", fill = "black", size = 1.5),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         aspect.ratio = 1)

# Bt_df %>% 
#   ggplot(aes(x = simulation_buffering,
#              y = scaled_simulation_buffering_SSD_residuals,
#              colour = autocorrelation)) +
#   geom_abline(intercept = 0, 
#               slope = 0,
#               colour = "gold",
#               linetype = "dashed",
#               size = 1) +
#   geom_point() +
#   scale_colour_gradient2(low = "blue",
#                          mid = "white ",
#                          high = "red",
#                          midpoint = 0) +
#   theme_blank() +
#   theme(text = element_text(size = 12, family = c("Roboto")),
#         panel.background = element_rect(colour = "black", fill = "black", size = 1.5),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         aspect.ratio = 1)

Bt_scaled_residuals_2 <- Bt_df %>% 
  ggplot(aes(x = autocorrelation,
             y = scaled_simulation_buffering_SSD_residuals,
             fill = variance)) +
  geom_abline(intercept = 0, 
              slope = 0,
              colour = "gold",
              linetype = "dashed",
              size = 1) +
  geom_point(alpha = 1,
             shape = 21,
             size = 3) +
  scale_x_continuous(breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(limits = c(-0.1, 0.13)) +
  scale_fill_gradient(low = "white",
                         high = "dark blue") +
  labs(y = "residuals") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

Bt_buffering_residuals_model_1 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation,
                           data = Bt_df )

Bt_buffering_residuals_model_2 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(autocorrelation^2),
                           data = Bt_df )

Bt_buffering_residuals_model_3 <- lm(scaled_simulation_buffering_SSD_residuals ~ variance * autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                           data = Bt_df )

summary(Bt_buffering_residuals_model_1)
summary(Bt_buffering_residuals_model_2)
summary(Bt_buffering_residuals_model_3)

AIC(Bt_buffering_residuals_model_1, Bt_buffering_residuals_model_2, Bt_buffering_residuals_model_3)

eta_squared(Bt_buffering_residuals_model_3)




Bt_average_buffering <- output$df %>% 
  filter(Genus_species == "Berberis_thunbergii") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(avg_stage_simulation_buffering))) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0), breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = 0.638351541191835) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.114285714285714/2, 
                xmax=0.114285714285714/2, 
                ymin=((0.985714285714286+1)/2), 
                ymax=((1+1.01428571428571 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = "buff") +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)










Cc_df <- output_df %>% 
  filter(Genus_species == "Calathea_crotalifera")

Cc_df$scaled_simulation_buffering <- scale(Cc_df$simulation_buffering)

Cc_df$scaled_simulation_buffering_SSD <- scale(as.numeric(Cc_df$simulation_buffering_SSD))

Cc_scaled_residuals_1 <- Cc_df %>% 
  ggplot(aes(x = scaled_simulation_buffering,
             y = scaled_simulation_buffering_SSD)) +
  geom_abline(intercept = 0, 
              slope = 1,
              colour = "gold",
              linetype = "dashed",
              size = 1) +
  geom_point(colour = "dark grey",
             size = 1.5) +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)
summary(lm(scaled_simulation_buffering_SSD ~ scaled_simulation_buffering, data = Cc_df))


Cc_df$scaled_simulation_buffering_SSD_residuals <- Cc_df$scaled_simulation_buffering_SSD - Cc_df$scaled_simulation_buffering
# 
# Cc_df %>% 
#   ggplot(aes(x = simulation_buffering,
#              y = scaled_simulation_buffering_SSD_residuals,
#              colour = variance)) +
#   geom_abline(intercept = 0, 
#               slope = 0,
#               colour = "gold",
#               linetype = "dashed",
#               size = 1) +
#   geom_point() +
#   scale_colour_gradient2(low = "green",
#                          mid = "orange",
#                          high = "pink",
#                          midpoint = 1) +
#   theme_blank() +
#   theme(text = element_text(size = 12, family = c("Roboto")),
#         panel.background = element_rect(colour = "black", fill = "black", size = 1.5),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         aspect.ratio = 1)

# Cc_df %>% 
#   ggplot(aes(x = simulation_buffering,
#              y = scaled_simulation_buffering_SSD_residuals,
#              colour = autocorrelation)) +
#   geom_abline(intercept = 0, 
#               slope = 0,
#               colour = "gold",
#               linetype = "dashed",
#               size = 1) +
#   geom_point() +
#   scale_colour_gradient2(low = "blue",
#                          mid = "white",
#                          high = "red",
#                          midpoint = 0) +
#   theme_blank() +
#   theme(text = element_text(size = 12, family = c("Roboto")),
#         panel.background = element_rect(colour = "black", fill = "black", size = 1.5),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         aspect.ratio = 1)

# Cc_df %>% 
#   ggplot(aes(x = autocorrelation,
#              y = scaled_simulation_buffering_SSD_residuals,
#              colour = variance)) +
#   geom_abline(intercept = 0, 
#               slope = 0,
#               colour = "gold",
#               linetype = "dashed",
#               size = 1) +
#   geom_point() +
#   scale_colour_gradient2(low = "blue",
#                          mid = "white",
#                          high = "red",
#                          midpoint = 1) +
#   theme_blank() +
#   theme(text = element_text(size = 12, family = c("Roboto")),
#         panel.background = element_rect(colour = "black", fill = "black", size = 1.5),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         aspect.ratio = 1)

Cc_scaled_residuals_2 <- Cc_df %>% 
  ggplot(aes(x = autocorrelation,
             y = scaled_simulation_buffering_SSD_residuals,
             fill = variance)) +
  geom_abline(intercept = 0, 
              slope = 0,
              colour = "gold",
              linetype = "dashed",
              size = 1) +
  geom_point(alpha = 1,
             shape = 21,
             size = 3) +
  scale_fill_gradient(low = "white",
                        high = "dark blue") +
  scale_x_continuous(breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(limits = c(-0.1, 0.13)) +
  theme_minimal() +
  labs(y = "residuals") +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

Cc_mean_stage <- output_df %>% 
  filter(Genus_species == "Calathea_crotalifera") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(avg_stage_simulation_buffering))) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0), breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = 0.896377245359335) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.114285714285714/2, 
                xmax=0.114285714285714/2, 
                ymin=((0.985714285714286+1)/2), 
                ymax=((1+1.01428571428571 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = "buff") +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)








Ht_df <- output_df %>% 
  filter(Genus_species == "Heliconia_tortuosa")

Ht_df$scaled_simulation_buffering <- scale(Ht_df$simulation_buffering)

Ht_df$scaled_simulation_buffering_SSD <- scale(as.numeric(Ht_df$simulation_buffering_SSD))

Ht_df$scaled_simulation_buffering_SSD_residuals <- Ht_df$scaled_simulation_buffering_SSD - Ht_df$scaled_simulation_buffering

Ht_scaled_residuals_1 <- Ht_df %>% 
  ggplot(aes(x = scaled_simulation_buffering,
             y = scaled_simulation_buffering_SSD)) +
  geom_abline(intercept = 0, 
              slope = 1,
              colour = "gold",
              linetype = "dashed",
              size = 1) +
  geom_point(colour = "dark grey",
             size = 1.5) +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

summary(lm(scaled_simulation_buffering_SSD ~ scaled_simulation_buffering, data = Ht_df))


Ht_scaled_residuals_2 <- Ht_df %>% 
  ggplot(aes(x = autocorrelation,
             y = scaled_simulation_buffering_SSD_residuals,
             fill = variance)) +
  geom_abline(intercept = 0, 
              slope = 0,
              colour = "gold",
              linetype = "dashed",
              size = 1) +
  geom_point(alpha = 1,
             shape = 21,
             size = 3) +
  scale_fill_gradient(low = "white",
                        high = "dark blue") +
  scale_x_continuous(breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(limits = c(-0.1, 0.13)) +
  theme_minimal() +
  labs(y = "residuals") +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

output_df %>% 
  filter(Genus_species == "Heliconia_tortuosa") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(avg_stage_simulation_buffering))) +
  geom_raster() +
  scale_x_continuous(expand = c(0, 0), breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = 0.84951647362003) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.114285714285714/2, 
                xmax=0.114285714285714/2, 
                ymin=((0.985714285714286+1)/2), 
                ymax=((1+1.01428571428571 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = "buff") +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)


# Subkernel buffering figures

output_df$P_F_difference <- as.numeric(output_df$simulation_P_buffering) - as.numeric(output_df$simulation_F_buffering)



output_df %>% 
  ggplot(aes(x = P_F_difference,
             y = simulation_buffering,
             fill = Genus_species)) +
  geom_vline(xintercept = 0,
             colour = "dark grey",
             size = 1) +
  geom_hline(yintercept = 0,
             colour = "dark grey",
             size = 1) +
  geom_point(alpha = 1,
             shape = 21,
             size = 3) +
  scale_fill_manual(values = c("#BD4E03", "#6785BB", "#686868")) +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1, legend.position = "none")

output_df$P_F_difference <- as.numeric(output_df$simulation_P_buffering) - as.numeric(output_df$simulation_F_buffering)



cor.test(simulation_buffering ~ P_F_difference, 
         data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"),
         method=c("pearson", "kendall", "spearman"))



output_df %>% 
  filter(Genus_species == "Berberis_thunbergii") |> 
  ggplot(aes(x = variance,
             y = P_F_difference,
             fill = autocorrelation)) +
  geom_point(alpha = 1,
             shape = 21,
             size = 3) +
  scale_fill_gradient(low = "white",
                      high = "orange")+
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

output_df %>% 
  filter(Genus_species == "Calathea_crotalifera") |> 
  ggplot(aes(x = variance,
             y = P_F_difference,
             fill = autocorrelation)) +
  geom_point(alpha = 1,
             shape = 21,
             size = 3) +
  scale_fill_gradient(low = "white",
                      high = "orange")+
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

output_df %>% 
  filter(Genus_species == "Heliconia_tortuosa") |> 
  ggplot(aes(x = variance,
             y = P_F_difference,
             fill = autocorrelation)) +
  geom_point(alpha = 1,
             shape = 21,
             size = 3) +
  scale_fill_gradient(low = "white",
                      high = "orange")+
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)



output_df %>% 
  filter(Genus_species == "Calathea_crotalifera") %>% 
  ggplot(aes(x = simulation_buffering,
             y = P_F_difference,
             colour = variance)) +
  geom_point() +
  scale_colour_gradient2(low = "green",
                         mid = "orange",
                         high = "pink",
                         midpoint = 1) +
  theme_blank() +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.background = element_rect(colour = "black", fill = "black", size = 1.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        aspect.ratio = 1)


output_df %>% 
  filter(Genus_species == "Heliconia_tortuosa") %>% 
  ggplot(aes(x = simulation_buffering,
             y = P_F_difference,
             colour = variance)) +
  geom_point() +
  scale_colour_gradient2(low = "green",
                         mid = "orange",
                         high = "pink",
                         midpoint = 1) +
  theme_blank() +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.background = element_rect(colour = "black", fill = "black", size = 1.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        aspect.ratio = 1)







# Intoduction

# Based on life history affects 

# Perturbation analysis is currently agnostic of the environmental sequence
# Vital rate decomposition and a mechanistic approach to the perturbation analysis used to quantify demographic buffering.

# Next paper: phylo comparative tools
# Reminder pdf of phylo comparative tools (long term)
# Mid-term: Draft outline of poster. Future directions


# Monday afternoon 4pm new version of Intro

# End of next week new version with methods and results updated (4pm on Friday) - calendar invite

# Rob Freckleton Phylo methods

# 



















output_df %>% 
  filter(Genus_species != "Heliconia_tortuosa") %>% 
  ggplot(aes(x = stochastic_lambda, fill = Genus_species)) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             size = 0.5,
             colour = "dark orange") +
  geom_density(alpha = 0.5,
               trim = TRUE,
               colour = "dark grey",
               size = 0.5) +
  geom_point(aes(y = 3), position = position_jitter(width = 0, height = 3),
             shape = 21) +
  scale_x_continuous(limits = c(0.8, 1.4)) +
  scale_fill_discrete(labels = c("Berberis thunbergii", "Calathea crotalifera")) +
  labs(x = expression(lambda[s]), y = "Frequency", fill = "Species") +
  theme_minimal() +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

















output$df %>% 
  filter(Genus_species == "Heliconia_tortuosa") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(simulation_buffering))) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.0341281525454058) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)


Bt_stochastic_lambda_model_1 <- lm(stochastic_lambda ~ variance + autocorrelation,
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_stochastic_lambda_model_2 <- lm(stochastic_lambda ~ variance + autocorrelation + I(autocorrelation^2),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))

Bt_stochastic_lambda_model_3 <- lm(stochastic_lambda ~ variance + autocorrelation + I(autocorrelation^2) + I(autocorrelation^3),
                                   data = subset(output_df, output_df$Genus_species == "Berberis_thunbergii"))


summary(Bt_stochastic_lambda_model_1)
summary(Bt_stochastic_lambda_model_2)
summary(Bt_stochastic_lambda_model_3)

AIC(Bt_stochastic_lambda_model_1, Bt_stochastic_lambda_model_2, Bt_stochastic_lambda_model_3) # Evidence to take the quadratic model

plot_model(Bt_stochastic_lambda_model_2)


output$df %>% 
  filter(Genus_species == "Heliconia_tortuosa") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(stochastic_lambda))) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = 1.36744709529114) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)



ggplot(output$df, aes(x = as.numeric(autocorrelation), 
                      y = as.numeric(variance), 
                      fill = as.numeric(simulation_buffering))) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.303545372426807) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

# Let's do some cleaning

output$df <- output$df %>% 
  mutate(autocorrelation = as.numeric(autocorrelation)) %>% 
  mutate(variance = as.numeric(variance)) %>% 
  mutate(simulation_buffering = as.numeric(simulation_buffering))

ggplot(output_df, aes(x = autocorrelation, 
                      y = variance, 
                      fill = simulation_buffering)) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.303545372426807) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

# Let's get rid of the mose extreme autocorrelation values

output_df %>% 
  filter(autocorrelation > -0.894117647058823) %>% 
  filter(autocorrelation < 0.894117647058823) %>% 
  ggplot(aes(x = autocorrelation, 
             y = variance, 
             fill = simulation_buffering)) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.303545372426807) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  scale_x_continuous(breaks = seq(from = -0.8, to = 0.8, by = 0.4)) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)




############

library(tidyverse)

load("data/simulation 2023-01-09 04.36.41.RData")

library(metR)
library(ggallin)
library(rasterVis)
library(raster)
library(rgl)

library(scales)

ggplot(output$df, aes(x = as.numeric(autocorrelation), 
                      y = as.numeric(variance), 
                      fill = as.numeric(simulation_buffering))) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.303545372426807) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

# Let's do some cleaning

output_df <- output$df %>% 
  mutate(autocorrelation = as.numeric(autocorrelation)) %>% 
  mutate(variance = as.numeric(variance)) %>% 
  mutate(simulation_buffering = as.numeric(simulation_buffering))

ggplot(output_df, aes(x = autocorrelation, 
                      y = variance, 
                      fill = simulation_buffering)) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.303545372426807) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

# Let's get rid of the mose extreme autocorrelation values

output_df %>% 
  filter(autocorrelation > -0.894117647058823) %>% 
  filter(autocorrelation < 0.894117647058823) %>% 
  ggplot(aes(x = autocorrelation, 
                        y = variance, 
                        fill = simulation_buffering)) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.303545372426807) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  scale_x_continuous(breaks = seq(from = -0.8, to = 0.8, by = 0.4)) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)
