library(tidyverse)

load("data/simulation 2023-03-01 08.57.26.RData")

library(metR)
library(ggallin)
library(rasterVis)
library(raster)
library(rgl)
library(sjPlot)
library(scales)


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
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.247412203841646) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

output_df %>% 
  filter(Genus_species == "Berberis_thunbergii") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(stochastic_lambda))) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = 1.37813145461294) +
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
  filter(Genus_species == "Calathea_crotalifera") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(simulation_buffering))) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = -0.282909267143583) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

output$df %>% 
  filter(Genus_species == "Calathea_crotalifera") %>% 
  ggplot(aes(x = as.numeric(autocorrelation), 
             y = as.numeric(variance), 
             fill = as.numeric(stochastic_lambda))) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "#710193",
                       mid = "white",
                       high = "#005500",
                       midpoint = 0.975827577300978) +
  geom_rect(size=1, fill=NA, colour="black",
            aes(xmin=-0.07916667/2  , xmax=0.07916667/2  , ymin=((0.9916667+1)/2), ymax=((1+1.0083333 )/2))) +
  labs(x = "autocorrelation", y = "proportional variance", fill = expression(sum(E[a[ij]]^S^sigma))) +
  theme(text = element_text(size = 12, family = c("Roboto")),
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
