# Set-up ----


# Clear the environment

rm(list = ls())


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


# Import libraries

library(tidyverse)
#library(scales)
library(reshape2)




# Figure 1 ----

# Let's make a plot to explain the plotting scheme

## Figure 1a ----

blank_matrix <- matrix(0, nrow = 15, ncol = 15)

blank_matrix_df <- blank_matrix |> 
  melt()

Fig1a_plot <- blank_matrix |> 
  melt() |> 
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

## Figure 1b ----

demo_autocorrelation <- seq(from = -0.8, to = 0.8, length.out = 15)
demo_variance <- seq(from = 0.9, to = 1.1, length.out = 15)

demo_buffering_function <- function(auto, var){
  
  output <- 0.1*auto + -0.5*auto^3 + 1*var + 0.1*auto*var
  
  
}
set.seed(1234)
test_demo <- outer(demo_autocorrelation, demo_variance, demo_buffering_function)


test_demo_df <- test_demo |> 
  melt() 

midpoint_demo <- test_demo_df$value[113]

Fig1b_plot <- test_demo |> 
  melt() |> 
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




# Figure 2 ----

## Figure 2a ----

Fig2a_plot <- Bt_df |> 
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


## Figure 2b ----

Fig2b_plot <- Cc_df |> 
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


## Figure 2c ----

Fig2c_plot <- Ht_df |> 
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




# Figure 3 ----

## Distributions of the scaled differences ----
# (i.e., stochastic elasticities|ASD - stochastic elasticities).

### Figure 3a ----

Bt_df$scaled_simulation_buffering <- scale(Bt_df$simulation_buffering)

Bt_df$scaled_simulation_buffering_SSD <- scale(as.numeric(Bt_df$simulation_buffering_SSD))

Fig3a_plot <- Bt_df |> 
  ggplot(aes(x = scaled_simulation_buffering_SSD - scaled_simulation_buffering)) +
  geom_density(fill = "#BD4E03", colour = "black", alpha = 0.1) +
  geom_point(aes(y = 1.4), 
             position = position_jitter(width = 0, height = 1.05),
             shape = 21,
             colour = "black", 
             fill = "#BD4E03",
             size = 2) + 
  scale_x_continuous(limits = c(-0.1, 0.15), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 7), expand = c(0,0)) +
  labs(x = "Difference", y = "Density") +
  theme_minimal() +
  theme(text = element_text(size = 16, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)


# Calculate the standard deviation of the distribution

Bt_df$scaled_simulation_buffering_SSD_residuals <- Bt_df$scaled_simulation_buffering_SSD - Bt_df$scaled_simulation_buffering

sd(Bt_df$scaled_simulation_buffering_SSD_residuals)


### Figure 3d ----

Cc_df$scaled_simulation_buffering <- scale(Cc_df$simulation_buffering)

Cc_df$scaled_simulation_buffering_SSD <- scale(as.numeric(Cc_df$simulation_buffering_SSD))

Fig3d_plot <- Cc_df |> 
  ggplot(aes(x = scaled_simulation_buffering_SSD - scaled_simulation_buffering)) +
  geom_density(fill = "#6785BB", colour = "black", alpha = 0.1) +
  geom_point(aes(y = 19), 
             position = position_jitter(width = 0, height = 14.25),
             shape = 21,
             colour = "black", 
             fill = "#6785BB",
             size = 2) + 
  scale_x_continuous(limits = c(-0.1, 0.15), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 95), expand = c(0,0)) +
  labs(x = "Difference", y = "Density") +
  theme_minimal() +
  theme(text = element_text(size = 16, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)


# Calculate the standard deviation of the distribution

Cc_df$scaled_simulation_buffering_SSD_residuals <- Cc_df$scaled_simulation_buffering_SSD - Cc_df$scaled_simulation_buffering

sd(Cc_df$scaled_simulation_buffering_SSD_residuals)


### Figure 3g ----

Ht_df$scaled_simulation_buffering <- scale(Ht_df$simulation_buffering)

Ht_df$scaled_simulation_buffering_SSD <- scale(as.numeric(Ht_df$simulation_buffering_SSD))

Fig3g_plot <- Ht_df |> 
  ggplot(aes(x = scaled_simulation_buffering_SSD - scaled_simulation_buffering)) +
  geom_density(fill = "#686868", colour = "black", alpha = 0.1) +
  geom_point(aes(y = 4), 
             position = position_jitter(width = 0, height = 3),
             shape = 21,
             colour = "black", 
             fill = "#686868",
             size = 2) +  
  scale_x_continuous(limits = c(-0.1, 0.15), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 20), expand = c(0,0)) +
  labs(x = "Difference", y = "Density") +
  theme_minimal() +
  theme(text = element_text(size = 16, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)


# Calculate the standard deviation of the distribution

Ht_df$scaled_simulation_buffering_SSD_residuals <- Ht_df$scaled_simulation_buffering_SSD - Ht_df$scaled_simulation_buffering

sd(Ht_df$scaled_simulation_buffering_SSD_residuals)



## Analysing population structure ----

### Berberis thunbergii ----

#### Figure 3b ----

Fig3b_plot <- Bt_df |> 
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


#### Figure 3c ----

Fig3c_plot <- Bt_df |> 
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


### Calathea crotalifera ----

#### Figure 3e ----

Fig3e_plot <- Cc_df |> 
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


#### Figure 3f ----

Fig3f_plot <- Cc_df |> 
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


### Heliconia tortuosa ----

#### Figure 3h ----

Fig3h_plot <- Ht_df |> 
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


#### Figure 3i ----

Fig3i_plot <- Ht_df|> 
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



# Figure 4 ----

## Figure 4a ----

output_df$P_F_difference <- as.numeric(output_df$simulation_P_buffering) - as.numeric(output_df$simulation_F_buffering)


Fig4a_plot <- output_df |> 
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
  labs(x = "P-F contribution") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1, legend.position = "none")

output_df$P_F_difference <- as.numeric(output_df$simulation_P_buffering) - as.numeric(output_df$simulation_F_buffering)




## Figure 4b ----

Fig4b_plot <- output_df |> 
  filter(Genus_species == "Berberis_thunbergii") |> 
  ggplot(aes(x = variance,
             y = P_F_difference,
             fill = autocorrelation)) +
  geom_point(alpha = 1,
             shape = 21,
             size = 3) +
  scale_fill_gradient(low = "white",
                      high = "orange")+
  labs(x = "Proportional varaince", y = "P-F contribution") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)


## Figure 4c ----

Fig4c_plot <- output_df |> 
  filter(Genus_species == "Calathea_crotalifera") |> 
  ggplot(aes(x = variance,
             y = P_F_difference,
             fill = autocorrelation)) +
  geom_point(alpha = 1,
             shape = 21,
             size = 3) +
  scale_fill_gradient(low = "white",
                      high = "orange")+
  labs(x = "Proportional varaince", y = "P-F contribution") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)


## Figure 4d ----

Fig4d_plot <- output_df |> 
  filter(Genus_species == "Heliconia_tortuosa") |> 
  ggplot(aes(x = variance,
             y = P_F_difference,
             fill = autocorrelation)) +
  geom_point(alpha = 1,
             shape = 21,
             size = 3) +
  scale_fill_gradient(low = "white",
                      high = "orange")+
  labs(x = "Proportional varaince", y = "P-F contribution") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)




# Supplementary Figure 1 -----

SupFig1_plot <- output_df |> 
  ggplot(aes(x = as.numeric(stochastic_lambda), fill = Genus_species)) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             size = 1,
             colour = "dark red") +
  geom_density(alpha = 0.5,
               colour = "black",
               size = 1) +
  scale_y_continuous(expand = c(0, 0.25)) +
  scale_x_continuous(limits = c(0.8, 1.4)) +
  scale_fill_manual(labels = c("Berberis thunbergii", "Calathea crotalifera", "Heliconia tortuosa"),
                    values = c("#BD4E03", "#6785BB", "#686868")) +
  labs(x = expression(lambda[s]), y = "Frequency", fill = "Species") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = c("Roboto")),
        panel.border = element_rect(colour = "black", fill = F, size = 1.5),
        aspect.ratio = 1)

