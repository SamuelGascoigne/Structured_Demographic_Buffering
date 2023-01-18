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
