# functions to aid plotting and visualization



# plot a matrix with transformations to maximize fill contrast

kernel_plot <- function(kernel, name = "value", number_sqrt = 0, number_log = 0){
  
  kernel_df <- melt(kernel)
  
  if(number_sqrt != 0){
    
    for(root in 1:number_sqrt){
      
      kernel_df$value <- sqrt(kernel_df$value)
      
    }
  }
  
  if(number_log != 0){
    
    for(base in 1:number_log){
      
      kernel_df$value <- log(kernel_df$value)
      
    }
  }
  
  max_size <- max(kernel_df$Var1)
  
  kernel_df$Var1 <- kernel_df$Var1 / max_size
  
  kernel_df$Var2 <- kernel_df$Var2 / max_size
  
  
  
  plot <- kernel_df |> 
    ggplot(aes(x = Var2, y = Var1, fill = value)) +
    geom_raster() +
    theme_minimal() +
    scale_fill_viridis_c() +
    labs(x = "relative size (t)", y = "relative size (t + 1)", fill = name) +
    theme(text = element_text(size = 20, family = c("Montserrat")),
          panel.border = element_rect(colour = "black", fill = F, size = 1.5))
  
  return(plot)
  
}

kernel_buffering_plot <- function(kernel, name = "value", number_sqrt = 0, number_log = 0){
  
  kernel_df <- melt(kernel)
  
  if(number_sqrt != 0){
    
    for(root in 1:number_sqrt){
      
      kernel_df$value <- sqrt(kernel_df$value)
      
    }
  }
  
  if(number_log != 0){
    
    for(base in 1:number_log){
      
      kernel_df$value <- log(kernel_df$value)
      
    }
  }
  
  max_size <- max(kernel_df$Var1)
  
  kernel_df$Var1 <- kernel_df$Var1 / max_size
  
  kernel_df$Var2 <- kernel_df$Var2 / max_size
  
  
  
  plot <- kernel_df |> 
    ggplot(aes(x = Var2, y = Var1, fill = value)) +
    geom_raster() +
    theme_minimal() +
    scale_fill_gradient2(low = muted("purple"),
                         mid = NA,
                         high = muted("aquamarine"),
                         midpoint = 0) +
    labs(x = "relative size (t)", y = "relative size (t + 1)", fill = name) +
    theme(text = element_text(size = 20, family = c("Montserrat")),
          panel.border = element_rect(colour = "black", fill = F, size = 1.5))
  
  return(plot)
  
}


# colours for three plotted points/lines in ggplot

colours <- c("#3A548C", "#25AB82", "#D4E21B")



