

# Load necessary libraries
library(readr)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(mosaic)

# Define a function to read data, fit a model, and plot results
plot_model <- function(data_path, formula_str, x_label, y_label, output_path, point_color = "blue") {
  # Load the data
  df <- read.csv(data_path, sep = "|")
  
  # Fit the linear model
  formula <- as.formula(formula_str)
  model <- lm(formula, data = df)
  
  # Get model summary statistics
  model_summary <- summary(model)
  p_value <- coef(model_summary)[2, 4]
  r_squared <- model_summary$r.squared
  x_var <- all.vars(formula)[2]
  y_var <- all.vars(formula)[1]
  
  # Compute Spearman correlation
  spearman_test <- cor.test(df[[x_var]], df[[y_var]], method = "spearman")
  spearman_correlation <- spearman_test$estimate
  spearman_p_value <- spearman_test$p.value
  
  # Print model summary and Spearman correlation results
  print(model_summary)
  print(paste("Spearman Correlation:", round(spearman_correlation, 2)))
  print(paste("Spearman p-value:", round(spearman_p_value, 5)))
  
  # Define labels for x and y axes
  x_lab <- as.expression(parse(text = x_label))
  y_lab <- as.expression(parse(text = y_label))
  
  # Define a color palette
  palette10 <- c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000")
  
  # Create the plot
  p <- df %>%
    ggplot(aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.4, aes(color = CONTINENT, size = GDP_MD_EST)) + 
    scale_color_manual(values = palette10) +
    geom_errorbar(aes_string(ymin = paste0(y_var, "- log_lower"), ymax = paste0(y_var, "+ log_upper"), color = "CONTINENT")) +
    labs(y = y_lab, x = x_lab, 
         title = paste0("R = ", round(spearman_correlation, 2), 
                        ", p-value = ", round(spearman_p_value, 4))) +
    labs(size = "GDP", colour = "Continent") +
    geom_smooth(method = 'lm', se = FALSE, alpha = 0.3, color = "snow4", lwd = 0.4) +
    ggrepel::geom_text_repel(size = 3, data = df %>% filter(df[[y_var]] >= quantile(df[[y_var]], probs = 0.75)), 
                             aes(label = ADM0_A3)) +
    theme_bw()
  
  # Save the plot
  ggsave(output_path, p, width = 10, height = 8, dpi = 300)
  
  return(p)
}

# Define file path and model parameters
data_path <- "science20.csv"
formula_str <- "log_preva_2020 ~ log_count"
x_label <- "log[10](Science ~ News)"
y_label <- "log[10](Prevalence ~ LongCovid ~ 2020)"
output_path <- "plt_science20_.png"

# Plot the model
p <- plot_model(data_path, formula_str, x_label, y_label, output_path)

# Print the plot
print(p)
