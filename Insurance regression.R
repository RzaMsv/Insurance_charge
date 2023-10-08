#In this script, we analyzed a data set downloaded from Kaggle.com about health insurance

# loading and checking the data
data <- read.csv("insurance.csv")
data

# Now we check the distribution of each numerical variable
# Load the necessary library; ggplot2, reshape2, and viridis
# Load the necessary libraries
library(ggplot2)
library(reshape2)
library(viridis)

# Now we separate numeric variables in a different vector and transpose it to fit ggplot2
numeric_variables <- c("age", "bmi")
numeric_data <- data[, numeric_variables]
melted_data <- melt(numeric_data)

# Now we create two histograms for BMI and Age
ggplot(data = melted_data, aes(x = value)) +
  geom_histogram(binwidth = 5, fill = viridis(4)[1], color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "Value", y = "Frequency",) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.background = element_rect(fill = "lightgray", color = "white"),
  ) +
  ggtitle("Insurance customers")

------------------------------------------------------------------------------

# Now we generate scatter plots: Age vs. BMI and Age vs. insurance charge

# We first need to do statistics, fitting the data to linear regression model

# Fit a linear regression model for Age vs. BMI
model_age_bmi <- lm(age ~ bmi, data = data)

# Fit a linear regression model for Charges vs. BMI
model_charges_bmi <- lm(charges ~ bmi, data = data)

# Create a function to extract coefficients and R-squared
get_regression_info <- function(model) {
  coef_info <- coef(model)
  r_squared_info <- summary(model)$r.squared
  return(list(coefficients = coef_info, r_squared = r_squared_info))
}

# Get regression info for Age vs. BMI
age_bmi_regression_info <- get_regression_info(model_age_bmi)

# Get regression info for Charges vs. BMI
charges_bmi_regression_info <- get_regression_info(model_charges_bmi)

# Create scatterplot for Age vs. BMI with regression line
plot_age_bmi <- ggplot(data, aes(x = bmi, y = age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Age vs. BMI",
    x = "BMI",
    y = "Age",
    subtitle = paste(
      "Age =",
      round(age_bmi_regression_info$coefficients[1], 2),
      "+",
      round(age_bmi_regression_info$coefficients[2], 2),
      "* BMI"
    ),
    caption = paste("R-squared:", round(age_bmi_regression_info$r_squared, 4))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), 
    axis.title = element_text(size = 14, face = "bold"), 
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  )

# Create scatterplot for Charges vs. BMI with regression line
plot_charges_bmi <- ggplot(data, aes(x = bmi, y = charges)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Charges vs. BMI",
    x = "BMI",
    y = "Charge",
    subtitle = paste(
      "Charges =",
      round(charges_bmi_regression_info$coefficients[1], 2),
      "+",
      round(charges_bmi_regression_info$coefficients[2], 2),
      "* BMI"
    ),
    caption = paste("R-squared:", round(charges_bmi_regression_info$r_squared, 4))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), 
    axis.title = element_text(size = 14, face = "bold"), 
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  )

# Combine both plots into a single figure
library(gridExtra)
combined_plots <- grid.arrange(plot_age_bmi, plot_charges_bmi, ncol = 2)

# Display the combined plots
print(combined_plots)
