rm(list = ls())

elections <- read.csv("2020 November General Election - Turnout Rates.csv", header = TRUE)

head(elections)
str(elections)
required_packages <- c("ggplot2", "dplyr", "tidyr", "scales")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

print("All required packages are successfully installed and loaded.")



#1.  
#a)
elections$Vote.for.Highest.Office..President. <- as.numeric(gsub(",", "", elections$Vote.for.Highest.Office..President.))
elections$VEP.Turnout.Rate <- as.numeric(gsub("%", "", elections$VEP.Turnout.Rate))

ggplot(elections, aes(x = Vote.for.Highest.Office..President., y = VEP.Turnout.Rate)) +
  geom_point() +  # Plot points
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "Votes for Highest Office (President)", y = "VEP Turnout Rate (%)") +
  theme_minimal()


#b)
# Clean the VEP Turnout Rate column
elections$VEP.Turnout.Rate <- as.numeric(gsub("%", "", elections$VEP.Turnout.Rate))

# Create the histogram with density overlay and title
ggplot(elections, aes(x = VEP.Turnout.Rate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +  # Histogram plot
  geom_density(color = "red") +  # Density curve
  labs(
    title = "Distribution of VEP Turnout Rate Across States in the 2020 U.S. Election",  # Added title
    x = "VEP Turnout Rate (%)",
    y = "Density"
  ) +
  theme_minimal()





#2.
#a)  1. Boxplot with Outliers
# Clean VEP.Turnout.Rate to remove '%' signs and convert to numeric
elections$VEP.Turnout.Rate <- as.numeric(gsub("%", "", elections$VEP.Turnout.Rate))
ggplot(elections, aes(x = State, y = VEP.Turnout.Rate)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red") +  
  labs(x = "State", y = "VEP Turnout Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  




#b) 2. Histogram with Normal Curve Overlay
ggplot(elections, aes(x = VEP.Turnout.Rate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +  
  geom_density(color = "red") +  
  labs(x = "VEP Turnout Rate (%)", y = "Density") +
  theme_minimal()

# Normalize and reshape the data
elections_stacked <- elections %>%
  select(State, X..Non.citizen, Prison, Probation, Parole) %>%
  mutate(across(-State, ~ as.numeric(gsub(",", "", .)), .names = "cleaned_{col}")) %>%
  pivot_longer(cols = starts_with("cleaned"), names_to = "Category", values_to = "Count") %>%
  mutate(Category = gsub("cleaned_", "", Category))  

# Drop rows with NA values in Count
elections_stacked <- elections_stacked %>%
  filter(!is.na(Count))

elections_stacked_normalized <- elections_stacked %>%
  group_by(State) %>%
  mutate(Percentage = ifelse(sum(Count, na.rm = TRUE) > 0, Count / sum(Count, na.rm = TRUE) * 100, NA)) %>%
  ungroup() %>%
  filter(!is.na(Percentage))  

# Create the normalized stacked bar chart
ggplot(elections_stacked_normalized, aes(x = State, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +  
  labs(
    x = "State", 
    y = "Percentage (%)", 
    title = "Normalized Stacked Bar Chart: Proportions of Categories by State"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  


#1
elections$VEP.Turnout.Rate <- as.numeric(gsub("%", "", elections$VEP.Turnout.Rate))

ggplot(elections, aes(x = VEP.Turnout.Rate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +  
  geom_density(color = "red", linetype = "solid", size = 1) +  
  labs(
    title = "Histogram with Normal Curve Overlay for VEP Turnout Rate",
    x = "VEP Turnout Rate (%)",
    y = "Density"
  ) +
  theme_minimal()

normality_assumption <- "borderline"  

if (normality_assumption == "yes") {
  print("The normal curve overlay follows the contours of the data. Use Pearson’s r for correlation.")
} else if (normality_assumption == "no") {
  print("The normal curve overlay does not follow the shape of the data. Use Spearman’s Rho or Kendall’s Tau for correlation.")
} else {
  print("The example is borderline in terms of normality. When in doubt, use a non-parametric test (Spearman’s Rho or Kendall’s Tau).")
}




#2
elections$VEP.Turnout.Rate <- as.numeric(gsub("%", "", elections$VEP.Turnout.Rate))

ggplot(elections, aes(x = VEP.Turnout.Rate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +  
  geom_density(color = "red", linetype = "solid", size = 1) +  
  labs(
    title = "Histogram with Normal Curve Overlay for VEP Turnout Rate",
    x = "VEP Turnout Rate (%)",
    y = "Density"
  ) +
  theme_minimal()

normality_assumption <- "borderline"  

if (normality_assumption == "yes") {
  print("The normal curve overlay follows the contours of the data. Use a parametric test: t-test.")
} else if (normality_assumption == "no") {
  print("The normal curve overlay does not follow the shape of the data. Use a non-parametric test: Wilcoxon or Mann-Whitney U Test.")
} else {
  print("The example is borderline in terms of normality. When in doubt, choose the non-parametric test: Wilcoxon or Mann-Whitney U Test.")
}



elections_clean <- elections %>%
  filter(State.Abv != "", Official.Unofficial != "")
contingency_table <- table(elections_clean$Official.Unofficial, elections_clean$State.Abv)

print("Contingency Table:")
print(contingency_table)


# Perform the chi-square test
chi_square_result <- chisq.test(contingency_table)

print("Chi-Square Test Results:")
print(chi_square_result)

if (chi_square_result$p.value < 0.05) {
  print("There is a significant relationship between the official/unofficial status and state.")
} else {
  print("There is no significant relationship between the official/unofficial status and state.")
}

# Save the plot as a PNG file
ggsave("VEP_Turnout_vs_States.png", width = 10, height = 8)

ggsave("histogram_normality.png", width = 8, height = 6)
