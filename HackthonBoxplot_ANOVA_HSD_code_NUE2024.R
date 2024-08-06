# Load required libraries
#......................
library(dplyr)
library(broom)
library(ggplot2)
library(tidyr)
library(purrr)

data<- read.csv("C:/Users/jitenderr15/Downloads/RESEARCH PROJECTS/2024 NUE Workshop_Illinois/Methods_Code/CornCorn_treatment_weatherdata_NUE2024.csv")
# Load the dataset
data1 <- read.csv("C:/Users/jitenderr15/Downloads/RESEARCH PROJECTS/2024 NUE Workshop_Illinois/Methods_Code/Soybean_treatment_weatherdata_NUE2024.csv")

# Attach the data
attach(data1)

# Check the structure of the data
str(data1)

# Check for missing values
sum(is.na(data1))

# Check unique values in NAME and Regions
unique(data1$NAME)
unique(data1$Regions)

# Ensure Yield_Mgha is numeric
data1$Yield_Mgha <- as.numeric(data1$Yield_Mgha)

# Convert Yield_Mgha to kilograms (1 Mg = 1000 kg)
data1$Yield_kg_ha <- data1$Yield_Mgha * 1000

# Create the boxplot
sc <- ggplot(data1, aes(x = as.factor(Nrate_kgha), y = Yield_kg_ha, fill = NAME)) +
  geom_boxplot() +
  facet_wrap(~ Regions) +
  labs(
    x = "N Rate (kg/ha)",
    y = "Yield (kg/ha)",
    fill = "ROI"
  ) +
  ggtitle("Region-wise Yield Distribution at Different Nitrogen Rates: Soybean-Corn") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = c(0.60, 0.78),
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.ticks = element_line(color = "black", size = 1.5),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "grey"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', color = "grey"),
    panel.border = element_rect(color = "black", size = 1),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  scale_y_continuous(breaks = c(4000, 8000, 12000, 16000, 20000, 24000))

# Display the boxplot
print(sc)

# Save the plot
ggsave("SoyCornboxplot_yield_vs_nrateNEW.png", plot = sc, width = 10, height = 6, dpi = 300)

# Save the plot
ggsave("boxplot_yield_vs_nrate.png", plot = boxplot, width = 10, height = 6, dpi = 300)




#Check the structure of the data
# Split the data by regions
# Split the data by regions
region_list <- split(data, data$Regions)

# Function to perform ANOVA and tidy the results for each region
perform_anova <- function(region_data, region_name) {
  anova_result <- aov(Yield_buac ~ Nrate_lbac + CLDD.cooling.degree.day + HTDD.Heating.degree.days +
                        ELEVATION + PRCP_In + TAVG_F + TMIN_F + TMAX_F + Max_WS_Mph +
                        Avg_WS_Mph + TSR_MJ.MM + Avg_TSR_MJ.MM + Max_RH_Prct + Min_RH_Prct +
                        Avg_DewP_F + Total_Eva_In + Avg_Tot_Evap_in + Avg_4inST_F + Avg_8inST_F, 
                      data = region_data)
  tidy_result <- tidy(anova_result)
  tidy_result <- tidy_result %>% mutate(Region = region_name)
  return(tidy_result)
}

# Perform ANOVA for each region and combine results
anova_results <- map2_dfr(region_list, names(region_list), perform_anova)

# Filter results where p-value is less than 0.05
significant_results <- anova_results %>% filter(p.value < 0.05)

# Print the significant results to console
print(significant_results)

# Save the significant results to a CSV file
write.csv(significant_results, "significant_anova_results_by_region.csv")

# Visualize the significant results
ggplot(significant_results, aes(x = term, y = -log10(p.value), fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Region) +
  labs(x = "Term", y = "-log10(p-value)", fill = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Significant ANOVA Results (p < 0.05) by Region")

# Save the plot
ggsave("significant_anova_results_by_region.png", width = 10, height = 6, dpi = 300)



# Perform ANOVA using base R
anova_result <- aov(Yield_buac ~ Nrate_lbac + CLDD.cooling.degree.day+HTDD.Heating.degree.days+
                      ELEVATION  + PRCP_In+TAVG_F +TMIN_F+TMAX_F+Max_WS_Mph+
                      Avg_WS_Mph+TSR_MJ.MM+Avg_TSR_MJ.MM+Max_RH_Prct+Min_RH_Prct+
                      Avg_DewP_F+Total_Eva_In+Avg_Tot_Evap_in+Avg_4inST_F+Avg_8inST_F
                      , data = data)

# View the summary of the ANOVA
summary(anova_result)

# Load necessary libraries
library(dplyr)
library(broom)
library(knitr)
library(kableExtra)
# Tidy the results
tidy_anova <- tidy(anova_result)

# Create the table
tidy_anova %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Perform Tukey HSD test
tukey_result <- TukeyHSD(anova_result)

# View the results
print(tukey_result)


# Clean the data to remove non-finite values
clean_data <- data[is.finite(data$Chlorophyll), ]

# Custom color scheme for boxplot
ggplot(clean_data, aes(x = Application, y = Chlorophyll, fill = Application)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +  # Use a predefined color palette
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "black") +
  geom_text(stat = "summary", fun = mean, aes(label = round(..y.., 2)), vjust = -0.5, color = "black") +
  ggtitle("Chlorophyll Content by Nutrient Application") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 20, face = "bold",hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.ticks = element_line(color = "black", size = 1.5),  # Increase size of ticks
    strip.text = element_text(size = 14, face = "bold")
  ) 
# Load necessary library
library(ggplot2)

# Clean the data to remove non-finite values
clean_data <- data[is.finite(data$Chlorophyll), ]

# Perform ANOVA
anova_result <- aov(Chlorophyll ~ Nutrient., data = clean_data)
summary(anova_result)


# Perform ANOVA
anova_result <- aov(Chlorophyll ~ Application+Nutrient., data = clean_data)
summary(anova_result)

# Load necessary libraries
library(ggplot2)
library(multcomp)
library(multcompView)

# Clean the data to remove non-finite values
clean_data <- data[is.finite(data$Chlorophyll), ]

# Perform ANOVA
anova_result <- aov(Chlorophyll ~ Application, data = clean_data)

# Tukey HSD test
tukey_result <- TukeyHSD(anova_result)

# Extract Tukey HSD test results
tukey_letters <- multcompLetters4(anova_result, tukey_result)

# Create a dataframe for plotting
means <- aggregate(Chlorophyll ~ Application, data = clean_data, mean)
means$Letters <- tukey_letters$`Application`$Letters

# Bar chart with significant letters
ggplot(means, aes(x = Application, y = Chlorophyll, fill = Application)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Letters), vjust = -0.5, size = 6) +
  ggtitle("Average Chlorophyll Content by Nutrient application (Tukey HSD)") +
  xlab("Nutrient application") +
  ylab("Average Chlorophyll Content") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 20, face = "bold",hjust = 0.5),
  
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.ticks = element_line(color = "black", size = 1.5),  # Increase size of ticks
    strip.text = element_text(size = 14, face = "bold")
  )  
