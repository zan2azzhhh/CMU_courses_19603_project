library(tidyverse)
library(ggplot2)

# Load raw data
file_path <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "dataset/NYCBuildingEnergyUse/nyc_benchmarking_disclosure_data_reported_in_2016.csv")
raw_data <- read_csv(file_path)
# Pre-processing
my_data <- raw_data %>%
  filter(DOF_Benchmarking_Submission_Status == "In Compliance") %>% # filter valid Benchmarking Submission Status
  select(Record_Number, 
         Site_EUI_kBtu_per_sqft,
         Weather_Normalized_Site_Electricity_Intensity_kWh_per_sqft,
         Weather_Normalized_Site_Natural_Gas_Intensity_therms_per_sqft,
         Total_GHG_Emissions_Metric_Tons_CO2e, 
         Municipally_Supplied_Potable_Water_Indoor_Intensity_gal_per_sqft
         ) %>%
  na.omit() %>% # quit properties with missing values
  rename(Record = Record_Number, # rename as the column names are too long
         EUI = Site_EUI_kBtu_per_sqft,
         EI = Weather_Normalized_Site_Electricity_Intensity_kWh_per_sqft,
         NGI = Weather_Normalized_Site_Natural_Gas_Intensity_therms_per_sqft,
         GHG = Total_GHG_Emissions_Metric_Tons_CO2e, 
         WI = Municipally_Supplied_Potable_Water_Indoor_Intensity_gal_per_sqft)

# Fit in linear model with different X variables
model <- lm(EUI ~ EI, data = my_data)
# Create a scatterplot with regression line
ggplot(data = my_data, aes(x = EI, y = EUI)) + 
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") +
  geom_point(size = 0.7) + 
  labs(x = "Electricity Intensity(kWh/sqft)", y = "EUI(kBtu/sqft)", title = "Linear Regression of EUI vs EI") +
  theme(plot.title = element_text(hjust = 0.5)) # make title in the middle
ggplot(data = my_data, aes(x = NGI, y = EUI)) + 
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") +
  geom_point(size = 0.7) + 
  labs(x = "Natural Gas Intensity(therms/sqft)", y = "EUI(kBtu/sqft)", title = "Linear Regression of EUI vs NGI") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(data = my_data, aes(x = GHG, y = EUI)) + 
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") +
  geom_point(size = 0.7) + 
  labs(x = "Total GHG Emissions(tons)", y = "EUI(kBtu/sqft)", title = "Linear Regression of EUI vs GHG") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(data = my_data, aes(x = WI, y = EUI)) + 
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") +
  geom_point(size = 0.7) + 
  labs(x = "Water Intensity(gal/sqft)", y = "EUI(kBtu/sqft)", title = "Linear Regression of EUI vs WI") +
  theme(plot.title = element_text(hjust = 0.5))
