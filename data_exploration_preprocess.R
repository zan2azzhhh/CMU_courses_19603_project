# The purpose of this section is to understand the dataset that we are going to use as the input of building energy consumption intensity - 2015 building energy consumption benchmarking data collected under NYC Local Law 84/133 Energy Benchmarking, which requires owners and managers of buildings larger than 50,000 square (25,000 after 2016) to report their building’s energy usage to the City of New York on a yearly basis. 
# The column representing the energy consumption intensity is Site_EUI.
library(tidyverse)


# 1. Load raw data
file_path <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "dataset/NYCBuildingEnergyUse/nyc_benchmarking_disclosure_data_reported_in_2016.csv")
raw_data <- read_csv(file_path)


# 2. Pre-processing
my_data <- raw_data %>%
  filter(DOF_Benchmarking_Submission_Status == "In Compliance") %>% # filter valid Benchmarking Submission Status
  select(Record_Number, 
         Primary_Property_Type,
         Weather_Normalized_Site_EUI_kBtu_per_sqft,
         Weather_Normalized_Site_Electricity_Intensity_kWh_per_sqft,
         Weather_Normalized_Site_Natural_Gas_Intensity_therms_per_sqft,
         Total_GHG_Emissions_Metric_Tons_CO2e, # use net value 
         Municipally_Supplied_Potable_Water_Indoor_Intensity_gal_per_sqft,
         Year_Built,
         DOF_Property_Floor_Area_sqft
  ) %>%
  na.omit() %>% # quit properties with missing values
  rename(Record = Record_Number, # rename as the column names are too long
         TYPE = Primary_Property_Type,
         EUI = Weather_Normalized_Site_EUI_kBtu_per_sqft,
         EI = Weather_Normalized_Site_Electricity_Intensity_kWh_per_sqft,
         NGI = Weather_Normalized_Site_Natural_Gas_Intensity_therms_per_sqft,
         GHG = Total_GHG_Emissions_Metric_Tons_CO2e, 
         WI = Municipally_Supplied_Potable_Water_Indoor_Intensity_gal_per_sqft,
         YB = Year_Built,
         FA_DOF = DOF_Property_Floor_Area_sqft
  ) %>%
  mutate(BA = 2020 - as.numeric(YB)) %>% # Building Age
  filter(EUI < 500) %>% # filter out obcious outliers
  filter(EUI > 0)


# 3. Find and Plot numerical correlation
library(lattice)
library(reshape2)
# calculate correlation matrix
cor_matrix <- cor(my_data[,c("EUI", 
                             "EI", 
                             "NGI", 
                             "GHG", 
                             "WI", 
                             "BA",
                             "FA_DOF")])

corr_mel <- melt(cor_matrix)
corr_mel
# Plotting
library(corrplot)
corrplot(cor_matrix, method="color")


# 4. Some visualizations
# Plots for later basic factors modeling
library(modelr)
# Select top 6 property type composed of all properties
new_data <- my_data %>%
  filter(TYPE == "Multifamily Housing" |
           TYPE == "Office" |
           TYPE == "Hotel" |
           TYPE == "Senior Care Community" |
           TYPE == "Non-Refrigerated Warehouse" |
           TYPE == "Residence Hall/Dormitory"
  )

# 4.1 EUI vs EI
new_data1 <- new_data %>%
  filter(EI < 1500) # filter out 4 obvious outlier
new_data2 <- new_data %>%
  filter(TYPE != "Multifamily Housing" & EI < 15000) # filter out 2 obvious outlier
# y ~ x  
lm1.ei <- lm(EUI ~ EI * TYPE, data = new_data1)
summary(lm1.ei)
newdata1.ei <- new_data1 %>%
  data_grid(TYPE, EI, EUI) %>%
  add_predictions(lm1.ei)
ggplot(data = new_data1) +
  geom_point(aes(x = EI, y = EUI, color = TYPE), size = 0.2, alpha = 0.4) +
  geom_line(data = newdata1.ei, aes(x = EI, y = pred, color = TYPE)) +
  labs(x = "Electricity Intensity(kWh/sqft)", y = "EUI(kBtu/sqft)", title = "Linear Regression of EUI vs EI") +
  theme(plot.title = element_text(hjust = 0.5)) # make title in the middle
# log(y) ~ x
lm2.ei <- lm(log(EUI) ~ EI * TYPE, data = new_data1)
summary(lm2.ei)
newdata2.ei <- new_data1 %>%
  data_grid(TYPE, EI, EUI) %>%
  add_predictions(lm2.ei)
ggplot(data = new_data1) +
  geom_point(aes(x = EI, y = log(EUI), color = TYPE), size = 0.2, alpha = 0.4) +
  geom_line(data = newdata2.ei, aes(x = EI, y = pred, color = TYPE)) +
  labs(x = "Electricity Intensity(kWh/sqft)", y = "log(EUI)(kBtu/sqft)", title = "Linear Regression of log(EUI) vs EI") +
  theme(plot.title = element_text(hjust = 0.5))
# 4.2 EUI vs NGI
new_data3 <- new_data %>%
  filter(NGI < 400) # filter out 2 obvious outlier
# y ~ x  
lm1.ngi <- lm(EUI ~ NGI * TYPE, data = new_data3)
summary(lm1.ngi)
newdata1.ngi <- new_data3 %>%
  data_grid(TYPE, NGI, EUI) %>%
  add_predictions(lm1.ngi)
ggplot(data = new_data3) +
  geom_point(aes(x = NGI, y = EUI, color = TYPE), size = 0.2, alpha = 0.4) +
  geom_line(data = newdata1.ngi, aes(x = NGI, y = pred, color = TYPE)) +
  labs(x = "Natural Gas Intensity(therms/sqft)", y = "EUI(kBtu/sqft)", title = "Linear Regression of EUI vs NGI") +
  theme(plot.title = element_text(hjust = 0.5)) # make title in the middle
# log(y) ~ x
lm2.ngi <- lm(log(EUI) ~ NGI * TYPE, data = new_data3)
summary(lm2.ngi)
newdata2.ngi <- new_data3 %>%
  data_grid(TYPE, NGI, EUI) %>%
  add_predictions(lm2.ngi)
ggplot(data = new_data3) +
  geom_point(aes(x = NGI, y = log(EUI), color = TYPE), size = 0.2, alpha = 0.4) +
  geom_line(data = newdata2.ngi, aes(x = NGI, y = pred, color = TYPE)) +
  labs(x = "Natural Gas Intensity(therms/sqft)", y = "log(EUI)(kBtu/sqft)", title = "Linear Regression of log(EUI) vs NGI") +
  theme(plot.title = element_text(hjust = 0.5))
new_data <- my_data %>%
  filter(TYPE == "Multifamily Housing" |
           TYPE == "Office" |
           TYPE == "Hotel" |
           TYPE == "Senior Care Community" |
           TYPE == "Non-Refrigerated Warehouse" |
           TYPE == "Residence Hall/Dormitory"
  )
# 4.3 EUI vs WI
new_data4 <- new_data %>%
  filter(WI < 20000) # filter out 2 obvious outlier
sort(new_data4$EUI, descending = TRUE)
# y ~ x  
lm1.wi <- lm(EUI ~ WI * TYPE, data = new_data4)
summary(lm1.wi)
newdata1.wi <- new_data4 %>%
  data_grid(TYPE, WI, EUI) %>%
  add_predictions(lm1.wi)
ggplot(data = new_data4) +
  geom_point(aes(x = WI, y = EUI, color = TYPE), size = 0.2, alpha = 0.4) +
  geom_line(data = newdata1.wi, aes(x = WI, y = pred, color = TYPE)) +
  labs(x = "Water Intensity(gal/sqft)", y = "EUI(kBtu/sqft)", title = "Linear Regression of EUI vs WI") +
  theme(plot.title = element_text(hjust = 0.5)) # make title in the middle
# log(y) ~ x
lm2.wi <- lm(log(EUI) ~ WI * TYPE, data = new_data4)
summary(lm2.wi)
newdata2.wi <- new_data4 %>%
  data_grid(TYPE, WI, EUI) %>%
  add_predictions(lm2.wi) 
ggplot(data = new_data4) +
  geom_point(aes(x = WI, y = log(EUI), color = TYPE), size = 0.2, alpha = 0.4) +
  geom_line(data = newdata2.wi, aes(x = WI, y = pred, color = TYPE)) +
  labs(x = "Water Intensity(gal/sqft)", y = "log(EUI)(kBtu/sqft)", title = "Linear Regression of log(EUI) vs WI") +
  theme(plot.title = element_text(hjust = 0.5))
# 4.4 EUI vs BA
# y ~ x  
lm1.ba <- lm(EUI ~ BA * TYPE, data = new_data)
summary(lm1.ba)
newdata1.ba <- new_data %>%
  data_grid(TYPE, BA, EUI) %>%
  add_predictions(lm1.ba)
ggplot(data = new_data) +
  geom_point(aes(x = BA, y = EUI, color = TYPE), size = 0.2) +
  geom_line(data = newdata1.ba, aes(x = BA, y = pred, color = TYPE)) +
  labs(x = "Building Age(year)", y = "EUI(kBtu/sqft)", title = "Linear Regression of EUI vs BA") +
  theme(plot.title = element_text(hjust = 0.5)) # make title in the middle
# log(y) ~ x
lm2.ba <- lm(log(EUI) ~ BA * TYPE, data = new_data)
summary(lm2.ba)
newdata2.ba <- new_data %>%
  data_grid(TYPE, BA, EUI) %>%
  add_predictions(lm2.ba) 
ggplot(data = new_data) +
  geom_point(aes(x = BA, y = log(EUI), color = TYPE), size = 0.2) +
  geom_line(data = newdata2.ba, aes(x = BA, y = pred, color = TYPE)) +
  labs(x = "Building Age(year)", y = "log(EUI)(kBtu/sqft)", title = "Linear Regression of log(EUI) vs BA") +
  theme(plot.title = element_text(hjust = 0.5))
# 4.5 EUI vs FA_DOF
# y ~ x  
lm1.fa <- lm(EUI ~ FA_DOF * TYPE, data = new_data)
summary(lm1.fa)
newdata1.fa <- new_data %>%
  data_grid(TYPE, FA_DOF, EUI) %>%
  add_predictions(lm1.fa)
ggplot(data = new_data) +
  geom_point(aes(x = FA_DOF, y = EUI, color = TYPE), size = 0.2) +
  geom_line(data = newdata1.fa, aes(x = FA_DOF, y = pred, color = TYPE)) +
  labs(x = "Floor Area(sqft)", y = "EUI(kBtu/sqft)", title = "Linear Regression of EUI vs FA") +
  theme(plot.title = element_text(hjust = 0.5)) # make title in the middle
# log(y) ~ x
lm2.fa <- lm(log(EUI) ~ FA_DOF * TYPE, data = new_data)
summary(lm2.fa)
newdata2.fa <- new_data %>%
  data_grid(TYPE, FA_DOF, EUI) %>%
  add_predictions(lm2.fa) 
ggplot(data = new_data) +
  geom_point(aes(x = FA_DOF, y = log(EUI), color = TYPE), size = 0.2) +
  geom_line(data = newdata2.fa, aes(x = FA_DOF, y = pred, color = TYPE)) +
  labs(x = "Floor Age(sqft)", y = "log(EUI)(kBtu/sqft)", title = "Linear Regression of log(EUI) vs FA") +
  theme(plot.title = element_text(hjust = 0.5))