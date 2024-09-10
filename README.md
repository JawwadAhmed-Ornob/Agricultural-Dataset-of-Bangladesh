# Agricultural-Dataset-of-Bangladesh
It's an Analysis on Agricultural Dataset of Bangladesh



# Loadiing Necessary Librariies

library(readr)   
library(dplyr)   
library(ggplot2)  
library(ggcorrplot)
library(GGally)
library(shiny)


# Import Dataset

Agri_Data <- read.csv("C:/Users/HP/R Programming/Agricultural Dataset.csv")


head(Agri_Data)
ncol(Agri_Data)
colnames(Agri_Data)

summary(Agri_Data)



# Adding Average Tmperatrue in Dataset

Agri_Data <- Agri_Data %>% 
  mutate(Avg_temp = (min_temperature+max_temperature)/2)

head(Agri_Data)
View(Agri_Data)

# Soil Moisture by District

soil_moisture_by_district<- Agri_Data %>%
  arrange(desc(soil.moisture)) %>%
  select(District, soil.moisture,year)



# Highest Soil Moisture
head(soil_moisture_by_district,n=1)  


# Lowest Soil Moisture
tail(soil_moisture_by_district,n=1) 


# Data Visualization


ggplot(Agri_Data, aes(x = reorder(District, -soil.moisture), y = soil.moisture, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Soil Moisture by District", x = "District", y = "Soil Moisture") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



# Group by district and calculate the total (sum) of Humidity for each district
humidity_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_humidity = sum(humidity, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(humidity_sum_by_district, by = "District")



# Total Rainfall 

total_humidity_by_district <- Agri_Data %>% 
  arrange(desc(total_humidity)) %>% 
  select(District, total_humidity)


# Highest Total Humidity by District
head(total_humidity_by_district,n=1)


# Lowest Total Rainfall By District
tail(total_humidity_by_district, n=1)


# Data Visualization

ggplot(Agri_Data, aes(x = reorder(District, -total_humidity), y = total_humidity, color = District)) +
  geom_point(size = 4) +
  labs(title = "Dot Plot of Total Humidity by District", x = "District", y = "Total Humidity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Group by district and calculate the total (sum) of rainfall for each district
rainfall_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_rainfall = sum(avg_rainfall, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(rainfall_sum_by_district, by = "District")



# Total Rainfall 

total_rainfall_by_district <- Agri_Data %>% 
  arrange(desc(total_rainfall)) %>% 
  select(District, total_rainfall)


# Highest Total rainfall by District
head(total_rainfall_by_district,n=1)


# Lowest Total Rainfall By District
tail(total_rainfall_by_district, n=1)



# Data Visualization

ggplot(Agri_Data, aes(x = reorder(District, -total_rainfall), y = total_rainfall, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Rainfall by District", x = "District", y = "Total Rainfall") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



# Group by district and calculate the total (sum) of Tempeture for each district

temp_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_temp = sum(Avg_temp, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(temp_sum_by_district, by = "District")


# Total Temperature

total_temp_by_district <- Agri_Data %>% 
  arrange(desc(total_temp)) %>% 
  select(District, total_temp)


# Highest Total Temperature by District
head(total_temp_by_district,n=1)

# Lowest Total Temperature by District
tail(total_temp_by_district, n=1)


# Data Visualization

ggplot(Agri_Data, aes(x = reorder(District, -total_temp), y = total_temp)) +
  geom_point(aes(color = District), size = 4) +
  labs(title = "Total Temperature by District", x = "District", y = "Total Temperature") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Group by district and calculate the total (sum) of Aus for each district

aus_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_aus = sum(aus, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(aus_sum_by_district, by = "District")


# Total Aus Production by District

total_aus_by_district <- Agri_Data %>% 
  arrange(desc(total_aus)) %>% 
  select(District, total_aus)


# Highest total Aus Production by District
head(total_aus_by_district,n=1)


# Lowest Total Aus Production by District
tail(total_aus_by_district, n=1)



# Data Visualization for Total Aus Production by District
ggplot(Agri_Data, aes(x = reorder(District, -total_aus), y = total_aus)) +
  geom_bar(stat = "identity", aes(fill = District)) + 
  labs(title = "Total Aus Production by District", x = "District", y = "Total Aus Production") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  




## Data Analysis of Aus Production based on Average Temperature, Average Rainfall, Humidity and Soil Moisture

# Correlation Matrix

analysis_data <- Agri_Data %>%
  select(aus, Avg_temp, avg_rainfall , humidity, soil.moisture)
correlation_matrix <- cor(analysis_data, use = "complete.obs")
ggcorrplot(correlation_matrix, method = "circle", lab = TRUE)


# Scatter plot matrix to visualize relationships
pairs(~ aus + Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data, 
      main = "Scatter Plot Matrix of Aus Production and Predictors")


## Indiividual Scatter Plots
#  Aus Production vs. Average Temperature:
ggplot(Agri_Data, aes(x = Avg_temp, y = aus)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Aus Production vs Average Temperature", x = "Average Temperature", y = "Aus Production")


# Aus Production vs. Rainfall:
ggplot(Agri_Data, aes(x = avg_rainfall, y = aus)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Aus Production vs Rainfall", x = "Rainfall", y = "Aus Production")

# Aus Production vs. Humidity:
ggplot(Agri_Data, aes(x = humidity, y = aus)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Aus Production vs Humidity", x = "Humidity", y = "Aus Production")


#  Aus Production vs. Soil Moisture:
ggplot(Agri_Data, aes(x = soil.moisture, y = aus)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Aus Production vs Soil Moisture", x = "Soil Moisture", y = "Aus Production")


# Linear regression model
model <- lm(aus ~ Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data)
summary(model)

# Diagnostic plots for the linear regression model
par(mfrow = c(2, 2)) 
plot(model)


# Group by district and calculate the total (sum) of Aman for each district

aman_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_aman = sum(aman, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(aman_sum_by_district, by = "District")


# Total Aus Production by District

total_aman_by_district <- Agri_Data %>% 
  arrange(desc(total_aman)) %>% 
  select(District, total_aman)


# Highest total Aus Production by District
head(total_aman_by_district,n=1)


# Lowest Total Aus Production by District
tail(total_aman_by_district, n=1)




# Data Visualization for Total Aus Production by District
ggplot(Agri_Data, aes(x = reorder(District, -total_aman), y = total_aman)) +
  geom_bar(stat = "identity", aes(fill = District)) + 
  labs(title = "Total Aman Production by District", x = "District", y = "Total Aman Production") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



## Data Analysis of Aus Production based on Average Temperature, Average Rainfall, Humidity and Soil Moisture

# Correlation Matrix

analysis_data <- Agri_Data %>%
  select(aman, Avg_temp, avg_rainfall , humidity, soil.moisture)
correlation_matrix <- cor(analysis_data, use = "complete.obs")
ggcorrplot(correlation_matrix, method = "circle", lab = TRUE)


# Scatter plot matrix to visualize relationships
pairs(~ aman + Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data, 
      main = "Scatter Plot Matrix of Aman Production and Predictors")


## Indiividual Scatter Plots
#  Aman Production vs. Average Temperature:
ggplot(Agri_Data, aes(x = Avg_temp, y = aman)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Aman Production vs Average Temperature", x = "Average Temperature", y = "Aman Production")


# Aus Production vs. Rainfall:
ggplot(Agri_Data, aes(x = avg_rainfall, y = aman)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Aman Production vs Rainfall", x = "Rainfall", y = "Aman Production")

# Aus Production vs. Humidity:
ggplot(Agri_Data, aes(x = humidity, y = aman)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Aman Production vs Humidity", x = "Humidity", y = "Aman Production")


#  Aus Production vs. Soil Moisture:
ggplot(Agri_Data, aes(x = soil.moisture, y = aman)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Aman Production vs Soil Moisture", x = "Soil Moisture", y = "Aman Production")


# Linear regression model
model <- lm(aman ~ Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data)
summary(model)

# Diagnostic plots for the linear regression model
par(mfrow = c(2, 2)) 
plot(model)


## Group by district and calculate the total (sum) of Boro for each district

boro_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_boro = sum(boro, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(boro_sum_by_district, by = "District")


# Total Boro Production by District

total_boro_by_district <- Agri_Data %>% 
  arrange(desc(total_boro)) %>% 
  select(District, total_boro)


# Highest total Boro Production by District
head(total_boro_by_district,n=1)


# Lowest Total Boro Production by District
tail(total_boro_by_district, n=1)




# Data Visualization for Total Aus Production by District
ggplot(Agri_Data, aes(x = reorder(District, -total_boro), y = total_boro)) +
  geom_bar(stat = "identity", aes(fill = District)) + 
  labs(title = "Total Boro Production by District", x = "District", y = "Total Boro Production") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



## Data Analysis of Boro Production based on Average Temperature, Average Rainfall, Humidity and Soil Moisture

# Correlation Matrix

analysis_data <- Agri_Data %>%
  select(boro, Avg_temp, avg_rainfall , humidity, soil.moisture)
correlation_matrix <- cor(analysis_data, use = "complete.obs")
ggcorrplot(correlation_matrix, method = "circle", lab = TRUE)


# Scatter plot matrix to visualize relationships
pairs(~ boro + Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data, 
      main = "Scatter Plot Matrix of Boro Production and Predictors")


## Indiividual Scatter Plots
#  Boro Production vs. Average Temperature:
ggplot(Agri_Data, aes(x = Avg_temp, y = boro)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Boro Production vs Average Temperature", x = "Average Temperature", y = "Boro Production")


# Boro Production vs. Rainfall:
ggplot(Agri_Data, aes(x = avg_rainfall, y = boro)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Boro Production vs Rainfall", x = "Rainfall", y = "Boro Production")

# Boro Production vs. Humidity:
ggplot(Agri_Data, aes(x = humidity, y = boro)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Boro Production vs Humidity", x = "Humidity", y = "Boro Production")


#  Aus Production vs. Soil Moisture:
ggplot(Agri_Data, aes(x = soil.moisture, y = boro)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Boro Production vs Soil Moisture", x = "Soil Moisture", y = "Boro Production")


# Linear regression model
model <- lm(boro ~ Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data)
summary(model)

# Diagnostic plots for the linear regression model
par(mfrow = c(2, 2)) 
plot(model)



## Group by district and calculate the total (sum) of Wheat for each district

wheat_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_wheat = sum(wheat, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(wheat_sum_by_district, by = "District")


# Total Wheat Production by District

total_wheat_by_district <- Agri_Data %>% 
  arrange(desc(total_wheat)) %>% 
  select(District, total_wheat)


# Highest total Boro Production by District
head(total_wheat_by_district,n=1)


# Lowest Total Boro Production by District
tail(total_wheat_by_district, n=1)


# Data Visualization for Total Wheat Production by District
ggplot(Agri_Data, aes(x = reorder(District, -total_wheat), y = total_wheat)) +
  geom_bar(stat = "identity", aes(fill = District)) + 
  labs(title = "Total Wheat Production by District", x = "District", y = "Total Wheat Production") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  




## Data Analysis of Wheat Production based on Average Temperature, Average Rainfall, Humidity and Soil Moisture

# Correlation Matrix

analysis_data <- Agri_Data %>%
  select(wheat, Avg_temp, avg_rainfall , humidity, soil.moisture)
correlation_matrix <- cor(analysis_data, use = "complete.obs")
ggcorrplot(correlation_matrix, method = "circle", lab = TRUE)


# Scatter plot matrix to visualize relationships
pairs(~ wheat + Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data, 
      main = "Scatter Plot Matrix of Wheat Production and Predictors")


## Indiividual Scatter Plots
#  Wheat Production vs. Average Temperature:
ggplot(Agri_Data, aes(x = Avg_temp, y = wheat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Wheat Production vs Average Temperature", x = "Average Temperature", y = "Wheat Production")


# Wheat Production vs. Rainfall:
ggplot(Agri_Data, aes(x = avg_rainfall, y = wheat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Wheat Production vs Rainfall", x = "Rainfall", y = "Wheat Production")

# Wheat Production vs. Humidity:
ggplot(Agri_Data, aes(x = humidity, y = wheat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Wheat Production vs Humidity", x = "Humidity", y = "Wheat Production")


#  Wheat Production vs. Soil Moisture:
ggplot(Agri_Data, aes(x = soil.moisture, y = wheat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Wheat Production vs Soil Moisture", x = "Soil Moisture", y = "Wheat Production")


# Linear regression model
model <- lm(wheat ~ Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data)
summary(model)

# Diagnostic plots for the linear regression model
par(mfrow = c(2, 2)) 
plot(model)




## Group by district and calculate the total (sum) of Potato for each district

potato_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_potato = sum(potato, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(potato_sum_by_district, by = "District")


# Total Wheat Production by District

total_potato_by_district <- Agri_Data %>% 
  arrange(desc(total_potato)) %>% 
  select(District, total_potato)


# Highest total Potato Production by District
head(total_potato_by_district,n=1)


# Lowest Total Potato Production by District
tail(total_potato_by_district, n=1)


# Data Visualization for Total Wheat Production by District
ggplot(Agri_Data, aes(x = reorder(District, -total_potato), y = total_potato)) +
  geom_bar(stat = "identity", aes(fill = District)) + 
  labs(title = "Total Potato Production by District", x = "District", y = "Total Potato Production") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



## Data Analysis of Potato Production based on Average Temperature, Average Rainfall, Humidity and Soil Moisture

# Correlation Matrix

analysis_data <- Agri_Data %>%
  select(potato, Avg_temp, avg_rainfall , humidity, soil.moisture)
correlation_matrix <- cor(analysis_data, use = "complete.obs")
ggcorrplot(correlation_matrix, method = "circle", lab = TRUE)


# Scatter plot matrix to visualize relationships
pairs(~ potato + Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data, 
      main = "Scatter Plot Matrix of Potato Production and Predictors")


## Indiividual Scatter Plots
#  Potato Production vs. Average Temperature:
ggplot(Agri_Data, aes(x = Avg_temp, y = potato)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Potato Production vs Average Temperature", x = "Average Temperature", y = "Potato Production")


# Potato Production vs. Rainfall:
ggplot(Agri_Data, aes(x = avg_rainfall, y = potato)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Potato Production vs Rainfall", x = "Rainfall", y = "Potato Production")

# Potato Production vs. Humidity:
ggplot(Agri_Data, aes(x = humidity, y = potato)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Potato Production vs Humidity", x = "Humidity", y = "Potato Production")


#  Potato Production vs. Soil Moisture:
ggplot(Agri_Data, aes(x = soil.moisture, y = potato)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Potato Production vs Soil Moisture", x = "Soil Moisture", y = "Potato Production")


# Linear regression model
model <- lm(potato ~ Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data)
summary(model)

# Diagnostic plots for the linear regression model
par(mfrow = c(2, 2)) 
plot(model)




## Group by district and calculate the total (sum) of Jute for each district

jute_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_jute = sum(jute, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(jute_sum_by_district, by = "District")


# Total Wheat Production by District

total_jute_by_district <- Agri_Data %>% 
  arrange(desc(total_jute)) %>% 
  select(District, total_jute)


# Highest total Jute Production by District
head(total_jute_by_district,n=1)


# Lowest Total Jute Production by District
tail(total_jute_by_district, n=1)


# Data Visualization for Total Wheat Production by District
ggplot(Agri_Data, aes(x = reorder(District, -total_jute), y = total_jute)) +
  geom_bar(stat = "identity", aes(fill = District)) + 
  labs(title = "Total Jute Production by District", x = "District", y = "Total Jute Production") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



## Data Analysis of Jute Production based on Average Temperature, Average Rainfall, Humidity and Soil Moisture

# Correlation Matrix

analysis_data <- Agri_Data %>%
  select(jute, Avg_temp, avg_rainfall , humidity, soil.moisture)
correlation_matrix <- cor(analysis_data, use = "complete.obs")
ggcorrplot(correlation_matrix, method = "circle", lab = TRUE)


# Scatter plot matrix to visualize relationships
pairs(~ jute + Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data, 
      main = "Scatter Plot Matrix of Jute Production and Predictors")


## Indiividual Scatter Plots
#  Jute Production vs. Average Temperature:
ggplot(Agri_Data, aes(x = Avg_temp, y = jute)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Jute Production vs Average Temperature", x = "Average Temperature", y = "Jute Production")


# Jute Production vs. Rainfall:
ggplot(Agri_Data, aes(x = avg_rainfall, y = jute)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Jute Production vs Rainfall", x = "Rainfall", y = "Jute Production")

# Jute Production vs. Humidity:
ggplot(Agri_Data, aes(x = humidity, y = jute)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Jute Production vs Humidity", x = "Humidity", y = "Jute Production")


#  Jute Production vs. Soil Moisture:
ggplot(Agri_Data, aes(x = soil.moisture, y = jute)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Jute Production vs Soil Moisture", x = "Soil Moisture", y = "Jute Production")


# Linear regression model
model <- lm(jute ~ Avg_temp + avg_rainfall + humidity + soil.moisture, data = Agri_Data)
summary(model)

# Diagnostic plots for the linear regression model
par(mfrow = c(2, 2)) 
plot(model)


## Identify the most and least stormy districts

storm_summary <- Agri_Data %>%
  group_by(District) %>%
  summarise(Storm_Count = sum(storm == "yes", na.rm = TRUE)) %>%
  arrange(desc(Storm_Count))  

print(storm_summary)
most_stormy_district <- storm_summary[1, ] 
least_stormy_district <- storm_summary[nrow(storm_summary), ]  
print(paste("The most stormy district is", most_stormy_district$District, "with", most_stormy_district$Storm_Count, "storms."))
print(paste("The least stormy district is", least_stormy_district$District, "with", least_stormy_district$Storm_Count, "storms."))

## Finding Out the Ideal Fertilizer for Each Crops


## Aus Production

aus_model <- lm(aus ~ urea + tsp + mp + DAP, data = Agri_Data)
summary(aus_model)

## mp has a significant positive coefficicent indicating it's beneficial for Aus Production
## DAP also has a positive coefficient but not as much as mp
## urea & tsp are not actually statistically significant


## Aman Production

aman_model <- lm(aman ~ urea + tsp + mp + DAP, data = Agri_Data)
summary(aman_model)

## Urea and DAP are both statistically significant and have positive impacts on Aman production.Urea and DAP are both statistically significant and have positive impacts on Aman production.
## TSP and MP do not show statistically significant effects on Aman production in this model.


## Boro Production

boro_model <- lm(boro ~ urea + tsp + mp + DAP, data = Agri_Data)
summary(boro_model)

## Urea is the only fertilizer that has a significant positive impact on crop production. Its p-value is highly significant (p < 0.001), making it the best-performing fertilizer in this model.
## TSP, MP, and DAP are not statistically significant, meaning their impacts on crop production cannot be conclusively determined from this data. Although TSP has a positive coefficient, it is not significant, while MP and DAP both have negative coefficients without statistical significance.


## Wheat Production

wheat_model <- lm(wheat ~ urea + tsp + mp + DAP, data = Agri_Data)
summary(wheat_model)

## Urea has a strong positive and highly significant effect on crop yield. Increasing urea usage is likely to increase yield.
## DAP has a strong negative and highly significant effect on crop yield. Increasing DAP usage is likely to decrease yield
## TSP and MP are not statistically significant in this model, meaning their impact on crop production cannot be conclusively determined from the data.



## Potato Production

potato_model <- lm(potato ~ urea + tsp + mp + DAP, data = Agri_Data)
summary(potato_model)

## TSP and MP have strong positive effects on crop yield. Their p-values are highly significant, indicating that increasing the use of these fertilizers leads to higher production.
## DAP has a strong negative effect on crop yield. It is also highly significant, suggesting that increasing DAP usage reduces crop production.
## Urea has a negative coefficient, but its effect is not statistically significant in this model, meaning there is no conclusive evidence of its impact on yield based on this data.


## Jute Production

jute_model <- lm(jute ~ urea + tsp + mp + DAP, data = Agri_Data)
summary(jute_model)

## Urea has a strong positive effect on crop yield and is highly significant, indicating that increasing urea usage will likely increase production.
## TSP has a marginally positive effect on yield, with borderline statistical significance. Its impact should be investigated further.
## MP and DAP both have strong negative effects on crop yield and are statistically significant. Increasing the use of these fertilizers could harm crop production.

### Land Types

## Inundationland_Highland
highland_data_by_district<- Agri_Data %>%
  arrange(desc(inundationland_Highland)) %>%
  select(District, inundationland_Highland)

# Most highland District
head(highland_data_by_district, n=1)

# Least Highland District
tail(highland_data_by_district, n=1)

# Highland Chart Visualization


ggplot(Agri_Data, aes(x = reorder(District, -inundationland_Highland), y = inundationland_Highland, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Highland by District", x = "District", y = "Highland") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

## Inundationland_Mediumhighland

mediumhighland_data_by_district<- Agri_Data %>%
  arrange(desc(inundationland_mediumhighland)) %>%
  select(District, inundationland_mediumhighland)

# Most highland District
head(mediumhighland_data_by_district, n=1)

# Least Highland District
tail(mediumhighland_data_by_district, n=1)

# MediumHighland Chart Visualization


ggplot(Agri_Data, aes(x = reorder(District, -inundationland_mediumhighland), y = inundationland_mediumhighland, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Medium Highland by District", x = "District", y = "Medium_Highland") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



## Inundationland_Medium Low land

mediumlowland_data_by_district<- Agri_Data %>%
  arrange(desc(inundationland_mediumlowland)) %>%
  select(District, inundationland_mediumlowland)

# Most Low land District
head(mediumlowland_data_by_district, n=1)

# Least Low land District
tail(mediumlowland_data_by_district, n=1)

# Medium Low land Chart Visualization


ggplot(Agri_Data, aes(x = reorder(District, -inundationland_mediumlowland), y = inundationland_mediumlowland, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Medium Low land by District", x = "District", y = "Medium_Lowland") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  




## Miscellaneous land


# Group by district and calculate the total (sum) of Miscellaneous Land for each district
miscellaneousland_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_miscellaneousland = sum(Miscellaneous.Land, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(miscellaneousland_sum_by_district, by = "District")


miscellaneousland_data_by_district<- Agri_Data %>%
  arrange(desc(total_miscellaneousland)) %>%
  select(District, total_miscellaneousland)

# Most Miscellaneous land District
head(miscellaneousland_data_by_district, n=1)

# Least Miscellaneous land District
tail(miscellaneousland_data_by_district, n=1)

# Miscellaneous land Chart Visualization
ggplot(Agri_Data, aes(x = reorder(District, -total_miscellaneousland), y = total_miscellaneousland, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Miscellaneous land by District", x = "District", y = "Miscellaneous land") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


### Soil Type


# Group by district and calculate the total (sum) of Noncalcareous Alluvium soil for each district
noncalcareous_alluvium_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_noncalcareous_alluvium= sum(Noncalcareous.Alluvium, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(noncalcareous_alluvium_sum_by_district, by = "District")


noncalcareous_alluvium_sum_by_district<- Agri_Data %>%
  arrange(desc(total_noncalcareous_alluvium)) %>%
  select(District, total_noncalcareous_alluvium)

# Most Noncalcareous Alluvium soil District
head(noncalcareous_alluvium_sum_by_district, n=1)

# Least Noncalcareous Alluvium soil District
tail(noncalcareous_alluvium_sum_by_district, n=1)

# Noncalcareous Alluvium soil Chart Visualization
ggplot(Agri_Data, aes(x = reorder(District, -total_noncalcareous_alluvium), y = total_noncalcareous_alluvium, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Noncalcareous Alluvium soil by District", x = "District", y = "Noncalcareous Alluvium soil") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  





# Group by district and calculate the total (sum) of Acid Basin Clay for each district
acid_basin_clay_sum_by_district <- Agri_Data %>%
  group_by(District) %>%
  summarise(total_acid_basin_clay= sum(Acid.Basin.Clay, na.rm = TRUE))

Agri_Data <- Agri_Data %>%
  left_join(acid_basin_clay_sum_by_district, by = "District")


acid_basin_clay_sum_by_district<- Agri_Data %>%
  arrange(desc(total_acid_basin_clay)) %>%
  select(District, total_acid_basin_clay)

# Most Acid Basin Clay District
head(acid_basin_clay_sum_by_district, n=1)

# Least Acid Basin Clay District
tail(acid_basin_clay_sum_by_district, n=1)

# Noncalcareous Alluvium soil Chart Visualization
ggplot(Agri_Data, aes(x = reorder(District, -total_acid_basin_clay), y = total_acid_basin_clay, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Acid Basin Clay by District", x = "District", y = "Acid Basin Clay") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

