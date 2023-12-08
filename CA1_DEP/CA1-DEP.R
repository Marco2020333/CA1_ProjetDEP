
# Most of code was provided from Tutorial 5 by  lecturer  Dr Muhmmad Lqbal
# Load necessary libraries
install.packages(c("tidyverse", "conflicted"))
install.packages("skimr")
install.packages("caret")

library(conflicted)
library(tidyverse)
library(skimr)
library(dplyr)
library(caret)
library(ggplot2)
# read the dataset
covid_2022 <-read_csv(file="covid_2022.csv")
view(covid_2022)

# variable create to get skim method
skim_report <- skim(covid_2022)

# Print the summary
print(skim_report)
# Load the dplyr library

# bar plot the Categorical Variables
ggplot(covid_2022, aes(x = countriesAndTerritories)) +
  geom_bar() +
  labs(title = "Distribution of Countries and Territories") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels at a 45-degree angle

ggplot(covid_2022, aes(x = geoId)) +
  geom_bar() +
  labs(title = "Distribution of Geographic IDs")
   

ggplot(covid_2022, aes(y = countryterritoryCode)) +
  geom_bar() +
  labs(title = "Distribution of Country/Territory Codes")

ggplot(covid_2022, aes(x = continentExp)) +
  geom_bar(stat = "count", width = 0.7) +
  labs(title = "Distribution of Continents") +
  theme_minimal()

# Convert dateRep to Date type
covid_2022$dateRep <- as.Date(covid_2022$dateRep, format = "%d/%m/%Y")#https://community.rstudio.com/t/convert-character-string-to-date/130524

# Create a variable 'count' with a constant value of 1 for each date
covid_2022$Count <- 1

# 
ggplot(covid_2022, aes(x = dateRep)) +
  geom_bar(stat = "count") +
  labs(title = "DateRep")


#  bar plot to Discrete Variables
ggplot(covid_2022, aes(x = day, fill = "red")) +  # Fill setting the color
  geom_bar() +
  labs(title = "Distribution of Days")

ggplot(covid_2022, aes(x = month, fill = "red")) +
  geom_bar() +
  labs(title = "Distribution of Months")

ggplot(covid_2022, aes(x = year, fill = "red")) +
  geom_bar() +
  labs(title = "Distribution of Years")

ggplot(covid_2022, aes(x = cases)) +
  geom_bar(binwidth = 1000, fill = "purple", color = "red") +
  labs(title = "Distribution of Cases")
  


ggplot(covid_2022, aes(x = deaths)) +
  geom_bar(binwidth = 1000, fill = "purple", color = "red") +
  labs(title = "Distribution of Deaths")
  


# scatter Plot the Continuous Variables
ggplot(covid_2022, aes(x = popData2020)) +
  geom_density() +
  labs(title = "Density of Population Data") +
  scale_x_continuous(labels = scales::comma) 

#Question B
# Glimpse at the dataset
glimpse(covid_2022)

summary(covid_2022)


#Question C Min-Max Normalization, Z-score Standardization and Robust scalar
preprocess_num <-preProcess(covid_2022[, c(2, 3,4,5,6,10)], method=c('center','scale'))

#
data_Process <-predict(preprocess_num,covid_2022[, c(2, 3,4,5,6,10)])

#
summary(data_Process)

#Question D
# Scatter plot
ggplot(covid_2022, aes(x = cases, y = deaths)) +
  geom_point() +
  labs(x = "Cases", y = "Deaths", title = "Cases vs Deaths") +
  scale_x_continuous(labels = scales::number_format(scale = 1e-3)) + # this line from :https://thiyanga.netlify.app/post/plotaxis/
  scale_y_continuous(labels = scales::number_format(scale = 1e-3))   # this line from :https://thiyanga.netlify.app/post/plotaxis/

# Heatmaps

ggplot(covid_2022, aes(x = factor(month), y = factor(year), fill = cases)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "pink", labels = scales::comma_format()) + #https://rstudio-pubs-static.s3.amazonaws.com/224435_1a0eb58a057a4c69b1fe1eb21553ecef.html
  labs(x = "Month", y = "Year", title = "Cases by Month and Year")

# Line Plot
library(ggplot2)

ggplot(covid_2022, aes(x = as.Date(dateRep), y = cases, group = countryterritoryCode, color = countryterritoryCode)) +
  geom_line() +
  labs(title = "Cases Over Time",
       x = "Date",
       y = "Cases")+
scale_y_continuous(labels = scales::comma_format())# this line from :https://thiyanga.netlify.app/post/plotaxis/

#Question E
# 
ggplot(covid_2022, aes(x = day, y = cases)) +
  geom_point() +
  labs(title = "COVID-19 Cases by Day of the Month",
       x = "Day",
       y = "Cases") +
  scale_y_continuous(labels = scales::comma_format()) # this line from :https://thiyanga.netlify.app/post/plotaxis/

#G- question PCA
columns_num <-covid_2022[, c(2, 3, 4, 5, 6, 10)]
data_clean<- na.omit(columns_num)

covid_2022.pca <- prcomp(data_clean, center = TRUE, scale. = TRUE)

summary(covid_2022.pca) 

#F from :tutorial 5 by lecturer Muhmmad
head(covid_2022)

covid_2022$Year_2020<- ifelse(covid_2022$year == '2020', 1, 0)
covid_2022$Year_2021 <- ifelse(covid_2022$year == '2021', 1, 0)
covid_2022$Year_2022 <- ifelse(covid_2022$year == '2022', 1, 0)
print(covid_2022)
View(covid_2022)
