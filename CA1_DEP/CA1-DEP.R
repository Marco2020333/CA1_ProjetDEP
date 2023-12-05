# Load necessary libraries

install.packages(c("tidyverse","dummy","caTools","conflicted"))
install.packages("skimr")
install.packages("caret")


library(conflicted)
library(tidyverse)
library(dummy)
library(caTools)
library(skimr)
library(dplyr)
library(caret)
library(ggplot2)

covid_2022 <-read_csv(file="covid_2022.csv")
view(covid_2022)

#A
skim_report <- skim(covid_2022)

# Print the summary
print(skim_report)
# Load the dplyr library

# Categorical Variables
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
# Discrete Variables
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


#Continuous Variables
ggplot(covid_2022, aes(x = popData2020)) +
  geom_density() +
  labs(title = "Density Plot of Population Data") +
  scale_x_continuous(labels = scales::comma) 

# Convert dateRep to Date type
covid_2022$dateRep <- as.Date(covid_2022$dateRep, format = "%d/%m/%Y")

# Create a variable 'count' with a constant value of 1 for each date
covid_2022$Count <- 1

# Plot the bar chart
ggplot(covid_2022, aes(x = dateRep)) +
  geom_bar(stat = "count") +
  labs(title = "Bar Chart of DateRep")

#Question B
# Glimpse at the dataset
glimpse(covid_2022)

#
convert_date$dateRep <- as.Date(covid_2022$dateRep, format = "%d/%m/%Y")
 ggplot(convert_date, aes(x = dateRep)) +
   geom_line() +
   labs(title = "Time Series Plot of DateRep")
summary(covid_2022)

#Question C
preprocess_num <-preProcess(covid_2022[, c(2, 3,4,5,6,10)], method=c('center','scale'))

#
data_Process <-predict(preprocess_num,covid_2022[, c(2, 3,4,5,6,10)])

#
summary(data_Process)

#Question D
# Scatter plot
ggplot(covid_2022, aes(x = cases, y = deaths)) +
  geom_point() +
  labs(x = "Cases", y = "Deaths", title = "Scatter Plot of Cases vs Deaths")


# Heatmaps
library(ggplot2)
ggplot(covid_2022, aes(x = factor(month), y = factor(year), fill = cases)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "pink") +
  labs(x = "Month", y = "Year", title = "Heatmap of Cases by Month and Year")


# Line Plot
library(ggplot2)

ggplot(covid_2022, aes(x = as.Date(dateRep), y = cases, group = countryterritoryCode, color = countryterritoryCode)) +
  geom_line() +
  labs(title = "Cases Over Time",
       x = "Date",
       y = "Cases")

#G- question
columns_num <-covid_2022[, c(2, 3, 4, 5, 6, 10)]
data_clean<- na.omit(columns_num)

covid_2022.pca <- prcomp(data_clean, center = TRUE, scale. = TRUE)

summary(covid_2022.pca) 

#F from :tutorial 5 by lecture Muhmmad
head(covid_2022)

covid_2022$Year_2020<- ifelse(covid_2022$year == '2020', 1, 0)
covid_2022$Year_2021 <- ifelse(covid_2022$year == '2021', 1, 0)
covid_2022$Year_2022 <- ifelse(covid_2022$year == '2022', 1, 0)
print(covid_2022)
view(covid_2022)
