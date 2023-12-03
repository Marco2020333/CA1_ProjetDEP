# Load necessary libraries


library(tidyverse)


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

covid_2022 <-read_csv(file="covid_2022.csv")
view(covid_2022)

#
skim_report <- skim(covid_2022)

# Print the summary
print(skim_report)
# Load the dplyr library
library(dplyr)

# Glimpse at the dataset
glimpse(covid_2022)

#
summary(covid_2022)

#
preprocess_num <-preProcess(covid_2022[, c(2, 3,4,5,6,10)], method=c('center','scale'))

#
data_Process <-predict(preprocess_num,covid_2022[, c(2, 3,4,5,6,10)])

#
summary(data_Process)


