library(data.table)
library(dplyr)
library(lmtest)
library(sandwich)
library(ggplot2)
library(flextable)
library(panelView)
library(marginaleffects)
library(broom)
library(jtools)
library(openxlsx)
library(readxl)
library(sjPlot)
library(tidyr)
library(plm)


# This script processes and prepares panel data for a regression analysis 
# examining the impact of various factors on changes in political party support in Poland.
# 
# 1. Calculates vote shares for Law and Justice (PiS) and Civic Platform (PO).
# 2. Computes logarithmic transformations of key variables such as salary and population.
# 3. Creates lagged variables and computes period-to-period differences (delta).
# 4. Constructs interaction terms to analyze the effect of the ruling party on unemployment and salaries.
# 5. Includes squared terms (e.g., unemployment rate squared) to capture nonlinear effects.
# 6. Accounts for asymmetric effects of increasing and decreasing unemployment on party support.
# 
# These transformations allow for further econometric modeling and statistical analysis.


Panel <- final_merge_selected_clean
Panel$`Vote.share.PiS` <- Panel$`Prawo i Sprawiedliwosc` / Panel$`Valid ballot papers`
Panel$`Vote.share.PO` <- Panel$`Platforma Obywatelska` / Panel$`Valid ballot papers`
Panel$average_salary <- as.numeric(Panel$average_salary)
Panel$log_salary <- log(Panel$average_salary)
Panel$`Rulling.Party_PiS` <- ifelse(Panel$year %in% c(2005:2007, 2015:2023), '1', '0')

# Update after adding 2023 election data
Panel$`Rulling.Party_PO` <- ifelse(Panel$year %in% c(2007:2015, 2023), '1', '0')
#Panel$`Rulling.Party_PO` <- ifelse(Panel$year %in% 2007:2015, '1', '0')

# Update after adding 2023 election data
#Panel$`Rulling.Party` <- ifelse(Panel$year %in% 2007:2011, 'Platforma Obywatelska', 'Prawo i Sprawiedliwosc') 
Panel$`Rulling.Party` <- ifelse(Panel$year %in% c(2007:2011,2023), 'Platforma Obywatelska', 'Prawo i Sprawiedliwosc') 

Data <- Panel[, c("Code", "year", "log_salary", "average_salary", "Vote.share.PiS", "unemployment_rate", "Vote.share.PO", "population_size", "Rulling.Party", "Prawo i Sprawiedliwosc", "Platforma Obywatelska", "Rulling.Party_PiS", "Rulling.Party_PO", "Valid ballot papers")]

Data$`Rulling.Party_PiS` <- as.numeric(Data$`Rulling.Party_PiS`)
Data$`Rulling.Party_PO` <- as.numeric(Data$`Rulling.Party_PO`)

Data$`Prawo i Sprawiedliwosc` <- as.numeric(Data$`Prawo i Sprawiedliwosc`)

Data <- Data %>%
  mutate(`Prawo i Sprawiedliwosc.T.minus.1` = dplyr::lag(`Prawo i Sprawiedliwosc`, 1), .by = Code)
Data$`Delta_PiS` = Data$`Prawo i Sprawiedliwosc` - Data$`Prawo i Sprawiedliwosc.T.minus.1`

Data <- Data %>%
  mutate(`Platforma Obywatelska.T.minus.1` = dplyr::lag(`Platforma Obywatelska`, 1), .by = Code)
Data$`Delta_PO` <- Data$`Platforma Obywatelska` - Data$`Platforma Obywatelska.T.minus.1`

Data <- Data %>%
  mutate(`Platforma Obywatelska.T.minus.2` = dplyr::lag(`Platforma Obywatelska`, 2), .by = Code)

Data <- Data %>%
  mutate(`Vote.share.PiS.T.minus.1` = dplyr::lag(`Vote.share.PiS`, 1), .by = Code)
Data$`Delta_Vote_Share_PiS` <- Data$`Vote.share.PiS` - Data$`Vote.share.PiS.T.minus.1`

Data <- Data %>%
  mutate(`Vote.share.PO.T.minus.1` = dplyr::lag(`Vote.share.PO`, 1), .by = Code)
Data$`Delta_Vote_Share_PO` <- Data$`Vote.share.PO` - Data$`Vote.share.PO.T.minus.1`

Data <- Data %>%
  mutate(`unemployment_rate` = as.numeric(`unemployment_rate`),
         `unemployment_rate.T.minus.1` = as.numeric(dplyr::lag(`unemployment_rate`, 1)), 
         .by = Code)

Data$`Delta_Unemployment_Rate` <- Data$`unemployment_rate` - Data$`unemployment_rate.T.minus.1`

Data <- Data %>%
  mutate(`log_salary.T.minus.1` = dplyr::lag(`log_salary`, 1), .by = Code)
Data$`Delta_Log_Average_Salary` <- Data$`log_salary` - Data$`log_salary.T.minus.1`

Data <- Data %>%
  mutate(`Rulling.Party_PO.T.minus.2` = dplyr::lag(`Rulling.Party_PO`, 2), .by = Code)

Data <- Data %>%
  mutate(`Rulling.Party_PO.T.minus.1` = dplyr::lag(`Rulling.Party_PO`, 1), .by = Code)

Data$`Vote_Share` <- ifelse(Data$`Rulling.Party` == "Prawo i Sprawiedliwosc", Data$`Delta_Vote_Share_PiS`,
                            ifelse(Data$`Rulling.Party` == "Platforma Obywatelska", Data$`Delta_Vote_Share_PO`, NA))

Data$`Vote_Share` <- Data$`Vote_Share` * 100

Data$`population_size` <- as.numeric(Data$`population_size`)
Data$`Log.population_size` <- log(Data$`population_size`)


Data <- Data %>%
  mutate(`Log.population_size.T.minus.1` = dplyr::lag(`Log.population_size`, 1), .by = Code)
Data$`Delta_Log.population_size` <- Data$`Log.population_size` - Data$`Log.population_size.T.minus.1`




#Interactions

Data$POxLog_People <- I(Data$Log.population_size * Data$Rulling.Party_PO - Data$Log.population_size.T.minus.1 * Data$Rulling.Party_PO)

Data$POxLog_Salary <- I(Data$log_salary  * Data$Rulling.Party_PO - Data$log_salary.T.minus.1  * Data$Rulling.Party_PO)

Data$POxUnemployment <- I(Data$unemployment_rate  * Data$Rulling.Party_PO - Data$unemployment_rate.T.minus.1  * Data$Rulling.Party_PO)

SalaryxUnemployment <- I(Data$unemployment_rate * Data$log_salary - Data$unemployment_rate.T.minus.1 * Data$log_salary)


#Delta squared
Data$Unemployment_Rate_squared_T <- Data$unemployment_rate ^2

Data <- Data %>%
  mutate(Unemployment_Rate_squared_T.minus.1 = dplyr::lag(Unemployment_Rate_squared_T, 1), .by = Code)
Data$`Delta_Rate_squared` = Data$Unemployment_Rate_squared_T - Data$Unemployment_Rate_squared_T.minus.1


Data$log_salary_squared_T <- Data$unemployment_rate ^2
Data <- Data %>%
  mutate(log_salary_squared_T.minus.1 = dplyr::lag(log_salary_squared_T, 1), .by = Code)

Data$`Delta_log_salary_squared` = Data$log_salary_squared_T - Data$log_salary_squared_T.minus.1

#Asymmetric influence
Data$delta_unemployment_if_pos <- ifelse(Data$Delta_Unemployment_Rate >0, Data$Delta_Unemployment_Rate,0)
Data$delta_unemployment_if_neg <- ifelse(Data$Delta_Unemployment_Rate <0, Data$Delta_Unemployment_Rate,0)
Data$delta_unemployment_if_posxPO <- ifelse(Data$Delta_Unemployment_Rate >0, Data$Delta_Unemployment_Rate * Data$Rulling.Party_PO,0)
Data$delta_unemployment_if_negxPiS <- ifelse(Data$Delta_Unemployment_Rate <0, Data$Delta_Unemployment_Rate * (1- Data$Rulling.Party_PO),0)

