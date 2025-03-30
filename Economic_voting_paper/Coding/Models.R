#Main model

Final_Basic_Model <- lm(Data$Vote_Share ~ as.factor(Data$year) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$POxLog_Salary + Data$POxLog_People + Data$POxUnemployment + Data$Delta_Log.population_size - 1, weights = Data$population_size)

summary(Final_Basic_Model)

clustered_se_bm <- vcovHC(Delta_Squared_Weights, type = "HC1", cluster = ~ cluster_var)
coeftest(Delta_Squared_Weights, vcov = clustered_se_bm)


#Delta_Squared_Weights 

Delta_Squared_Weights <-lm(Data$Vote_Share ~ as.factor(Data$year) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + I(Data$Delta_Log_Average_Salary^2) +  I(Data$Delta_Unemployment_Rate^2) + I(Data$Delta_Log_Average_Salary^2 * Data$Rulling.Party_PO) + I(Data$Delta_Unemployment_Rate^2 * Data$Rulling.Party_PO) + Data$Delta_Log.population_size -1, weights = Data$population_size)

summary(Delta_Squared_Weights)

clustered_se_sw <- vcovHC(Delta_Squared_Weights, type = "HC1", cluster = ~ cluster_var)
coeftest(Delta_Squared_Weights, vcov = clustered_se_sw)

#Asymetric influence

Data <- Data[is.finite(Data$POxLog_Salary) & is.finite(Data$log_salary) & is.finite(Data$log_salary.T.minus.1), ]
Asymmetric_influence <- lm(Data$Vote_Share ~ as.factor(Data$year) + Data$Delta_Log_Average_Salary + Data$delta_unemployment_if_neg + Data$delta_unemployment_if_negxPiS + Data$delta_unemployment_if_posxPO + Data$Delta_Log.population_size - 1, weights = Data$population_size)

clustered_se_i <- vcovHC(Asymmetric_influence, type = "HC1", cluster = ~ cluster_var)
coeftest(Asymmetric_influence, vcov = clustered_se_i)