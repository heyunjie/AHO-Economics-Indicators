library("readxl")
library(ggplot2)
Growth_of_output <- read_excel("4.1_Growth_of_output.xlsx")
Structure_of_output <- read_excel("4.2_Structure_of_output.xlsx")
Structure_of_manufacturing <- read_excel("4.3_Structure_of_manufacturing.xlsx")
Structure_of_merchandise_exports <- read_excel("4.4_Structure_of_merchandise_exports.xlsx")
Structure_of_merchandise_imports <- read_excel("4.5_Structure_of_merchandise_imports.xlsx")
Structure_of_service_exports <- read_excel("4.6_Structure_of_service_exports.xlsx")
Structure_of_service_imports <- read_excel("4.7_Structure_of_service_imports.xlsx")
Structure_of_demand <- read_excel("4.8_Structure_of_demand.xlsx")
Growth_of_consumption_investment_and_trade <- read_excel("4.9_Growth_of_consumption_investment_and_trade.xlsx")
Towards_a_broader_measure_of_national_income <- read_excel("4.10_Towards_a_broader_measure_of_national_income.xlsx")
Towards_a_broader_measure_of_savings <- read_excel("4.11_Towards_a_broader_measure_of_savings.xlsx")
Central_government_finances <- read_excel("4.12_Central_government_finances.xlsx")
Central_government_expenditure <- read_excel("4.13_Central_government_expenditure.xlsx")
Central_government_revenues<- read_excel("4.14_Central_government_revenues.xlsx")
Monetary_indicators<- read_excel("4.15_Monetary_indicators.xlsx")
Exchange_rates_and_prices_<- read_excel("4.16_Exchange_rates_and_prices_.xlsx")
Balance_of_payments_current_account<- read_excel("4.17_Balance_of_payments_current_account.xlsx")


Data_raw <- list("Growth_of_output"=Growth_of_output,"Structure_of_output"=Structure_of_output,"Structure_of_manufacturing"=Structure_of_manufacturing,"Structure_of_merchandise_exports"=Structure_of_merchandise_exports,
             "Structure_of_merchandise_imports"=Structure_of_merchandise_imports,"Structure_of_service_exports "=Structure_of_service_exports,"Structure_of_service_imports "=Structure_of_service_imports,
             "Structure_of_demand"=Structure_of_demand,"Growth_of_consumption_investment_and_trade"=Growth_of_consumption_investment_and_trade,"Towards_a_broader_measure_of_national_income "=Towards_a_broader_measure_of_national_income,
             "Towards_a_broader_measure_of_savings"=Towards_a_broader_measure_of_savings ,"Central_government_finances"=Central_government_finances,"Central_government_expenditure"=Central_government_expenditure,
             "Central_government_revenues"=Central_government_revenues,"Monetary_indicators"=Monetary_indicators,"Exchange_rates_and_prices_"=Exchange_rates_and_prices_,"Balance_of_payments_current_account"=Balance_of_payments_current_account)


############################## Growth_of_output
Growth_of_output_dataset <- Data_raw[[1]][4:69,]
View(Growth_of_output_dataset)
colnames(Growth_of_output_dataset) <- c("Country","Gross domestic product1","Gross domestic product2", "Agriculture1", "Agriculture2", "Industry1", "Industry2","Manufacturing1","Manufacturing2", "Services1", "Services2")
Growth_of_output_dataset <- as.data.frame(Growth_of_output_dataset)

for(i in 2:ncol(Growth_of_output_dataset)){
    
    for(j in 1:nrow(Growth_of_output_dataset)){
        
        if(Growth_of_output_dataset[j,i] == '..'| is.na(Growth_of_output_dataset[j,i])){
            Growth_of_output_dataset[j,i] <- NA
        }
        
    }
}

#obtain train&test datasets
Growth_of_output_dataset_train <- Growth_of_output_dataset[,c(1,2,4,6,8,10)]
Growth_of_output_dataset_test <- Growth_of_output_dataset[,c(1,3,5,7,9,11)]

Model <- lm(`Gross domestic product1`~Agriculture1+Industry1+Services1+Manufacturing1,data = Growth_of_output_dataset_train[1:55,])
summary(Model)


#################### structure of output
Structure_of_output_clean <- Data_raw[[2]][4:69,]
View(Growth_of_output_dataset)
colnames(Structure_of_output_clean) <- c("Country","Gross domestic product2005","Gross domestic product2016", "Agriculture2005", "Agriculture2016", "Industry2005", "Industry2016","Manufacturing2005","Manufacturing2016", "Services2005", "Services2016")
Structure_of_output_clean <- as.data.frame(Structure_of_output_clean)

for(i in 2:ncol(Structure_of_output_clean)){
    
    for(j in 1:nrow(Structure_of_output_clean)){
        
        if(Structure_of_output_clean[j,i] == '..'| is.na(Structure_of_output_clean[j,i])){
            Structure_of_output_clean[j,i] <- NA
        }
        
    }
}

###################### manufacturing
manufacturing <- Data_raw[[3]][4:69,]
colnames(manufacturing) <- c("Country", "Manufacturing value added 2000", "Manufacturing value added 2015","Food, beverages and tobacco 2000","Food, beverages and tobacco 2012",
                             "Textiles and clothing 2000","Textiles and clothing 2012","Machinery and transport equipment 2000","Machinery and transport equipment 2012","Chemicals 2000","Chemicals 2012",
                             "Other manufacturing 2000","Other manufacturing 2012")

for(i in 2:ncol(manufacturing)){
    
    for(j in 1:nrow(manufacturing)){
        
        if(manufacturing[j,i] == '..'| is.na(manufacturing[j,i])){
            manufacturing[j,i] <- NA
        }
        
    }
}

################### merchaindise exports
Merchaindise_exports <- Data_raw[[4]][4:68,]
colnames(Merchaindise_exports) <- c("Country", "Merchandise exports2005", "Merchandise exports2016","Food2005","Food2016",
                             "Agricultural raw materials2005","Agricultural raw materials2016", "Fuels2005", "Fuels2016", "Ores and metals2005" , "Ores and metals2016" ,
                             "Manufactures2005" , "Manufactures2016" )

for(i in 2:ncol(Merchaindise_exports)){
    
    for(j in 1:nrow(Merchaindise_exports)){
        
        if(Merchaindise_exports[j,i] == '..'| is.na(Merchaindise_exports[j,i])){
            Merchaindise_exports[j,i] <- NA
        }
        
    }
}


################## merchaindise imports
Merchaindise_imports <- Data_raw[[5]][4:69,]
colnames(Merchaindise_imports) <- c("Country",  "Merchandise imports2005",  "Merchandise imports2016","Food2005","Food2016",
                                    "Agricultural raw materials2005","Agricultural raw materials2016", "Fuels2005", "Fuels2016", "Ores and metals2005" , "Ores and metals2016" ,
                                    "Manufactures2005" , "Manufactures2016" )

for(i in 2:ncol(Merchaindise_imports)){
    
    for(j in 1:nrow(Merchaindise_imports)){
        
        if(Merchaindise_imports[j,i] == '..'| is.na(Merchaindise_imports[j,i])){
            Merchaindise_imports[j,i] <- NA
        }
        
    }
}


################## service exports
Service_exports <- Data_raw[[6]][4:68,]
colnames(Service_exports) <- c("Country",  "Commercial service exports2005"  ,  "Commercial service exports2016","Transport2005","Transport2016",
                                    "Travel2005","Travel2016", "Insurance and financial services2005", "Insurance and financial services2016", "Computer, information, communications, and other commercial services2005" , "Computer, information, communications, and other commercial services2016")

for(i in 2:ncol(Service_exports)){
    
    for(j in 1:nrow(Service_exports)){
        
        if(Service_exports[j,i] == '..'| is.na(Service_exports[j,i])){
            Service_exports[j,i] <- NA
        }
        
    }
}

################## service imports
Service_imports <- Data_raw[[7]][4:68,]
colnames(Service_imports) <- c("Country",  "Commercial service imports2005"  ,  "Commercial service imports2016","Transport2005","Transport2016",
                               "Travel2005","Travel2016", "Insurance and financial services2005", "Insurance and financial services2016", "Computer, information, communications, and other commercial services2005" , "Computer, information, communications, and other commercial services2016")

for(i in 2:ncol(Service_imports)){
    
    for(j in 1:nrow(Service_imports)){
        
        if(Service_imports[j,i] == '..'| is.na(Service_imports[j,i])){
            Service_imports[j,i] <- NA
        }
        
    }
}

################# Demand
Demand <- Data_raw[[8]][4:69,]
colnames(Demand) <- c("Country",  "Household final consumption expenditure2005"  ,  "Household final consumption expenditure2016","General government final consumption expenditure2005","General government final consumption expenditure2016",
                               "Gross capital formation2005","Gross capital formation2016", "Exports of goods and services2005", "Exports of goods and services2016", "Imports of goods and services2005" , "Imports of goods and services2016", "Gross savings2005", "Gross savings2016"    )

for(i in 2:ncol(Demand)){
    
    for(j in 1:nrow(Demand)){
        
        if(Demand[j,i] == '..'| is.na(Demand[j,i])){
            Demand[j,i] <- NA
        }
        
    }
}

################# Growth_consumption
Growth_consumption <- Data_raw[[9]][1:69,]

for(i in 2:ncol(Growth_consumption)){
    
    for(j in 1:nrow(Growth_consumption)){
        
        if(Growth_consumption[j,i] == '..'| is.na(Growth_consumption[j,i])){
            Growth_consumption[j,i] <- NA
        }
        
    }
}


################ National income
National_income <- Data_raw[[10]][4:69,]

for(i in 2:ncol(National_income)){
    
    for(j in 1:nrow(National_income)){
        
        if(National_income[j,i] == '..'| is.na(National_income[j,i])){
            National_income[j,i] <- NA
        }
        
    }
}

colnames(National_income) <- c("Country","Gross domestic product 2015","Gross domestic product(annual growth rate) 2000-2015", "Gross national income 2015", "Gross national income(annual growth rate) 2000-2015",
                               "Consumption of fixed capital %GNI 2015","Natural resource depletion %GNI 2015","Adjusted net national income 2015","Adjusted net national income(annual growth rate) 2000-2015")


############### Savings
Savings <- Data_raw[[11]][4:69,]
for(i in 2:ncol(Savings)){
    
    for(j in 1:nrow(Savings)){
        
        if(Savings[j,i] == '..'| is.na(Savings[j,i])){
            Savings[j,i] <- NA
        }
        
    }
}


############ government_finances
government_finances <- Data_raw[[12]][4:69,]

for(i in 2:ncol(government_finances)){
    
    for(j in 1:nrow(government_finances)){
        
        if(government_finances[j,i] == '..'| is.na(government_finances[j,i])){
            government_finances[j,i] <- NA
        }
        
    }
}

colnames(government_finances) <- c("Country", "Revenue2000", "Revenue2014", "Expense2000", "Expense2014", "Net investment in nonfinancial assets2000", "Net investment in nonfinancial assets2014",
                                   "Net lending (+) / net borrowing (-)2000", "Net lending (+) / net borrowing (-)2014","Net acquisition of financial assets2000","Net acquisition of financial assets2014",
                                   "Net incurrence of liabilities2000", "Net incurrence of liabilities2014","Debt and interest payments2000","Debt and interest payments2014")


########### government expenditure
government_expenditure <- Data_raw[[13]][4:68,]

for(i in 2:ncol(government_expenditure)){
    
    for(j in 1:nrow(government_expenditure)){
        
        if(government_expenditure[j,i] == '..'| is.na(government_expenditure[j,i])){
            government_expenditure[j,i] <- NA
        }
        
    }
}

colnames(government_expenditure) <- c("Country", "Goods and services2000", "Goods and services2014", "Compensation of employees2000", "Compensation of employees2014", "Interest payments2000", "Interest payments2014",
                                   "Subsidies and other transfers2000", "Subsidies and other transfers2014","Other expense2000","Other expense2014")


########### government revenue
government_revenue <- Data_raw[[14]][4:69,]

for(i in 2:ncol(government_revenue)){
    
    for(j in 1:nrow(government_revenue)){
        
        if(government_revenue[j,i] == '..'| is.na(government_revenue[j,i])){
            government_revenue[j,i] <- NA
        }
        
    }
}

colnames(government_revenue) <- c("Country", "Taxes on income, profits and capital gains2000", "Taxes on income, profits and capital gains2014", "Taxes on goods and services2000", "Taxes on goods and services2014", "Taxes on international trade2000", "Taxes on international trade2014",
                                      "Other taxes2000", "Other taxes2014","Social contributions2000","Social contributions2014","Grants and other revenue2000","Grants and other revenue2014")



############## Monetary indicators
monetary_indicators <- Data_raw[[15]][4:57,]

for(i in 2:ncol(monetary_indicators)){
    
    for(j in 1:nrow(monetary_indicators)){
        
        if(monetary_indicators[j,i] == '..'| is.na(monetary_indicators[j,i])){
            monetary_indicators[j,i] <- NA
        }
        
    }
}

colnames(monetary_indicators) <- c("Country", "Broad money2005","Broad money2016","Claims on domestic economy2005","Claims on domestic economy2016",
                                   "Claims on central governments2005", "Claims on central governments2016","Interest rate(Deposit)2005","Interest rate(Deposit)2016",
                                   "Interest rate(Lending)2005","Interest rate(Lending)2016","Interest rate(real)2005","Interest rate(real)2016")


################ Exchange rates and prices
Exchanges_rates_prices <- Data_raw[[16]][4:58,]

for(i in 2:ncol(Exchanges_rates_prices)){
    
    for(j in 1:nrow(Exchanges_rates_prices)){
        
        if(Exchanges_rates_prices[j,i] == '..'| is.na(Exchanges_rates_prices[j,i])){
            Exchanges_rates_prices[j,i] <- NA
        }
        
    }
}

colnames(Exchanges_rates_prices) <- c("Country","local currency units to $ 2015","local currency units to $ 2016",
                                      "local currency units to international $ 2015","local currency units to international $ 2016",
                                      "Ratio of PPP conversion factor to market exchange rate 2015","Ratio of PPP conversion factor to market exchange rate 2016",
                                      "Real effective exchange rate 2015","Real effective exchange rate 2016","GDP implicit deflator 2014-15","GDP implicit deflator 2015-16",
                                      "Consumer price index 2014-15" ,"Consumer price index 2015-16","Wholesale price index 2014-15","Wholesale price index 2015-16")



################ Balance of payment current account
Balance <- Data_raw[[17]][4:70,]

for(i in 2:ncol(Balance)){
    
    for(j in 1:nrow(Balance)){
        
        if(Balance[j,i] == '..'| is.na(Balance[j,i])){
            Balance[j,i] <- NA
        }
        
    }
}

colnames(Balance) <- c("Country","Goods and services(Exports) 2005","Goods and services(Exports) 2016","Goods and services(Imports) 2005","Goods and services(Imports) 2016",
                       "Net primary income 2005", "Net primary income 2016","Net secondary income 2005","Net secondary income 2016", "Current account balance 2005", "Current account balance 2016",
                       "Total reserves 2005","Total reserves 2016")

########################################################## Data cleaned ############################################

Data_cleaned <- list(as.data.frame(Growth_of_output_dataset),as.data.frame(Structure_of_output_clean),
                     as.data.frame(manufacturing),as.data.frame(Merchaindise_exports),as.data.frame(Merchaindise_imports),as.data.frame(Service_exports),as.data.frame(Service_imports),
                     as.data.frame(Demand),as.data.frame(Growth_consumption),as.data.frame(National_income),as.data.frame(Savings),as.data.frame(government_finances),
                     as.data.frame(government_expenditure),as.data.frame(government_revenue),as.data.frame(monetary_indicators),
                     as.data.frame(Exchanges_rates_prices),as.data.frame(Balance))



##########################################################################################################
Model1 <-  lm(Data_cleaned[[1]][,2]~ as.numeric(Data_cleaned[[1]][,4]) + as.numeric(Data_cleaned[[1]][,6]) + as.numeric(Data_cleaned[[1]][,8]) + as.numeric(Data_cleaned[[1]][,10]))
summary(Model1)

## Recheck our idea by plots
boxplot(data.frame("Agriculture" = as.numeric(Data_cleaned[[2]][,4]),"Industry" = as.numeric(Data_cleaned[[2]][,6]),"manufacturing" = as.numeric(Data_cleaned[[2]][,8]),"services" = as.numeric(Data_cleaned[[2]][,10])), main = "GDP contributors in 2005", cex.axis = 0.7)
boxplot(data.frame("Agriculture" = as.numeric(Data_cleaned[[2]][,5]),"Industry" = as.numeric(Data_cleaned[[2]][,7]),"manufacturing" = as.numeric(Data_cleaned[[2]][,9]),"services" = as.numeric(Data_cleaned[[2]][,11])),main = "GDP contributors in 2016",cex.axis = 0.7 )

##########################################################################################################

boxplot(data.frame("Food, beverages and tobacco" = as.numeric(Data_cleaned[[3]][,4]),"Textiles and clothing" = as.numeric(Data_cleaned[[3]][,6]),"Machinery and transport equipment" = as.numeric(Data_cleaned[[3]][,8]),"Chemicals" = as.numeric(Data_cleaned[[3]][,10]),"Other manufacturing"=as.numeric(Data_cleaned[[3]][,12])), main = "manufacturing products in 2000", cex.axis = 0.5)
boxplot(data.frame("Food, beverages and tobacco" = as.numeric(Data_cleaned[[3]][,5]),"Textiles and clothing" = as.numeric(Data_cleaned[[3]][,7]),"Machinery and transport equipment" = as.numeric(Data_cleaned[[3]][,9]),"Chemicals" = as.numeric(Data_cleaned[[3]][,11]),"Other manufacturing"=as.numeric(Data_cleaned[[3]][,13])),main = "manufacturing products in 2015",cex.axis = 0.5)

##########################################################################################################
boxplot(data.frame("Food" = as.numeric(Data_cleaned[[4]][,4]),"Agricultural raw materials" = as.numeric(Data_cleaned[[4]][,6]),"Fuels" = as.numeric(Data_cleaned[[4]][,8]),"Ores and metals" = as.numeric(Data_cleaned[[4]][,10]),"Manufactures"=as.numeric(Data_cleaned[[4]][,12])), main = "Exports in 2005", cex.axis = 0.5)
boxplot(data.frame("Food" = as.numeric(Data_cleaned[[4]][,5]),"Agricultural raw materials" = as.numeric(Data_cleaned[[4]][,7]),"Fuels" = as.numeric(Data_cleaned[[4]][,9]),"Ores and metals" = as.numeric(Data_cleaned[[4]][,11]),"Manufactures"=as.numeric(Data_cleaned[[4]][,13])),main = "Exports in 2016",cex.axis = 0.5)

##########################################################################################################
boxplot(data.frame("Food" = as.numeric(Data_cleaned[[5]][,4]),"Agricultural raw materials" = as.numeric(Data_cleaned[[5]][,6]),"Fuels" = as.numeric(Data_cleaned[[5]][,8]),"Ores and metals" = as.numeric(Data_cleaned[[5]][,10]),"Manufactures"=as.numeric(Data_cleaned[[5]][,12])), main = "Imports in 2005", cex.axis = 0.5)
boxplot(data.frame("Food" = as.numeric(Data_cleaned[[5]][,5]),"Agricultural raw materials" = as.numeric(Data_cleaned[[5]][,7]),"Fuels" = as.numeric(Data_cleaned[[5]][,9]),"Ores and metals" = as.numeric(Data_cleaned[[5]][,11]),"Manufactures"=as.numeric(Data_cleaned[[5]][,13])),main = "Imports in 2016",cex.axis = 0.5)







