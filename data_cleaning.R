###################################
###DATA CLEANING: CONSUMPTION 68###
###Ananya Iyengar##################
###################################

#Setting Working Directory

setwd("C:/Ananya Iyengar/Delhi School of Economics/406_Applied Consumption Analysis/ACA Term Paper")

################################################################################

#Loading Packages

library(tidyr) 
library(data.table)
library(dplyr)

################################################################################

#Loading Data

head(consumption68)
cons <- consumption68

################################################################################

#Data Cleaning

#Selecting Relevant Variables

cons1 <- cons%>%dplyr::select("HHID", "Item_Code", "Sector.x", "Total_Consumption_Quantity", "Total_Consumption_Value", "Cooking_Code", "Lighting_Code", "Dwelling_unit_Code", "Regular_salary_earner", "Possess_ration_card", "type_of_ration_card", "MPCE_URP", "MPCE_MRP", "HH_Size", "HH_Type", "Religion", "Social_Group", "whether_Land_owned", "Land_total_possessed", "NSS.x", "NSC.x", "MLT.x", "Combined_multiplier.y.y", "Subsample_multiplier.y.y")

#Dividing the Data Frame into Subsections 

total_cons_qty <- cons1%>%dplyr::select("HHID", "Item_Code", "Total_Consumption_Quantity")
total_cons_value <- cons1%>%dplyr::select("HHID", "Item_Code", "Total_Consumption_Value")
covariates <- cons1%>%dplyr::select("HHID", "Cooking_Code", "Lighting_Code", "Dwelling_unit_Code", "Regular_salary_earner", "Possess_ration_card", "type_of_ration_card", "MPCE_URP", "MPCE_MRP", "HH_Size", "HH_Type", "Religion", "Social_Group", "whether_Land_owned", "Land_total_possessed", "NSS.x", "NSC.x", "MLT.x", "Combined_multiplier.y.y", "Subsample_multiplier.y.y" )

#Reshaping the Total Consumption Quantity File 

wide_total_cons_qty <- total_cons_qty%>%dcast(HHID ~ Item_Code, value.var = "Total_Consumption_Quantity")

#Reshaping the Total Consumption Value File

wide_total_cons_value <- total_cons_value%>%dcast(HHID ~ Item_Code, value.var = "Total_Consumption_Value")
