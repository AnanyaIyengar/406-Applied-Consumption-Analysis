###########################################################
###STEP 1: Data Extraction: Applied Consumption Analysis###
###########################################################

#############################################
###Ananya Iyengar, Rajsi Sah, Bhavya Palta###
#############################################

#Setting Working Directory

setwd("C:/Ananya Iyengar/Delhi School of Economics/406_Applied Consumption Analysis/ACA Term Paper")

######################################################################################################

#Loading Packages

library(readr) #to extract .txt files
library(dplyr) #for data manipulation
library(fastDummies) #to comvert categorical variables into dummies
library(haven)

######################################################################################################

#Data Extraction: 68th Round Consumer Expenditure Survey NSSO 

#Level One: Household Level

conslvl1 <- paste("Block_1_2_level_1_68.txt")

consumption_level1_68 <- read_fwf(file = conslvl1, 
                                  fwf_cols(centrecd = c(1,3), fsuslno = c(4,8),
                                           round = c(9,10), sch = c(11,13),
                                           sample = c(14,14), sector = c(15, 15),
                                           nssregion = c(16,18), district = c(19,20),
                                           stratum = c(21,22), substratum = c(23,24), 
                                           scheduletype = c(25,25), subround = c(26,26), 
                                           subsample = c(27,27), fodsubrg = c(28,31), hamletgroup = c(32,32),
                                           secondstgstrno = c(33,33), hh = c(34,35),
                                           level = c(36,37), filler = c(38,42),
                                           slnoinformant = c(43,44), responsecode = c(45,45),
                                           surveycode = c(46,46), substitutioncode = c(47,47),
                                           dateofsurvey = c(48,53), dateofdispatch = c(54,59), 
                                           canvasstime = c(60,62), remarks_1 = c(63,63), remarks_2 = c(64,64),
                                           remarks_elsewhere_1 = c(65,65), remarks_elsewhere_2 = c(66,66),
                                           special_characters = c(67,68), blank = c(69,126), nss= c(127,129), nsc = c(130,132),
                                           multiplier = c(133,142)), col_types = cols(centrecd = col_character(), fsuslno = col_character(),
                                                                                      round = col_character(), sch = col_character(),
                                                                                      sample = col_character(), sector = col_integer(),
                                                                                      nssregion = col_character(), district = col_character(),
                                                                                      stratum = col_character(), substratum = col_character(),
                                                                                      scheduletype = col_character(), subround = col_character(), 
                                                                                      subsample = col_character, fodsubrg = col_character(), hamletgroup = col_character(),
                                                                                      secondstgstrno = col_character(), hh = col_character(),
                                                                                      level = col_character(), filler = col_character(),
                                                                                      slnoinformant = col_character(), responsecode = col_character(),
                                                                                      surveycode = col_character(), substitutioncode = col_character(),
                                                                                      dateofsurvey = col_date(), dateofdispatch = col_date(),
                                                                                      canvasstime = col_character(), remarks_1 = col_character(), remarks_2 = col_character(),
                                                                                      remarks_elsewhere_1 = col_character(), remarks_elsewhere_2 = col_character(),
                                                                                      special_characters = col_character(), blank = col_character(), nss =  col_number(), 
                                                                                      nsc = col_number(), multiplier = col_number())))       
                               
                          

#Level Two: Household Level: Household Characteristics

conslvl2 <- paste("Block_3_level_2_68.txt")

consumption_level2_68 <- read_fwf(file = conslvl2,
                               fwf_cols(commonid = c(1,35), level = c(36,37),
                                        filler = c(38,42), hhsize = c(43,44), 
                                        nic_2008 = c(45,49), nco_2004 = c(50,52),
                                        hhtype = c(53,53), religion =c(54,54), 
                                        social_group = c(55,55), whether_owns_land = c(56,56),
                                        typeofland = c(57,57), land_owned = c(58,65),
                                        land_leased_in = c(66,73), otherwise_possessed = c(74,81),
                                        land_leased_out = c(82,89), land_total_possessed = c(90,97),
                                        cultivated_july10_june11 = c(98,105), irrigated_july10_june11 = c(106,113),
                                        special_characters = c(114.115), blank = c(116,126), nss = c(127, 129),
                                        nsc=c(130,132), mlt=c(133,142)),
                               col_types = cols(commonid = col_character(), level = col_character(),
                                                filler = col_character(), hhsize = col_character(), 
                                                nic_2008 = col_character(), nco_2004 = col_character(),
                                                hhtype = col_character(), religion =col_character(), 
                                                social_group = col_character(), whether_owns_land = col_character(),
                                                typeofland = col_character(), land_owned = col_character(),
                                                land_leased_in = col_character(), otherwise_possessed = col_character(),
                                                land_leased_out = col_character()), land_total_possessed = col_character(),
                               cultivated_july10_june11 = col_character(), irrigated_july10_june11 = col_character(),
                               special_characters = col_character(), blank = col_character(), nss = col_number(),
                               nsc=col_number(), mlt=col_number()))



#Level Three (FULL): Household Level: Household Characteristics 

conslvl3 <- paste("Block_4_level_3.txt") 

consumption_level_3_68 <- read_fwf(file=conslvl3,
                               fwf_cols(commonid = c(1,35), level = c(36,37),
                                        filler = c(38,42), cooking_code = c(43,44),
                                        lightning_code = c(45,45), dwelling_unit_code = c(46,46),
                                        regular_salary_earner = c(47,47), perform_ceremony = c(48,48),
                                        meals_served_nonhhdmembers = c(49,52), possess_ration_card = c(53,53),
                                        type_of_ration_card = c(54,54), mpce_urp = c(55,63), mpce_mrp = c(64,72),
                                        special_characters_okstamp = c(73,74), blank = c(75,126), nss=c(127,129),
                                        nsc= c(130,132), mlt = c(133,142)),
                               col_types = cols(commonid = col_character(), level = col_character(),
                                                filler = col_character(), cooking_code = col_character(),
                                                lightning_code = col_character(), dwelling_unit_code = col_character(),
                                                regular_salary_earner = col_character(), perform_ceremony = col_character(),
                                                meals_served_nonhhmembers = col_character(), possess_ration_card = col_character(),
                                                type_of_ration_card = col_character(), mpce_urp = col_character(), mpce_mrp = col_character(),
                                                special_characters_okstamp = col_character(), blank = col_character(), nss = col_number(),
                                                nsc = col_number(), mlt = col_number()))

#Level Four (FULL): Individual Level : Demographic and other particulars of Households

conslvl4 <- paste("Block_5_level_4.txt")

consumption_level4_68 <- read_fwf(file=conslvl4,
                               fwf_cols(commonid = c(1,35), level = c(36,37),
                                        filler = c(38,40), person_sl_no = c(41,42), 
                                        relation =c(43,43), sex=c(44,44), age=c(45,47),
                                        maritalstatus = c(48,48), education = c(49,50), 
                                        days_stayedaway = c(51,52), meals_perday = c(53,53),
                                        meals_school = c(54,55), meals_employer = c(56,57), 
                                        meals_others = c(58,59), meals_payment = c(60,61), 
                                        meals_athome = c(62,63), special_characters_okstamp = c(64,65),
                                        blank = c(66,126), nss=c(127,129),
                                        nsc= c(130,132), mlt = c(133,142)),
                               col_types = cols(commonid = col_character(), level = col_character(),
                                                filler = col_character(), person_sl_no = col_character(),
                                                relation = col_character(), sex = col_character(), age = col_character(),
                                                maritalstatus = col_character(), education = col_character(),
                                                days_stayedaway = col_character(), meals_perday = col_character(),
                                                meals_school = col_character(), meals_employer = col_character(),
                                               meals_other = col_character(), meals_payment = col_character(),
                                               meals_athome = col_character(), meals_payment = col_character(),
                                               meals_athome = col_character(), special_characters_okstamp = col_character(),
                                               blank = col_character(), nss = col_number(),
                                               nsc = col_number(), mlt = col_number()))


#Level Five (FULL) (Blocks 5.1,5.2 & 6): Consumption of food, pan, tobacco and intoxicants, & energy

conslvl5 <- paste("Level_5.txt")

consumption_level5_68 <- read_fwf(file=conslvl5,
                               fwf_cols(commonid = c(1,35), level = c(36,37),
                                        filler = c(38,39), item_code = c(40,42), home_produce_qty = c(43,51),
                                        home_produce_value = c(52,59), total_cons_qty = c(60,68), total_cons_value = c(69,76),
                                        source_code = c(77,77), special_characters_okstamp = c(78,79),
                                        blank = c(80,126), nss=c(127,129),
                                        nsc= c(130,132), mlt = c(133,142)),
                               col_types = cols(commonid = col_character(), level = col_character(),
                                                filler = col_character(), item_code = col_character(), home_produce_qty = col_character(),
                                                home_produce_value = col_character(), total_cons_qty = col_character(), total_cons_value = col_character(),
                                                source_code = col_character(), special_characters_okstamp = col_character(),
                                                blank = col_character(), nss = col_number(),
                                                nsc = col_number(), mlt = col_number())



#Level Six (FULL)(blocks 7 and 8): Consumption expenditure on clothing, bedding, etc & footwear

consnlvl6 <- paste("Level_6.txt")

consumption_level6_68 <- read_fwf(file=consnlvl6,
                               fwf_cols(commonid = c(1,35), level = c(36,37),
                                        filler = c(38,39), item_code = c(40,42),
                                        last_30days_qty = c(43,51), last_30days_value = c(52,59),
                                        last_365days_qty = c(60,68), last_365days_value = c(69,76),
                                        special_characters_okstamp = c(77,78),
                                        blank = c(79,126), nss=c(127,129),
                                        nsc= c(130,132), mlt = c(133,142)),
                               col_types = cols(commonid = col_character(), level = col_character(),
                                                filler = col_character(), item_code = col_character(),
                                                last_30days_qty = col_character(), last_30days_value = col_character(),
                                                last_365days_qty = col_character(), last_365days_value = col_character(),
                                                special_characters_okstamp = col_character(),
                                                blank = col_character(), nss = col_number(),
                                                nsc = col_number(), mlt = col_number()))


#Level Seven (block 9): Expenditure on education and medical (institutional) goods and services
consnlvl7 <- paste("Block_9_level_7.txt")

consumption_level7_68 <- read_fwf(file=consnlvl7,
                               fwf_cols(commonid = c(1,35), level = c(36,37),
                                        filler = c(38,39), item_code = c(40,42),
                                        last_30days_value = c(43,50),
                                        last_365days_value = c(51,58),
                                        special_characters_okstamp = c(59,60),
                                        blank = c(61,126), nss=c(127,129),
                                        nsc= c(130,132), mlt = c(133,142)),
                               col_types = cols(commonid = col_character(), level = col_character(),
                                                filler = col_character(), item_code = col_character(),
                                                last_30days_value = col_character(),
                                                last_365days_value = col_character(),
                                                special_characters_okstamp = col_character(),
                                                blank = col_character(), nss = col_number(),
                                                nsc = col_number(), mlt = col_number))

#Level eight (block 10): Expenditure on miscellaneous goods and services

consnlvl8 <- paste("Level_8.txt")

consumption_level8_68 <- read_fwf(file=consnlvl8,
                                  fwf_cols(commonid = c(1,35), level = c(36,37),
                                           filler = c(38,39), item_code = c(40,42),
                                           value = c(43,50),
                                           special_characters_okstamp = c(51,52),
                                           blank = c(53,126), nss=c(127,129),
                                           nsc= c(130,132), mlt = c(133,142)),
                                  col_types = cols(commonid = col_character(), level = col_character(),
                                                   filler = col_character(), item_code = col_character(),
                                                   value = col_character(),
                                                   special_characters_okstamp = col_character(),
                                                   blank = col_character(), nss = col_number(),
                                                   nsc = col_number(), mlt = col_number()))

#Level nine (block 11): Expenditure for purchase and construction of durable goods for domestic use
  
consnlvl9 <- paste("Level_9.txt")

consumption_level9_68 <- read_fwf(file=consnlvl9,
                                  fwf_cols(commonid = c(1,35), level = c(36,37),
                                           filler = c(38,39), item_code = c(40,42),
                                           whether_possesses = c(43,43), first_hand_purchase_number_30days = c(44,46),
                                           whether_hire_puchased_30days = c(47,47), first_hand_purchase_value_30days = c(48,55),
                                           Cost_raw_material_service_and_repair_30days = c(56,63), second_hand_purchase_value_30days = c(64,71),
                                           total_expenditure_30days = c(72,79), first_hand_purchase_number_365days = c(80,82),
                                           whether_hire_puchased_365days = c(83,83), first_hand_purchase_value_365days = c(84,91),
                                           Cost_raw_material_service_and_repair_365days = c(92,99), second_hand_purchase_number_365days = c(100,102),
                                           second_hand_purchase_value_365days = c(103,110),
                                           total_expenditure_365days = c(111,118),special_characters_okstamp = c(119,120),
                                           blank = c(121,126), nss=c(127,129),
                                           nsc= c(130,132), mlt = c(133,142)),
                                  col_types = cols(commonid = col_character(), level = col_character(),
                                                   filler = col_character(), item_code = col_character(),
                                                   whether_possesses = col_character(), first_hand_purchase_number_30days = col_character(),
                                                   whether_hire_puchased_30days = col_character(), first_hand_purchase_value_30days = col_character(),
                                                   Cost_raw_material_service_and_repair_30days = col_character(), second_hand_purchase_value_30days = col_character(),
                                                   total_expenditure_30days = col_character(), first_hand_purchase_number_365days = col_character(),
                                                   whether_hire_puchased_365days = col_character(), first_hand_purchase_value_365days = col_character(),
                                                   Cost_raw_material_service_and_repair_365days = col_character(), second_hand_purchase_number_365days = col_character(),
                                                   second_hand_purchase_value_365days = col_character(),
                                                   total_expenditure_365days = col_character(),special_characters_okstamp = col_character(),
                                                   blank = col_character(), nss=col_number(),
                                                   nsc = col_number(), mlt = col_number()))

#Level ten (block 13): Information on AYUSH 

consnlvl10 <- paste("Level_10.txt")

consumption_level10_68 <- read_fwf(file=consnlvl10,
                                   fwf_cols(commonid = c(1,35), level = c(36,37),
                                            filler = c(38,42), Whether_any_member_used_AYUSH = c(43,43),
                                            If_no_in_item_1_most_important_reason = c(44,44), If_yes_in_item_1_most_important_reason = c(45,45),
                                            System_of_medicines_used_Indian_system = c(46,46), System_of_medicines_used_Homoeopathy = c(47,47),
                                            System_of_medicines_used_Yoga_and_Naturopathy = c(48,48), Where_did_you_usually_get_Indian_system_of_medicines = c(49,49), 
                                            Where_did_you_usually_get_Homoeopathic_medicines = c(50,50), 
                                            How_often_do_you_visit_AYUSH_hospital = c(51,51), How_often_during_your_visits_did_you_find_doctors = c(52,52), 
                                            what_is_your_assessment_about_the_AYUSH_medicines_from_hospital_on_availability = c(53,53), 
                                            What_is_your_assessment_about_the_AYUSH_medicines_from_hospital_on_effectiveness = c(54,54),
                                            Who_advised_to_take_Indian_system_of_medicine = c(55,55), 
                                            Who_advised_to_take_Homoeopathy = c(56,56), Who_advised_to_take_Yoga_and_Naturopathy = c(57,57), 
                                            special_characters_okstamp = c(58,59),
                                            blank = c(60,126), nss=c(127,129),
                                            nsc= c(130,132), mlt = c(133,142)),
                                   col_types = cols(commonid = col_character(), level = col_character(),
                                                    filler = col_character(), Whether_any_member_used_AYUSH = col_character(),
                                                    If_no_in_item_1_most_important_reason = col_character(), If_yes_in_item_1_most_important_reason = col_character(),
                                                    System_of_medicines_used_Indian_system = col_character(), System_of_medicines_used_Homoeopathy = col_character(),
                                                    System_of_medicines_used_Yoga_and_Naturopathy = col_character(), Where_did_you_usually_get_Indian_system_of_medicines = col_character(), 
                                                    Where_did_you_usually_get_Homoeopathic_medicines = col_character(), 
                                                    How_often_do_you_visit_AYUSH_hospital = col_character(), How_often_during_your_visits_did_you_find_doctors = col_character(), 
                                                    what_is_your_assessment_about_the_AYUSH_medicines_from_hospital_on_availability = col_character(), 
                                                    What_is_your_assessment_about_the_AYUSH_medicines_from_hospital_on_effectiveness = col_character(),
                                                    Who_advised_to_take_Indian_system_of_medicine = col_character(), 
                                                    Who_advised_to_take_Homoeopathy = col_character(), Who_advised_to_take_Yoga_and_Naturopathy = col_character(), 
                                                    special_characters_okstamp = col_character(),
                                                    blank = col_character(), nss= col_number(),
                                                    nsc= col_number(), mlt = col_number())
  
#Level eleven (block 12): Summary of consumer expediture

consnlvl11 <- paste("Level_11.txt")

consumption_level11_68 <- read_fwf(file=consnlvl11,
                                   fwf_cols(commonid = c(1,35), level = c(36,37),
                                            filler = c(38,40), srl_no = c(41,42), 
                                            value = c(43,52), special_characters_okstamp = c(53,54),
                                            blank = c(55,126), nss=c(127,129),
                                            nsc= c(130,132), mlt = c(133,142)),
                                   col_types = cols(commonid = col_character(), level = col_character(),
                                                    filler = col_character(), srl_no =col_character(), 
                                                    value = col_character(), special_characters_okstamp = col_character(),
                                                    blank = col_character(), nss=col_character(),
                                                    nsc= col_character(), mlt = col_character()))


#Creating the Primary Key 
  
consumption_level1_68$primarykey <- paste0(consumption_level1_68$fsuslno, consumption_level1_68$hamletgroup, consumption_level1_68$secondstgstrno, consumption_level1_68$hh)


consumption_level2_68$fsuslno <- substr(consumption_level2_68$commonid, 4, 8)
consumption_level2_68$hamletgroup <- substr(consumption_level2_68$commonid, 32, 32)
consumption_level2_68$secondstgstrno <- substr(consumption_level2_68$commonid, 33, 33)
consumption_level2_68$hh <- substr(consumption_level2_68$commonid, 34, 35)
consumption_level2_68$primarykey <- paste0(consumption_level2_68$fsuslno, consumption_level2_68$hamletgroup, consumption_level2_68$secondstgstrno, consumption_level2_68$hh)


consumption_level_3_68$fsuslno <- substr(consumption_level_3_68$commonid, 4, 8)
consumption_level_3_68$secondstgstrno <- substr(consumption_level_3_68$commonid, 33, 33)
consumption_level_3_68$hh <- substr(consumption_level_3_68$commonid, 34, 35)
consumption_level_3_68$primarykey <- paste0(consumption_level_3_68$fsuslno, consumption_level_3_68$secondstgstrno, consumption_level_3_68$hh)


consumption_level4_68$fsuslno <- substr(consumption_level4_68$commonid, 4, 8)
consumption_level4_68$secondstgstrno <- substr(consumption_level4_68$commonid, 33, 33)
consumption_level4_68$hh <- substr(consumption_level4_68$commonid, 34, 35)
consumption_level4_68$primarykey <- paste0(consumption_level4_68$fsuslno, consumption_level4_68$secondstgstrno, consumption_level4_68$hh)

consumption_level5_68$fsuslno <- substr(consumption_level5_68$commonid, 4, 8)
consumption_level5_68$secondstgstrno <- substr(consumption_level5_68$commonid, 33, 33)
consumption_level5_68$hh <- substr(consumption_level5_68$commonid, 34, 35)
consumption_level5_68$primarykey <- paste0(consumption_level5_68$fsuslno, consumption_level5_68$secondstgstrno, consumption_level5_68$hh)

consumption_level6_68$fsuslno <- substr(consumption_level6_68$commonid, 4, 8)
consumption_level6_68$secondstgstrno <- substr(consumption_level6_68$commonid, 33, 33)
consumption_level6_68$hh <- substr(consumption_level6_68$commonid, 34, 35)
consumption_level6_68$primarykey <- paste0(consumption_level6_68$fsuslno, consumption_level6_68$secondstgstrno, consumption_level6_68$hh)

consumption_level7_68$fsuslno <- substr(consumption_level7_68$commonid, 4, 8)
consumption_level7_68$secondstgstrno <- substr(consumption_level7_68$commonid, 33, 33)
consumption_level7_68$hh <- substr(consumption_level7_68$commonid, 34, 35)
consumption_level7_68$primarykey <- paste0(consumption_level7_68$fsuslno, consumption_level7_68$secondstgstrno, consumption_level7_68$hh)


consumption_level8_68$fsuslno <- substr(consumption_level8_68$commonid, 4, 8)
consumption_level8_68$secondstgstrno <- substr(consumption_level8_68$commonid, 33, 33)
consumption_level8_68$hh <- substr(consumption_level8_68$commonid, 34, 35)
consumption_level8_68$primarykey <- paste0(consumption_level8_68$fsuslno, consumption_level8_68$secondstgstrno, consumption_level8_68$hh)

consumption_level9_68$fsuslno <- substr(consumption_level9_68$commonid, 4, 8)
consumption_level9_68$secondstgstrno <- substr(consumption_level9_68$commonid, 33, 33)
consumption_level9_68$hh <- substr(consumption_level9_68$commonid, 34, 35)
consumption_level9_68$primarykey <- paste0(consumption_level9_68$fsuslno, consumption_level9_68$secondstgstrno, consumption_level9_68$hh)


consumption_level10_68$fsuslno <- substr(consumption_level10_68$commonid, 4, 8)
consumption_level10_68$secondstgstrno <- substr(consumption_level10_68$commonid, 33, 33)
consumption_level10_68$hh <- substr(consumption_level10_68$commonid, 34, 35)
consumption_level10_68$primarykey <- paste0(consumption_level10_68$fsuslno, consumption_level10_68$secondstgstrno, consumption_level10_68$hh)


consumption_level11_68$fsuslno <- substr(consumption_level11_68$commonid, 4, 8)
consumption_level11_68$secondstgstrno <- substr(consumption_level11_68$commonid, 33, 33)
consumption_level11_68$hh <- substr(consumption_level11_68$commonid, 34, 35)
consumption_level11_68$primarykey <- paste0(consumption_level11_68$fsuslno, consumption_level11_68$secondstgstrno, consumption_level11_68$hh)

#Merging Data

con12 <- inner_join(consumption_level1_68, consumption_level2_68, by = "primarykey")
con123 <- inner_join(con12, consumption_level_3_68, by = "primarykey")
con1235 <- inner_join(con123, consumption_level5_68, by = "primarykey")
con12356 <- inner_join(con1235, consumption_level6_68, by = "primarykey")
con123567 <- inner_join(con12356, consumption_level7_68, by = "primarykey")
con1235678 <- inner_join(con123567, consumption_level8_68, by = "primarykey")
con12356789 <- inner_join(con1235678, consumption_level9_68, by = "primarykey")
con1235678910 <- inner_join(con12356789, consumption_level10_68, by = "primarykey")


### NSSO Round 64 ###

##Importing the extracted dta files 

consumption_level1_64 <- read_dta("level1.dta")
consumption_level2_64 <- read_dta("level2.dta")
consumption_level3_64 <- read_dta("level3.dta")
consumption_level4_64 <- read_dta("level4.dta")
consumption_level5_64 <- read_dta("level5.dta")
consumption_level6_64 <- read_dta("level6.dta")
consumption_level7_64 <- read_dta("level7.dta")
consumption_level_summ_64 <- read_dta("summary.dta")

## Creating the primary key for merging the levels 

consumption_level1_64$primarykey <- paste0(consumption_level1_64$fsu, consumption_level1_64$stratum, consumption_level1_64$substrm, 
                                           consumption_level1_64$hamlet, consumption_level1_64$ssstr, consumption_level1_64$hhno)


consumption_level2_64$primarykey <- paste0(consumption_level2_64$fsu, consumption_level2_64$stratum, consumption_level2_64$substrm, 
                                           consumption_level2_64$hamlet, consumption_level2_64$ssstr, consumption_level2_64$hhno)


consumption_level4_64$primarykey <- paste0(consumption_level4_64$fsu, consumption_level4_64$stratum, consumption_level4_64$substrm, 
                                           consumption_level4_64$hamlet, consumption_level4_64$ssstr, consumption_level4_64$hhno)

consumption_level5_64$primarykey <- paste0(consumption_level5_64$fsu, consumption_level5_64$stratum, consumption_level5_64$substrm, 
                                           consumption_level5_64$hamlet, consumption_level5_64$ssstr, consumption_level5_64$hhno)

consumption_level6_64$primarykey <- paste0(consumption_level6_64$fsu, consumption_level6_64$stratum, consumption_level6_64$substrm, 
                                           consumption_level6_64$hamlet, consumption_level6_64$ssstr, consumption_level6_64$hhno)

consumption_level7_64$primarykey <- paste0(consumption_level7_64$fsu, consumption_level7_64$stratum, consumption_level7_64$substrm, 
                                           consumption_level7_64$hamlet, consumption_level7_64$ssstr, consumption_level7_64$hhno)


## Merging the levels

con12_64 <- inner_join(consumption_level1_64, consumption_level2_64, by = "primarykey")
con124_64 <- inner_join(con12_64, consumption_level4_64, by = "primarykey")
con1245_64 <- inner_join(con124_64, consumption_level5_64, by = "primarykey")
con12456_64 <- inner_join(con1245_64, consumption_level6_64, by = "primarykey")
con124567_64 <- inner_join(con12456_64, consumption_level7_64, by = "primarykey")
