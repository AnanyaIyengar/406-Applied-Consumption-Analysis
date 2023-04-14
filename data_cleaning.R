###################################
###DATA CLEANING: CONSUMPTION 68###
###Ananya & Rajsi##################
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

################################################################################

#Renaming Item_Code from the NSS 68 Layout File

colnames(wide_total_cons_qty)

old_names_qty <-  c("HHID", "101" ,  "102" ,  "103" ,  "104"  , "105" ,  "106" ,  "107"  ,
                    "108" ,  "110"  , "111" ,  "112" ,  "113" ,  "114" , 
                    "115" ,  "116",   "117",   "118",   "120" ,"121" ,"122" ,  "129" ,  "139" ,  "140" ,  "141" ,  "142" ,  "143",   "144",   "145" , 
                    "146"   ,"147",   "148",   "150",   "151" ,"152"  , "159" ,  "160",   "161" ,  "162",  "163", "164",   "165" ,  "166",   "167" , 
                    "169" ,  "170" ,  "171",   "172",   "173"  , "174" ,"175" , "179" ,  "180",   "181" , "182" ,  "183"  , "184" ,  "185" ,  "189" , 
                    "190"  , "191",   "192",   "193",   "194" ,  "195", "196"  , "199" ,  "200",   "201" ,  "202" ,  "203",   "204" ,  "205",   "206",  
                    "207",   "208",   "210",   "211",   "212" ,  "213",   "214" ,  "215",   "216" ,  "217",   "219",   "220",   "221",   "222" ,  "223" , 
                    "224",   "225",  "226",   "227",   "228"  , "230" ,  "231","232"  , "233" ,  "234",   "235",   "236" ,  "237",   "238",   "239",  
                    "240",   "241",   "242",   "243",   "244"  , "245",   "246" ,  "247",   "249",   "250" ,  "251" ,  "252",   "253",   "254" ,  "255",  
                    "256",   "257",   "258",   "260",   "261" ,  "269",   "270" , "271"  , "272",   "273",   "274",   "275" ,  "276",   "277",   "279" , 
                    "280",   "281",   "282",   "283",   "284"  , "289",   "290" ,  "291" ,  "292",   "293",   "294" ,  "295",   "296",   "299",   "300",  
                    "301",   "302",   "309",   "310",   "311" ,  "312",   "313" ,  "314", "315",   "316", "317",   "319" ,  "320",   "321" ,  "322" , 
                    "323",   "324",   "325",   "329",   "330" ,  "331",   "332" ,  "333" ,  "334",   "335",   "336"  , "337"  , "338",   "340",   "341",  
                    "342",   "343" ,  "344",   "345",   "349" )

new_names_qty <- c("HHID", "qty_101" ,  "qty_102" ,  "qty_103" ,  "qty_104"  , "qty_105" ,  "qty_106" ,  "qty_107"  ,
                                     "qty_108" ,  "qty_110"  , "qty_111" ,  "qty_112" ,  "qty_113" ,  "qty_114" , 
                                     "qty_115" ,  "qty_116",   "qty_117",   "qty_118",   "qty_120" ,"qty_121" ,"qty_122" ,  "qty_129" ,  "qty_139" ,  "qty_140" ,  "qty_141" ,  "qty_142" ,  "qty_143",   "qty_144",   "qty_145" , 
                                     "qty_146"   ,"qty_147",   "qty_148",   "qty_150",   "qty_151" ,"qty_152"  , "qty_159" ,  "qty_160",   "qty_161" ,  "qty_162",  "qty_163", "qty_164",   "qty_165" ,  "qty_166",   "qty_167" , 
                                     "qty_169" ,  "qty_170" ,  "qty_171",   "qty_172",   "qty_173"  , "qty_174" ,"qty_175" , "qty_179" ,  "qty_180",   "qty_181" , "qty_182" ,  "qty_183"  , "qty_184" ,  "qty_185" ,  "qty_189" , 
                                     "qty_190"  , "qty_191",   "qty_192",   "qty_193",   "qty_194" ,  "qty_195", "qty_196"  , "qty_199" ,  "qty_200",   "qty_201" ,  "qty_202" ,  "qty_203",   "qty_204" ,  "qty_205",   "qty_206",  
                                     "qty_207",   "qty_208",   "qty_210",   "qty_211",   "qty_212" ,  "qty_213",   "qty_214" ,  "qty_215",   "qty_216" ,  "qty_217",   "qty_219",   "qty_220",   "qty_221",   "qty_222" ,  "qty_223" , 
                                     "qty_224",   "qty_225",  "qty_226",   "qty_227",   "qty_228"  , "qty_230" ,  "qty_231","qty_232"  , "qty_233" ,  "qty_234",   "qty_235",   "qty_236" ,  "qty_237",   "qty_238",   "qty_239",  
                                     "qty_240",   "qty_241",   "qty_242",   "qty_243",   "qty_244"  , "qty_245",   "qty_246" ,  "qty_247",   "qty_249",   "qty_250" ,  "qty_251" ,  "qty_252",   "qty_253",   "qty_254" ,  "qty_255",  
                                     "qty_256",   "qty_257",   "qty_258",   "qty_260",   "qty_261" ,  "qty_269",   "qty_270" , "qty_271"  , "qty_272",   "qty_273",   "qty_274",   "qty_275" ,  "qty_276",   "qty_277",   "qty_279" , 
                                     "qty_280",   "qty_281",   "qty_282",   "qty_283",   "qty_284"  , "qty_289",   "qty_290" ,  "qty_291" ,  "qty_292",   "qty_293",   "qty_294" ,  "qty_295",   "qty_296",   "qty_299",   "qty_300",  
                                     "qty_301",   "qty_302",   "qty_309",   "qty_310",   "qty_311" ,  "qty_312",   "qty_313" ,  "qty_314", "qty_315",   "qty_316", "qty_317",   "qty_319" ,  "qty_320",   "qty_321" ,  "qty_322" , 
                                     "qty_323",   "qty_324",   "qty_325",   "qty_329",   "qty_330" ,  "qty_331",   "qty_332" ,  "qty_333" ,  "qty_334",   "qty_335",   "qty_336"  , "qty_337"  , "qty_338",   "qty_340",   "qty_341",  
                                     "qty_342",   "qty_343" ,  "qty_344",   "qty_345",   "qty_349" )

old_names_val <-  c("HHID", "101" ,  "102" ,  "103" ,  "104"  , "105" ,  "106" ,  "107"  ,
                    "108" ,  "110"  , "111" ,  "112" ,  "113" ,  "114" , 
                    "115" ,  "116",   "117",   "118",   "120" ,"121" ,"122" ,  "129" ,  "139" ,  "140" ,  "141" ,  "142" ,  "143",   "144",   "145" , 
                    "146"   ,"147",   "148",   "150",   "151" ,"152"  , "159" ,  "160",   "161" ,  "162",  "163", "164",   "165" ,  "166",   "167" , 
                    "169" ,  "170" ,  "171",   "172",   "173"  , "174" ,"175" , "179" ,  "180",   "181" , "182" ,  "183"  , "184" ,  "185" ,  "189" , 
                    "190"  , "191",   "192",   "193",   "194" ,  "195", "196"  , "199" ,  "200",   "201" ,  "202" ,  "203",   "204" ,  "205",   "206",  
                    "207",   "208",   "210",   "211",   "212" ,  "213",   "214" ,  "215",   "216" ,  "217",   "219",   "220",   "221",   "222" ,  "223" , 
                    "224",   "225",  "226",   "227",   "228"  , "230" ,  "231","232"  , "233" ,  "234",   "235",   "236" ,  "237",   "238",   "239",  
                    "240",   "241",   "242",   "243",   "244"  , "245",   "246" ,  "247",   "249",   "250" ,  "251" ,  "252",   "253",   "254" ,  "255",  
                    "256",   "257",   "258",   "260",   "261" ,  "269",   "270" , "271"  , "272",   "273",   "274",   "275" ,  "276",   "277",   "279" , 
                    "280",   "281",   "282",   "283",   "284"  , "289",   "290" ,  "291" ,  "292",   "293",   "294" ,  "295",   "296",   "299",   "300",  
                    "301",   "302",   "309",   "310",   "311" ,  "312",   "313" ,  "314", "315",   "316", "317",   "319" ,  "320",   "321" ,  "322" , 
                    "323",   "324",   "325",   "329",   "330" ,  "331",   "332" ,  "333" ,  "334",   "335",   "336"  , "337"  , "338",   "340",   "341",  
                    "342",   "343" ,  "344",   "345",   "349" )


new_names_val <-  c("HHID", "val_101" ,  "val_102" ,  "val_103" ,  "val_104"  , "val_105" ,  "val_106" ,  "val_107"  ,
                    "val_108" ,  "val_110"  , "val_111" ,  "val_112" ,  "val_113" ,  "val_114" , 
                    "val_115" ,  "val_116",   "val_117",   "val_118",   "val_120" ,"val_121" ,"val_122" ,  "val_129" ,  "val_139" ,  "val_140" ,  "val_141" ,  "val_142" ,  "val_143",   "val_144",   "val_145" , 
                    "val_146"   ,"val_147",   "val_148",   "val_150",   "val_151" ,"val_152"  , "val_159" ,  "val_160",   "val_161" ,  "val_162",  "val_163", "val_164",   "val_165" ,  "val_166",   "val_167" , 
                    "val_169" ,  "val_170" ,  "val_171",   "val_172",   "val_173"  , "val_174" ,"val_175" , "val_179" ,  "val_180",   "val_181" , "val_182" ,  "val_183"  , "val_184" ,  "val_185" ,  "val_189" , 
                    "val_190"  , "val_191",   "val_192",   "val_193",   "val_194" ,  "val_195", "val_196"  , "val_199" ,  "val_200",   "val_201" ,  "val_202" ,  "val_203",   "val_204" ,  "val_205",   "val_206",  
                    "val_207",   "val_208",   "val_210",   "val_211",   "val_212" ,  "val_213",   "val_214" ,  "val_215",   "val_216" ,  "val_217",   "val_219",   "val_220",   "val_221",   "val_222" ,  "val_223" , 
                    "val_224",   "val_225",  "val_226",   "val_227",   "val_228"  , "val_230" ,  "val_231","val_232"  , "val_233" ,  "val_234",   "val_235",   "val_236" ,  "val_237",   "val_238",   "val_239",  
                    "val_240",   "val_241",   "val_242",   "val_243",   "val_244"  , "val_245",   "val_246" ,  "val_247",   "val_249",   "val_250" ,  "val_251" ,  "val_252",   "val_253",   "val_254" ,  "val_255",  
                    "val_256",   "val_257",   "val_258",   "val_260",   "val_261" ,  "val_269",   "val_270" , "val_271"  , "val_272",   "val_273",   "val_274",   "val_275" ,  "val_276",   "val_277",   "val_279" , 
                    "val_280",   "val_281",   "val_282",   "val_283",   "val_284"  , "val_289",   "val_290" ,  "val_291" ,  "val_292",   "val_293",   "val_294" ,  "val_295",   "val_296",   "val_299",   "val_300",  
                    "val_301",   "val_302",   "val_309",   "val_310",   "val_311" ,  "val_312",   "val_313" ,  "val_314", "val_315",   "val_316", "val_317",   "val_319" ,  "val_320",   "val_321" ,  "val_322" , 
                    "val_323",   "val_324",   "val_325",   "val_329",   "val_330" ,  "val_331",   "val_332" ,  "val_333" ,  "val_334",   "val_335",   "val_336"  , "val_337"  , "val_338",   "val_340",   "val_341",  
                    "val_342",   "val_343" ,  "val_344",   "val_345",   "val_349" )

colnames(wide_total_cons_qty) <- new_names_qty
colnames(wide_total_cons_value) <- new_names_val

#Dealing with 0 variables 

wide_total_cons_qty$qty_169 <- rowSums(wide_total_cons_qty[c("qty_160",   "qty_161" ,  "qty_162",  "qty_163", "qty_164",   "qty_165" , 
                                                             "qty_166",   "qty_167")], na.rm = TRUE)


wide_total_cons_qty$qty_199 <- rowSums(wide_total_cons_qty[c("qty_190"  , "qty_191",   "qty_192",   "qty_193",   "qty_194" ,  
                                                             "qty_195", "qty_196")], na.rm = TRUE)

wide_total_cons_qty$qty_219 <- rowSums(wide_total_cons_qty[c("qty_200",   "qty_201" ,  "qty_202" ,  "qty_203",   "qty_204" ,  "qty_205",   "qty_206",  
                                                             "qty_207",   "qty_208",   "qty_210",   "qty_211",   "qty_212" ,  "qty_213",   "qty_214" , 
                                                             "qty_215",   "qty_216" ,  "qty_217")], na.rm = TRUE)


wide_total_cons_qty$qty_239 <- rowSums(wide_total_cons_qty[c("qty_220",   "qty_221",   "qty_222" ,  "qty_223" , 
                                                             "qty_224",   "qty_225",  "qty_226",   "qty_227",  
                                                             "qty_228"  , "qty_230" ,  "qty_231","qty_232"  , "qty_233" ,  
                                                             "qty_234",   "qty_235",   "qty_236" ,  "qty_237",   "qty_238")], na.rm = TRUE)

wide_total_cons_qty$qty_279 <- rowSums(wide_total_cons_qty[c("qty_270" , "qty_271"  , "qty_272",   "qty_273",   "qty_274",   "qty_275" ,  "qty_276", 
                                                             "qty_277")], na.rm = TRUE)

wide_total_cons_qty$qty_289 <- rowSums(wide_total_cons_qty[c("qty_280",   "qty_281",   "qty_282",   "qty_283",   "qty_284")], na.rm = TRUE)

wide_total_cons_qty$qty_299 <- rowSums(wide_total_cons_qty[c("qty_290" ,  "qty_291" ,  "qty_292",   "qty_293",   "qty_294" ,  
                                                             "qty_295",   "qty_296")], na.rm = TRUE)

wide_total_cons_qty$qty_309 <- rowSums(wide_total_cons_qty[c("qty_300",  
                                                             "qty_301",   "qty_302")], na.rm = TRUE)

wide_total_cons_qty$qty_319 <- rowSums(wide_total_cons_qty[c("qty_310",   "qty_311" ,  "qty_312",   "qty_313" ,  "qty_314", "qty_315",  
                                                             "qty_316", "qty_317")], na.rm = TRUE)

wide_total_cons_qty$qty_329 <- rowSums(wide_total_cons_qty[c("qty_321" ,  "qty_322" , 
                                                             "qty_323",   "qty_324",   "qty_325")], na.rm = TRUE)

################################################################################

#Selecting Relevant Variables

#Variable Descriptions and Item Codes:

# - Cereals is 129 
# - Cereal Substitutes is 139 
# - Pulses is 159 
# - Milk and milk products is 169
# - Salt and Sugar is 179
# - Edible oil is 189 
# - Meat Eggs Fish is 199
# - Vegetables is 219
# - Fresh Fruits is 239 
# - Fruits Dry is 249
# - Spices is 269
# - Beverages is 279
# - Served Processed Food is 289
# - Packaged Processed Food is 299
# - Paan is 309
# - Tobacco is 319
# - Intoxicants is 329
# - Fuel and Light is 349

food_qty <- wide_total_cons_qty%>%dplyr::select("HHID", "qty_129", "qty_139", "qty_159", "qty_169", "qty_179", "qty_189", "qty_199", "qty_219", "qty_239", "qty_249", "qty_269", "qty_279", "qty_289", "qty_299", "qty_309", "qty_319", "qty_329")

food_val <- wide_total_cons_value%>%dplyr::select("HHID", "val_129", "val_139", "val_159", "val_169", "val_179", "val_189", "val_199", "val_219", "val_239", "val_249", "val_269", "val_279", "val_289", "val_299", "val_309", "val_319", "val_329")

#Keeping only distinct values of the covariates data set 

unique_cov <- dplyr::distinct(covariates)

################################################################################

#Converting quantities to kilogram (kg) terms 

food_merged <- food_merged%>%dplyr::mutate(across(c("qty_129", "qty_139", "qty_159", "qty_169", "qty_179", "qty_189", "qty_199", "qty_219", "qty_239", "qty_249", "qty_269", "qty_279", "qty_289", "qty_299", "qty_309", "qty_319", "qty_329"), function(x) x/1000))
