

##### --- 0.1. Population Structure ------ 

pop_str_cl_1718 = read_csv("1_RawData/population_str_cl_1718.csv")

# Population percentages by age class/catogory
age_class = c("MEDAGEPOP", "PC_Y65_MAX", "PC_Y60_MAX",
              "PC_FM", "OLDDEP1", "OLDDEP2", "MMEDAGEPOP",
              "FMEDAGEPOP")


pop_str_cl_17 = pop_str_cl_1718 %>% 
  filter(time == 2017) %>% 
  select(-unit, -time) %>% 
  select(geo, indic_de, values)

pop_str_cl_17_wide = pop_str_cl_17 %>% 
  spread(indic_de, values)



### ---- Population Classes : Country/Regions

pop_str_cl_17_wide_c = pop_str_cl_17_wide %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 2) %>% 
  select(-len)


pop_str_cl_17_wide_r = pop_str_cl_17_wide %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len > 3) %>% 
  select(-len)


# Countries
pop_str_cl_17_wide_c = pop_str_cl_17_wide_c %>% 
  select(geo, PC_Y60_MAX, PC_FM)


pop_str_cl_17_wide_r = pop_str_cl_17_wide_r %>% 
  select(geo, PC_Y60_MAX, PC_FM)



pop_str_gr_1718 = read_csv("1_RawData/population_str_gr_1718.csv")

# Population percentages by age group
age_groups = c("PC_Y60_64" , "PC_Y65_69", "PC_Y70_74", 
               "PC_Y75_79", "PC_Y80_84", "PC_Y85_MAX")

pop_str_gr_17 = pop_str_gr_1718 %>% 
  filter(time == 2017) %>% 
  select(-unit, -time) %>% 
  select(geo, indic_de, values)



### For LATER weitght calculations
pop_str_gr_17_wide = pop_str_gr_17 %>% 
  spread(indic_de, values)

# Country
pop_str_gr_17_wide_c = pop_str_gr_17_wide %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 2) %>% 
  select(-len)

# Regional
pop_str_gr_17_wide_r = pop_str_gr_17_wide %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len > 3) %>% 
  select(-len)

##### --- 0.2. Income data (part of it)  ------ 

### Get regional data in tsv from website
tsv_inc = list.files(path = "1_RawData", pattern = ".tsv$")
tsv_inc_f = paste0("C:/000000_EuroProject/1_RawData/", tsv_inc)
tsv_df = map(tsv_inc_f, read_tsv)

### 5 files about GDP_cap, GDP_cap indexed, HH disposable income, HH primary ...

# Regional gross domestic product (PPS per inhabitant) by NUTS 2 regions (tgs00005)
gdp_cap = as.data.frame(tsv_df[1])
gdp_cap_row = gdp_cap %>% select(1) # row naming (extract geo)


gdp_cap_parsed = gdp_cap %>%
  select(-1) %>%
  map_df(parse_number) %>%
  cbind(gdp_cap_row) %>%
  select(unit.geo.time, everything())

names(gdp_cap_parsed) = c("unit.geo.time",
                          seq(2006,2017,1))

gdp_cap_parsed = gdp_cap_parsed %>%
  separate(unit.geo.time, into = c("unit", "geo"), sep = ",", remove = FALSE) %>%
  select(-1)

# NA for Norway
gdp_cap_17 = gdp_cap_parsed %>% 
  select(geo, "2017")

# No NAs
gdp_cap_16 = gdp_cap_parsed %>% 
  select(geo, "2016")

### ---- This data: GDP_cap: Regional UNIQUEMENT - 2016 since no NAs
gdp_cap_16_r  = gdp_cap_16


#### ---- 0.2.1 - For COUNTRY data: GDP_cap for 2017: earlier Country dataset ------

gdp_cap_old = read_csv("1_RawData/gdp_cap.csv")

gdp_cap_old_c = gdp_cap_old %>%   # ATTENTION au EU28 and the like - NO PBM: left_join
  filter(unit == "CLV10_EUR_HAB") %>% 
  filter(time == 2017) %>% 
  select(geo, GDP_cap = values)


### ------- 0.3. Description of diseases! ------

disease = read_csv("1_RawData/Patient_Disease.csv")

categories = disease %>% filter(nchar(Acronym) == 1)
# list_categories = categories %>% pull(Acronym)


##### ---- 0.4. Extra Hospital Data ------ #####

### (unit == "P_HTHAB")
### Par regions: Pas bcp de donnees par regions!
hosp_beds17 = read_csv("1_RawData/hosp_beds_17.csv")

hosp_beds17 = hosp_beds17 %>% 
  select(-unit, -time, geo, facility, values)


hosp_beds17_c = hosp_beds17 %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 2) %>% 
  select(-len)


# Country
hosp_beds17_c = hosp_beds17_c %>% 
  select(-facility) %>% 
  rename(Hospital_Beds = values)


# Regional
hosp_beds17_r = hosp_beds17 %>%  # ATTENTION au EU28 and the like - NO PBM: left_join
  mutate(len = nchar(geo)) %>% 
  filter(len > 3) %>% 
  select(-len)

### Donnees Par regions:
health_pers17 = read_csv("1_RawData/health_pers_17.csv")

health_pers17 = health_pers17 %>% 
  select(-unit, -isco08, -time)

health_pers17_c = health_pers17 %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 2) %>% 
  select(-len)

# Country
health_pers17_c = health_pers17_c %>% 
  rename(Health_Personal = values)

# Regional
health_pers17_r = health_pers17 %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 4) %>% 
  select(-len)


### (unit %in% c("FTE_HTHAB", "HC_HTHAB"))y
### Par pays: health_pers_hosp
health_pers_hosp17 = read_csv("1_RawData/health_pers_hosp_17.csv")

# skim(health_pers_hosp17)

health_pers_hosp17 = health_pers_hosp17 %>% 
  select(-time)

# unit == "HC_HTHAB" - Fuller dataset (less NAs)
health_pers_hosp17_HC = health_pers_hosp17 %>% 
  filter(unit == "HC_HTHAB") %>% 
  select(-unit) %>% 
  select(geo, isco08, values)

health_pers_hosp17_HC_wide = health_pers_hosp17_HC %>% 
  spread(isco08, values)

health_pers_hosp17_HC_wide_c = health_pers_hosp17_HC_wide  


###
# Par pays: health_graduates
health_grad17 = read_csv("1_RawData/health_grad_17.csv")

health_grad17 = health_grad17 %>% 
  filter(unit == "P_HTHAB") %>% 
  select(-unit, -time) %>% 
  select(geo, isco08, values)

health_grad17_wide = health_grad17 %>% 
  spread(isco08, values)

health_grad17_wide_c = health_grad17_wide


##### ---- 1. All relevant Hospital Data ------ #####

### For bulk reading
data_list = c("hosp_dis_t17", "hosp_dis_m17", "hosp_dis_f17",
              "hosp_dis_day_t17", "hosp_dis_day_m17", "hosp_dis_day_f17",
              "hosp_length_tot17", "hosp_length_male17", "hosp_length_female17")


extension_list = paste0("1_RawData/", data_list, ".csv")

### Bulk reading
df_hosp_list = map(extension_list, read_csv)


### Bulk extracting elements as single df!
hosp_dis_t17  =  df_hosp_list[[1]]     
hosp_dis_m17    =  df_hosp_list[[2]]      
hosp_dis_f17        =  df_hosp_list[[3]]  
hosp_dis_day_t17    =  df_hosp_list[[4]]  
hosp_dis_day_m17    =  df_hosp_list[[5]]  
hosp_dis_day_f17    =  df_hosp_list[[6]]  
hosp_length_tot17   =  df_hosp_list[[7]]  
hosp_length_male17  =  df_hosp_list[[8]]  
hosp_length_female17=  df_hosp_list[[9]]  


### Enframing to list-columns
### Nested full data for Batch operations
hosp_list_nest = df_hosp_list %>% 
  set_names(data_list) %>% 
  enframe() %>% 
  rename(Dataframe = name,
         AgeGroupPop = value) 

##### ------ 2. WIDE datasets for PCA - Batch operations on Lists------

### Nested operations: Selecting TOTAL-AGE population and SPREADING
### Countries/Regions not accounted for!
hosp_list_nest_totpop =  hosp_list_nest %>% 
  mutate(TotalPopWide = map(AgeGroupPop, ~(.x %>% 
                                             select(-indic_he, -unit, -time) %>% 
                                             select(geo, age, icd10, values) %>% 
                                             filter(age == "TOTAL") %>% 
                                             select(-age) %>% 
                                             spread(icd10, values))))


### Testing nested operations: Splitting countries and regions
hosp_list_nest_totpop_geo =  hosp_list_nest_totpop %>% 
  mutate(CountryTotalWide = map(TotalPopWide, ~(.x %>% 
                                                  mutate(len = nchar(geo)) %>% 
                                                  filter(len == 2) %>% 
                                                  select(-len)))) 


hosp_list_nest_totpop_geo =  hosp_list_nest_totpop_geo %>% 
  mutate(RegionTotalWide = map(TotalPopWide, ~(.x %>% 
                                                 mutate(len = nchar(geo)) %>% 
                                                 filter(len > 3) %>% 
                                                 select(-len)))) 

#### ------ 2.1. FLEX: Enframing 3 variables By Country -------

### Enframing to 3 list-columns: DISCHARGES (inpatient and day cares) + Stay Length!
### Nested full data for Batch operations
hosp_dis_list_nest = df_hosp_list[1:3] %>% 
  set_names(data_list[1:3]) %>% 
  enframe() %>% 
  rename(Discharge_Inpatients = name,
         AgeGroupPop = value) 


### ATTENTION: Sex codage: T, M, F as TRUE, MALE, FALSE (PBM: logic  Vs character)
hosp_dis_list_unnest = hosp_dis_list_nest %>% 
  mutate(AgeGroupPop = map(AgeGroupPop, ~.x %>% mutate_if(is.logical, as.character))) %>% 
  unnest(AgeGroupPop) %>% 
  bind_rows() %>% 
  mutate(Gender = if_else(sex == TRUE, "All", 
                          if_else(sex == "M", "Male",
                                  "Female"))) %>% 
  select(-sex)

### ATTENTION: BY COUNTRY: _c missing here!!! NOT for regional: _r!!!
hosp_dis_list_unnest = hosp_dis_list_unnest %>% 
  filter(age == "TOTAL") %>% 
  select(geo, icd10, Gender, Rate = values) %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 2) %>% 
  select(-len) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(icd10 = fct_recode(icd10,
                            "Endocrine, nutritional and metabolic diseases" = "E",
                            "Mental and behavioural disorders" = "F",
                            "Diseases of the nervous system" = "G",
                            "Diseases of the circulatory system" = "I",
                            "Diseases of the respiratory system" = "J",
                            "Diseases of the digestive system" = "K",
                            "Diseases of the skin and subcutaneous tissue" = "L",
                            "Diseases of the musculoskeletal system and connective tissue" = "M",
                            "Diseases of the genitourinary system" = "N",
                            "Pregnancy, childbirth and the puerperium" = "O",
                            "Certain conditions originating in the perinatal period" = "P",
                            "Congenital malformations, deformations and chromosomal abnormalities" = "Q",
                            "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "R",
                            "Factors influencing health status and contact with health services" = "Z"))


hosp_dis_day_list_nest = df_hosp_list[4:6] %>% 
  set_names(data_list[4:6]) %>% 
  enframe() %>% 
  rename(Discharge_Days = name,
         AgeGroupPop = value) 

### ATTENTION: Sex codage: T, M, F as TRUE, MALE, FALSE (PBM: logic  Vs character)
hosp_dis_day_list_unnest = hosp_dis_day_list_nest %>% 
  mutate(AgeGroupPop = map(AgeGroupPop, ~.x %>% mutate_if(is.logical, as.character))) %>% 
  unnest(AgeGroupPop) %>% 
  bind_rows() %>% 
  mutate(Gender = if_else(sex == TRUE, "All", 
                          if_else(sex == "M", "Male",
                                  "Female"))) %>% 
  select(-sex)


hosp_dis_day_list_unnest = hosp_dis_day_list_unnest %>% 
  filter(age == "TOTAL") %>% 
  select(geo, icd10, Gender, Rate = values) %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 2) %>% 
  select(-len) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(icd10 = fct_recode(icd10,
                            "Endocrine, nutritional and metabolic diseases" = "E",
                            "Mental and behavioural disorders" = "F",
                            "Diseases of the nervous system" = "G",
                            "Diseases of the circulatory system" = "I",
                            "Diseases of the respiratory system" = "J",
                            "Diseases of the digestive system" = "K",
                            "Diseases of the skin and subcutaneous tissue" = "L",
                            "Diseases of the musculoskeletal system and connective tissue" = "M",
                            "Diseases of the genitourinary system" = "N",
                            "Pregnancy, childbirth and the puerperium" = "O",
                            "Certain conditions originating in the perinatal period" = "P",
                            "Congenital malformations, deformations and chromosomal abnormalities" = "Q",
                            "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "R",
                            "Factors influencing health status and contact with health services" = "Z"))


hosp_length_list_nest = df_hosp_list[7:9] %>% 
  set_names(data_list[7:9]) %>% 
  enframe() %>% 
  rename(Stay_Length= name,
         AgeGroupPop = value) 


hosp_length_list_unnest = hosp_length_list_nest %>% 
  mutate(AgeGroupPop = map(AgeGroupPop, ~.x %>% mutate_if(is.logical, as.character))) %>% 
  unnest(AgeGroupPop) %>% 
  bind_rows() %>% 
  mutate(Gender = if_else(sex == TRUE, "All", 
                          if_else(sex == "M", "Male",
                                  "Female"))) %>% 
  select(-sex)

hosp_length_list_unnest = hosp_length_list_unnest %>% 
  filter(age == "TOTAL") %>% 
  select(geo, icd10, Gender, Rate = values) %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 2) %>% 
  select(-len) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(icd10 = fct_recode(icd10,
                            "Endocrine, nutritional and metabolic diseases" = "E",
                            "Mental and behavioural disorders" = "F",
                            "Diseases of the nervous system" = "G",
                            "Diseases of the circulatory system" = "I",
                            "Diseases of the respiratory system" = "J",
                            "Diseases of the digestive system" = "K",
                            "Diseases of the skin and subcutaneous tissue" = "L",
                            "Diseases of the musculoskeletal system and connective tissue" = "M",
                            "Diseases of the genitourinary system" = "N",
                            "Pregnancy, childbirth and the puerperium" = "O",
                            "Certain conditions originating in the perinatal period" = "P",
                            "Congenital malformations, deformations and chromosomal abnormalities" = "Q",
                            "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "R",
                            "Factors influencing health status and contact with health services" = "Z"))


### ------ Final Enframing for FLEX 

list_test = list(hosp_dis_list_unnest, hosp_dis_day_list_unnest, hosp_length_list_unnest)

# Hospital inpatients' discharges
# Hospital day discharges
# Hospital stay lengths

Var_hosp = c("hosp_dis", "hosp_dis_day", "hosp_length")
data_list_country = list_test %>% set_names(Var_hosp)

data_names_c = c("hosp_dis_t17", "hosp_dis_m17", "hosp_dis_f17",
                 "hosp_dis_day_t17", "hosp_dis_day_m17", "hosp_dis_day_f17", 
                 "hosp_length_tot17", "hosp_length_male17", "hosp_length_female17")


#### ------ 2.2 FLEX: GIS data from LONG Countries and Regions -------
df_hosp_list_long = df_hosp_list %>% 
  set_names(data_names_c) %>% 
  enframe()

df_hosp_list_long = df_hosp_list_long %>% 
  rename(Combination = name,
         DataLong = value) %>% 
  mutate(DataLongCountry = map(DataLong, ~.x %>% 
                                 filter(age == "TOTAL") %>% 
                                 filter(nchar(geo) == 2) %>% 
                                 select(geo, icd10, values))) %>% 
  mutate(DataLongRegion = map(DataLong, ~.x %>% 
                                filter(age == "TOTAL") %>% 
                                filter(nchar(geo) == 4) %>% 
                                select(geo, icd10, values))) 

df_hosp_list_long_c = df_hosp_list_long %>% 
  select(Combination, DataLongCountry)

df_hosp_list_long_r = df_hosp_list_long %>% 
  select(Combination, DataLongRegion)


### ---- 2.2.1 NUTS0 data for COUNTRY -----
nuts0g = st_read("C:/000000_EuroProject/1_RawData/EU_Nuts0.shp")

### GIS data NUTS0 - PREFERABLY Right_join then filter is.na!!!
df_hosp_list_glong_c =  df_hosp_list_long_c %>% 
  mutate(DataLongCountryGIS = map(DataLongCountry, ~.x %>% 
                                    left_join(nuts0g %>% select(geo, NUTS_NAME),
                                              by = c("geo")))) %>% 
  select(-DataLongCountry)

df_hosp_list_glong_c = df_hosp_list_glong_c %>% 
  mutate(DataLongCountryGIS = map(DataLongCountryGIS, ~.x %>% 
                                    select(Country = geo,
                                           Country_Name = NUTS_NAME, 
                                           icd10, 
                                           values,
                                           geometry) %>% ### REMEMBER GIS!!!
                                    mutate(icd10 = fct_recode(icd10,
                                                              "Endocrine, nutritional and metabolic diseases" = "E",
                                                              "Mental and behavioural disorders" = "F",
                                                              "Diseases of the nervous system" = "G",
                                                              "Diseases of the circulatory system" = "I",
                                                              "Diseases of the respiratory system" = "J",
                                                              "Diseases of the digestive system" = "K",
                                                              "Diseases of the skin and subcutaneous tissue" = "L",
                                                              "Diseases of the musculoskeletal system and connective tissue" = "M",
                                                              "Diseases of the genitourinary system" = "N",
                                                              "Pregnancy, childbirth and the puerperium" = "O",
                                                              "Certain conditions originating in the perinatal period" = "P",
                                                              "Congenital malformations, deformations and chromosomal abnormalities" = "Q",
                                                              "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "R",
                                                              "Factors influencing health status and contact with health services" = "Z"))))



### ---- 2.2.2 NUTS2 data for REGIONS-----

### NUTS2
nuts2g = st_read("C:/000000_EuroProject/1_RawData/EU_Nuts2.shp")

### GIS data NUTS2 - PREFERABLY Right_join then filter is.na!!!
df_hosp_list_glong_r =  df_hosp_list_long_r %>% 
  mutate(DataLongRegionGIS = map(DataLongRegion, ~.x %>% 
                                   left_join(nuts2g %>% select(geo, NUTS_NAME),
                                             by = c("geo")))) %>% 
  select(-DataLongRegion)

df_hosp_list_glong_r = df_hosp_list_glong_r %>% 
  mutate(DataLongRegionGIS = map(DataLongRegionGIS, ~.x %>% 
                                   select(Country = geo,
                                          Country_Name = NUTS_NAME, 
                                          icd10, 
                                          values,
                                          geometry) %>% ### REMEMBER!!!
                                   mutate(icd10 = fct_recode(icd10,
                                                             "Endocrine, nutritional and metabolic diseases" = "E",
                                                             "Mental and behavioural disorders" = "F",
                                                             "Diseases of the nervous system" = "G",
                                                             "Diseases of the circulatory system" = "I",
                                                             "Diseases of the respiratory system" = "J",
                                                             "Diseases of the digestive system" = "K",
                                                             "Diseases of the skin and subcutaneous tissue" = "L",
                                                             "Diseases of the musculoskeletal system and connective tissue" = "M",
                                                             "Diseases of the genitourinary system" = "N",
                                                             "Pregnancy, childbirth and the puerperium" = "O",
                                                             "Certain conditions originating in the perinatal period" = "P",
                                                             "Congenital malformations, deformations and chromosomal abnormalities" = "Q",
                                                             "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "R",
                                                             "Factors influencing health status and contact with health services" = "Z")))) 


#### ------ 2.3. FLEX: Enframing 3 variables By REGION-------

#### ------ FLEX: Enframing for 3 LUMPED variables (by REGION: INPATIENTS/DAYS/LENGTH)
### Enframing to list-columns
### Nested full data for Batch operations
hosp_dis_list_nest_r = df_hosp_list[1:3] %>% 
  set_names(data_list[1:3]) %>% 
  enframe() %>% 
  rename(Discharge_Inpatients = name,
         AgeGroupPop = value) 

hosp_dis_list_unnest_r = hosp_dis_list_nest_r %>% 
  mutate(AgeGroupPop = map(AgeGroupPop, ~.x %>% mutate_if(is.logical, as.character))) %>% 
  unnest(AgeGroupPop) %>% 
  bind_rows() %>% 
  mutate(Gender = if_else(sex == TRUE, "All", 
                          if_else(sex == "M", "Male",
                                  "Female"))) %>% 
  select(-sex)


hosp_dis_list_unnest_r = hosp_dis_list_unnest_r %>% 
  filter(age == "TOTAL") %>% 
  select(geo, icd10, Gender, Rate = values) %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 4) %>% 
  select(-len) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(icd10 = fct_recode(icd10,
                            "Endocrine, nutritional and metabolic diseases" = "E",
                            "Mental and behavioural disorders" = "F",
                            "Diseases of the nervous system" = "G",
                            "Diseases of the circulatory system" = "I",
                            "Diseases of the respiratory system" = "J",
                            "Diseases of the digestive system" = "K",
                            "Diseases of the skin and subcutaneous tissue" = "L",
                            "Diseases of the musculoskeletal system and connective tissue" = "M",
                            "Diseases of the genitourinary system" = "N",
                            "Pregnancy, childbirth and the puerperium" = "O",
                            "Certain conditions originating in the perinatal period" = "P",
                            "Congenital malformations, deformations and chromosomal abnormalities" = "Q",
                            "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "R",
                            "Factors influencing health status and contact with health services" = "Z"))

hosp_dis_day_list_nest_r = df_hosp_list[4:6] %>% 
  set_names(data_list[4:6]) %>% 
  enframe() %>% 
  rename(Discharge_Days = name,
         AgeGroupPop = value) 

hosp_dis_day_list_unnest_r = hosp_dis_day_list_nest_r %>% 
  mutate(AgeGroupPop = map(AgeGroupPop, ~.x %>% mutate_if(is.logical, as.character))) %>% 
  unnest(AgeGroupPop) %>% 
  bind_rows() %>% 
  mutate(Gender = if_else(sex == TRUE, "All", 
                          if_else(sex == "M", "Male",
                                  "Female"))) %>% 
  select(-sex)


hosp_dis_day_list_unnest_r = hosp_dis_day_list_unnest_r %>% 
  filter(age == "TOTAL") %>% 
  select(geo, icd10, Gender, Rate = values) %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 4) %>% 
  select(-len) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(icd10 = fct_recode(icd10,
                            "Endocrine, nutritional and metabolic diseases" = "E",
                            "Mental and behavioural disorders" = "F",
                            "Diseases of the nervous system" = "G",
                            "Diseases of the circulatory system" = "I",
                            "Diseases of the respiratory system" = "J",
                            "Diseases of the digestive system" = "K",
                            "Diseases of the skin and subcutaneous tissue" = "L",
                            "Diseases of the musculoskeletal system and connective tissue" = "M",
                            "Diseases of the genitourinary system" = "N",
                            "Pregnancy, childbirth and the puerperium" = "O",
                            "Certain conditions originating in the perinatal period" = "P",
                            "Congenital malformations, deformations and chromosomal abnormalities" = "Q",
                            "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "R",
                            "Factors influencing health status and contact with health services" = "Z"))



hosp_length_list_nest_r = df_hosp_list[7:9] %>% 
  set_names(data_list[7:9]) %>% 
  enframe() %>% 
  rename(Stay_Length= name,
         AgeGroupPop = value) 

hosp_length_list_unnest_r = hosp_length_list_nest_r %>% 
  mutate(AgeGroupPop = map(AgeGroupPop, ~.x %>% mutate_if(is.logical, as.character))) %>% 
  unnest(AgeGroupPop) %>% 
  bind_rows() %>% 
  mutate(Gender = if_else(sex == TRUE, "All", 
                          if_else(sex == "M", "Male",
                                  "Female"))) %>% 
  select(-sex)

hosp_length_list_unnest_r = hosp_length_list_unnest_r %>% 
  filter(age == "TOTAL") %>% 
  select(geo, icd10, Gender, Rate = values) %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 4) %>% 
  select(-len) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(icd10 = fct_recode(icd10,
                            "Endocrine, nutritional and metabolic diseases" = "E",
                            "Mental and behavioural disorders" = "F",
                            "Diseases of the nervous system" = "G",
                            "Diseases of the circulatory system" = "I",
                            "Diseases of the respiratory system" = "J",
                            "Diseases of the digestive system" = "K",
                            "Diseases of the skin and subcutaneous tissue" = "L",
                            "Diseases of the musculoskeletal system and connective tissue" = "M",
                            "Diseases of the genitourinary system" = "N",
                            "Pregnancy, childbirth and the puerperium" = "O",
                            "Certain conditions originating in the perinatal period" = "P",
                            "Congenital malformations, deformations and chromosomal abnormalities" = "Q",
                            "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "R",
                            "Factors influencing health status and contact with health services" = "Z"))



### ------ Final Enframing for FLEX - 3 Variables by Region

list_test_r = list(hosp_dis_list_unnest_r, hosp_dis_day_list_unnest_r, hosp_length_list_unnest_r)


Var_hosp = c("hosp_dis", "hosp_dis_day", "hosp_length")
data_list_region = list_test_r %>% set_names(Var_hosp)


####### ------ 3. FLEX-supplement: LUMPING Hospital Extra DATA for LONG ------

hosp_beds17_xtra = hosp_beds17 %>% 
  filter(geo != "EU28") %>% 
  mutate(facility = fct_recode(facility,
                               "Hospital Beds" = "HBEDT")) %>% 
  rename(variable = facility)


health_pers17_xtra = health_pers17 %>% 
  bind_cols(variable = rep("Health Personel", 267)) %>% 
  select(variable, geo, values)


health_pers_hosp17_HC_xtra = health_pers_hosp17_HC %>% 
  rename(variable = isco08) %>% 
  select(variable, geo, values) %>% 
  mutate(variable = fct_recode(variable,
                               "Medical doctors" = "OC221",	
                               "Nursing professionals and midwives" = "OC222_3222",	
                               "Nursing associate professionals" = "OC3221",	
                               "Health care assistants" = "OC5321",	
                               "Hospital employment" = "HOSP",	
                               "Other health service providers employed by hospital" = "OTH_SERV",
                               "Other staff employed by hospital" = "OTH_HOSP"))	



hosp_extra = bind_rows(hosp_beds17_xtra, health_pers17_xtra, health_pers_hosp17_HC_xtra)

# Country extra hospital data
hosp_extra_country = hosp_extra %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 2) %>% 
  select(-len) %>% 
  mutate_if(is.character, as.factor)


hosp_extra_gcountry = nuts0g %>% select(geo, NUTS_NAME) %>% 
  left_join(hosp_extra_country, by = "geo") %>% 
  filter(!(is.na(values))) %>% 
  mutate(variable = as.factor(variable))


# Regional extra hospital data
hosp_extra_region = hosp_extra %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 4) %>% 
  select(-len) %>% 
  mutate_if(is.character, as.factor)

hosp_extra_gregion = nuts2g %>% select(geo, NUTS_NAME) %>% 
  left_join(hosp_extra_region, by = "geo") %>% 
  filter(!(is.na(values))) %>% 
  mutate(variable = as.factor(variable))


####### ------ 4. FLEX-Variables-supplement (SOCIO): LUMPING Pop/Gdp Extra DATA for LONG ------

# socio_extra_country 

pop_str_cl_17_xtra = pop_str_cl_17 %>% 
  rename(variable = indic_de) %>% 
  mutate(variable = fct_recode(variable,
                               "Old dependency ratio 65_15" = "OLDDEP1",
                               "Old dependency ratio 60_20" = "OLDDEP2", 
                               "Women per 100 men" = "PC_FM",
                               "Proportion of 60+ population" = "PC_Y60_MAX",
                               "Proportion of 65+ population" = "PC_Y65_MAX")) %>% 
  select(variable, geo, values) 


pop_str_gr_17_xtra = pop_str_gr_17 %>% 
  filter(indic_de %in% c("PC_Y60_64", "PC_Y65_69")) %>% 
  rename(variable = indic_de) %>% 
  mutate(variable = fct_recode(variable,
                               "Proportion of 60-64 population" = "PC_Y60_64",
                               "Proportion of 65-69 population" = "PC_Y65_69")) %>% 
  select(variable, geo, values) 

# Country merging by population FIRST since INDCOME has missing country/regional
pop_extra = bind_rows(pop_str_cl_17_xtra, pop_str_gr_17_xtra)

pop_extra_country = pop_extra %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 2) %>% 
  select(-len)


gdp_cap_old_c_xtra = gdp_cap_old_c %>% 
  filter(!(geo %in% c("EA19", "EU28"))) %>% 
  cbind(variable = rep("GDP per capita", 35)) %>% 
  select(variable, geo, values = GDP_cap)

socio_extra_country = bind_rows(pop_extra_country, gdp_cap_old_c_xtra) %>% 
  mutate_if(is.character, as.factor)

### socio country gis
socio_extra_gcountry = nuts0g %>% select(geo, NUTS_NAME) %>% 
  left_join(socio_extra_country, by = "geo") %>% 
  filter(!(is.na(values))) %>% 
  mutate(variable = as.factor(variable))


### Regional Merging
pop_extra_region = pop_extra %>% 
  mutate(len = nchar(geo)) %>% 
  filter(len == 4) %>% 
  select(-len)


gdp_cap_16_r_xtra = gdp_cap_16_r %>% 
  cbind(variable = rep("GDP per capita", 323)) %>% 
  select(variable, geo, "2016") %>% 
  rename(values = "2016")


socio_extra_region = bind_rows(pop_extra_region, gdp_cap_16_r_xtra) %>% 
  mutate_if(is.character, as.factor)


socio_extra_gregion = nuts2g %>% select(geo, NUTS_NAME) %>% 
  left_join(socio_extra_region, by = "geo") %>% 
  filter(!(is.na(values))) %>% 
  mutate(variable = as.factor(variable))


## ------ 5. SUMMARY: Discharge data by Category and Country/Region -----

### Country

sum_hosp_list_long_c = df_hosp_list_long_c %>% 
  mutate(Mins = map(DataLongCountry, ~.x %>% 
                      count(icd10, wt = min(values, na.rm = TRUE)) %>% 
                      rename(Min = n))) %>% 
  mutate(Medians = map(DataLongCountry, ~.x %>% 
                         count(icd10, wt = median(values, na.rm = TRUE)) %>% 
                         rename(Median = n))) %>% 
  mutate(Means = map(DataLongCountry, ~.x %>% 
                       count(icd10, wt = mean(values, na.rm = TRUE)) %>% 
                       rename(Mean = n))) %>% 
  mutate(Maxs = map(DataLongCountry, ~.x %>% 
                      count(icd10, wt = max(values, na.rm = TRUE)) %>% 
                      rename(Max = n))) %>% 
  mutate(SD = map(DataLongCountry, ~.x %>% 
                    count(icd10, wt = sd(values, na.rm = TRUE)) %>% 
                    rename(Standard_Deviation = n))) %>% 
  select(-DataLongCountry)



sum_hosp_list_long_country = sum_hosp_list_long_c %>% 
  unnest %>% 
  select_if(!(str_detect(names(.), "icd10."))) %>% 
  arrange(-Median)



### Region


sum_hosp_list_long_r = df_hosp_list_long_r %>% 
  mutate(Mins = map(DataLongRegion, ~.x %>% 
                      count(icd10, wt = min(values, na.rm = TRUE)) %>% 
                      rename(Min = n))) %>% 
  mutate(Medians = map(DataLongRegion, ~.x %>% 
                         count(icd10, wt = median(values, na.rm = TRUE)) %>% 
                         rename(Median = n))) %>% 
  mutate(Means = map(DataLongRegion, ~.x %>% 
                       count(icd10, wt = mean(values, na.rm = TRUE)) %>% 
                       rename(Mean = n))) %>% 
  mutate(Maxs = map(DataLongRegion, ~.x %>% 
                      count(icd10, wt = max(values, na.rm = TRUE)) %>% 
                      rename(Max = n))) %>% 
  mutate(SD = map(DataLongRegion, ~.x %>% 
                    count(icd10, wt = sd(values, na.rm = TRUE)) %>% 
                    rename(Standard_Deviation = n))) %>% 
  select(-DataLongRegion)



sum_hosp_list_long_region = sum_hosp_list_long_r %>% 
  unnest %>% 
  select_if(!(str_detect(names(.), "icd10."))) %>% 
  arrange(-Median)
