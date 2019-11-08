

### -------- 1. REGION data - Leftjoining Variables Supplementaires !!! ---------------

hosp_list_nest_totpop_geo_r = hosp_list_nest_totpop_geo %>% 
  select(Dataframe, RegionTotalWide)


hosp_list_nest_totpop_geo_r = hosp_list_nest_totpop_geo_r %>% 
  mutate(RegionTotal_Merged = map(RegionTotalWide, 
                                  ~ (.x %>% 
                                       left_join(pop_str_cl_17_wide_r,
                                                 by = "geo"))),
         RegionTotal_Merged = map(RegionTotal_Merged, 
                                  ~ (.x %>% 
                                       left_join(pop_str_gr_17_wide_r %>% 
                                                   select(geo, "PC_Y60_64", "PC_Y65_69", "PC_Y70_74"),
                                                 by = "geo"))),
         
         RegionTotal_Merged = map(RegionTotal_Merged, 
                                  ~ (.x %>% 
                                       left_join(gdp_cap_16_r,
                                                 by = "geo"))),
         RegionTotal_Merged = map(RegionTotal_Merged, 
                                  ~ (.x %>% 
                                       left_join(hosp_beds17_r %>% 
                                                   select(-facility) %>% 
                                                   rename(Hospital_Beds = values),
                                                 by = "geo"))),
         RegionTotal_Merged = map(RegionTotal_Merged, 
                                  ~ (.x %>% 
                                       left_join(health_pers17_r %>% 
                                                   rename(Health_Personel = values),
                                                 by = "geo"))))

### Adding a Country Category!!!
hosp_list_nest_totpop_geo_r_c = hosp_list_nest_totpop_geo_r %>% 
  mutate(RegionTotal_Country = map(RegionTotal_Merged, 
                                   ~ .x %>% 
                                     mutate(Country = str_sub(geo, 1, 2)) %>% 
                                     select(geo, Country, everything()))) %>% 
  select(Dataframe, RegionTotal_Country)


data_names_r = hosp_list_nest_totpop_geo_r_c %>% pull(Dataframe)


### Alternative BATCH looping: Imputation function - Bacth adapte qui FONCTIONNE!!!
### BONNE structure de SOURCE de donnees ICI !!!

r_fun_PCA_imp_reg = function(data){
  
  # ALternative: SINGLE Procedure
  Imp = hosp_list_nest_totpop_geo_r_c %>%  
    filter(Dataframe  == data) %>% 
    select(RegionTotal_Country) %>% 
    unnest() %>% 
    select(-1,-2) %>% 
    # select(-O) %>% # not taken out???
    as.data.frame() 
  
  rownames(Imp) = hosp_list_nest_totpop_geo_r_c %>%  
    filter(Dataframe  == data) %>% 
    select(RegionTotal_Country) %>% 
    unnest() %>% 
    select(1) %>%
    pull()
  
  comp <- imputePCA(Imp, ncp=2, scale=TRUE) ## Compl?te le tableau
  
  # Adding Countries to imputed table
  data_imp = cbind(comp$completeObs, hosp_list_nest_totpop_geo_r_c %>%  
                     filter(Dataframe  == data) %>% 
                     select(RegionTotal_Country) %>% 
                     unnest() %>% 
                     select(2))
} 


PCA_imp_reg = lapply(data_names_r, r_fun_PCA_imp_reg)

# Total/female Vs Male data (bco missing "O" column)
PCA_imp_reg_male = PCA_imp_reg %>% 
  keep(~length(colnames(.x)) == 22)

PCA_imp_reg_no_male = PCA_imp_reg %>% 
  keep(~length(colnames(.x)) > 22)


##### ----- 2. NO_MALE PCA for Regions ----

r_fun_PCA = function(data){
  
  ### PCA function
  res.pca <- PCA(data,
                 quanti.sup = 15:22,
                 quali.sup = 23)
  
  
}


### No male = Total and female : PCA depending on 1,3,4,6,7,9
r_pca_data_no_male = map(PCA_imp_reg_no_male, r_fun_PCA)


### PCA describe dim12 function
r_fun_pca_dim12 = function(pca){
  
  dimdesc(r_pca_data_no_male[[pca]], axes = 1:2)
  
}


totfem_dim12_reg = map(c(1:6), r_fun_pca_dim12) %>% 
  set_names(data_names_r[c(1,3,4,6,7,9)]) %>%
  enframe() %>% 
  unnest(value) %>% 
  rename(Dataframe = name,
         Dimensions12 = value)

### PCA describe dim34 function
r_fun_pca_dim34 = function(pca){
  
  dimdesc(r_pca_data_no_male[[pca]], axes = 3:4)
  
}


totfem_dim34_reg = map(c(1:6), r_fun_pca_dim34) %>% 
  set_names(data_names_r[c(1,3,4,6,7,9)]) %>%
  enframe() %>% 
  unnest(value) %>% 
  rename(Dataframe = name,
         Dimensions34 = value)


res11.hcpc <- HCPC(r_pca_data_no_male[[1]], 
                   nb.clust=8, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)



res22.hcpc <- HCPC(r_pca_data_no_male[[2]], 
                   nb.clust=6, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)


res33.hcpc <- HCPC(r_pca_data_no_male[[3]], 
                   nb.clust=5, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)

res44.hcpc <- HCPC(r_pca_data_no_male[[4]], 
                   nb.clust=5, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)

res55.hcpc <- HCPC(r_pca_data_no_male[[5]], 
                   nb.clust=7, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)


res66.hcpc <- HCPC(r_pca_data_no_male[[6]], 
                   nb.clust=7, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)


### PCA function
r_fun_PCA = function(data){
  
  res.pca <- PCA(data,
                 quanti.sup = 14:21,
                 quali.sup = 22)
}

r_pca_data_male = map(PCA_imp_reg_male, r_fun_PCA)


### PCA describe dim12 function
r_fun_pca_dim12 = function(pca){
  
  dimdesc(r_pca_data_male[[pca]], axes = 1:2)
  
}


male_dim12_reg = map(c(1:3), r_fun_pca_dim12) %>% 
  set_names(data_names_r[c(2,5,8)]) %>%
  enframe() %>% 
  unnest(value) %>% 
  rename(Dataframe = name,
         Dimensions12 = value)


### PCA describe dim34 function
r_fun_pca_dim34 = function(pca){
  
  dimdesc(r_pca_data_male[[pca]], axes = 3:4)
  
}

# map(c(1:3), r_fun_pca_dim34)

male_dim34_reg = map(c(1:3), r_fun_pca_dim34) %>% 
  set_names(data_names_r[c(2,5,8)]) %>%
  enframe() %>% 
  unnest(value) %>% 
  rename(Dataframe = name,
         Dimensions34 = value)


res77.hcpc <- HCPC(r_pca_data_male[[1]], 
                   nb.clust=6, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)

res88.hcpc <- HCPC(r_pca_data_male[[2]], 
                   nb.clust=5, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)


res99.hcpc <- HCPC(r_pca_data_male[[3]], 
                   nb.clust=6, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)


# Cluster Map
res.pca_list_r = list(res11.hcpc, 
                      res77.hcpc, 
                      res22.hcpc, 
                      res33.hcpc, 
                      res88.hcpc, 
                      res44.hcpc, 
                      res55.hcpc, 
                      res99.hcpc, 
                      res66.hcpc)
