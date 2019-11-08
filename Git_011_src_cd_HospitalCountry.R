

### -------- 1. COUNTRY data - Leftjoining Variables Supplementaires !!! ---------------

hosp_list_nest_totpop_geo_c = hosp_list_nest_totpop_geo %>% 
  select(Dataframe, CountryTotalWide)


hosp_list_nest_totpop_geo_c = hosp_list_nest_totpop_geo_c %>% 
  mutate(CountryTotal_Merged = map(CountryTotalWide, 
                                   ~ (.x %>% 
                                        left_join(pop_str_cl_17_wide_c,
                                                  by = "geo"))),
         CountryTotal_Merged = map(CountryTotal_Merged, 
                                   ~ (.x %>% 
                                        left_join(pop_str_gr_17_wide_c %>% 
                                                    select(geo, "PC_Y60_64", "PC_Y65_69", "PC_Y70_74"),
                                                  by = "geo"))),
         
         CountryTotal_Merged = map(CountryTotal_Merged, 
                                   ~ (.x %>% 
                                        left_join(gdp_cap_old_c,
                                                  by = "geo"))),
         CountryTotal_Merged = map(CountryTotal_Merged, 
                                   ~ (.x %>% 
                                        left_join(hosp_beds17_c,
                                                  by = "geo"))),
         CountryTotal_Merged = map(CountryTotal_Merged, 
                                   ~ (.x %>% 
                                        left_join(health_pers17_c,
                                                  by = "geo"))),
         CountryTotal_Merged = map(CountryTotal_Merged, 
                                   ~ (.x %>% 
                                        left_join(health_pers_hosp17_HC_wide_c,
                                                  by = "geo"))),
         CountryTotal_Merged = map(CountryTotal_Merged, 
                                   ~ (.x %>% 
                                        left_join(health_grad17_wide_c %>% 
                                                    select(-OC221, -OC3221),
                                                  by = "geo")))) 

data_names_c = hosp_list_nest_totpop_geo_c %>% pull(Dataframe)


### Alternative BATCH looping: Imputation function - Bacth adapte qui FONCTIONNE!!!

c_fun_PCA_imp_nat = function(data){
  
  # ALternative: SINGLE Procedure
  Imp = hosp_list_nest_totpop_geo_c %>%  
    filter(Dataframe  == data) %>% 
    select(CountryTotal_Merged) %>% 
    unnest() %>% 
    select(-1) %>% 
    # select(-O) %>% # not taken out???
    as.data.frame() 
  
  rownames(Imp) = hosp_list_nest_totpop_geo_c %>%  
    filter(Dataframe  == data) %>% 
    select(CountryTotal_Merged) %>% 
    unnest() %>% 
    select(1) %>%
    pull()
  
   
  comp <- imputePCA(Imp, ncp=2, scale=TRUE) ## Compl?te le tableau
  
  # Adding Countries to imputed table
  data_imp = comp$completeObs
  
} 


PCA_imp_nat = lapply(data_names_c, c_fun_PCA_imp_nat)


# male - nat for country/national!!!
PCA_imp_nat_male = PCA_imp_nat %>% 
  keep(~length(colnames(.x)) == 33)

# no male
PCA_imp_nat_no_male = PCA_imp_nat %>% 
  keep(~length(colnames(.x)) > 33)


c_fun_PCA = function(data){
  
  ### PCA function
  res.pca <- PCA(data,
                 quanti.sup = 15:34)
  
  
}


##### ----- 2. NO_MALE PCA for Countries ----

### No male = Total and female : PCA depending on 1,3,4,6,7,9
c_pca_data_no_male = map(PCA_imp_nat_no_male, c_fun_PCA)

### PCA describe dim12 function
c_fun_pca_dim12 = function(pca){
  
  dimdesc(c_pca_data_no_male[[pca]], axes = 1:2)
  
}


totfem_dim12_nat = map(c(1:6), c_fun_pca_dim12) %>% 
  set_names(data_names_c[c(1,3,4,6,7,9)]) %>%
  enframe() %>% 
  unnest(value) %>% 
  rename(Dataframe = name,
         Dimensions12 = value)

### PCA describe dim34 function
c_fun_pca_dim34 = function(pca){
  
  dimdesc(c_pca_data_no_male[[pca]], axes = 3:4)
  
}


totfem_dim34_nat = map(c(1:6), c_fun_pca_dim34) %>% 
  set_names(data_names_c[c(1,3,4,6,7,9)]) %>%
  enframe() %>% 
  unnest(value) %>% 
  rename(Dataframe = name,
         Dimensions34 = value)


res1.hcpc <- HCPC(c_pca_data_no_male[[1]],
                  nb.clust=6,
                  kk = Inf,
                  graph=F,
                  consol=F)

res2.hcpc <- HCPC(c_pca_data_no_male[[2]],
                  nb.clust=5,
                  kk = Inf,
                  graph=F,
                  consol=F)

res3.hcpc <- HCPC(c_pca_data_no_male[[3]],
                  nb.clust=6,
                  kk = Inf,
                  graph=F,
                  consol=F)

res4.hcpc <- HCPC(c_pca_data_no_male[[4]],
                  nb.clust=6,
                  kk = Inf,
                  graph=F,
                  consol=F)

res5.hcpc <- HCPC(c_pca_data_no_male[[5]],
                  nb.clust=6,
                  kk = Inf,
                  graph=F,
                  consol=F)

res6.hcpc <- HCPC(c_pca_data_no_male[[6]],
                  nb.clust=7,
                  kk = Inf,
                  graph=F,
                  consol=F)



### PCA function
c_fun_PCA = function(data){
  
  res.pca <- PCA(data,
                 quanti.sup = 14:33)
}

c_pca_data_male = map(PCA_imp_nat_male, c_fun_PCA)

### PCA describe dim12 function
c_fun_pca_dim12 = function(pca){
  
  dimdesc(c_pca_data_male[[pca]], axes = 1:2)
  
}


male_dim12_nat = map(c(1:3), c_fun_pca_dim12) %>% 
  set_names(data_names_c[c(2,5,8)]) %>%
  enframe() %>% 
  unnest(value) %>% 
  rename(Dataframe = name,
         Dimensions12 = value)


### PCA describe dim34 function
c_fun_pca_dim34 = function(pca){
  
  dimdesc(c_pca_data_male[[pca]], axes = 3:4)
  
}


male_dim34_nat = map(c(1:3), c_fun_pca_dim34) %>% 
  set_names(data_names_c[c(2,5,8)]) %>%
  enframe() %>% 
  unnest(value) %>% 
  rename(Dataframe = name,
         Dimensions34 = value)


res7.hcpc <- HCPC(c_pca_data_male[[1]], 
                  nb.clust=5, 
                  kk = Inf, 
                  graph=F, 
                  consol=F)

res8.hcpc <- HCPC(c_pca_data_male[[2]], 
                  nb.clust=6, 
                  kk = Inf, 
                  graph=F, 
                  consol=F)



res9.hcpc <- HCPC(c_pca_data_male[[3]], 
                  nb.clust=6, 
                  kk = Inf, 
                  graph=F, 
                  consol=F)


res.pca_list_c = list(res1.hcpc, 
                      res7.hcpc, 
                      res2.hcpc, 
                      res3.hcpc, 
                      res8.hcpc, 
                      res4.hcpc, 
                      res5.hcpc, 
                      res9.hcpc, 
                      res6.hcpc)
