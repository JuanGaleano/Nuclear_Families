library(tidyverse)
library(haven)
library(readxl)
library(scales)
library(countrycode)

options(scipen=999)

# Load GLAD #####
load("CORESIDENCE_GLAD_2025.Rda")

# Extract harmonized dataset IPUMS + EU-LFS #####
harmo<-GLAD[["HARMONIZED"]]

# Functions #####
calculate_gradients <- function(data) {
  data |>
    select(SAMPLE, EDATTAIN2, prop, rate, POPW) |>
    tidyr::pivot_wider(
      names_from  = EDATTAIN2,
      values_from = c(prop, rate, POPW)
    ) |>
    mutate(
      rate_HM = (rate_H * prop_H + rate_M * prop_M) / (prop_H + prop_M),
      rate_LM = (rate_L * prop_L + rate_M * prop_M) / (prop_L + prop_M),
      
      G_hl = rate_H - rate_L,
      
      G_cond = if_else(
        prop_L > 0.5,
        rate_HM - rate_L,
        rate_H  - rate_LM
      ),
      
      rule_applied = if_else(prop_L > 0.5, "(H+M)-L", "H-(M+L)"),
      
      sign_change  = sign(G_hl) != sign(G_cond),
      flag_tiny_HM = (prop_H + prop_M) < 0.05
    )
}
`%notin%` <- Negate(`%in%`)

# LATINAMERICA ######
la_samples <- c(
  "ARG_1970_IPUMS", "ARG_2001_IPUMS", "BOL_1976_IPUMS", "BOL_2012_IPUMS",
  "BRA_1970_IPUMS", "BRA_2010_IPUMS", "CHL_1970_IPUMS", "CHL_2002_IPUMS",
  "COL_1973_IPUMS", "COL_2005_IPUMS", "CRI_1973_IPUMS", "CRI_2011_IPUMS",
  "CUB_2002_IPUMS", "CUB_2012_IPUMS", "DOM_1981_IPUMS", "DOM_2010_IPUMS",
  "ECU_1982_IPUMS", "ECU_2010_IPUMS", "SLV_1992_IPUMS", "SLV_2007_IPUMS",
  "GTM_1973_IPUMS", "GTM_2002_IPUMS", "HTI_1971_IPUMS", "HTI_2003_IPUMS",
  "HND_1974_IPUMS", "HND_2001_IPUMS", "JAM_1982_IPUMS", "JAM_2001_IPUMS",
  "MEX_1990_IPUMS", "MEX_2020_IPUMS", "NIC_1971_IPUMS", "NIC_2005_IPUMS",
  "PAN_1970_IPUMS", "PAN_2010_IPUMS", "PRY_1972_IPUMS", "PRY_2002_IPUMS",
  "PER_1993_IPUMS", "PER_2017_IPUMS", "PRI_1970_IPUMS", "PRI_2020_IPUMS",
  "TTO_1970_IPUMS", "TTO_2011_IPUMS", "USA_1970_IPUMS", "USA_2020_IPUMS",
  "URY_1975_IPUMS", "URY_2011_IPUMS", "VEN_1971_IPUMS", "VEN_2001_IPUMS"
)

la_df<-harmo|>filter(SAMPLE %in%la_samples )

# Split data by country ######

data_list_glad <- split(la_df, f = la_df$SAMPLE)

la_glad<-lapply(data_list_glad, function(df) {
  
  df<-df|>
    mutate(
      AGE2= if_else(AGE == "30-34"| AGE=="35-39","30-39", AGE),
      EDATTAIN2=ifelse(EDATTAIN=="9", "L",EDATTAIN)
    )|>
    filter(AGE2=="30-39")|>
    group_by(CONTINENT,SAMPLE, EDATTAIN2, AGE2,LAI)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  
  df_LAT<-df|>
    mutate(LAT=substr(LAI,1,2))|>
    group_by(CONTINENT,SAMPLE,AGE2,EDATTAIN2,LAT)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  df_LAT_NUCLEAR<-df_LAT|>
    filter(LAT %in% c(50,51,52))|>
    mutate(LAT2=ifelse(LAT%in% c(50), 5, 6))|>
    group_by(SAMPLE,EDATTAIN2,LAT2)|>
    summarise(POPW=sum(POPW))|>
    ungroup()|>
    group_by(SAMPLE,EDATTAIN2)|>
    mutate(n_rel=POPW/sum(POPW))|>
    ungroup()|>
    filter(LAT2==5)|>
    mutate(id=paste(SAMPLE, EDATTAIN2,sep="_"))|>
    select(id, n_rel)
  
  aver<-df_LAT|>
    filter(LAT %in% c(50,51,52))|>
    mutate(LAT2=ifelse(LAT%in% c(50), 5, 6),
           EDATTAIN2=as.factor(EDATTAIN2),
           EDATTAIN2=fct_relevel(EDATTAIN2,
                                 "L","M","H"))|>
    filter(LAT2==5)|>
    group_by(SAMPLE,EDATTAIN2)|>
    summarise(POPW=sum(POPW))|>
    ungroup()|>
    group_by(SAMPLE)|>
    mutate(n_rel=POPW/sum(POPW))|>
    ungroup()|>
    mutate(id=paste(SAMPLE, EDATTAIN2,sep="_"))|>
    left_join(df_LAT_NUCLEAR, by="id")#|>
  #filter(EDATTAIN2!="M")
  
  colnames(aver)[4]<-"prop"
  colnames(aver)[6]<-"rate"
  
  aver <- aver %>%
    complete(
      nesting(SAMPLE), 
      EDATTAIN2 = c("L", "M", "H"), 
      fill = list(POPW = 0, prop = 0, rate = 0)
    ) %>%
    mutate(id = if_else(is.na(id), paste(SAMPLE, EDATTAIN2, sep = "_"), id))
  
})

la_gradient <- data.table::rbindlist(la_glad)
rm(data_list_glad)
rm(la_df)
rm(la_glad)
# AFRICA ########

af_samples <- c(
  "BWA_1981_IPUMS", "BWA_2011_IPUMS", "CMR_1976_IPUMS", "CMR_2005_IPUMS",
  "BEN_1979_IPUMS", "BEN_2013_IPUMS", "ETH_1984_IPUMS", "ETH_2007_IPUMS",
  "GHA_2000_IPUMS", "GHA_2010_IPUMS", "GIN_1996_IPUMS", "GIN_2014_IPUMS",
  "CIV_1988_IPUMS", "CIV_1998_IPUMS", "KEN_1989_IPUMS", "KEN_2009_IPUMS",
  "LSO_1996_IPUMS", "LSO_2006_IPUMS", "MWI_1987_IPUMS", "MWI_2008_IPUMS",
  "MLI_1987_IPUMS", "MLI_2009_IPUMS", "MUS_1990_IPUMS", "MUS_2011_IPUMS",
  "MAR_1982_IPUMS", "MAR_2014_IPUMS", "MOZ_1997_IPUMS", "MOZ_2007_IPUMS",
  "NGA_2006_IPUMS", "NGA_2010_IPUMS", "RWA_2002_IPUMS", "RWA_2012_IPUMS",
  "SEN_1988_IPUMS", "SEN_2013_IPUMS", "SLE_2004_IPUMS", "SLE_2015_IPUMS",
  "ZAF_1996_IPUMS", "ZAF_2011_IPUMS", "TGO_1970_IPUMS", "TGO_2010_IPUMS",
  "UGA_1991_IPUMS", "UGA_2014_IPUMS", "EGY_1986_IPUMS", "EGY_2006_IPUMS",
  "TZA_1988_IPUMS", "TZA_2012_IPUMS", "BFA_1996_IPUMS", "BFA_2006_IPUMS",
  "ZMB_1990_IPUMS", "ZMB_2010_IPUMS"
)

af_df<-harmo|>filter(SAMPLE %in%af_samples )

# Split data by country ######

data_list_glad <- split(af_df, f = af_df$SAMPLE)


af_glad<-lapply(data_list_glad, function(df) {
  
  df<-df|>
    mutate(
      AGE2= if_else(AGE == "30-34"| AGE=="35-39","30-39", AGE),
      EDATTAIN2=ifelse(EDATTAIN=="9", "L",EDATTAIN)
    )|>
    filter(AGE2=="30-39")|>
    group_by(CONTINENT,SAMPLE, EDATTAIN2, AGE2,LAI)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  df_LAT<-df|>
    mutate(LAT=substr(LAI,1,2))|>
    group_by(CONTINENT,SAMPLE,AGE2,EDATTAIN2,LAT)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  df_LAT_NUCLEAR<-df_LAT|>
    filter(LAT %in% c(50,51,52))|>
    mutate(LAT2=ifelse(LAT%in% c(50), 5, 6))|>
    group_by(SAMPLE,EDATTAIN2,LAT2)|>
    summarise(POPW=sum(POPW))|>
    ungroup()|>
    group_by(SAMPLE,EDATTAIN2)|>
    mutate(n_rel=POPW/sum(POPW))|>
    ungroup()|>
    filter(LAT2==5)|>
    mutate(id=paste(SAMPLE, EDATTAIN2,sep="_"))|>
    select(id, n_rel)
  
  aver<-df_LAT|>
    filter(LAT %in% c(50,51,52))|>
    mutate(LAT2=ifelse(LAT%in% c(50), 5, 6),
           EDATTAIN2=as.factor(EDATTAIN2),
           EDATTAIN2=fct_relevel(EDATTAIN2,
                                 "L","M","H"))|>
    filter(LAT2==5)|>
    group_by(SAMPLE,EDATTAIN2)|>
    summarise(POPW=sum(POPW))|>
    ungroup()|>
    group_by(SAMPLE)|>
    mutate(n_rel=POPW/sum(POPW))|>
    ungroup()|>
    mutate(id=paste(SAMPLE, EDATTAIN2,sep="_"))|>
    left_join(df_LAT_NUCLEAR, by="id")#|>
  #filter(EDATTAIN2!="M")
  
  colnames(aver)[4]<-"prop"
  colnames(aver)[6]<-"rate"
  
  aver <- aver %>%
    complete(
      nesting(SAMPLE), 
      EDATTAIN2 = c("L", "M", "H"), 
      fill = list(POPW = 0, prop = 0, rate = 0)
    ) %>%
    mutate(id = if_else(is.na(id), paste(SAMPLE, EDATTAIN2, sep = "_"), id))
  
  
})
af_gradient <- data.table::rbindlist(af_glad)
rm(data_list_glad)
rm(af_df)
rm(af_glad)
gc()

# ASIA ########
as_samples <- c(
  "ARM_2001_IPUMS", "ARM_2011_IPUMS", "KHM_1998_IPUMS", "KHM_2019_IPUMS",
  "CHN_1982_IPUMS", "CHN_2000_IPUMS", "FJI_1986_IPUMS", "FJI_2014_IPUMS",
  "PSE_1997_IPUMS", "PSE_2017_IPUMS", "IND_1983_IPUMS", "IND_2009_IPUMS",
  "IDN_1971_IPUMS", "IDN_2010_IPUMS", "IRN_2006_IPUMS", "IRN_2011_IPUMS",
  "ISR_1972_IPUMS", "ISR_1995_IPUMS", "KGZ_1999_IPUMS", "KGZ_2009_IPUMS",
  "LAO_1995_IPUMS", "LAO_2015_IPUMS", "MYS_1970_IPUMS", "MYS_2000_IPUMS",
  "MNG_1989_IPUMS", "MNG_2000_IPUMS", "NPL_2001_IPUMS", "NPL_2011_IPUMS",
  "PAK_1973_IPUMS", "PAK_1998_IPUMS", "PHL_1990_IPUMS", "PHL_2010_IPUMS",
  "VNM_1989_IPUMS", "VNM_2019_IPUMS", "THA_1970_IPUMS", "THA_2000_IPUMS",
  "TUR_1985_IPUMS", "TUR_2000_IPUMS"
)

as_df<-harmo|>filter(SAMPLE %in%as_samples )

# Split data by country ######

data_list_glad <- split(as_df, f = as_df$SAMPLE)

as_glad<-lapply(data_list_glad, function(df) {
  
  df<-df|>
    mutate(
      AGE2= if_else(AGE == "30-34"| AGE=="35-39","30-39", AGE),
      EDATTAIN2=ifelse(EDATTAIN=="9", "L",EDATTAIN)
    )|>
    filter(AGE2=="30-39")|>
    group_by(CONTINENT,SAMPLE, EDATTAIN2, AGE2,LAI)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  # Total pop ####
  total_pop<-df|>group_by(SAMPLE)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  
  df_LAT<-df|>
    mutate(LAT=substr(LAI,1,2))|>
    group_by(CONTINENT,SAMPLE,AGE2,EDATTAIN2,LAT)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  df_LAT_NUCLEAR<-df_LAT|>
    filter(LAT %in% c(50,51,52))|>
    mutate(LAT2=ifelse(LAT%in% c(50), 5, 6))|>
    group_by(SAMPLE,EDATTAIN2,LAT2)|>
    summarise(POPW=sum(POPW))|>
    ungroup()|>
    group_by(SAMPLE,EDATTAIN2)|>
    mutate(n_rel=POPW/sum(POPW))|>
    ungroup()|>
    filter(LAT2==5)|>
    mutate(id=paste(SAMPLE, EDATTAIN2,sep="_"))|>
    select(id, n_rel)
  
  aver<-df_LAT|>
    filter(LAT %in% c(50,51,52))|>
    mutate(LAT2=ifelse(LAT%in% c(50), 5, 6),
           EDATTAIN2=as.factor(EDATTAIN2),
           EDATTAIN2=fct_relevel(EDATTAIN2,
                                 "L","M","H"))|>
    filter(LAT2==5)|>
    group_by(SAMPLE,EDATTAIN2)|>
    summarise(POPW=sum(POPW))|>
    ungroup()|>
    group_by(SAMPLE)|>
    mutate(n_rel=POPW/sum(POPW))|>
    ungroup()|>
    mutate(id=paste(SAMPLE, EDATTAIN2,sep="_"))|>
    left_join(df_LAT_NUCLEAR, by="id")
  
  colnames(aver)[4]<-"prop"
  colnames(aver)[6]<-"rate"
  
  aver <- aver %>%
    complete(
      nesting(SAMPLE), 
      EDATTAIN2 = c("L", "M", "H"), 
      fill = list(POPW = 0, prop = 0, rate = 0)
    ) %>%
    mutate(id = if_else(is.na(id), paste(SAMPLE, EDATTAIN2, sep = "_"), id))
  
})

as_gradient <- data.table::rbindlist(as_glad)

rm(data_list_glad)
rm(as_df)
rm(as_glad)

# EUROPE ######

eu_samples <- c(
  "AUT_1971_IPUMS", "AUT_2015_LFS",   "BEL_2000_LFS",   "BEL_2010_LFS",  
  "BGR_2005_LFS",   "BGR_2015_LFS",   "BLR_1999_IPUMS", "BLR_2009_IPUMS",
  "CHE_1970_IPUMS", "CHE_2000_IPUMS", "CYP_2000_LFS",   "CYP_2015_LFS",  
  "CZE_2000_LFS",   "CZE_2015_LFS",   "DEU_2005_LFS",   "DEU_2015_LFS",  
  "ESP_1991_IPUMS", "ESP_2015_LFS",   "EST_2000_LFS",   "EST_2015_LFS",  
  "FRA_1962_IPUMS", "FRA_2015_LFS",   "GRC_2000_LFS",   "GRC_2015_LFS",  
  "HRV_2005_LFS",   "HRV_2015_LFS",   "HUN_1980_IPUMS", "HUN_2015_LFS",  
  "ITA_2000_LFS",   "ITA_2015_LFS",   "LTU_2005_LFS",   "LTU_2015_LFS",  
  "LUX_2000_LFS",   "LUX_2010_LFS",   "LVA_2010_LFS",   "LVA_2015_LFS",  
  "NLD_2000_LFS",   "NLD_2015_LFS",   "POL_2002_IPUMS", "POL_2015_LFS",  
  "PRT_1981_IPUMS", "PRT_2015_LFS",   "ROU_1977_IPUMS", "ROU_2015_LFS",  
  "RUS_2002_IPUMS", "RUS_2010_IPUMS", "SVK_2000_LFS",   "SVK_2015_LFS",  
  "SVN_2000_LFS",   "SVN_2015_LFS"
)

eu_df<-harmo|>filter(SAMPLE %in%eu_samples )

# Split data by country ######

data_list_glad <- split(eu_df, f = eu_df$SAMPLE)

eu_glad<-lapply(data_list_glad, function(df) {
  
  df<-df|>
    mutate(
      AGE2= if_else(AGE == "30-34"| AGE=="35-39","30-39", AGE),
      EDATTAIN2=ifelse(EDATTAIN=="9", "L",EDATTAIN),
      EDATTAIN2=ifelse(is.na(EDATTAIN2), "L",EDATTAIN2)
    )|>
    filter(AGE2=="30-39")|>
    group_by(CONTINENT,SAMPLE, EDATTAIN2, AGE2,LAI)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  df_LAT<-df|>
    mutate(LAT=substr(LAI,1,2))|>
    group_by(CONTINENT,SAMPLE,AGE2,EDATTAIN2,LAT)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  df_LAT_NUCLEAR<-df_LAT|>
    filter(LAT %in% c(50,51,52))|>
    mutate(LAT2=ifelse(LAT%in% c(50), 5, 6))|>
    group_by(SAMPLE,EDATTAIN2,LAT2)|>
    summarise(POPW=sum(POPW))|>
    ungroup()|>
    group_by(SAMPLE,EDATTAIN2)|>
    mutate(n_rel=POPW/sum(POPW))|>
    ungroup()|>
    filter(LAT2==5)|>
    mutate(id=paste(SAMPLE, EDATTAIN2,sep="_"))|>
    select(id, n_rel)
  
  aver<-df_LAT|>
    filter(LAT %in% c(50,51,52))|>
    mutate(LAT2=ifelse(LAT%in% c(50), 5, 6),
           EDATTAIN2=as.factor(EDATTAIN2),
           EDATTAIN2=fct_relevel(EDATTAIN2,
                                 "L","M","H"))|>
    filter(LAT2==5)|>
    group_by(SAMPLE,EDATTAIN2)|>
    summarise(POPW=sum(POPW))|>
    ungroup()|>
    group_by(SAMPLE)|>
    mutate(n_rel=POPW/sum(POPW))|>
    ungroup()|>
    mutate(id=paste(SAMPLE, EDATTAIN2,sep="_"))|>
    left_join(df_LAT_NUCLEAR, by="id")
  
  colnames(aver)[4]<-"prop"
  colnames(aver)[6]<-"rate"
  
  aver <- aver %>%
    complete(
      nesting(SAMPLE), 
      EDATTAIN2 = c("L", "M", "H"), 
      fill = list(POPW = 0, prop = 0, rate = 0)
    ) %>%
    mutate(id = if_else(is.na(id), paste(SAMPLE, EDATTAIN2, sep = "_"), id))
  
  
})
eu_gradient <- data.table::rbindlist(eu_glad)

rm(data_list_glad)
rm(eu_df)
rm(eu_glad)
gc()

# GRADIENTS ######

for_gradients<-bind_rows(as_gradient,
                         af_gradient,
                         la_gradient,
                         eu_gradient)


for_gradients<-for_gradients|>mutate(CNTRY=substr(SAMPLE, 1,3),
                                     YEAR=substr(SAMPLE, 5, 8))

for_gradients_t1 <- for_gradients %>%
  group_by(CNTRY) %>%
  # 1. Filter to keep only the earliest and latest available year for each country (as per original request)
  filter(
    YEAR == max(YEAR)
  ) %>%
  ungroup()


for_gradients_t1<- for_gradients_t1 %>%
  mutate(
    country_iso = CNTRY, # Your 3-letter code (e.g., ARG, USA)
    continent = countrycode(country_iso, origin = "iso3c", destination = "continent")
  )|>
  mutate(
    region_group = as.factor(case_when(
      continent == "Africa" & CNTRY %in% c("MAR", "EGY", "ISR", "PSE", "TUR","IRN","ARM","JOR","CYP") ~ "MENA",
      continent == "Asia" & CNTRY %in% c("MAR", "EGY", "ISR", "PSE", "TUR","IRN","ARM","JOR","CYP") ~ "MENA",
      continent == "Africa" ~ "Subsaharian Africa",
      continent == "Europe" ~ "Europe North America",
      continent == "Americas" & CNTRY %in% c("USA", "CAN") ~ "Europe North America",
      continent == "Americas" ~ "Latin America",
      continent == "Asia" & CNTRY %in% c("CHN") ~ "China",
      continent == "Asia" & CNTRY %in% c("PAK", "IND", "NPL", "BGD") ~ "South Asia",
      continent == "Asia" ~ "Southeast Asia",
      
      TRUE ~ as.character(continent))),
  )

wide <- calculate_gradients(for_gradients_t1) 


wide<-wide|>mutate(CNTRY=substr(SAMPLE, 1,3),
                   YEAR=substr(SAMPLE, 5, 8))

wide<- wide %>%
  mutate(
    country_iso = CNTRY, # Your 3-letter code (e.g., ARG, USA)
    continent = countrycode(country_iso, origin = "iso3c", destination = "continent")
  )|>
  mutate(
    region_group = case_when(
      continent == "Africa" & CNTRY %in% c("MAR", "EGY", "ISR", "PSE", "TUR","IRN","ARM","JOR","CYP") ~ "MENA",
      continent == "Asia" & CNTRY %in% c("MAR", "EGY", "ISR", "PSE", "TUR","IRN","ARM","JOR","CYP") ~ "MENA",
      continent == "Africa" ~ "Subsaharian Africa",
      continent == "Europe" ~ "Europe North America",
      continent == "Americas" & CNTRY %in% c("USA", "CAN") ~ "Europe North America",
      continent == "Americas" ~ "Latin America",
      continent == "Asia" & CNTRY %in% c("CHN") ~ "China",
      continent == "Asia" & CNTRY %in% c("PAK", "IND", "NPL", "BGD") ~ "South Asia",
      continent == "Asia" ~ "Southeast Asia",
      
      TRUE ~ as.character(continent)),
  )


wide<-wide|>filter(region_group!="Oceania")

wide<-wide|>
  mutate(region_group=ifelse(region_group=="Europe North America", "Europe and North America",
                             ifelse(region_group=="Subsaharian Africa", "Sub-Saharan Africa",region_group)),
         region_group=as.factor(region_group),
         region_group=fct_relevel(region_group,
                                  "Latin America",
                                  "South Asia",
                                  "Southeast Asia",
                                  "Sub-Saharan Africa",
                                  "MENA",
                                  "Europe and North America",
                                  "China"
         ))


wide <- wide %>%
  mutate(POPCOUNTRY = rowSums(select(., POPW_H, POPW_M,POPW_L), na.rm = TRUE))

# Compute weighted mean of gradient by region
plot_df <- wide %>%
  filter(!is.na(region_group), !is.na(G_cond), !is.na(POPCOUNTRY)) %>%
  group_by(region_group) %>%
  summarise(
    weighted_gradient = weighted.mean(G_cond, w = POPCOUNTRY, na.rm = TRUE),
    total_pop = sum(POPCOUNTRY, na.rm = TRUE),
    n_countries = n(),
    .groups = "drop"
  ) %>%
  arrange(weighted_gradient) %>%
  mutate(
    region_group = fct_inorder(region_group),
    sign = ifelse(weighted_gradient < 0, "Negative", "Positive")
  )


# LOGIT #####


for_gradients_t1 <- for_gradients %>%
  group_by(CNTRY) %>%
  # 1. Filter to keep only the earliest and latest available year for each country (as per original request)
  filter(
    YEAR == max(YEAR)
  ) %>%
  ungroup()


for_gradients_t1<- for_gradients_t1 %>%
  mutate(
    country_iso = CNTRY, # Your 3-letter code (e.g., ARG, USA)
    continent = countrycode(country_iso, origin = "iso3c", destination = "continent")
  )|>
  mutate(
    region_group = as.factor(case_when(
      continent == "Africa" & CNTRY %in% c("MAR", "EGY", "ISR", "PSE", "TUR","IRN","ARM","JOR","CYP") ~ "MENA",
      continent == "Asia" & CNTRY %in% c("MAR", "EGY", "ISR", "PSE", "TUR","IRN","ARM","JOR","CYP") ~ "MENA",
      continent == "Africa" ~ "Subsaharian Africa",
      continent == "Europe" ~ "Europe North America",
      continent == "Americas" & CNTRY %in% c("USA", "CAN") ~ "Europe North America",
      continent == "Americas" ~ "Latin America",
      continent == "Asia" & CNTRY %in% c("CHN") ~ "China",
      continent == "Asia" & CNTRY %in% c("PAK", "IND", "NPL", "BGD") ~ "South Asia",
      continent == "Asia" ~ "Southeast Asia",
      
      TRUE ~ as.character(continent)))
  )


wide <- for_gradients_t1 |>
  select(SAMPLE, EDATTAIN2, prop,rate, POPW)|>
  tidyr::pivot_wider(
    names_from  = EDATTAIN2,
    values_from = c(prop, rate,POPW)
  ) |>
  # weighted rate for combined groups
  mutate(
    rate_HM = (rate_H * prop_H + rate_M * prop_M) / (prop_H + prop_M),
    rate_LM = (rate_L * prop_L + rate_M * prop_M) / (prop_L + prop_M),
    
    # standard gradient
    G_hl = rate_H - rate_L,
    
    # conditional gradient
    G_cond = if_else(
      prop_L > 0.5,
      rate_HM - rate_L,    # (H+M) - L
      rate_H  - rate_LM    # H - (M+L)
    ),
    
    rule_applied = if_else(prop_L > 0.5, "(H+M)-L", "H-(M+L)"),
    
    # diagnostic flags
    sign_change  = sign(G_hl) != sign(G_cond),
    flag_tiny_HM = (prop_H + prop_M) < 0.05
  )


wide<-wide|>mutate(CNTRY=substr(SAMPLE, 1,3),
                   YEAR=substr(SAMPLE, 5, 8))

wide<- wide %>%
  mutate(
    country_iso = CNTRY, # Your 3-letter code (e.g., ARG, USA)
    continent = countrycode(country_iso, origin = "iso3c", destination = "continent")
  )|>
  mutate(
    region_group = case_when(
      continent == "Africa" & CNTRY %in% c("MAR", "EGY", "ISR", "PSE", "TUR","IRN","ARM","JOR","CYP") ~ "MENA",
      continent == "Asia" & CNTRY %in% c("MAR", "EGY", "ISR", "PSE", "TUR","IRN","ARM","JOR","CYP") ~ "MENA",
      continent == "Africa" ~ "Subsaharian Africa",
      continent == "Europe" ~ "Europe North America",
      continent == "Americas" & CNTRY %in% c("USA", "CAN") ~ "Europe North America",
      continent == "Americas" ~ "Latin America",
      continent == "Asia" & CNTRY %in% c("CHN") ~ "China",
      continent == "Asia" & CNTRY %in% c("PAK", "IND", "NPL", "BGD") ~ "South Asia",
      continent == "Asia" ~ "Southeast Asia",
      
      TRUE ~ as.character(continent))
  )


df<-wide

df<-df|>
  mutate(region_group=ifelse(region_group=="Oceania", "Southeast Asia",
                             ifelse(region_group=="Europe North America", "Europe and North America",
                                    ifelse(region_group=="Subsaharian Africa", "Sub-Saharan Africa",region_group))),
         region_group=as.factor(region_group),
         region_group=fct_relevel(region_group,
                                  "China",
                                  "Europe and North America",
                                  "Latin America",
                                  "MENA",
                                  "South Asia",
                                  "Southeast Asia",
                                  "Sub-Saharan Africa",
                                  
                                  
                                  
         ))




# ── helper functions ─────────────────────────────────────────────────────────
logit     <- function(p) log(p / (1 - p))
inv_logit <- function(x) exp(x) / (1 + exp(x))

# ── identify the two rates that actually went into G_cond ───────────────────
# when rule_applied == "(H+M)-L"  → focal rate = rate_HM, reference rate = rate_L
# when rule_applied == "H-(M+L)"  → focal rate = rate_H,  reference rate = rate_LM

df <- df |>
  mutate(
    rate_focal = if_else(rule_applied == "(H+M)-L", rate_HM, rate_H),
    rate_ref   = if_else(rule_applied == "(H+M)-L", rate_L,  rate_LM),
    
    logit_rate_focal = logit(rate_focal),
    logit_rate_ref    = logit(rate_ref)
  ) |>
  mutate(
    # raw absolute gradient — should reproduce your existing G_cond
    G_raw        = rate_focal - rate_ref,
    
    # gradient on logit scale (log odds ratio)
    G_logit      = logit_rate_focal - logit_rate_ref,
    
    # odds ratio (exponentiated log OR)
    odds_ratio   = exp(G_logit),
    
    # mean rate of the two groups — to contextualise compression
    mean_rate    = (rate_focal + rate_ref) / 2,
    
    # relative gradient: G as share of the reference group's rate
    G_relative   = G_raw / rate_ref,
    
    # rescaled G: adjusts for ceiling/floor compression
    max_possible = if_else(G_raw >= 0, 1 - rate_ref, rate_ref),
    G_rescaled   = G_raw / max_possible
  )

# ── verification: G_raw should equal your existing G_cond ───────────────────
df |>
  summarise(max_diff = max(abs(G_raw - G_cond), na.rm = TRUE))

# ── compare G_cond with its logit version ────────────────────────────────────
cor_check <- df |>
  summarise(
    spearman = cor(G_cond, G_logit, method = "spearman"),
    pearson  = cor(G_cond, G_logit, method = "pearson")
  )

print(cor_check)


df <- df |>
  mutate(country_name = countrycode(country_iso,
                                    origin      = "iso3c",
                                    destination = "country.name"))

df <- df %>%
  mutate(POPCOUNTRY = rowSums(select(., POPW_H, POPW_M,POPW_L), na.rm = TRUE))


# ── 2. apply Haldane-Anscombe correction to the three group rates ────────────
#     only adjusts rates that are exactly 0 or 1, using POPW_* as the
#     effective group size; leaves all other rates untouched
df <- df %>%
  mutate(
    rate_H_adj = case_when(
      rate_H == 0 ~ 0.5 / (POPW_H + 1),
      rate_H == 1 ~ 1 - 0.5 / (POPW_H + 1),
      TRUE        ~ rate_H
    ),
    rate_M_adj = case_when(
      rate_M == 0 ~ 0.5 / (POPW_M + 1),
      rate_M == 1 ~ 1 - 0.5 / (POPW_M + 1),
      TRUE        ~ rate_M
    ),
    rate_L_adj = case_when(
      rate_L == 0 ~ 0.5 / (POPW_L + 1),
      rate_L == 1 ~ 1 - 0.5 / (POPW_L + 1),
      TRUE        ~ rate_L
    )
  )

# ── 3. recompute the combined HM / LM rates using the corrected group rates ──
df <- df %>%
  mutate(
    rate_HM_adj = (rate_H_adj * prop_H + rate_M_adj * prop_M) / (prop_H + prop_M),
    rate_LM_adj = (rate_L_adj * prop_L + rate_M_adj * prop_M) / (prop_L + prop_M)
  )

# ── 4. apply the same conditional rule used for G_cond, but on adjusted rates─
df <- df %>%
  mutate(
    rate_focal_adj = if_else(rule_applied == "(H+M)-L", rate_HM_adj, rate_H_adj),
    rate_ref_adj   = if_else(rule_applied == "(H+M)-L", rate_L_adj,  rate_LM_adj),
    
    logit_rate_focal = logit(rate_focal_adj),
    logit_rate_ref   = logit(rate_ref_adj),
    
    G_logit    = logit_rate_focal - logit_rate_ref,
    odds_ratio = exp(G_logit)
  )

# ── 5. sanity check: confirm no remaining Inf/NaN, and see which rows were corrected ─
df %>%
  filter(rate_H == 0 | rate_H == 1 | rate_M == 0 | rate_M == 1 | rate_L == 0 | rate_L == 1) %>%
  select(SAMPLE, rate_H, rate_M, rate_L, flag_tiny_HM, G_logit)

df %>%
  filter(!is.finite(G_logit)) %>%
  select(SAMPLE, rate_focal_adj, rate_ref_adj, G_logit)
# should return zero rows

# ── 6. weighted regional estimates, G_cond (probability scale) ───────────────
plot_df <- df %>%
  filter(!is.na(region_group), !is.na(G_cond), !is.na(POPCOUNTRY)) %>%
  group_by(region_group) %>%
  summarise(
    weighted_gradient = weighted.mean(G_cond, w = POPCOUNTRY, na.rm = TRUE),
    total_pop   = sum(POPCOUNTRY, na.rm = TRUE),
    n_countries = n(),
    .groups = "drop"
  ) %>%
  arrange(weighted_gradient) %>%
  mutate(region_group = fct_inorder(region_group),
         sign = ifelse(weighted_gradient < 0, "Negative", "Positive"))

# ── 7. weighted regional estimates, G_logit (logit scale) ────────────────────
plot_df_logit <- df %>%
  filter(!is.na(region_group), !is.na(G_logit), !is.na(POPCOUNTRY)) %>%
  group_by(region_group) %>%
  summarise(
    weighted_gradient = weighted.mean(G_logit, w = POPCOUNTRY, na.rm = TRUE),
    total_pop   = sum(POPCOUNTRY, na.rm = TRUE),
    n_countries = n(),
    .groups = "drop"
  ) %>%
  arrange(weighted_gradient) %>%
  mutate(region_group = fct_inorder(region_group),
         sign = ifelse(weighted_gradient < 0, "Negative", "Positive"),
         weighted_OR = exp(weighted_gradient))

colnames(plot_df)[2]<-"Mean G"
colnames(plot_df_logit)[2]<-"Mean G log-odds"

table_s1<-bind_cols(plot_df[c(1,4,2)],plot_df_logit[c(2)])

