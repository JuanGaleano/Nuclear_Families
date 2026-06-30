library(tidyverse)
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(countrycode)

options(scipen=999)



# Load GLAD #####
load("G:\\Shared drives\\CORESIDENCE\\WP2_DATA\\2_4_CORESIDENCE_DATABASE\\CORESIDENCE_GLAD/CORESIDENCE_GLAD_2025.Rda")

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
    # mutate(LAI=if_else(LAI=="520000007","500000000",LAI))|>
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
rm(af_glad)
rm(data_list_glad)
rm(la_glad)
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
  
  # # LA SEX
  # df_LA<-df|>
  #   mutate(LA=substr(LAI,1,1))|>
  #   group_by(CONTINENT,SAMPLE,AGE2, SEX,EDATTAIN2,LA)|>
  #   summarise(POPW=sum(POPW))|>
  #   ungroup()
  
  
  df_LAT<-df|>
    # mutate(LAI=if_else(LAI=="520000007","500000000",LAI))|>
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

as_gradient <- data.table::rbindlist(as_glad)

rm(as_glad)
rm(data_list_glad)

gc()

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
eu_gradient <- data.table::rbindlist(eu_glad)

rm(eu_glad)
rm(data_list_glad)
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


library(dplyr)
library(tidyr)


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


df<-wide

df<-df|>filter(region_group!="Oceania")

df<-df|>
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


df <- df %>%
  mutate(POPCOUNTRY = rowSums(select(., POPW_H, POPW_M,POPW_L), na.rm = TRUE))

# Compute weighted mean of gradient by region
plot_df <- df %>%
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

# Plot #######

ggplot(plot_df, aes(x = region_group, y = weighted_gradient, fill = sign)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey50") +
  geom_col(width = 0.7, color="black", linewidth = .25) +
  coord_flip() +
  scale_fill_manual(values=c("#b2182b","#2166ac"))+
  labs(
    x = NULL,
    y = "\nPopulation-weighted mean gradient",
    fill = "Gradient"
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Save #######
ggsave(paste("G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\cherlin\\complexity\\1_plots_article\\",
             "FIG_2.png",sep=""), 
       scale =1,
       height = 8,
       width=12, 
       dpi = 300) 







