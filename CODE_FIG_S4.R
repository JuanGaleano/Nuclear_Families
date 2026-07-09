library(tidyverse)
library(haven)
library(data.table)

options(scipen=999)

base_path <- "./folder with ipums samples as Rda files/"

samples <- c(
  "BRA_2010_IPUMS",
  "CHL_2002_IPUMS",
  "CUB_2012_IPUMS",
  "DOM_2010_IPUMS",
  "MEX_2020_IPUMS",
  "PRI_2020_IPUMS", 
  "TTO_2011_IPUMS",
  "URY_2011_IPUMS",
  "GTM_2002_IPUMS",
  "JAM_2001_IPUMS",
  "PRY_2002_IPUMS",
  "USA_2020_IPUMS",
  "BWA_2011_IPUMS",
  "CMR_2005_IPUMS",
  "GHA_2010_IPUMS",
  "GIN_2014_IPUMS",
  "MOZ_2007_IPUMS",
  "SLE_2015_IPUMS", 
  "ZAF_2011_IPUMS",
  "UGA_2014_IPUMS",
  "ZMB_2010_IPUMS",
  "IDN_2010_IPUMS",
  "PHL_2010_IPUMS"
)

file_paths <- file.path(base_path, paste0(samples, ".Rda"))

data_list <- map(file_paths, function(path) {
  env <- new.env()
  load(path, envir = env)
  as.list(env)[[1]]
})

names(data_list) <- samples

data_list<-lapply(data_list, function(df) {
  # Convert dataframe to data.table in place for maximum performance
  df<-setDT(df)
  
  # Efficiently evaluate group-by condition and assign new column
  df<-df[, step := as.integer(any(RELATED == 3300)), by = SERIAL]
  
  
})

combined_glad<-lapply(data_list, function(df) {
  
  df<-df|>
    mutate(
      AGE2= if_else(AGE2 == "30-34"| AGE2=="35-39","30-39", AGE2),
      EDATTAIN2=ifelse(EDATTAIN2=="9", "L",EDATTAIN2)
    )|>
    filter(AGE2=="30-39")|>
    group_by(CONTINENT,SAMPLE, EDATTAIN2, AGE2,LAI)|>
    summarise(POPW=sum(PWEIGHT))|>
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
    left_join(df_LAT_NUCLEAR, by="id")#|>
  #  filter(EDATTAIN2!="M")
  
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
combined_gradient <- data.table::rbindlist(combined_glad)

combined_glad1<-lapply(data_list, function(df) {
  
  df<-df|>
    mutate(
      AGE2= if_else(AGE2 == "30-34"| AGE2=="35-39","30-39", AGE2),
      EDATTAIN2=ifelse(EDATTAIN2=="9", "L",EDATTAIN2)
    )|>
    filter(AGE2=="30-39")|>
    group_by(CONTINENT,SAMPLE, EDATTAIN2, AGE2,LAI, step)|>
    summarise(POPW=sum(PWEIGHT))|>
    ungroup()
  
  # Total pop ####
  total_pop<-df|>group_by(SAMPLE)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  df_LAT<-df|>
    mutate(LAT=substr(LAI,1,2))|>
    group_by(CONTINENT,SAMPLE,AGE2,EDATTAIN2,LAT,step)|>
    summarise(POPW=sum(POPW))|>
    ungroup()
  
  df_LAT_NUCLEAR<-df_LAT|>
    filter(LAT %in% c(50,51,52))|>
    mutate(LAT2=ifelse(LAT%in% c(50) & step==0, 5, 6))|>
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
    mutate(LAT2=ifelse(LAT%in% c(50) & step==0, 5, 6),
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
combined_gradient1 <- data.table::rbindlist(combined_glad1)

wide_gradient <- combined_gradient|>
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
    flag_tiny_HM = (prop_H + prop_M) < 0.05,
    type="A" #original
  )



wide_gradient1 <- combined_gradient1|>
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
    flag_tiny_HM = (prop_H + prop_M) < 0.05,
    type="B"#step
  )

final<-bind_rows(wide_gradient,wide_gradient1)

# Sort SAMPLE based on the mean value of G_cond to ensure bars are in sorted order
final_plot <- final %>%
  mutate(SAMPLE = fct_reorder(SAMPLE, G_cond, .fun = mean))

final_plot<-final_plot|>mutate(CNTRY=substr(SAMPLE, 1,3),
                               YEAR=substr(SAMPLE, 5, 8))

final_plot<-final_plot|>
  mutate(
    country_iso = CNTRY, # Your 3-letter code (e.g., ARG, USA)
    country = countrycode(country_iso, origin = "iso3c", destination = "country.name.en"),
    country_year= paste(country,YEAR, sep=" " ))


final_plot <- final_plot %>%
  mutate(country_year = fct_reorder(country_year, G_cond, .fun = mean))


# Create the grouped bar plot
ggplot(final_plot|>filter(SAMPLE!="MEX_2020_IPUMS"), aes(x = G_cond, y = country_year, fill = type)) +
  geom_col(position = position_dodge(width = 0.8),color="black",
           linewidth=0.1) +
  labs(
    x = "Educational gradient",
    y = "Sample",
    fill = "Educational Gradient"
  ) +
  theme_science()+
  theme(legend.position = "bottom")

ggsave(paste("G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\cherlin\\complexity\\1_plots_article\\figures final\\",
             "FIG_4S.png",sep=""), 
       scale =1,
       height = 8,
       width=12, 
       dpi = 300) 


