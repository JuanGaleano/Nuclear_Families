library(tidyverse)
library(giscoR)

# Load GLAD #####

load("CORESIDENCE_GLAD_2025.Rda")

# Extract harmonized dataset IPUMS + EU-LFS #####
harmo<-GLAD[["HARMONIZED"]]

# Get last available data for each country #####
df <- harmo |>
  mutate(SEX = unclass(SEX),
         CONTINENT = paste(CONTINENT, SUBCONTINENT, sep = "-")) |>
  filter(SEX != 9, AGE != "999") |>
  group_by(CONTINENT, SAMPLE, SEX, EDATTAIN, AGE, LAI) |>
  summarise(POPW = sum(POPW)) |>
  ungroup() |>
  mutate(YEAR = as.numeric(substr(SAMPLE, 5, 8)), CNTRY = substr(SAMPLE, 1, 3))|>
  group_by(CONTINENT, CNTRY) |>
  # 1. Filter to keep only the earliest and latest available year for each country (as per original request)
  filter(
    YEAR == max(YEAR)
  ) |>
  ungroup()

# Split data by country ######

list_map <- split(df, f = df$CNTRY)

# Nuclearity rate pop 30-39 ######
CORE_LIST<-lapply(list_map, function(df) {
  
  df <- df |>
    mutate(
      EDATTAIN = replace_na(EDATTAIN, "L"),
      AGE2 = if_else(AGE == "30-34" | AGE == "35-39", "30-39", AGE),
      EDATTAIN2 = ifelse(EDATTAIN == "9", "L", EDATTAIN)
    ) |>
    filter(AGE2 == "30-39") |>
    group_by(CONTINENT, SAMPLE, EDATTAIN2, AGE2, LAI) |>
    summarise(POPW = sum(POPW)) |>
    ungroup()

  df_LAT <- df |>
    mutate(LAT = substr(LAI, 1, 2)) |>
    group_by(CONTINENT, SAMPLE, AGE2, EDATTAIN2, LAT) |>
    summarise(POPW = sum(POPW)) |>
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
  
  aver <- df_LAT |>
    filter(LAT %in% c(50, 51, 52)) |>
    mutate(
      LAT2 = ifelse(LAT %in% c(50), 5, 6),
      EDATTAIN2 = as.factor(EDATTAIN2),
      EDATTAIN2 = fct_relevel(EDATTAIN2, "L", "M", "H")
    ) |>
    filter(LAT2 == 5) |>
    group_by(SAMPLE, EDATTAIN2) |>
    summarise(POPW = sum(POPW)) |>
    ungroup() |>
    group_by(SAMPLE) |>
    mutate(n_rel = POPW / sum(POPW)) |>
    ungroup() |>
    mutate(id = paste(SAMPLE, EDATTAIN2, sep = "_")) |>
    left_join(df_LAT_NUCLEAR, by = "id")
  
  colnames(aver)[4]<-"prop"
  colnames(aver)[6]<-"rate"
  
  aver <- aver |>
    complete(
      nesting(SAMPLE), 
      EDATTAIN2 = c("L", "M", "H"), 
      fill = list(POPW = 0, prop = 0, rate = 0)
    ) |>
    mutate(id = if_else(is.na(id), paste(SAMPLE, EDATTAIN2, sep = "_"), id))|>
    dplyr::mutate(EDATTAIN2 = factor(EDATTAIN2, levels = c("L","M","H")))
  
})


# Compute conditional gradient G ######
g_df <- data.table::rbindlist(CORE_LIST)|>
  select(SAMPLE,EDATTAIN2,prop,rate) |>
  tidyr::pivot_wider(names_from  = EDATTAIN2, values_from = c(prop, rate)) |>
  # weighted rate for combined groups
  mutate(
    rate_HM = (rate_H * prop_H + rate_M * prop_M) / (prop_H + prop_M),
    rate_LM = (rate_L * prop_L + rate_M * prop_M) / (prop_L + prop_M),
    
    # standard gradient
    G_hl = rate_H - rate_L,
    
    # conditional gradient
    G_cond = if_else(
      prop_L > 0.5,
      rate_HM - rate_L,
      # (H+M) - L
      rate_H  - rate_LM    # H - (M+L)
    ),
    
    rule_applied = if_else(prop_L > 0.5, "(H+M)-L", "H-(M+L)"),
    
    # diagnostic flags
    sign_change  = sign(G_hl) != sign(G_cond),
    flag_tiny_HM = (prop_H + prop_M) < 0.05,
    country = substr(SAMPLE, 1, 3),
    YEAR = substr(SAMPLE, 5, 8),
    country_iso = country,
    continent = countrycode(country_iso, origin = "iso3c", destination = "continent"),
    diff_cat = ifelse(G_cond < 0, "Negative", "Positive"),
  ) |>
  select(1, 11, 15,19)

# Fetch world boundaries from giscoR #####
map_df <- gisco_get_countries(resolution = "20", year = "2020") |>
  filter(ISO3_CODE != "ATA")|>
  left_join(g_df, by = c("ISO3_CODE" = "country"))

# CRS Robin #####
crs_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"


# Create discrete choropleth map ######
ggplot(data = map_df) +
  geom_sf(aes(fill = diff_cat),
          color = "black",
          linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "Negative" = "#b2182b",
      "Positive" = "#2166ac"
    ),
    na.value = "lightgrey",
    name = "Educational gradient",
    guide = guide_legend(
      direction = "horizontal",
      nrow = 1,
      keywidth = 5,
      keyheight = .5,
      label.position = "bottom",
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  coord_sf(crs = crs_robin) +
  theme_void() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white")
  )

# Save image ######
ggsave(paste("FIG_1",".png",sep=""), 
       #  plot=b,
       scale = 1,
       height = 6.5,
       width=13,  
       dpi = 300) 
