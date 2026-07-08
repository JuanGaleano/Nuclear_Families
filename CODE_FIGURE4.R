library(tidyverse)
library(readr)
library(tibble)

df <- tibble(
  region = c("Southeast Asia", "Sub-Saharan Africa", "Latin America & Caribbean", 
             "South Asia", "MENA", "Eastern Europe", "Southeast Asia", 
             "Sub-Saharan Africa", "Latin America & Caribbean", "South Asia", 
             "MENA", "Eastern Europe"),
  mean_diff = c(-0.155065013, -0.057975996, -0.024102659, -0.002828991, 
                0.066408477, 0.073662629, -0.212101096, -0.095282339, 
                -0.093930221, -0.073538689, 0.0283347137455575, 0.09368893),
  median_diff = c(-0.15627197, -0.054941716, -0.04849361, -0.021878064, 
                  0.110296542, 0.08176104, -0.26327461, -0.121382559, 
                  -0.102077736, -0.08680349, 0.066521876, 0.076283345),
  n_countries = c(3L, 35L, 11L, 4L, 6L, 3L, 3L, 35L, 9L, 4L, 7L, 3L),
  sign = c("Negative", "Negative", "Negative", "Negative", "Positive", "Positive", 
           "Negative", "Negative", "Negative", "Negative", "Positive", "Positive"),
  type = c("Education", "Education", "Education", "Education", "Education", "Education", 
           "Wealth", "Wealth", "Wealth", "Wealth", "Wealth", "Wealth")
)


education<-df|>filter(type=="Education")

education<-education%>%
  arrange(mean_diff) %>%
  mutate(region = fct_inorder(region),
         type="Education")


ggplot(education, aes(x = region, y = mean_diff, fill = sign)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey50") +
  geom_col(width = 0.7, color="black", linewidth = .25) +
  coord_flip() +
  scale_fill_manual(values=c("#b2182b","#2166ac"))+
  labs(
    x = NULL,
    y = "\nPopulation-weighted mean gradient",
    fill = "Educational gradient"
  ) +
  theme_science()


ggsave(paste("G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\cherlin\\complexity\\1_plots_article\\",
             "FIG_4_A.png",sep=""), 
       scale =1,
       height = 6,
       width=8, 
       dpi = 300) 


wealth<-df|>filter(type=="Wealth")


wealth<-wealth%>%
  arrange(mean_diff) %>%
  mutate(region = fct_inorder(region),
         type="Wealth")


ggplot(wealth, aes(x = region, y = mean_diff, fill = sign)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey50") +
  geom_col(width = 0.7, color="black", linewidth = .25) +
  coord_flip() +
  scale_fill_manual(values=c("#b2182b","#2166ac"))+
  labs(
    x = NULL,
    y = "\nPopulation-weighted mean gradient",
    fill = "Wealth gradient"
  ) +
  theme_science()


ggsave(paste("G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\cherlin\\complexity\\1_plots_article\\",
             "FIG_4_B.png",sep=""), 
       scale =1,
       height = 6,
       width=8, 
       dpi = 300) 


ggplot(df, aes(x = region, y = mean_diff, fill = sign)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey50") +
  geom_col(width = 0.7, color="black", linewidth = .25) +
  coord_flip() +
  scale_fill_manual(values=c("#b2182b","#2166ac"))+
  labs(
    x = NULL,
    y = "\nPopulation-weighted mean gradient",
    fill = "Gradient"
  ) +
  facet_wrap(~type)+
  theme_science()


ggsave(paste("G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\cherlin\\complexity\\1_plots_article\\",
             "FIG_4_C.png",sep=""), 
       scale =1,
       height = 6,
       width=12, 
       dpi = 300) 

