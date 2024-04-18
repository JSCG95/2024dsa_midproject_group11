library(tidyverse)
library(corrr)
library(ggcorrplot)

raw_data<- read_csv("files/06-02_matrix-gh04.csv")

# Add new columns for conversion factor and area (cm2)
new_matrix <- raw_data %>%
  drop_na() %>%
  mutate(tray_id = factor(tray_id),
         
         # conversion factor based on a known length reference
         cf = case_when(tray_id == '1' ~ 62.89,
                        tray_id == '2' ~ 63.36,
                        tray_id == '3' ~ 66.42,
                        tray_id == '4' ~ 61.15,
                        tray_id == '5' ~ 71.74,
                        tray_id == '6' ~ 63.36),
         
         # transform area in pixels to area in cm
         area_cm2 = area / cf,
         
         # Add cumulative DLI per harvest day per tray
         cum_dli = case_when((tray_id == '1' & date == "10/30/2023") ~ 185.56,
                             (tray_id == '1' & date == "11/05/2023") ~ 316.3,
                             (tray_id == '1' & date == "11/13/2023") ~ 428.17,
                             
                             (tray_id == '2' & date == "10/30/2023") ~ 182.22,
                             (tray_id == '2' & date == "11/05/2023") ~ 312.37,
                             (tray_id == '2' & date == "11/13/2023") ~ 425.35,
                             
                             (tray_id == '3' & date == "10/30/2023") ~ 163.08,
                             (tray_id == '3' & date == "11/05/2023") ~ 276.62,
                             (tray_id == '3' & date == "11/13/2023") ~ 384.95,
                             
                             (tray_id == '4' & date == "10/30/2023") ~ 59.42,
                             (tray_id == '4' & date == "11/05/2023") ~ 109.61,
                             (tray_id == '4' & date == "11/13/2023") ~ 162.25,
                             
                             (tray_id == '5' & date == "10/30/2023") ~ 46.83,
                             (tray_id == '5' & date == "11/05/2023") ~ 89.39,
                             (tray_id == '5' & date == "11/13/2023") ~ 145.29,
                             
                             (tray_id == '6' & date == "10/30/2023") ~ 130.86,
                             (tray_id == '6' & date == "11/05/2023") ~ 204.93,
                             (tray_id == '6' & date == "11/13/2023") ~ 260.44)
                    )


new_matrix <- new_matrix %>%
  mutate(inc_light = (cum_dli/100) * area_cm2) 

new_matrix <- new_matrix %>%
  mutate_if(is.numeric,~ round(., 2))



#write_csv(new_matrix,'files/20-02_feat-matrix.csv')


new_matrix %>% group_by(date) %>%
  ggplot(aes(date,inc_light, color = tray_id)) +
  geom_boxplot() 


## Does cm2 improves correlation?


cor_matrix <- new_matrix %>%
  select(LFW_g,LDW_g,LA_mm2,area,area_cm2,inc_light,cum_dli,diameter) 




corr_percent <- round(cor(cor_matrix),2)
p.mat <- cor_pmat(cor_matrix)

ggcorrplot(cor_matrix, hc.order = TRUE, 
           outline.color = "black",
           type = "lower",
           lab = TRUE)
  



