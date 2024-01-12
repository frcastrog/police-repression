#-----------------------------Tables and Figures-------------------------------#
#-Author: Francisca Castro ------------------------ Created: November 30, 2023-#
#-R Version: 4.3.1 -------------------------------- Revised: November 30, 2023-#

# LOAD LIBRARIES

pacman::p_load(chilemapas, tidyr, dplyr, readr, readxl, gdata, stringr, writexl,
               lubridate, janitor, stringr, here, plm, magrittr, haven, ggplot2)


# FIGURE 1 - Comparing both data sources (ACLED/COES)

comparison_data <- final_df %>%
  group_by(date) %>%
  summarise(total_protest_coes = sum(protest_coes, na.rm = TRUE),
            total_contentious_acled = sum(contentious_acled, na.rm = TRUE))


# Creating a time series plot
comparison_acled_coes <- ggplot(comparison_data, aes(x = date)) +
  geom_line(aes(y = total_protest_coes, colour = "COES")) +
  geom_line(aes(y = total_contentious_acled, colour = "ACLED")) +
  scale_colour_manual(values = c("COES" = "red", "ACLED" = "blue"), 
                      name = "Data Source",
                      labels = c("COES" = "COES", "ACLED" = "ACLED")) +
  labs(title = "Comparison of Total Protest Counts Over Time",
       x = "Date",
       y = "Total Number of Protests") +
  theme_minimal()

# TABLE 1 - Descriptive statistics

freq_table_repression_raw <- final_df %>%
  group_by() %>%
  summarise_at(vars(disparos_total, golpiza_total, amenaza_total, detencion_total,
                    atropello_total, asfixia_total, invasion_hogar_total,
                    gaseado_total, piedrazo_total, ataque_animales_total,
                    obstruccion_total, tocaciones_total, quemado_total, 
                    estigmatizacion_total, destruccion_objetos_total, seguimiento_total,
                    desnudamiento_total, guanaco_total, otros_total), sum) #19 variables

freq_table_repression_raw %<>%
  tibble::rownames_to_column() %>% 
  gather(repression_type, value, -rowname) %>% 
  spread(rowname, value) 

#Rename rows
freq_table_repression <- freq_table_repression_raw %>%
  mutate(repression_type = recode (repression_type, amenaza_total = "Threats",
                                   asfixia_total = "Asphyxia",
                                   ataque_animales_total = "Attack with Animals",
                                   atropello_total = "Hit by a car",
                                   desnudamiento_total = "Stripping",
                                   destruccion_objetos_total = "Destruction personal items",
                                   detencion_total = "Arrests",
                                   disparos_total = "Shooting",
                                   estigmatizacion_total = "Stigmatization",
                                   gaseado_total = "Gassed",
                                   golpiza_total = "Beating",
                                   invasion_hogar_total = "Unauthorized entry/home invasion",
                                   guanaco_total = "Water impact",
                                   obstruccion_total = "Obstruction medical assistance",
                                   piedrazo_total = "Stone throwing",
                                   quemado_total = "Burned",
                                   seguimiento_total = "Follow up",
                                   tocaciones_total = "Touching",
                                   otros_total = "Other")) #19

colnames(freq_table_repression)[2] <- "frequency"

freq_table_repression %<>% mutate(percentage=100*frequency/sum(frequency))

# Group all the repressive types with less than 15 occurrences under the category "Other"
freq_table_repression %<>%
  mutate(repression_type = ifelse(frequency > 15, repression_type, "Other")) %>%
  group_by(repression_type) %>%
  summarise(frequency = sum(frequency)) %>%
  mutate(percentage=100*frequency/sum(frequency))

sum(freq_table_repression$percentage) #100
sum(freq_table_repression$frequency) #2438

# Order alphabetically by repression_type and round up to two decimals and create the row 'Total repressive acts'
freq_table_repression %<>%
  arrange(repression_type) %>%
  mutate(percentage = round(percentage, 2)) %>%
  mutate(`Total Repressive Acts` = sum(frequency))

freq_table_repression
