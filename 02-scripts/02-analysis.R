#-----------------------------------Analysis-----------------------------------#
#-Author: Francisca Castro ------------------------ Created: December 11, 2023-#
#-R Version: 4.3.1 --------------------------------- Revised: January 12, 2023-#

pacman::p_load(glmmTMB, modelsummary, MASS, brms, rstanarm, lme4, lmtest,
               car, effects, ggeffects, scales, fixest)

# Descriptive statistics

# Calculate descriptive statistics by region and month
descriptive_stats <- final_df %>%
  mutate(month = as.Date(date, format="%Y-%m-%d") %>% format("%B"),  # Extract month from date
         region_type = ifelse(nombre_region == "Metropolitana de Santiago", "Metropolitan Region", "Other Regions")) %>%
  filter(region_type %in% c("Metropolitan Region", "Other Regions")) %>%
  group_by(region_type, month) %>%
  summarize(
    total_protests = sum(protest_coes, na.rm = TRUE),
    total_beatings = sum(beatings, na.rm = TRUE),
    total_shootings = sum(shootings, na.rm = TRUE),
    total_arrests = sum(arrests, na.rm = TRUE),
    total_crowd_control = sum(crowd_control, na.rm = TRUE)
  ) %>%
  ungroup()

# Add a row for the total for the entire country
total_country <- descriptive_stats %>%
  summarise(region_type = "Total",
            month = "Total",
            total_protests = sum(total_protests),
            total_beatings = sum(total_beatings),
            total_shootings = sum(total_shootings),
            total_arrests = sum(total_arrests),
            total_crowd_control = sum(total_crowd_control))

# Add a row for the total for each region, regardless of the month
total_regions <- descriptive_stats %>%
  group_by(region_type) %>%
  summarise(month = "Total",
            total_protests = sum(total_protests),
            total_beatings = sum(total_beatings),
            total_shootings = sum(total_shootings),
            total_arrests = sum(total_arrests),
            total_crowd_control = sum(total_crowd_control))

# Add a row for the total for each month, regardless of the region
total_months <- descriptive_stats %>%
  group_by(month) %>%
  summarise(region_type = "Total",
            total_protests = sum(total_protests),
            total_beatings = sum(total_beatings),
            total_shootings = sum(total_shootings),
            total_arrests = sum(total_arrests),
            total_crowd_control = sum(total_crowd_control))

# Bind the totals for each region, the total for each month, and the total for the entire country to the descriptive_stats table
descriptive_stats <- bind_rows(descriptive_stats, total_country, total_regions, total_months)

descriptive_stats


# Frequency table for regions
freq_table_region <- final_df %>%
  group_by(nombre_region) %>%
  summarize(total_repression = sum(repression_total),
            total_protests = sum(contentious_total)) %>%
  mutate(perc_repression = 100 * total_repression / sum(total_repression),
         perc_protests = 100 * total_protests / sum(total_protests)) %>%
  mutate(perc_repression = round(perc_repression, 2),
         perc_protests = round(perc_protests, 2)) %>%
  mutate(`Total Repressive Acts` = sum(total_repression),
         `Total Contentious Acts` = sum(total_protests))

# Display the frequency table for regions
freq_table_region

# Frequency table for months
freq_table_month <- final_df %>%
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(total_repression = sum(repression_total),
            total_protests = sum(contentious_total)) %>%
  mutate(perc_repression = 100 * total_repression / sum(total_repression),
         perc_protests = 100 * total_protests / sum(total_protests)) %>%
  mutate(perc_repression = round(perc_repression, 2),
         perc_protests = round(perc_protests, 2)) %>%
  mutate(`Total Repressive Acts` = sum(total_repression),
         `Total Contentious Acts` = sum(total_protests))

# Display the frequency table for months
freq_table_month
# `glmmTMB` models

## Initial model 1 lag
model_lag1_coes <- glmmTMB(protest_coes ~ shootings_lag1 + beatings_lag1 + arrests_lag1 +
                             crowd_control_lag1 + protest_coes_lag1,
                           data = final_df_1day_lag, family = nbinom2)

## Model with t-1 lag + controls + random effects per municipality + zero inflated
model_lag1_coes2 <- glmmTMB(protest_coes ~ shootings_lag1 + beatings_lag1 + arrests_lag1 +
                              crowd_control_lag1 + protest_coes_lag1 + police_per_100k_lag1 +
                            weekday_category + distance_km + (1|nombre_comuna), 
                            data = final_df_1day_lag, zi = ~1, family = nbinom1)

## Model with 3day accumulation la + controls + random effects per municipality + zero inflated
model_3day_coes2 <- glmmTMB(protest_coes ~ acc_3day_shootings + acc_3day_beatings +
                            acc_3day_arrests + acc_3day_crowd_control + acc_3day_protest_coes +
                            acc_3day_police_per_100k + distance_km + (1|nombre_comuna),
                            family = nbinom1, zi = ~1, data = final_df_3day_lag_acc)

## Model with 7day accumulation + controls + random effects per municipality + zero inflated
model_7day_coes2 <- glmmTMB(protest_coes ~ acc_7day_shootings + acc_7day_beatings + 
                            acc_7day_arrests + acc_7day_crowd_control + acc_7day_protest_coes +
                            acc_7day_police_per_100k + distance_km + (1|nombre_comuna), 
                            family = nbinom1, zi = ~1, data = final_df_7day_lag_acc)

msummary(list(model_lag1_coes,model_lag1_coes2, model_3day_coes2, model_7day_coes2),
         stars = c('*' = .1, '**' = .05, '***' = .01),
         output = 'latex')

# Visualization

## Shootings
predict_shootings_lag1 <- ggpredict(model_lag1_coes2, terms = "shootings_lag1[all]")
predict_shootings_3day <- ggpredict(model_3day_coes2, terms = "acc_3day_shootings[all]")
predict_shootings_7day <- ggpredict(model_7day_coes2, terms = "acc_7day_shootings[all]")

predict_shootings_lag1$Model <- "1-day lag"
predict_shootings_3day$Model <- "3-day accumulation"
predict_shootings_7day$Model <- "7-day accumulation"

predict_shootings <- rbind(predict_shootings_lag1, predict_shootings_3day, predict_shootings_7day)

plot_shootings <- ggplot(predict_shootings, aes(x = x, y = predicted, group = Model)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey") +
  facet_wrap(~ Model, scales = "free_x") +  
  labs(x = "Shootings", y = "Predicted Protests") +
  theme_minimal() +
  theme(legend.position = "none",  
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12)) +
  coord_cartesian(ylim = c(0.00, 0.06)) 
  
plot_shootings

ggsave("03-outputs/02-figures/plot_shootings.png", plot = plot_shootings, dpi = 600, width = 7, height = 3)

## Crowd control
predict_crowd_control_lag1 <- ggpredict(model_lag1_coes2, terms = "crowd_control_lag1[all]")
predict_crowd_control_3day <- ggpredict(model_3day_coes2, terms = "acc_3day_crowd_control[all]")
predict_crowd_control_7day <- ggpredict(model_7day_coes2, terms = "acc_7day_crowd_control[all]")

predict_crowd_control_lag1$Model <- "1-day lag"
predict_crowd_control_3day$Model <- "3-day accumulation"
predict_crowd_control_7day$Model <- "7-day accumulation"

predict_crowd_control <- rbind(predict_crowd_control_lag1, predict_crowd_control_3day, predict_crowd_control_7day)

plot_crowd_control <- ggplot(predict_crowd_control, aes(x = x, y = predicted, group = Model)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey") +
  facet_wrap(~ Model, scales = "free_x") +  # Use facet_wrap to create separate panels
  labs(x = "Crowd Control Techniques", y = "Predicted Protests") +
  theme_minimal() +
  theme(legend.position = "none",  # Hide the legend, as Model is now in the facet title
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12)) +
  coord_cartesian(xlim = c(0, 6), ylim = c(0.00, 0.25))  

plot_crowd_control

ggsave("03-outputs/02-figures/plot_crowd_control.png", plot = plot_crowd_control, dpi = 600, width = 7, height = 3)

## Beatings

predict_beatings_lag1 <- ggpredict(model_lag1_coes2, terms = "beatings_lag1[all]")
predict_beatings_3day <- ggpredict(model_3day_coes2, terms = "acc_3day_beatings[all]")
predict_beatings_7day <- ggpredict(model_7day_coes2, terms = "acc_7day_beatings[all]")

predict_beatings_lag1$Model <- "1-day lag"
predict_beatings_3day$Model <- "3-day accumulation"
predict_beatings_7day$Model <- "7-day accumulation"

predict_beatings <- rbind(predict_beatings_lag1, predict_beatings_3day, predict_beatings_7day)

plot_beatings <- ggplot(predict_beatings, aes(x = x, y = predicted, group = Model)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey") +
  facet_wrap(~ Model, scales = "free_x") +  # Use facet_wrap to create separate panels
  labs(x = "Beatings", y = "Predicted Protests") +
  theme_minimal() +
  theme(legend.position = "none",  # Hide the legend
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12)) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
  coord_cartesian(xlim = c(0, 12), ylim = c(0.00, 0.15))  

plot_beatings

ggsave("03-outputs/02-figures/plot_beatings.png", plot = plot_beatings, dpi = 600, width = 7, height = 3)

## Arrests
predict_arrests_lag1 <- ggpredict(model_lag1_coes2, terms = "arrests_lag1[all]")
predict_arrests_3day <- ggpredict(model_3day_coes2, terms = "acc_3day_arrests[all]")
predict_arrests_7day <- ggpredict(model_7day_coes2, terms = "acc_7day_arrests[all]")

predict_arrests_lag1$Model <- "1-day lag"
predict_arrests_3day$Model <- "3-day accumulation"
predict_arrests_7day$Model <- "7-day accumulation"

predict_arrests <- rbind(predict_arrests_lag1, predict_arrests_3day, predict_arrests_7day)

plot_arrests <- ggplot(predict_arrests, aes(x = x, y = predicted, group = Model)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey") +
  facet_wrap(~ Model, scales = "free_x") +  # Use facet_wrap to create separate panels
  labs(x = "Arrests", y = "Predicted Protests") +
  theme_minimal() +
  theme(legend.position = "none",  # Hide the legend
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
  coord_cartesian(xlim = c(0, 10), ylim = c(0.00, 0.12))  

plot_arrests

ggsave("03-outputs/02-figures/plot_arrests.png", plot = plot_arrests, dpi = 600, width = 7, height = 3)

# Mechanism

# - To unveil the mechanism, i.e. that the use of rubber bullets has a deterrent effect on the long term
# - whereas crowd control techniques increase protests in the long term
# - and beatings increases in the short term

names(cep_survey_protest)

# - ESP_23_6 evaluation of carabineros in response to the oct 2019 crisis

table(cep_survey_protest$ESP_23_6)
cep_survey_protest$ESP_23_6 <- as.numeric(cep_survey_protest$ESP_23_6)

# Recode values 88 and 99 as NAs
cep_survey_protest %<>%
  mutate(eval_carab = case_when(
    ESP_23_6 %in% c(88, 99) ~ NA_real_,
    TRUE ~ ESP_23_6
  )) %>%
  mutate(eval_carab = case_when(
    eval_carab == 5 ~ 1,
    eval_carab == 4 ~ 2,
    eval_carab == 3 ~ 3,
    eval_carab == 2 ~ 4,
    eval_carab == 1 ~ 5,
    TRUE ~ eval_carab  # Keep other values unchanged
  ))


table(cep_survey_protest$eval_carab)

# Create the linear model
model1_cep_1day <- feols(eval_carab ~ shootings_lag1 + beatings_lag1 + 
                           arrests_lag1 + crowd_control_lag1 + 
                           police_per_100k_lag1 | nombre_comuna, data = cep_survey_protest)


model1_cep_3day <- feols(eval_carab ~ data_3day_shootings + data_3day_beatings + 
                           data_3day_arrests + data_3day_crowd_control + 
                           data_3day_police_per_100k | nombre_comuna, data = cep_survey_protest)

msummary(list(model1_cep_1day,model1_cep_3day),
         stars = c('*' = .1, '**' = .05, '***' = .01))


# - ESP_41_1 justification of protest participation

table(cep_survey_protest$ESP_41_1)

cep_survey_protest$ESP_41_1 <- as.numeric(cep_survey_protest$ESP_41_1)

# Recode values 88 and 99 as NAs
cep_survey_protest %<>%
  mutate(protest_justification = case_when(
    ESP_41_1 %in% c(88, 99) ~ NA_real_,
    TRUE ~ ESP_41_1
  )) %>%
  mutate(protest_justification = case_when(
    protest_justification == 5 ~ 1,
    protest_justification == 4 ~ 2,
    protest_justification == 3 ~ 3,
    protest_justification == 2 ~ 4,
    protest_justification == 1 ~ 5,
    TRUE ~ protest_justification  # Keep other values unchanged
  ))

model2_cep_1day <- feols(protest_justification ~ shootings_lag1 + beatings_lag1 + 
                           arrests_lag1 + crowd_control_lag1 + 
                           police_per_100k_lag1 | nombre_comuna, data = cep_survey_protest)


model2_cep_3day <- feols(protest_justification ~ data_3day_shootings + data_3day_beatings + 
                           data_3day_arrests + data_3day_crowd_control + 
                           data_3day_police_per_100k | nombre_comuna, data = cep_survey_protest)

msummary(list(model2_cep_1day,model2_cep_3day),
         stars = c('*' = .1, '**' = .05, '***' = .01),
         output = 'latex',
         title = 'Models for Protest Justification')

# - ESP_43 frequency human rights violations

table(cep_survey_protest$ESP_43)

cep_survey_protest$ESP_43 <- as.numeric(cep_survey_protest$ESP_43)

# Recode values 88 and 99 as NAs
cep_survey_protest %<>%
  mutate(hr_violations = case_when(
    ESP_43 %in% c(88, 99) ~ NA_real_,
    TRUE ~ ESP_43
  )) %>%
  mutate(hr_violations = case_when(
    hr_violations == 5 ~ 1,
    hr_violations == 4 ~ 2,
    hr_violations == 3 ~ 3,
    hr_violations == 2 ~ 4,
    hr_violations == 1 ~ 5,
    TRUE ~ hr_violations  # Keep other values unchanged
  ))

model3_cep_1day <- feols(hr_violations ~ shootings_lag1 + beatings_lag1 + 
                           arrests_lag1 + crowd_control_lag1 + eval_carab +
                           police_per_100k_lag1 | nombre_comuna, data = cep_survey_protest)


model3_cep_3day <- feols(hr_violations ~ data_3day_shootings + data_3day_beatings + 
                           data_3day_arrests + data_3day_crowd_control + eval_carab +
                           data_3day_police_per_100k | nombre_comuna, data = cep_survey_protest)

msummary(list(model3_cep_1day,model3_cep_3day),
         stars = c('*' = .1, '**' = .05, '***' = .01),
         output = 'latex',
         title = 'Models for Human Rights Violations')


#- confianza en carabineros
#- justificacion tecnicas de represion: uso lacrimonegas, fuerza contra un manifestante violento, disparar balines
#- frecuencia de violacion de ddhh carabienros, militares
#- justificacion participar de una marcha, participar de destrozos, provocar incendios, participar saqueos
#- frecuencia realizacion protestas, particiapcion huelga
#- frecuencia mirar programas tv, leer noticias politica, seguir temas politicos rrss
#- En temas de orden público, el Estado usa su poder legítimamente
