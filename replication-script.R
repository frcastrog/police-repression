#----------------------------- Replication Script -----------------------------#
#-R Version: 4.5.0 ----------------------------------- Revised: July 10, 2025-#

pacman::p_load(glmmTMB, modelsummary, ggeffects, scales, fixest, ggplot2, readxl,
               ggalluvial, dplyr, tidyr, broom, broom.mixed, stringr, gridExtra,
               sf, gtable)


options(scipen = 1234)

#--------------------------- Manuscript replication ---------------------------#

#--- Load data ---#

acd_dates_delitos <- read_excel("01-data/acd-dates-delitos.xlsx")
acled_violence <- read.csv("01-data/acled-violence-civilians-worldwide.csv")
load("01-data/cep_survey_protest.RData")
load("01-data/mmALL_073120_v16.RData")

# Police repression and protest data
load("01-data/final_df_rm_standardized.RData")
load("01-data/final_df_standardized.RData")
load("01-data/final_df.RData")
load("01-data/indh_clean.RData")


#--- Figure 1 ---#

events_ts_data <- final_df_standardized %>%
  group_by(date) %>%
  summarise(
    protest_coes = sum(protest_coes, na.rm = TRUE),
    beatings = sum(beatings, na.rm = TRUE),
    shootings = sum(shootings, na.rm = TRUE),
    arrests = sum(arrests, na.rm = TRUE),
    crowd_control = sum(crowd_control, na.rm = TRUE))

# Reshape the data for plotting
events_long <- events_ts_data %>%
  pivot_longer(cols = c(protest_coes, beatings, shootings, arrests, crowd_control),
    names_to = "type",
    values_to = "count") %>%
  mutate(type = factor(type, 
                       levels = c("protest_coes", "beatings", "shootings", "arrests", "crowd_control"),
                       labels = c("Protests", "Beatings", "Shootings", "Arrests", "Crowd Control")))

# Data frame for annotations
annotations <- data.frame(
  date = as.Date(c("2019-10-23", "2019-11-08", "2019-11-12", "2019-11-15", "2019-11-26")),
  count = c(300, 350, 300, 250, 200),  
  label = c(
    "23 Oct: President\n Piñera announces\n 'New Social Agenda'",
    "8 Nov: Gustavo Gatica\n loses both eyes",
    "12 Nov: Call for a\n general strike",
    "15 Nov: Plebiscite agreement for\n a new constitution",
    "26 Nov: Fabiola Campillai hit\n by tear gas grenade"))

ts_combined <- ggplot(events_long, aes(x = date, y = count)) +
  geom_line(aes(color = type), linewidth = 1) +
  geom_point(aes(color = type, shape = type), size = 2, 
             data = events_long[events_long$type != "Protests" & seq_len(nrow(events_long)) %% 4 == 1, ]) +
  scale_color_manual(
    values = c("Protests" = "#000000", "Beatings" = "#333333", "Shootings" = "#666666", 
               "Arrests" = "#999999", "Crowd Control" = "#CCCCCC")) +
  scale_shape_manual(
    values = c("Protests" = NA, "Beatings" = 17, "Shootings" = 15, "Arrests" = 8, "Crowd Control" = 3),
    guide = "none") +  # Hide shape legend completely
  geom_segment(data = annotations, aes(x = date, xend = date, y = 0, yend = count),
               linetype = "dashed", color = "gray50", inherit.aes = FALSE) +
  geom_text(data = annotations, aes(x = date, y = count, label = label),
            color = "gray30", size = 3, hjust = 0, vjust = -0.5, inherit.aes = FALSE) +
  scale_y_continuous(limits = c(0, max(max(events_long$count), max(annotations$count)) * 1.1),
                     breaks = scales::pretty_breaks()) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  labs(x = "Date (Start of Week)", y = "Number of Events") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  guides(
    color = guide_legend(override.aes = list(
      shape = c(NA, 17, 15, 8, 3)  # Protests, Beatings, Shootings, Arrests, Crowd Control
    )),
    shape = "none"
  )

ts_combined

ggsave("02-outputs/ts_combined.png", width = 8, height = 5.5, dpi = 600)

#--- Table 1 ---#

# Descriptive statistics

# Calculate descriptive statistics by region and month
descriptive_stats <- final_df_standardized %>%
  st_drop_geometry() %>%  # Remove geometry columns
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
            total_protests = sum(contentious_acled)) %>%
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
            total_protests = sum(contentious_acled)) %>%
  mutate(perc_repression = 100 * total_repression / sum(total_repression),
         perc_protests = 100 * total_protests / sum(total_protests)) %>%
  mutate(perc_repression = round(perc_repression, 2),
         perc_protests = round(perc_protests, 2)) %>%
  mutate(`Total Repressive Acts` = sum(total_repression),
         `Total Contentious Acts` = sum(total_protests))

# Display the frequency table for months
freq_table_month

#--- Figure 3, 4, 5, 6 ---#

# `glmmTMB` models

# Initial model with 1 lag - standardized version
model_lag1_coes_std <- glmmTMB(protest_coes ~ shootings_lag1_std + beatings_lag1_std + 
                                 arrests_lag1_std + crowd_control_lag1_std + 
                                 protest_coes_lag1_std,
                               data = final_df_standardized, 
                               family = nbinom1,  # Change this to match others
                               zi = ~1)

summary(model_lag1_coes_std)

# Model with t-1 lag + controls + random effects - standardized version
model_lag1_coes_std_ctr <- glmmTMB(protest_coes ~ shootings_lag1_std + beatings_lag1_std + 
                                     arrests_lag1_std + crowd_control_lag1_std + 
                                     protest_coes_lag1_std + police_per_100k_lag1_std +
                                     rain_lag1 + hot_day_lag1 +  
                                     weekday_category + distance_km_std + (1|nombre_comuna), 
                                   data = final_df_standardized, zi = ~1, family = nbinom1)

summary(model_lag1_coes_std_ctr)

# Model with 3-day accumulation + controls + random effects - standardized version
model_3day_coes_std <- glmmTMB(protest_coes ~ shootings_lag3_acc_std + beatings_lag3_acc_std +
                                 arrests_lag3_acc_std + crowd_control_lag3_acc_std + 
                                 protest_coes_lag3_acc_std + police_per_100k_lag3_acc_std +
                                 rain_lag3_acc + hot_day_lag3_acc +  
                                 distance_km_std + (1|nombre_comuna),
                               family = nbinom1, zi = ~1, data = final_df_standardized)

summary(model_3day_coes_std)

# Model with 7-day accumulation + controls + random effects - standardized version
model_7day_coes_std <- glmmTMB(protest_coes ~ shootings_lag7_acc_std + beatings_lag7_acc_std + 
                                 arrests_lag7_acc_std + crowd_control_lag7_acc_std + 
                                 protest_coes_lag7_acc_std + police_per_100k_lag7_acc_std +
                                 rain_lag7_acc + hot_day_lag7_acc +  
                                 distance_km_std + (1|nombre_comuna), 
                               family = nbinom1, zi = ~1, data = final_df_standardized)

summary(model_7day_coes_std)

# Visualization

## Beatings

predict_beatings_lag1 <- ggpredict(model_lag1_coes_std_ctr, 
                                   terms = "beatings_lag1_std[-1:9]",
                                   bias_correction = TRUE)
predict_beatings_3day <- ggpredict(model_3day_coes_std, 
                                   terms = "beatings_lag3_acc_std[-1:9]",
                                   bias_correction = TRUE)
predict_beatings_7day <- ggpredict(model_7day_coes_std, 
                                   terms = "beatings_lag7_acc_std[-1:9]",
                                   bias_correction = TRUE)

predict_beatings_lag1$Model <- "1-day lag"
predict_beatings_3day$Model <- "3-day accumulation"
predict_beatings_7day$Model <- "7-day accumulation"

predict_beatings <- rbind(predict_beatings_lag1, predict_beatings_3day, predict_beatings_7day)

plot_beatings <- ggplot(predict_beatings, aes(x = x, y = predicted, group = Model)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey") +
  facet_wrap(~ Model, scales = "free_x") +  
  labs(x = "Standardized Beatings (SD from mean)", y = "Predicted Effect") +
  theme_minimal() +
  theme(legend.position = "none",  
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 0.20)) +
  scale_y_continuous(breaks = seq(0, 0.20, 0.05))

plot_beatings # Figure 3

ggsave("02-outputs/plot_beatings.png", dpi = 600, width = 7, height = 3)

## Shootings
predict_shootings_lag1 <- ggpredict(model_lag1_coes_std_ctr, 
                                    terms = "shootings_lag1_std[-1:9]",  # Rounded from -0.51 to 8.49
                                    bias_correction = TRUE)
predict_shootings_3day <- ggpredict(model_3day_coes_std, 
                                    terms = "shootings_lag3_acc_std[-1:9]",  # Rounded from -0.74 to 7.47
                                    bias_correction = TRUE)
predict_shootings_7day <- ggpredict(model_7day_coes_std, 
                                    terms = "shootings_lag7_acc_std[-1:9]",  # Rounded from -1.02 to 7.26
                                    bias_correction = TRUE)

predict_shootings_lag1$Model <- "1-day lag"
predict_shootings_3day$Model <- "3-day accumulation"
predict_shootings_7day$Model <- "7-day accumulation"

predict_shootings <- rbind(predict_shootings_lag1, predict_shootings_3day, predict_shootings_7day)

plot_shootings <- ggplot(predict_shootings, aes(x = x, y = predicted, group = Model)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey") +
  facet_wrap(~ Model, scales = "free_x") +  
  labs(x = "Standardized Shootings (SD from mean)", y = "Predicted Effect") +
  theme_minimal() +
  theme(legend.position = "none",  
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 0.20)) +
  scale_y_continuous(breaks = seq(0, 0.20, 0.05))

plot_shootings # Figure 4

## Arrests
predict_arrests_lag1 <- ggpredict(model_lag1_coes_std_ctr, 
                                  terms = "arrests_lag1_std[-1:9]",
                                  bias_correction = TRUE)
predict_arrests_3day <- ggpredict(model_3day_coes_std, 
                                  terms = "arrests_lag3_acc_std[-1:9]",
                                  bias_correction = TRUE)
predict_arrests_7day <- ggpredict(model_7day_coes_std, 
                                  terms = "arrests_lag7_acc_std[-1:9]",
                                  bias_correction = TRUE)

predict_arrests_lag1$Model <- "1-day lag"
predict_arrests_3day$Model <- "3-day accumulation"
predict_arrests_7day$Model <- "7-day accumulation"

predict_arrests <- rbind(predict_arrests_lag1, predict_arrests_3day, predict_arrests_7day)

plot_arrests <- ggplot(predict_arrests, aes(x = x, y = predicted, group = Model)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey") +
  facet_wrap(~ Model, scales = "free_x") +  
  labs(x = "Standardized Arrests (SD from mean)", y = "Predicted Effect") +
  theme_minimal() +
  theme(legend.position = "none",  
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 0.20)) +
  scale_y_continuous(breaks = seq(0, 0.20, 0.05))

plot_arrests # Figure 5

## Crowd control
predict_crowd_lag1 <- ggpredict(model_lag1_coes_std_ctr, 
                                terms = "crowd_control_lag1_std[-1:9]",
                                bias_correction = TRUE)
predict_crowd_3day <- ggpredict(model_3day_coes_std, 
                                terms = "crowd_control_lag3_acc_std[-1:9]",
                                bias_correction = TRUE)
predict_crowd_7day <- ggpredict(model_7day_coes_std, 
                                terms = "crowd_control_lag7_acc_std[-1:9]",
                                bias_correction = TRUE)

predict_crowd_lag1$Model <- "1-day lag"
predict_crowd_3day$Model <- "3-day accumulation"
predict_crowd_7day$Model <- "7-day accumulation"

predict_crowd <- rbind(predict_crowd_lag1, predict_crowd_3day, predict_crowd_7day)

plot_crowd <- ggplot(predict_crowd, aes(x = x, y = predicted, group = Model)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey") +
  facet_wrap(~ Model, scales = "free_x") +  
  labs(x = "Standardized Crowd Control (SD from mean)", y = "Predicted Effect") +
  theme_minimal() +
  theme(legend.position = "none",  
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 0.20)) +
  scale_y_continuous(breaks = seq(0, 0.20, 0.05))

plot_crowd # Figure 6

#--- Table 3 ---#

# 1-day lag 
model_lag1_cont <- glmmTMB(protest_coes ~ violence_acled_lag1  + 
                             protest_coes_lag1_std + police_per_100k_lag1_std +
                             rain_lag1 + hot_day_lag1 + 
                             weekday_category + distance_km_std + (1|nombre_comuna), 
                           data = final_df_standardized, zi = ~1, family = nbinom1)

# 3-day window 
model_3day_cont <- glmmTMB(protest_coes ~ violence_acled_lag3_acc +
                             protest_coes_lag3_acc_std + police_per_100k_lag3_acc_std +
                             rain_lag3_acc + hot_day_lag3_acc + 
                             distance_km_std + (1|nombre_comuna),
                           family = nbinom1, zi = ~1, data = final_df_standardized)

# 7-day 
model_7day_cont <- glmmTMB(protest_coes ~ violence_acled_lag7_acc + 
                             protest_coes_lag7_acc_std + police_per_100k_lag7_acc_std +
                             rain_lag7_acc + hot_day_lag7_acc + 
                             distance_km_std + (1|nombre_comuna),
                           family = nbinom1, zi = ~1, data = final_df_standardized)

msummary(
  list("t-1" = model_lag1_cont, "3-day" = model_3day_cont, "7-day" = model_7day_cont),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  coef_map = c(
    "violence_acled_lag1" = "Violence (t-1)",
    "violence_acled_lag3_acc" = "Violence (3-day)",
    "violence_acled_lag7_acc" = "Violence (7-day)"),
  notes = "Standard errors in parentheses",
  title = "Effect of Violence on Protests")

#--- Figure 7 ---#

# - ESP_41_1 justification of protest participation -> protest_justification

# 1-day lag model
model2_cep_1day <- feols(protest_justification ~ shootings_lag1_std + beatings_lag1_std + 
                           arrests_lag1_std + crowd_control_lag1_std + 
                           police_per_100k_lag1_std +
                           education_level + gender + age |
                           nombre_comuna, 
                         cluster = "nombre_comuna",
                         data = cep_survey_protest)

# 3-day accumulation model 
model2_cep_3day <- feols(protest_justification ~ shootings_lag3_acc_std + beatings_lag3_acc_std + 
                           arrests_lag3_acc_std + crowd_control_lag3_acc_std + 
                           police_per_100k_lag3_acc_std +
                           education_level + gender + age |
                           nombre_comuna,
                         cluster = "nombre_comuna", 
                         data = cep_survey_protest)

# 7-day accumulation model
model2_cep_7day <- feols(protest_justification ~ shootings_lag7_acc_std + beatings_lag7_acc_std + 
                           arrests_lag7_acc_std + crowd_control_lag7_acc_std + 
                           police_per_100k_lag7_acc_std +
                           education_level + gender + age |
                           nombre_comuna,
                         cluster = "nombre_comuna",
                         data = cep_survey_protest)

# - ESP_43 frequency human rights violations -> hr_violations

# 1-day lag model
model3_cep_1day <- feols(hr_violations ~ shootings_lag1_std + beatings_lag1_std + 
                           arrests_lag1_std + crowd_control_lag1_std + eval_carab +
                           police_per_100k_lag1_std +
                           education_level + gender + age |
                           nombre_comuna, 
                         cluster = "nombre_comuna",
                         data = cep_survey_protest)

# 3-day accumulation model
model3_cep_3day <- feols(hr_violations ~ shootings_lag3_acc_std + beatings_lag3_acc_std + 
                           arrests_lag3_acc_std + crowd_control_lag3_acc_std + eval_carab +
                           police_per_100k_lag3_acc_std +
                           education_level + gender + age |
                           nombre_comuna,
                         cluster = "nombre_comuna",
                         data = cep_survey_protest)

# 7-day accumulation model
model3_cep_7day <- feols(hr_violations ~ shootings_lag7_acc_std + beatings_lag7_acc_std + 
                           arrests_lag7_acc_std + crowd_control_lag7_acc_std + eval_carab +
                           police_per_100k_lag7_acc_std +
                           education_level + gender + age |
                           nombre_comuna,
                         cluster = "nombre_comuna",
                         data = cep_survey_protest)

# Extract coefficients from protest justification models
cep_protest_1day <- tidy(model2_cep_1day) %>%
  filter(term %in% c("shootings_lag1_std", "beatings_lag1_std", 
                     "arrests_lag1_std", "crowd_control_lag1_std")) %>%
  mutate(time = "1-day lag",
         repression_type = case_when(
           str_detect(term, "shootings") ~ "Shootings",
           str_detect(term, "beatings") ~ "Beatings",
           str_detect(term, "arrests") ~ "Arrests",
           str_detect(term, "crowd_control") ~ "Crowd Control"))

cep_protest_3day <- tidy(model2_cep_3day) %>%
  filter(term %in% c("shootings_lag3_acc_std", "beatings_lag3_acc_std", 
                     "arrests_lag3_acc_std", "crowd_control_lag3_acc_std")) %>%
  mutate(time = "3-day acc.",
         repression_type = case_when(
           str_detect(term, "shootings") ~ "Shootings",
           str_detect(term, "beatings") ~ "Beatings",
           str_detect(term, "arrests") ~ "Arrests",
           str_detect(term, "crowd_control") ~ "Crowd Control"))

cep_protest_7day <- tidy(model2_cep_7day) %>%
  filter(term %in% c("shootings_lag7_acc_std", "beatings_lag7_acc_std", 
                     "arrests_lag7_acc_std", "crowd_control_lag7_acc_std")) %>%
  mutate(time = "7-day acc.",
         repression_type = case_when(
           str_detect(term, "shootings") ~ "Shootings",
           str_detect(term, "beatings") ~ "Beatings",
           str_detect(term, "arrests") ~ "Arrests",
           str_detect(term, "crowd_control") ~ "Crowd Control"))

# Combine protest coefficients
cep_protest_coefs <- bind_rows(cep_protest_1day, cep_protest_3day, cep_protest_7day) %>%
  mutate(dv = "Protest Justification")

# Extract coefficients from HR violations models
cep_hr_1day <- tidy(model3_cep_1day) %>%
  filter(term %in% c("shootings_lag1_std", "beatings_lag1_std", 
                     "arrests_lag1_std", "crowd_control_lag1_std")) %>%
  mutate(time = "1-day lag",
         repression_type = case_when(
           str_detect(term, "shootings") ~ "Shootings",
           str_detect(term, "beatings") ~ "Beatings",
           str_detect(term, "arrests") ~ "Arrests",
           str_detect(term, "crowd_control") ~ "Crowd Control"))

cep_hr_3day <- tidy(model3_cep_3day) %>%
  filter(term %in% c("shootings_lag3_acc_std", "beatings_lag3_acc_std", 
                     "arrests_lag3_acc_std", "crowd_control_lag3_acc_std")) %>%
  mutate(time = "3-day acc.",
         repression_type = case_when(
           str_detect(term, "shootings") ~ "Shootings",
           str_detect(term, "beatings") ~ "Beatings",
           str_detect(term, "arrests") ~ "Arrests",
           str_detect(term, "crowd_control") ~ "Crowd Control"))

cep_hr_7day <- tidy(model3_cep_7day) %>%
  filter(term %in% c("shootings_lag7_acc_std", "beatings_lag7_acc_std", 
                     "arrests_lag7_acc_std", "crowd_control_lag7_acc_std")) %>%
  mutate(time = "7-day acc.",
         repression_type = case_when(
           str_detect(term, "shootings") ~ "Shootings",
           str_detect(term, "beatings") ~ "Beatings",
           str_detect(term, "arrests") ~ "Arrests",
           str_detect(term, "crowd_control") ~ "Crowd Control"))

# Combine HR coefficients
cep_hr_coefs <- bind_rows(cep_hr_1day, cep_hr_3day, cep_hr_7day) %>%
  mutate(dv = "Human Rights Violations")

# Combine all CEP coefficients
all_cep_coefs <- bind_rows(cep_protest_coefs, cep_hr_coefs)

# Create separate plots
plot_protest <- all_cep_coefs %>%
  filter(dv == "Protest Justification") %>%
  ggplot(aes(x = estimate, y = repression_type, group = interaction(repression_type, time))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_linerange(aes(xmin = estimate - 1.96 * std.error, 
                     xmax = estimate + 1.96 * std.error),
                 position = position_dodge(width = 0.5), 
                 color = "black") +
  geom_linerange(aes(xmin = estimate - 1.645 * std.error, 
                     xmax = estimate + 1.645 * std.error),
                 position = position_dodge(width = 0.5), 
                 size = 1, 
                 color = "black") +
  geom_point(aes(shape = time), 
             position = position_dodge(width = 0.5), 
             size = 3, 
             fill = "white", 
             color = "black") +
  scale_shape_manual(values = c(21, 24, 22),
                     name = "Time") +
  coord_cartesian(xlim = c(-2.5, 2.5)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(5, 5, 5, 10)) +  # Add left margin
  labs(x = "Standardized Effect Size",
       title = "Protest Justification")

plot_protest

plot_hr <- all_cep_coefs %>%
  filter(dv == "Human Rights Violations") %>%
  ggplot(aes(x = estimate, y = repression_type, group = interaction(repression_type, time))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_linerange(aes(xmin = estimate - 1.96 * std.error, 
                     xmax = estimate + 1.96 * std.error),
                 position = position_dodge(width = 0.5), 
                 color = "black") +
  geom_linerange(aes(xmin = estimate - 1.645 * std.error, 
                     xmax = estimate + 1.645 * std.error),
                 position = position_dodge(width = 0.5), 
                 size = 1, 
                 color = "black") +
  geom_point(aes(shape = time), 
             position = position_dodge(width = 0.5), 
             size = 3, 
             fill = "white", 
             color = "black") +
  scale_shape_manual(values = c(21, 24, 22),
                     name = "Time") +
  coord_cartesian(xlim = c(-1.5, 1.5)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        plot.margin = margin(5, 10, 5, 5)) +  # Add right margin
  labs(x = "Standardized Effect Size",
       y = "Type of Repression",
       title = "Human Rights Violations")

plot_hr

# Extract the legend
legend <- gtable::gtable_filter(ggplotGrob(plot_hr + 
                                             theme(legend.position = "bottom", 
                                                   legend.box.margin = margin(0, 0, 0, 0))), 
                                "guide-box")


# Remove legend from HR plot
plot_hr <- plot_hr + theme(legend.position = "none")

# Combine plots with adjusted spacing
plot_cep_combined <- grid.arrange(
  arrangeGrob(plot_hr, plot_protest, 
              ncol = 2, 
              widths = c(1, 0.80)),
  legend,
  heights = c(8, 1),  # Reduced space before legend
  padding = unit(1, "line")  # Add padding between plots
)


#---------------------------- Appendix replication ----------------------------#

#--- Table A1 Distribution of repressive actions ---#

tabyl(indh_clean$repressive_type_original, sort = TRUE) #23 types


#--- Table B1 ---#

models_list <- list("1-day lag" = model_lag1_coes_std,
                    "1-day lag\n+ controls" = model_lag1_coes_std_ctr,
                    "3-day\naccumulated" = model_3day_coes_std,
                    "7-day\naccumulated" = model_7day_coes_std)

msummary(models_list,
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",  
         statistic = NULL,  # This keeps SEs on same line as coefficients
         coef_map = c("shootings_lag1_std" = "Shootings (t-1)",
                      "beatings_lag1_std" = "Beatings (t-1)",
                      "arrests_lag1_std" = "Arrests (t-1)",
                      "crowd_control_lag1_std" = "Crowd Control (t-1)",
                      "protest_coes_lag1_std" = "Protests (t-1)",
                      "police_per_100k_lag1_std" = "Police per 100k (t-1)",
                      "rain_lag1" = "Rain (t-1)",
                      "hot_day_lag1" = "Hot Day (t-1)",
                      "weekday_categoryWeekday" = "Weekday",
                      "distance_km_std" = "Distance to Capital",
                      # 3-day variables
                      "shootings_lag3_acc_std" = "Shootings (3-day)",
                      "beatings_lag3_acc_std" = "Beatings (3-day)",
                      "arrests_lag3_acc_std" = "Arrests (3-day)",
                      "crowd_control_lag3_acc_std" = "Crowd Control (3-day)",
                      "protest_coes_lag3_acc_std" = "Protests (3-day)",
                      "police_per_100k_lag3_acc_std" = "Police per 100k (3-day)",
                      "rain_lag3_acc" = "Rain Days (3-day)",
                      "hot_day_lag3_acc" = "Hot Days (3-day)",
                      # 7-day variables
                      "shootings_lag7_acc_std" = "Shootings (7-day)",
                      "beatings_lag7_acc_std" = "Beatings (7-day)",
                      "arrests_lag7_acc_std" = "Arrests (7-day)",
                      "crowd_control_lag7_acc_std" = "Crowd Control (7-day)",
                      "protest_coes_lag7_acc_std" = "Protests (7-day)",
                      "police_per_100k_lag7_acc_std" = "Police per 100k (7-day)",
                      "rain_lag7_acc" = "Rain Days (7-day)",
                      "hot_day_lag7_acc" = "Hot Days (7-day)"),
         notes = "Standard errors in parentheses",
         title = "Zero-Inflated Negative Binomial Models of Daily Protest Events",
         output = "markdown")


#--- Table B2. ZIBN without lagged DV ---#

# 1-day lag model without lagged protest variable
model_lag1_coes_std_no_ldv <- glmmTMB(protest_coes ~ 
                                        shootings_lag1_std + 
                                        beatings_lag1_std + 
                                        arrests_lag1_std + 
                                        crowd_control_lag1_std + 
                                        police_per_100k_lag1_std +
                                        rain_lag1 + 
                                        hot_day_lag1 +  
                                        weekday_category + 
                                        distance_km_std + 
                                        (1|nombre_comuna), 
                                      data = final_df_standardized, 
                                      family = nbinom1,  
                                      zi = ~1)

# 3-day accumulation model without lagged protest variable
model_3day_coes_std_no_ldv <- glmmTMB(protest_coes ~ 
                                        shootings_lag3_acc_std + 
                                        beatings_lag3_acc_std +
                                        arrests_lag3_acc_std + 
                                        crowd_control_lag3_acc_std + 
                                        police_per_100k_lag3_acc_std +
                                        rain_lag3_acc + 
                                        hot_day_lag3_acc +  
                                        distance_km_std + 
                                        (1|nombre_comuna),
                                      family = nbinom1, 
                                      zi = ~1, 
                                      data = final_df_standardized)

# 7-day accumulation model without lagged protest variable
model_7day_coes_std_no_ldv <- glmmTMB(protest_coes ~ 
                                        shootings_lag7_acc_std + 
                                        beatings_lag7_acc_std + 
                                        arrests_lag7_acc_std + 
                                        crowd_control_lag7_acc_std + 
                                        police_per_100k_lag7_acc_std +
                                        rain_lag7_acc + 
                                        hot_day_lag7_acc +  
                                        distance_km_std + 
                                        (1|nombre_comuna), 
                                      family = nbinom1, 
                                      zi = ~1, 
                                      data = final_df_standardized)


models_list_no_lag <- list("1-day lag\n+ controls" = model_lag1_coes_std_no_ldv,
                           "3-day\naccumulated" = model_3day_coes_std_no_ldv,
                           "7-day\naccumulated" = model_7day_coes_std_no_ldv)

msummary(models_list_no_lag,
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",  
         statistic = NULL,  # This keeps SEs on same line as coefficients
         coef_map = c("shootings_lag1_std" = "Shootings (t-1)",
                      "beatings_lag1_std" = "Beatings (t-1)",
                      "arrests_lag1_std" = "Arrests (t-1)",
                      "crowd_control_lag1_std" = "Crowd Control (t-1)",
                      "police_per_100k_lag1_std" = "Police per 100k (t-1)",
                      "rain_lag1" = "Rain (t-1)",
                      "hot_day_lag1" = "Hot Day (t-1)",
                      "weekday_categoryWeekday" = "Weekday",
                      "distance_km_std" = "Distance to Capital",
                      # 3-day variables
                      "shootings_lag3_acc_std" = "Shootings (3-day)",
                      "beatings_lag3_acc_std" = "Beatings (3-day)",
                      "arrests_lag3_acc_std" = "Arrests (3-day)",
                      "crowd_control_lag3_acc_std" = "Crowd Control (3-day)",
                      "police_per_100k_lag3_acc_std" = "Police per 100k (3-day)",
                      "rain_lag3_acc" = "Rain Days (3-day)",
                      "hot_day_lag3_acc" = "Hot Days (3-day)",
                      # 7-day variables
                      "shootings_lag7_acc_std" = "Shootings (7-day)",
                      "beatings_lag7_acc_std" = "Beatings (7-day)",
                      "arrests_lag7_acc_std" = "Arrests (7-day)",
                      "crowd_control_lag7_acc_std" = "Crowd Control (7-day)",
                      "police_per_100k_lag7_acc_std" = "Police per 100k (7-day)",
                      "rain_lag7_acc" = "Rain Days (7-day)",
                      "hot_day_lag7_acc" = "Hot Days (7-day)"),
         notes = "Standard errors in parentheses",
         title = "Zero-Inflated Negative Binomial Models of Daily Protest Events without LDV",
         output = "markdown")

#--- Figure C1. Comparison of effects ---#

names(final_df_rm_standardized)

# Initial model with 1 lag - standardized
model_lag1_coes_rm_std <- glmmTMB(protest_coes ~ shootings_lag1_std + beatings_lag1_std + 
                                    arrests_lag1_std + crowd_control_lag1_std + 
                                    protest_coes_lag1_std,
                                  data = final_df_rm_standardized, 
                                  family = nbinom1,
                                  zi = ~1)

# Model with t-1 lag + controls + random effects
model_lag1_coes_rm_std_ctr <- glmmTMB(protest_coes ~ shootings_lag1_std + beatings_lag1_std + 
                                        arrests_lag1_std + crowd_control_lag1_std + 
                                        protest_coes_lag1_std + police_per_100k_lag1_std +
                                        rain_lag1 + hot_day_lag1 +
                                        weekday_category + distance_km_std + (1|nombre_comuna), 
                                      data = final_df_rm_standardized, zi = ~1, family = nbinom1)

# Model with 3-day accumulation + controls + random effects
model_3day_coes_rm_std <- glmmTMB(protest_coes ~ shootings_lag3_acc_std + beatings_lag3_acc_std +
                                    arrests_lag3_acc_std + crowd_control_lag3_acc_std + 
                                    protest_coes_lag3_acc_std + police_per_100k_lag3_acc_std +
                                    rain_lag3_acc + hot_day_lag3_acc +
                                    distance_km_std + (1|nombre_comuna),
                                  family = nbinom1, zi = ~1, data = final_df_rm_standardized)

# Model with 7-day accumulation + controls + random effects
model_7day_coes_rm_std <- glmmTMB(protest_coes ~ shootings_lag7_acc_std + beatings_lag7_acc_std + 
                                    arrests_lag7_acc_std + crowd_control_lag7_acc_std + 
                                    protest_coes_lag7_acc_std + police_per_100k_lag7_acc_std +
                                    rain_lag7_acc + hot_day_lag7_acc +
                                    distance_km_std + (1|nombre_comuna), 
                                  family = nbinom1, zi = ~1, data = final_df_rm_standardized)

# Model comparison table
models_list_rm <- list("1-day lag" = model_lag1_coes_rm_std,
                       "1-day lag\n+ controls" = model_lag1_coes_rm_std_ctr,
                       "3-day\naccumulated" = model_3day_coes_rm_std,
                       "7-day\naccumulated" = model_7day_coes_rm_std)

msummary(models_list_rm,
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",
         statistic = NULL,
         coef_map = c("shootings_lag1_std" = "Shootings (t-1)",
                      "beatings_lag1_std" = "Beatings (t-1)",
                      "arrests_lag1_std" = "Arrests (t-1)",
                      "crowd_control_lag1_std" = "Crowd Control (t-1)",
                      "protest_coes_lag1_std" = "Protests (t-1)",
                      "police_per_100k_lag1_std" = "Police per 100k (t-1)",
                      "rain_lag1" = "Rain (t-1)",
                      "hot_day_lag1" = "Hot Day (t-1)",
                      "weekday_categoryWeekday" = "Weekday",
                      "distance_km_std" = "Distance to Capital",
                      "shootings_lag3_acc_std" = "Shootings (3-day)",
                      "beatings_lag3_acc_std" = "Beatings (3-day)",
                      "arrests_lag3_acc_std" = "Arrests (3-day)",
                      "crowd_control_lag3_acc_std" = "Crowd Control (3-day)",
                      "protest_coes_lag3_acc_std" = "Protests (3-day)",
                      "police_per_100k_lag3_acc_std" = "Police per 100k (3-day)",
                      "rain_lag3_acc" = "Rain Days (3-day)",
                      "hot_day_lag3_acc" = "Hot Days (3-day)",
                      "shootings_lag7_acc_std" = "Shootings (7-day)",
                      "beatings_lag7_acc_std" = "Beatings (7-day)",
                      "arrests_lag7_acc_std" = "Arrests (7-day)",
                      "crowd_control_lag7_acc_std" = "Crowd Control (7-day)",
                      "protest_coes_lag7_acc_std" = "Protests (7-day)",
                      "police_per_100k_lag7_acc_std" = "Police per 100k (7-day)",
                      "rain_lag7_acc" = "Rain Days (7-day)",
                      "hot_day_lag7_acc" = "Hot Days (7-day)"),
         notes = "Standard errors in parentheses",
         title = "Zero-Inflated Negative Binomial Models of Daily Protest Events",
         output = "markdown")

#- Now same models but with lacrimogenas and balines

# Initial model with 1 lag - standardized
model_lag1_coes_rm_police2_std <- glmmTMB(protest_coes ~ balines_total_lag1_std + lacrimogenas_total_lag1_std + 
                                            arrests_lag1_std + beatings_lag1_std + 
                                            protest_coes_lag1_std,
                                          data = final_df_rm_standardized, 
                                          family = nbinom1,
                                          zi = ~1)

# Model with t-1 lag + controls + random effects
model_lag1_coes_rm_police2_std_ctr <- glmmTMB(protest_coes ~ balines_total_lag1_std + lacrimogenas_total_lag1_std + 
                                                arrests_lag1_std + beatings_lag1_std + 
                                                protest_coes_lag1_std + police_per_100k_lag1_std +
                                                rain_lag1 + hot_day_lag1 +
                                                weekday_category + distance_km_std + (1|nombre_comuna), 
                                              data = final_df_rm_standardized, zi = ~1, family = nbinom1)

# Model with 3-day accumulation + controls + random effects
model_3day_coes_rm_police2_std <- glmmTMB(protest_coes ~ balines_total_lag3_acc_std + lacrimogenas_total_lag3_acc_std +
                                            arrests_lag3_acc_std + beatings_lag3_acc_std + 
                                            protest_coes_lag3_acc_std + police_per_100k_lag3_acc_std +
                                            rain_lag3_acc + hot_day_lag3_acc +
                                            distance_km_std + (1|nombre_comuna),
                                          family = nbinom1, zi = ~1, data = final_df_rm_standardized)

# Model with 7-day accumulation + controls + random effects
model_7day_coes_rm_police2_std <- glmmTMB(protest_coes ~ balines_total_lag7_acc_std + lacrimogenas_total_lag7_acc_std + 
                                            arrests_lag7_acc_std + beatings_lag7_acc_std + 
                                            protest_coes_lag7_acc_std + police_per_100k_lag7_acc_std +
                                            rain_lag7_acc + hot_day_lag7_acc +
                                            distance_km_std + (1|nombre_comuna), 
                                          family = nbinom1, zi = ~1, data = final_df_rm_standardized)

# Model comparison table
models_list_rm_police2 <- list("1-day lag" = model_lag1_coes_rm_police2_std,
                               "1-day lag\n+ controls" = model_lag1_coes_rm_police2_std_ctr,
                               "3-day\naccumulated" = model_3day_coes_rm_police2_std,
                               "7-day\naccumulated" = model_7day_coes_rm_police2_std)

msummary(models_list_rm_police2,
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",
         statistic = NULL,
         coef_map = c("balines_total_lag1_std" = "Rubber Bullets (t-1)",
                      "lacrimogenas_total_lag1_std" = "Tear Gas (t-1)",
                      "arrests_lag1_std" = "Arrests (t-1)",
                      "beatings_lag1_std" = "Beatings (t-1)",
                      "protest_coes_lag1_std" = "Protests (t-1)",
                      "police_per_100k_lag1_std" = "Police per 100k (t-1)",
                      "rain_lag1" = "Rain (t-1)",
                      "hot_day_lag1" = "Hot Day (t-1)",
                      "weekday_categoryWeekday" = "Weekday",
                      "distance_km_std" = "Distance to Capital",
                      "balines_total_lag3_acc_std" = "Rubber Bullets (3-day)",
                      "lacrimogenas_total_lag3_acc_std" = "Tear Gas (3-day)",
                      "arrests_lag3_acc_std" = "Arrests (3-day)",
                      "beatings_lag3_acc_std" = "Beatings (3-day)",
                      "protest_coes_lag3_acc_std" = "Protests (3-day)",
                      "police_per_100k_lag3_acc_std" = "Police per 100k (3-day)",
                      "rain_lag3_acc" = "Rain Days (3-day)",
                      "hot_day_lag3_acc" = "Hot Days (3-day)",
                      "balines_total_lag7_acc_std" = "Rubber Bullets (7-day)",
                      "lacrimogenas_total_lag7_acc_std" = "Tear Gas (7-day)",
                      "arrests_lag7_acc_std" = "Arrests (7-day)",
                      "beatings_lag7_acc_std" = "Beatings (7-day)",
                      "protest_coes_lag7_acc_std" = "Protests (7-day)",
                      "police_per_100k_lag7_acc_std" = "Police per 100k (7-day)",
                      "rain_lag7_acc" = "Rain Days (7-day)",
                      "hot_day_lag7_acc" = "Hot Days (7-day)"),
         notes = "Standard errors in parentheses",
         title = "Zero-Inflated Negative Binomial Models of Daily Protest Events (Police-Reported Data)",
         output = "markdown")

#- Now I'll plot the difference between INHD data and police data 

summary_1day_controls_original <- summary(model_lag1_coes_rm_std_ctr)
ci_1day_controls_original_95 <- confint(model_lag1_coes_rm_std_ctr, level=0.95)
ci_1day_controls_original_90 <- confint(model_lag1_coes_rm_std_ctr, level=0.90)

coefs_1day_controls_original <- data.frame(
  estimate = summary_1day_controls_original$coefficients$cond[c("shootings_lag1_std", "crowd_control_lag1_std"),1],
  std.error = summary_1day_controls_original$coefficients$cond[c("shootings_lag1_std", "crowd_control_lag1_std"),2],
  ci_lower_95 = ci_1day_controls_original_95[c("cond.shootings_lag1_std", "cond.crowd_control_lag1_std"),1],
  ci_upper_95 = ci_1day_controls_original_95[c("cond.shootings_lag1_std", "cond.crowd_control_lag1_std"),2],
  ci_lower_90 = ci_1day_controls_original_90[c("cond.shootings_lag1_std", "cond.crowd_control_lag1_std"),1],
  ci_upper_90 = ci_1day_controls_original_90[c("cond.shootings_lag1_std", "cond.crowd_control_lag1_std"),2],
  variable = c("shootings", "crowd_control"),
  source = "original"
)

# Police model
summary_1day_controls_police <- summary(model_lag1_coes_rm_police2_std_ctr)
ci_1day_controls_police_95 <- confint(model_lag1_coes_rm_police2_std_ctr, level=0.95)
ci_1day_controls_police_90 <- confint(model_lag1_coes_rm_police2_std_ctr, level=0.90)

coefs_1day_controls_police <- data.frame(
  estimate = summary_1day_controls_police$coefficients$cond[c("balines_total_lag1_std", "lacrimogenas_total_lag1_std"),1],
  std.error = summary_1day_controls_police$coefficients$cond[c("balines_total_lag1_std", "lacrimogenas_total_lag1_std"),2],
  ci_lower_95 = ci_1day_controls_police_95[c("cond.balines_total_lag1_std", "cond.lacrimogenas_total_lag1_std"),1],
  ci_upper_95 = ci_1day_controls_police_95[c("cond.balines_total_lag1_std", "cond.lacrimogenas_total_lag1_std"),2],
  ci_lower_90 = ci_1day_controls_police_90[c("cond.balines_total_lag1_std", "cond.lacrimogenas_total_lag1_std"),1],
  ci_upper_90 = ci_1day_controls_police_90[c("cond.balines_total_lag1_std", "cond.lacrimogenas_total_lag1_std"),2],
  variable = c("rubber_bullets", "tear_gas"),
  source = "police"
)

coefs_1day_controls_comparison <- rbind(coefs_1day_controls_original, coefs_1day_controls_police)

# 3-day models
summary_3day_original <- summary(model_3day_coes_rm_std)
ci_3day_original_95 <- confint(model_3day_coes_rm_std, level=0.95)
ci_3day_original_90 <- confint(model_3day_coes_rm_std, level=0.90)

coefs_3day_original <- data.frame(
  estimate = summary_3day_original$coefficients$cond[c("shootings_lag3_acc_std", "crowd_control_lag3_acc_std"),1],
  std.error = summary_3day_original$coefficients$cond[c("shootings_lag3_acc_std", "crowd_control_lag3_acc_std"),2],
  ci_lower_95 = ci_3day_original_95[c("cond.shootings_lag3_acc_std", "cond.crowd_control_lag3_acc_std"),1],
  ci_upper_95 = ci_3day_original_95[c("cond.shootings_lag3_acc_std", "cond.crowd_control_lag3_acc_std"),2],
  ci_lower_90 = ci_3day_original_90[c("cond.shootings_lag3_acc_std", "cond.crowd_control_lag3_acc_std"),1],
  ci_upper_90 = ci_3day_original_90[c("cond.shootings_lag3_acc_std", "cond.crowd_control_lag3_acc_std"),2],
  variable = c("shootings", "crowd_control"),
  source = "original",
  timeframe = "3-day"
)

summary_3day_police <- summary(model_3day_coes_rm_police2_std)
ci_3day_police_95 <- confint(model_3day_coes_rm_police2_std, level=0.95)
ci_3day_police_90 <- confint(model_3day_coes_rm_police2_std, level=0.90)

coefs_3day_police <- data.frame(
  estimate = summary_3day_police$coefficients$cond[c("balines_total_lag3_acc_std", "lacrimogenas_total_lag3_acc_std"),1],
  std.error = summary_3day_police$coefficients$cond[c("balines_total_lag3_acc_std", "lacrimogenas_total_lag3_acc_std"),2],
  ci_lower_95 = ci_3day_police_95[c("cond.balines_total_lag3_acc_std", "cond.lacrimogenas_total_lag3_acc_std"),1],
  ci_upper_95 = ci_3day_police_95[c("cond.balines_total_lag3_acc_std", "cond.lacrimogenas_total_lag3_acc_std"),2],
  ci_lower_90 = ci_3day_police_90[c("cond.balines_total_lag3_acc_std", "cond.lacrimogenas_total_lag3_acc_std"),1],
  ci_upper_90 = ci_3day_police_90[c("cond.balines_total_lag3_acc_std", "cond.lacrimogenas_total_lag3_acc_std"),2],
  variable = c("rubber_bullets", "tear_gas"),
  source = "police",
  timeframe = "3-day"
)

# 7-day models
summary_7day_original <- summary(model_7day_coes_rm_std)
ci_7day_original_95 <- confint(model_7day_coes_rm_std, level=0.95)
ci_7day_original_90 <- confint(model_7day_coes_rm_std, level=0.90)

coefs_7day_original <- data.frame(
  estimate = summary_7day_original$coefficients$cond[c("shootings_lag7_acc_std", "crowd_control_lag7_acc_std"),1],
  std.error = summary_7day_original$coefficients$cond[c("shootings_lag7_acc_std", "crowd_control_lag7_acc_std"),2],
  ci_lower_95 = ci_7day_original_95[c("cond.shootings_lag7_acc_std", "cond.crowd_control_lag7_acc_std"),1],
  ci_upper_95 = ci_7day_original_95[c("cond.shootings_lag7_acc_std", "cond.crowd_control_lag7_acc_std"),2],
  ci_lower_90 = ci_7day_original_90[c("cond.shootings_lag7_acc_std", "cond.crowd_control_lag7_acc_std"),1],
  ci_upper_90 = ci_7day_original_90[c("cond.shootings_lag7_acc_std", "cond.crowd_control_lag7_acc_std"),2],
  variable = c("shootings", "crowd_control"),
  source = "original",
  timeframe = "7-day"
)

summary_7day_police <- summary(model_7day_coes_rm_police2_std)
ci_7day_police_95 <- confint(model_7day_coes_rm_police2_std, level=0.95)
ci_7day_police_90 <- confint(model_7day_coes_rm_police2_std, level=0.90)

coefs_7day_police <- data.frame(
  estimate = summary_7day_police$coefficients$cond[c("balines_total_lag7_acc_std", "lacrimogenas_total_lag7_acc_std"),1],
  std.error = summary_7day_police$coefficients$cond[c("balines_total_lag7_acc_std", "lacrimogenas_total_lag7_acc_std"),2],
  ci_lower_95 = ci_7day_police_95[c("cond.balines_total_lag7_acc_std", "cond.lacrimogenas_total_lag7_acc_std"),1],
  ci_upper_95 = ci_7day_police_95[c("cond.balines_total_lag7_acc_std", "cond.lacrimogenas_total_lag7_acc_std"),2],
  ci_lower_90 = ci_7day_police_90[c("cond.balines_total_lag7_acc_std", "cond.lacrimogenas_total_lag7_acc_std"),1],
  ci_upper_90 = ci_7day_police_90[c("cond.balines_total_lag7_acc_std", "cond.lacrimogenas_total_lag7_acc_std"),2],
  variable = c("rubber_bullets", "tear_gas"),
  source = "police",
  timeframe = "7-day"
)

# Add timeframe to 1-day data and combine all
coefs_1day_controls_comparison$timeframe <- "1-day"

# Combine all datasets
coefs_all_comparison <- rbind(
  coefs_1day_controls_comparison,
  coefs_3day_original,
  coefs_3day_police,
  coefs_7day_original,
  coefs_7day_police)

#- Now to the plot

# Shooting of rubber bullets

# Filter and organize the data
shootings_data <- coefs_all_comparison[coefs_all_comparison$variable %in% c("shootings", "rubber_bullets"),]

# Create and consistently apply factor levels for 'source'
shootings_data$source <- ifelse(shootings_data$variable == "shootings", "INDH Reported", "Police Reported")
shootings_data$source <- factor(shootings_data$source, levels = c("INDH Reported", "Police Reported"))

# Sort data by 'timeframe' and 'source' for consistent plotting
shootings_data <- shootings_data[order(shootings_data$timeframe, shootings_data$source),]

# Generate the plot
plot_shootings_rubber <- ggplot(shootings_data, aes(x = estimate, y = timeframe, 
                                                    group = interaction(timeframe, source))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # 95% CI
  geom_linerange(aes(xmin = ci_lower_95, xmax = ci_upper_95),
                 position = position_dodge(width = 0.5), color = "black") +
  # 90% CI
  geom_linerange(aes(xmin = ci_lower_90, xmax = ci_upper_90),
                 position = position_dodge(width = 0.5), size = 1, color = "black") +
  # Points with shapes corresponding to the source
  geom_point(aes(shape = source), 
             position = position_dodge(width = 0.5), size = 3, fill = "white", color = "black") +
  scale_shape_manual(values = c(21, 24),  # Hollow circle and triangle
                     labels = c("INDH Reported", "Police Reported"),
                     name = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  labs(x = "Standardized Effect Size",
       y = "Time Frame",
       title = "Shooting of Rubber Bullets") +
  coord_cartesian(xlim = c(-0.5, 0.5))

# Display the plot
plot_shootings_rubber

# Crowd control / lacrimogenas

# Filter and organize the data
control_data <- coefs_all_comparison[coefs_all_comparison$variable %in% c("crowd_control", "tear_gas"),]
control_data$source <- ifelse(control_data$variable == "crowd_control", "INDH Reported", "Police Reported")
control_data$source <- factor(control_data$source, levels = c("INDH Reported", "Police Reported"))
control_data <- control_data[order(control_data$timeframe, control_data$source),]

# Generate plot
plot_control_gas <- ggplot(control_data, aes(x = estimate, y = timeframe, 
                                             group = interaction(timeframe, source))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_linerange(aes(xmin = ci_lower_95, xmax = ci_upper_95),
                 position = position_dodge(width = 0.5), color = "black") +
  geom_linerange(aes(xmin = ci_lower_90, xmax = ci_upper_90),
                 position = position_dodge(width = 0.5), size = 1, color = "black") +
  geom_point(aes(shape = source), 
             position = position_dodge(width = 0.5), size = 3, fill = "white", color = "black") +
  scale_shape_manual(values = c(21, 24),
                     labels = c("INDH Reported", "Police Reported"),
                     name = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  labs(x = "Standardized Effect Size",
       y = "Time Frame",
       title = "Crowd Control Techniques (Tear Gas)") +
  coord_cartesian(xlim = c(-0.5, 0.5))

plot_control_gas


# Arrange plots side by side
combined_plots_comparison <- gridExtra::grid.arrange(plot_shootings_rubber, plot_control_gas, ncol = 2)


#--- Table C1. Effect of media reports of violence against civilians ---#

# 1-day lag 
model_lag1_cont <- glmmTMB(protest_coes ~ violence_acled_lag1  + 
                             protest_coes_lag1_std + police_per_100k_lag1_std +
                             rain_lag1 + hot_day_lag1 + 
                             weekday_category + distance_km_std + (1|nombre_comuna), 
                           data = final_df_standardized, zi = ~1, family = nbinom1)

# 3-day window 
model_3day_cont <- glmmTMB(protest_coes ~ violence_acled_lag3_acc +
                             protest_coes_lag3_acc_std + police_per_100k_lag3_acc_std +
                             rain_lag3_acc + hot_day_lag3_acc + 
                             distance_km_std + (1|nombre_comuna),
                           family = nbinom1, zi = ~1, data = final_df_standardized)

# 7-day 
model_7day_cont <- glmmTMB(protest_coes ~ violence_acled_lag7_acc + 
                             protest_coes_lag7_acc_std + police_per_100k_lag7_acc_std +
                             rain_lag7_acc + hot_day_lag7_acc + 
                             distance_km_std + (1|nombre_comuna),
                           family = nbinom1, zi = ~1, data = final_df_standardized)


msummary(
  list("t-1" = model_lag1_cont, "3-day" = model_3day_cont, "7-day" = model_7day_cont),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  coef_map = c(
    "violence_acled_lag1" = "Violence (t-1)",
    "violence_acled_lag3_acc" = "Violence (3-day)",
    "violence_acled_lag7_acc" = "Violence (7-day)",
    
    "protest_coes_lag1_std" = "Protests (t-1)",
    "protest_coes_lag3_acc_std" = "Protests (3-day)",
    "protest_coes_lag7_acc_std" = "Protests (7-day)",
    
    "police_per_100k_lag1_std" = "Police per 100k (t-1)",
    "police_per_100k_lag3_acc_std" = "Police per 100k (3-day)",
    "police_per_100k_lag7_acc_std" = "Police per 100k (7-day)",
    
    "rain_lag1" = "Rain Days (t-1)",
    "rain_lag3_acc" = "Rain Days (3-day)",
    "rain_lag7_acc" = "Rain Days (7-day)",
    
    "hot_day_lag1" = "Hot Days (t-1)",
    "hot_day_lag3_acc" = "Hot Days (3-day)",
    "hot_day_lag7_acc" = "Hot Days (7-day)",
    
    "weekday_categoryWeekday" = "Weekday",
    "distance_km_std" = "Distance Regional Capital"
  ),
  notes = "Standard errors in parentheses",
  title = "Effect of Violence on Protests",
  output = "markdown")

#--- Table C2. Political opportunities and resources ---#

model_opportunities <- glmmTMB(protest_coes ~ shootings_lag1_std + beatings_lag1_std + 
                                 arrests_lag1_std + crowd_control_lag1_std + 
                                 protest_coes_lag1_std + police_per_100k_lag1_std +
                                 # Opportunity variable
                                 porcentaje_pinera +
                                 # Basic controls
                                 rain_lag1 + hot_day_lag1 + weekday_category + 
                                 distance_km_std + (1|nombre_comuna), 
                               data = final_df_standardized, zi = ~1, family = nbinom1)

model_resources <- glmmTMB(protest_coes ~ shootings_lag1_std + beatings_lag1_std + 
                             arrests_lag1_std + crowd_control_lag1_std + 
                             protest_coes_lag1_std + police_per_100k_lag1_std +
                             # Resource variables
                             strike_count + total_institutions + prop_opposition +
                             # Basic controls
                             rain_lag1 + hot_day_lag1 + weekday_category + 
                             distance_km_std + (1|nombre_comuna), 
                           data = final_df_standardized, zi = ~1, family = nbinom1)

model_opportunities_interaction <- glmmTMB(protest_coes ~ shootings_lag1_std + beatings_lag1_std + 
                                             arrests_lag1_std + crowd_control_lag1_std + 
                                             protest_coes_lag1_std + police_per_100k_lag1_std +
                                             strike_count + total_institutions + prop_opposition +
                                             porcentaje_pinera +
                                             # Interactions with opportunities
                                             shootings_lag1_std:porcentaje_pinera +
                                             beatings_lag1_std:porcentaje_pinera +
                                             arrests_lag1_std:porcentaje_pinera +
                                             crowd_control_lag1_std:porcentaje_pinera +
                                             # Basic controls
                                             rain_lag1 + hot_day_lag1 + weekday_category + 
                                             distance_km_std + (1|nombre_comuna), 
                                           data = final_df_standardized, zi = ~1, family = nbinom1)

model_resources_interaction <- glmmTMB(protest_coes ~ shootings_lag1_std + beatings_lag1_std + 
                                         arrests_lag1_std + crowd_control_lag1_std + 
                                         protest_coes_lag1_std + police_per_100k_lag1_std +
                                         strike_count + total_institutions + prop_opposition +
                                         porcentaje_pinera +
                                         # Interactions with resources
                                         shootings_lag1_std:strike_count +
                                         beatings_lag1_std:strike_count +
                                         arrests_lag1_std:strike_count +
                                         crowd_control_lag1_std:strike_count +
                                         shootings_lag1_std:total_institutions +
                                         beatings_lag1_std:total_institutions +
                                         arrests_lag1_std:total_institutions +
                                         crowd_control_lag1_std:total_institutions +
                                         shootings_lag1_std:prop_opposition +
                                         beatings_lag1_std:prop_opposition +
                                         arrests_lag1_std:prop_opposition +
                                         crowd_control_lag1_std:prop_opposition +
                                         # Basic controls
                                         rain_lag1 + hot_day_lag1 + weekday_category + 
                                         distance_km_std + (1|nombre_comuna), 
                                       data = final_df_standardized, zi = ~1, family = nbinom1)


msummary(
  list("Opportunities" = model_opportunities, 
       "Resources" = model_resources, 
       "Opportunities\n Interactions" = model_opportunities_interaction, 
       "Resources\n Interactions" = model_resources_interaction),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  coef_map = c(
    # Repression variables
    "shootings_lag1_std" = "Shootings (t-1)",
    "beatings_lag1_std" = "Beatings (t-1)",
    "arrests_lag1_std" = "Arrests (t-1)",
    "crowd_control_lag1_std" = "Crowd Control (t-1)",
    
    # Control variables
    "protest_coes_lag1_std" = "Protests (t-1)",
    "police_per_100k_lag1_std" = "Police per 100k (t-1)",
    
    # Resource variables
    "strike_count" = "Strike Count",
    "total_institutions" = "Universities",
    "prop_opposition" = "Opposition Council Share",
    
    # Opportunity variable
    "porcentaje_pinera" = "Pro-Government Vote Share",
    
    # Weather controls
    "rain_lag1" = "Rain",
    "hot_day_lag1" = "Hot Day",
    
    # Other controls
    "weekday_categoryWeekday" = "Weekday",
    "distance_km_std" = "Distance to Regional Capital",
    
    # Interaction terms - Resources
    "shootings_lag1_std:strike_count" = "Shootings × Strike Count",
    "beatings_lag1_std:strike_count" = "Beatings × Strike Count",
    "arrests_lag1_std:strike_count" = "Arrests × Strike Count",
    "crowd_control_lag1_std:strike_count" = "Crowd Control × Strike Count",
    
    "shootings_lag1_std:total_institutions" = "Shootings × Universities",
    "beatings_lag1_std:total_institutions" = "Beatings × Universities",
    "arrests_lag1_std:total_institutions" = "Arrests × Universities",
    "crowd_control_lag1_std:total_institutions" = "Crowd Control × Universities",
    
    "shootings_lag1_std:prop_opposition" = "Shootings × Opposition Share",
    "beatings_lag1_std:prop_opposition" = "Beatings × Opposition Share",
    "arrests_lag1_std:prop_opposition" = "Arrests × Opposition Share",
    "crowd_control_lag1_std:prop_opposition" = "Crowd Control × Opposition Share",
    
    # Interaction terms - Opportunities
    "shootings_lag1_std:porcentaje_pinera" = "Shootings × Gov. Vote Share",
    "beatings_lag1_std:porcentaje_pinera" = "Beatings × Gov. Vote Share",
    "arrests_lag1_std:porcentaje_pinera" = "Arrests × Gov. Vote Share",
    "crowd_control_lag1_std:porcentaje_pinera" = "Crowd Control × Gov. Vote Share"
  ),
  notes = "All continuous variables are standardized (mean = 0, SD = 1). Standard errors in parentheses.",
  title = "Repression Effects with Resources and Political Opportunities",
  output = "markdown")

#--- Table D1. Models for Protest Justification ---#

# - ESP_41_1 justification of protest participation -> protest_justification

# 1-day lag model
model2_cep_1day <- feols(protest_justification ~ shootings_lag1_std + beatings_lag1_std + 
                           arrests_lag1_std + crowd_control_lag1_std + 
                           police_per_100k_lag1_std +
                           education_level + gender + age |
                           nombre_comuna, 
                         cluster = "nombre_comuna",
                         data = cep_survey_protest)

# 3-day accumulation model 
model2_cep_3day <- feols(protest_justification ~ shootings_lag3_acc_std + beatings_lag3_acc_std + 
                           arrests_lag3_acc_std + crowd_control_lag3_acc_std + 
                           police_per_100k_lag3_acc_std +
                           education_level + gender + age |
                           nombre_comuna,
                         cluster = "nombre_comuna", 
                         data = cep_survey_protest)

# 7-day accumulation model
model2_cep_7day <- feols(protest_justification ~ shootings_lag7_acc_std + beatings_lag7_acc_std + 
                           arrests_lag7_acc_std + crowd_control_lag7_acc_std + 
                           police_per_100k_lag7_acc_std +
                           education_level + gender + age |
                           nombre_comuna,
                         cluster = "nombre_comuna",
                         data = cep_survey_protest)

msummary(
  list(model2_cep_1day, model2_cep_3day, model2_cep_7day),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  coef_omit = "factor\\(education_level\\)\\.Q|factor\\(education_level\\)\\.C", # regex to omit .Q and .C
  coef_map = c(
    "shootings_lag1_std" = "Shootings (t-1)",
    "beatings_lag1_std" = "Beatings (t-1)",
    "arrests_lag1_std" = "Arrests (t-1)",
    "crowd_control_lag1_std" = "Crowd Control (t-1)",
    "police_per_100k_lag1_std" = "Police per 100k (t-1)",
    
    "shootings_lag3_acc_std" = "Shootings (3-day)",
    "beatings_lag3_acc_std" = "Beatings (3-day)",
    "arrests_lag3_acc_std" = "Arrests (3-day)",
    "crowd_control_lag3_acc_std" = "Crowd Control (3-day)",
    "police_per_100k_lag3_acc_std" = "Police per 100k (3-day)",
    
    "shootings_lag7_acc_std" = "Shootings (7-day)",
    "beatings_lag7_acc_std" = "Beatings (7-day)",
    "arrests_lag7_acc_std" = "Arrests (7-day)",
    "crowd_control_lag7_acc_std" = "Crowd Control (7-day)",
    "police_per_100k_lag7_acc_std" = "Police per 100k (7-day)",
    
    # Just map the linear term
    "education_level.L" = "Education Level (Linear)",
    
    "genderFemale" = "Female",
    "age" = "Age"
  ),
  notes = "Standard errors in parentheses",
  title = "Models for Protest Justification",
  output = "markdown")

#--- Table D2. Models for Human Rights violations ---#


# - ESP_43 frequency human rights violations -> hr_violations

# 1-day lag model
model3_cep_1day <- feols(hr_violations ~ shootings_lag1_std + beatings_lag1_std + 
                           arrests_lag1_std + crowd_control_lag1_std + eval_carab +
                           police_per_100k_lag1_std +
                           education_level + gender + age |
                           nombre_comuna, 
                         cluster = "nombre_comuna",
                         data = cep_survey_protest)

# 3-day accumulation model
model3_cep_3day <- feols(hr_violations ~ shootings_lag3_acc_std + beatings_lag3_acc_std + 
                           arrests_lag3_acc_std + crowd_control_lag3_acc_std + eval_carab +
                           police_per_100k_lag3_acc_std +
                           education_level + gender + age |
                           nombre_comuna,
                         cluster = "nombre_comuna",
                         data = cep_survey_protest)

# 7-day accumulation model
model3_cep_7day <- feols(hr_violations ~ shootings_lag7_acc_std + beatings_lag7_acc_std + 
                           arrests_lag7_acc_std + crowd_control_lag7_acc_std + eval_carab +
                           police_per_100k_lag7_acc_std +
                           education_level + gender + age |
                           nombre_comuna,
                         cluster = "nombre_comuna",
                         data = cep_survey_protest)

msummary(
  list(model3_cep_1day, model3_cep_3day, model3_cep_7day),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  coef_map = c(
    "shootings_lag1_std" = "Shootings (t-1)",
    "beatings_lag1_std" = "Beatings (t-1)",
    "arrests_lag1_std" = "Arrests (t-1)",
    "crowd_control_lag1_std" = "Crowd Control (t-1)",
    "police_per_100k_lag1_std" = "Police per 100k (t-1)",
    
    "shootings_lag3_acc_std" = "Shootings (3-day)",
    "beatings_lag3_acc_std" = "Beatings (3-day)",
    "arrests_lag3_acc_std" = "Arrests (3-day)",
    "crowd_control_lag3_acc_std" = "Crowd Control (3-day)",
    "police_per_100k_lag3_acc_std" = "Police per 100k (3-day)",
    
    "shootings_lag7_acc_std" = "Shootings (7-day)",
    "beatings_lag7_acc_std" = "Beatings (7-day)",
    "arrests_lag7_acc_std" = "Arrests (7-day)",
    "crowd_control_lag7_acc_std" = "Crowd Control (7-day)",
    "police_per_100k_lag7_acc_std" = "Police per 100k (7-day)",
    
    # Polynomial terms for education_level
    "education_level.L" = "Education Level (Linear)",
    
    "genderFemale" = "Female",
    "age" = "Age"
  ),
  notes = "Standard errors in parentheses",
  title = 'Models for Human Rights Violations',
  output = 'markdown')

#--- Figure E1. Trends in mass mobilization per year ---#

load("01-data/01-raw-data/mass-mobilization/mmALL_073120_v16.RData")
mm_data <- table
rm(table)


# Plot yearly trends on protests
yearly_protests <- mm_data %>%
  filter(year != 2020) %>%  # Exclude the year 2020
  group_by(year) %>%
  summarise(protestnumber = sum(protestnumber, na.rm = TRUE))


## Create the time series plot
protest_trends <- ggplot(yearly_protests, aes(x = year, y = protestnumber)) +
  geom_line() +  
  geom_point() + 
  labs(x = "Year", y = "Number of Protests") +
  scale_x_continuous(breaks = yearly_protests$year) + 
  scale_y_continuous(limits = c(NA, 20000)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

protest_trends


#--- Figure E2. Trends in violence against civilians ---#

# Plot yearly trends on violence against civilians
violence_civilians <- acled_violence %>%
  group_by(year) %>%
  summarise(number_of_events = n())

## Create the time trend plot
violence_civilians_trends <- ggplot(violence_civilians, aes(x = year, y = number_of_events)) +
  geom_line() +  
  geom_point() +  
  labs(x = "Year", y = "Number of Events") +
  scale_x_continuous(breaks = violence_civilians$year) + 
  scale_y_continuous(limits = c(NA, 6000)) + 
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  #
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

violence_civilians_trends


#--- Figure E3. Violence against civilians ---#

# Total number of violence events
sum(final_df_standardized$violence_acled, na.rm = TRUE)

# Number of days with at least one violence event
sum(final_df_standardized$violence_acled > 0, na.rm = TRUE)

# Plot distribution of violence against civilians

# Aggregate violence by date
violence_ts <- final_df_standardized %>%
  group_by(date) %>%
  summarise(
    total_violence = sum(violence_acled, na.rm = TRUE)
  )

# Create time series plot
violence_civilians_plot <- ggplot(violence_ts, aes(x = date, y = total_violence)) +
  geom_col(fill = "black", alpha = 0.7) +
  geom_smooth(method = "loess", color = "#e74c3c", se = FALSE, size = 1) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, max(violence_ts$total_violence), 1)) +  # Dynamic breaks
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  labs(
    x = "Date",
    y = "Number of Violence Events"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank())

violence_civilians_plot


#--- Figure E4. Distribution of arrests ---#

names(acd_dates_delitos)

str(acd_dates_delitos)


# Summarize data for the alluvial plot
alluvial_data <- acd_dates_delitos %>%
  mutate(
    protest_related = as.factor(protest_related),  # Convert to factor
    decreto_pp = as.factor(decreto_pp)  # Convert to factor
  ) %>%
  count(protest_related, decreto_pp)

# Update the labels for `protest_related` and `decreto_pp` and set order
alluvial_data <- alluvial_data %>%
  mutate(
    protest_related = factor(recode(protest_related, `0` = "No", `1` = "Yes"), levels = c("Yes", "No")),
    decreto_pp = factor(recode(decreto_pp, `No` = "No", `Sí` = "Yes"), levels = c("Yes", "No")))

# Plot the alluvial chart with updated labels and reordered categories
acd_plot <- ggplot(alluvial_data, aes(axis1 = protest_related, axis2 = decreto_pp, y = n)) +
  geom_alluvium(aes(fill = protest_related), width = 0.1, alpha = 0.8) +
  geom_stratum(width = 0.2, color = "black", fill = "grey90") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  scale_x_discrete(limits = c("Protest Related Offense", "Arrest Warrant"), expand = c(0.15, 0.15)) +
  scale_fill_manual(values = c("red", "gray70")) +
  labs(x = "", y = "Number of Detentions") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_blank())

acd_plot