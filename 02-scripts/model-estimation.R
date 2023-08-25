#---------------------------------Models Estimation--------------------------------#
#-Author: Francisca Castro -------------------------------- Created: June 30, 2023-#
#-R Version: 4.3.1 -------------------------------------- Revised: August 25, 2023-#

pacman::p_load(modelsummary, MASS, pscl, glmmTMB, AICcmodavg, ggplot2, DHARMa, car, broom.mixed,
               dotwhisker, lubridate, magrittr, tidyverse, optimx, marginaleffects, boot, forcats,
               emmeans, VGAM, insight, ggeffects, dplyr, lme4)


# 1) Preparing the data

## Create lags

final_df_lagged <- final_df %>%
  group_by(nombre_comuna) %>%
  arrange(date) %>%
  mutate(across(c(contentious_total, disparos_total, gaseado_total,
                  mojado_guanaco_total, golpiza_total, detencion_total,
                  invasion_ingreso_total, amenaza_total, atropello_total,
                  otros_aggr),
                ~ dplyr::lag(., 1), .names = "{.col}_lag1")) %>%
  mutate(across(c(contentious_total, disparos_total, gaseado_total,
                  mojado_guanaco_total, golpiza_total, detencion_total,
                  invasion_ingreso_total, amenaza_total, atropello_total,
                  otros_aggr),
                ~ dplyr::lag(.,2), .names = "{.col}_lag2")) %>%
  mutate(across(c(contentious_total, disparos_total, gaseado_total,
                  mojado_guanaco_total, golpiza_total, detencion_total,
                  invasion_ingreso_total, amenaza_total, atropello_total,
                  otros_aggr),
                ~ dplyr::lag(.,3), .names = "{.col}_lag3"))

## Remove municipalities without any protest

final_df_lag_filter <- final_df_lagged %>%
  group_by(nombre_comuna) %>%
  mutate(total_protests = sum(contentious_total)) %>%
  filter(total_protests > 0) %>%
  dplyr::select(-total_protests)

unique(final_df$nombre_comuna)
unique(final_df_lag_filter$nombre_comuna)


# 2) Model Estimation

## Initial exploration with lags

# Model with t-1 lag
model_lag1 <- glmmTMB(contentious_total ~ contentious_total_lag1 + disparos_total_lag1 + 
                        gaseado_total_lag1 + mojado_guanaco_total_lag1 + golpiza_total_lag1 +
                        detencion_total_lag1 + invasion_ingreso_total_lag1 + amenaza_total_lag1 + 
                        atropello_total_lag1 + otros_aggr_lag1, data = final_df_lag_filter, 
                      family = nbinom2,
                      ziformula=~1)

# Model with t-2 lag
model_lag2 <- glmmTMB(contentious_total ~ contentious_total_lag1 + disparos_total_lag1 + 
                        gaseado_total_lag1 + mojado_guanaco_total_lag1 + golpiza_total_lag1 +
                        detencion_total_lag1 + invasion_ingreso_total_lag1 + amenaza_total_lag1 + 
                        atropello_total_lag1 + otros_aggr_lag1 + contentious_total_lag2 + disparos_total_lag2 + 
                        gaseado_total_lag2 + mojado_guanaco_total_lag2 + golpiza_total_lag2 +
                        detencion_total_lag2 + invasion_ingreso_total_lag2 + amenaza_total_lag2 + 
                        atropello_total_lag2 + otros_aggr_lag2, data = final_df_lag_filter, 
                      family = nbinom2,
                      ziformula=~1)


# Model with t-3 lag
model_lag3 <- glmmTMB(contentious_total ~ contentious_total_lag1 + disparos_total_lag1 + 
                        gaseado_total_lag1 + mojado_guanaco_total_lag1 + golpiza_total_lag1 +
                        detencion_total_lag1 + invasion_ingreso_total_lag1 + amenaza_total_lag1 + 
                        atropello_total_lag1 + otros_aggr_lag1 + contentious_total_lag2 + disparos_total_lag2 + 
                        gaseado_total_lag2 + mojado_guanaco_total_lag2 + golpiza_total_lag2 +
                        detencion_total_lag2 + invasion_ingreso_total_lag2 + amenaza_total_lag2 + 
                        atropello_total_lag2 + otros_aggr_lag2 + contentious_total_lag3 + disparos_total_lag3 + 
                        gaseado_total_lag3 + mojado_guanaco_total_lag3 + golpiza_total_lag3 +
                        detencion_total_lag3 + invasion_ingreso_total_lag3 + amenaza_total_lag3 + 
                        atropello_total_lag3 + otros_aggr_lag3, data = final_df_lag_filter, 
                      family = nbinom2,
                      ziformula=~1)

msummary(list(model_lag1,model_lag2,model_lag3),
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_omit = "^(?!.*_lag)",
         output = "markdown")

## Compare the models

bbmle::AICtab(model_lag1,model_lag2,model_lag3)

#`model_lag3` has a better fit, which will be the model that will be used for subsequent analysis

# 3) Further specifications of the model

# Model with FE by municipal level
model_lag3_fe <- glmmTMB(contentious_total ~ contentious_total_lag1 + disparos_total_lag1 + 
                           gaseado_total_lag1 + mojado_guanaco_total_lag1 + golpiza_total_lag1 +
                           detencion_total_lag1 + invasion_ingreso_total_lag1 + amenaza_total_lag1 + 
                           atropello_total_lag1 + otros_aggr_lag1 + contentious_total_lag2 + disparos_total_lag2 + 
                           gaseado_total_lag2 + mojado_guanaco_total_lag2 + golpiza_total_lag2 +
                           detencion_total_lag2 + invasion_ingreso_total_lag2 + amenaza_total_lag2 + 
                           atropello_total_lag2 + otros_aggr_lag2 + contentious_total_lag3 + disparos_total_lag3 + 
                           gaseado_total_lag3 + mojado_guanaco_total_lag3 + golpiza_total_lag3 +
                           detencion_total_lag3 + invasion_ingreso_total_lag3 + amenaza_total_lag3 + 
                           atropello_total_lag3 + otros_aggr_lag3 + nombre_comuna, data = final_df_lag_filter, 
                         family = nbinom2,
                         ziformula=~1)

# Model with RE by municipal level
model_lag3_re <- glmmTMB(contentious_total ~ contentious_total_lag1 + disparos_total_lag1 + 
                           gaseado_total_lag1 + mojado_guanaco_total_lag1 + golpiza_total_lag1 +
                           detencion_total_lag1 + invasion_ingreso_total_lag1 + amenaza_total_lag1 + 
                           atropello_total_lag1 + otros_aggr_lag1 + contentious_total_lag2 + disparos_total_lag2 + 
                           gaseado_total_lag2 + mojado_guanaco_total_lag2 + golpiza_total_lag2 +
                           detencion_total_lag2 + invasion_ingreso_total_lag2 + amenaza_total_lag2 + 
                           atropello_total_lag2 + otros_aggr_lag2 + contentious_total_lag3 + disparos_total_lag3 + 
                           gaseado_total_lag3 + mojado_guanaco_total_lag3 + golpiza_total_lag3 +
                           detencion_total_lag3 + invasion_ingreso_total_lag3 + amenaza_total_lag3 + 
                           atropello_total_lag3 + otros_aggr_lag3 + (1|nombre_comuna), data = final_df_lag_filter, 
                         family = nbinom2,
                         ziformula=~1)

bbmle::AICtab(model_lag3,model_lag3_fe,model_lag3_re)

# `model_lag3_fe` has a better fit
# Subsequently I'll use `model_lag3_fe` as a final model

msummary(list(model_lag1,model_lag2,model_lag3, model_lag3_fe, model_lag3_re),
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_omit = "^(?!.*_lag)",
         output = "latex")

# 4) Robustness

model_lag3_fe_poisson <- glm(contentious_total ~ contentious_total_lag1 + disparos_total_lag1 + 
                               gaseado_total_lag1 + mojado_guanaco_total_lag1 + golpiza_total_lag1 +
                               detencion_total_lag1 + invasion_ingreso_total_lag1 + amenaza_total_lag1 + 
                               atropello_total_lag1 + otros_aggr_lag1 + contentious_total_lag2 + disparos_total_lag2 + 
                               gaseado_total_lag2 + mojado_guanaco_total_lag2 + golpiza_total_lag2 +
                               detencion_total_lag2 + invasion_ingreso_total_lag2 + amenaza_total_lag2 + 
                               atropello_total_lag2 + otros_aggr_lag2 + contentious_total_lag3 + disparos_total_lag3 + 
                               gaseado_total_lag3 + mojado_guanaco_total_lag3 + golpiza_total_lag3 +
                               detencion_total_lag3 + invasion_ingreso_total_lag3 + amenaza_total_lag3 + 
                               atropello_total_lag3 + otros_aggr_lag3 + nombre_comuna, data = final_df_lag_filter, 
                             family = poisson(link = "log"))

model_lag3_fe_nb <- glm.nb(contentious_total ~ contentious_total_lag1 + disparos_total_lag1 + 
                             gaseado_total_lag1 + mojado_guanaco_total_lag1 + golpiza_total_lag1 +
                             detencion_total_lag1 + invasion_ingreso_total_lag1 + amenaza_total_lag1 + 
                             atropello_total_lag1 + otros_aggr_lag1 + contentious_total_lag2 + disparos_total_lag2 + 
                             gaseado_total_lag2 + mojado_guanaco_total_lag2 + golpiza_total_lag2 +
                             detencion_total_lag2 + invasion_ingreso_total_lag2 + amenaza_total_lag2 + 
                             atropello_total_lag2 + otros_aggr_lag2 + contentious_total_lag3 + disparos_total_lag3 + 
                             gaseado_total_lag3 + mojado_guanaco_total_lag3 + golpiza_total_lag3 +
                             detencion_total_lag3 + invasion_ingreso_total_lag3 + amenaza_total_lag3 + 
                             atropello_total_lag3 + otros_aggr_lag3 + nombre_comuna, data = final_df_lag_filter)

msummary(list(model_lag3_fe_poisson,model_lag3_fe_nb),
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_omit = "^(?!.*_lag)",
         output = "latex")

# 5) Additional controls
model_controls_lag1 <- glmmTMB(contentious_total ~ contentious_total_lag1 + disparos_total_lag1 + 
                                 gaseado_total_lag1 + mojado_guanaco_total_lag1 + golpiza_total_lag1 +
                                 detencion_total_lag1 + invasion_ingreso_total_lag1 + amenaza_total_lag1 + 
                                 atropello_total_lag1 + otros_aggr_lag1 + weekday_category + distance_km +
                                 precipitation_binary + anomalous_temp, 
                               data = final_df_lag_filter_cont, 
                               family = nbinom2,
                               ziformula=~1)

model_controls_lag2 <- glmmTMB(contentious_total ~ contentious_total_lag1 + disparos_total_lag1 + 
                                 gaseado_total_lag1 + mojado_guanaco_total_lag1 + golpiza_total_lag1 +
                                 detencion_total_lag1 + invasion_ingreso_total_lag1 + amenaza_total_lag1 + 
                                 atropello_total_lag1 + otros_aggr_lag1 + contentious_total_lag2 + disparos_total_lag2 + 
                                 gaseado_total_lag2 + mojado_guanaco_total_lag2 + golpiza_total_lag2 +
                                 detencion_total_lag2 + invasion_ingreso_total_lag2 + amenaza_total_lag2 + 
                                 atropello_total_lag2 + otros_aggr_lag2 + weekday_category + distance_km +
                                 precipitation_binary + anomalous_temp, 
                               data = final_df_lag_filter_cont, 
                               family = nbinom2,
                               ziformula=~1)

model_controls_lag3 <- glmmTMB(contentious_total ~ contentious_total_lag1 + disparos_total_lag1 + 
                                 gaseado_total_lag1 + mojado_guanaco_total_lag1 + golpiza_total_lag1 +
                                 detencion_total_lag1 + invasion_ingreso_total_lag1 + amenaza_total_lag1 + 
                                 atropello_total_lag1 + otros_aggr_lag1 + contentious_total_lag2 + disparos_total_lag2 + 
                                 gaseado_total_lag2 + mojado_guanaco_total_lag2 + golpiza_total_lag2 +
                                 detencion_total_lag2 + invasion_ingreso_total_lag2 + amenaza_total_lag2 + 
                                 atropello_total_lag2 + otros_aggr_lag2 + contentious_total_lag3 + disparos_total_lag3 + 
                                 gaseado_total_lag3 + mojado_guanaco_total_lag3 + golpiza_total_lag3 +
                                 detencion_total_lag3 + invasion_ingreso_total_lag3 + amenaza_total_lag3 + 
                                 atropello_total_lag3 + otros_aggr_lag3  + weekday_category + distance_km +
                                 precipitation_binary + anomalous_temp, 
                               data = final_df_lag_filter_cont, 
                               family = nbinom2,
                               ziformula=~1)

msummary(list(model_controls_lag1, model_controls_lag2, model_controls_lag3),
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_omit = "^\\(Intercept\\)$", 
         output = "latex")



