#-----------------------------------Analysis-----------------------------------#
#-Author: Francisca Castro ------------------------ Created: December 11, 2023-#
#-R Version: 4.3.1 -------------------------------- Revised: December 22, 2023-#

pacman::p_load(glmmTMB, modelsummary, MASS, brms, bayesplot, rstanarm, lme4, lmtest,
               car)

# Explore data

summary(final_df_1day_lag)


## Initial exploration with lags
model_lag1_coes <- glmmTMB(protest_coes ~ shootings_lag1 + beatings_lag1 + arrests_lag1 +
                             gas_lag1 + protest_coes_lag1,
                           data = final_df_1day_lag, family = nbinom2)

# Model with t-1 lag + controls
model_lag1_coes2 <- glmmTMB(protest_coes ~ shootings_lag1 + beatings_lag1 + arrests_lag1 +
                              gas_lag1 + protest_coes_lag1 + police_per_100k_lag1 +
                            weekday_category + distance_km, 
                            data = final_df_1day_lag, family = nbinom1)

msummary(list(model_lag1_coes,model_lag1_coes2),
         stars = c('*' = .1, '**' = .05, '***' = .01))

# Model with 3day accumulation + controls
model_3day_coes2 <- glmmTMB(protest_coes ~ accumulated_shootings + accumulated_beatings +
                               accumulated_arrests + accumulated_gas + accumulated_protest_coes +
                               accumulated_police_per_100k +
                         distance_km, 
                        family = nbinom1,data = final_df_3day_lag_acc)

msummary(list(model_lag1_coes,model_lag1_coes2, model_3day_coes2),
         stars = c('*' = .1, '**' = .05, '***' = .01))


# Model with 7day accumulation + controls
model_7day_coes2 <- glmmTMB(protest_coes ~ accumulated_7days_shootings + accumulated_7days_beatings +
                              accumulated_7days_arrests + accumulated_7days_gas + accumulated_7days_protest_coes +
                              accumulated_7days_police_per_100k +
                              distance_km, 
                            family = nbinom1,data = final_df_7day_lag_acc)


msummary(list(model_lag1_coes,model_lag1_coes2, model_3day_coes2, model_7day_coes2),
         stars = c('*' = .1, '**' = .05, '***' = .01))


