#---------------------------------Tables and Plots---------------------------------#
#-Author: Francisca Castro -------------------------------- Created: June 30, 2023-#
#-R Version: 4.3.1 -------------------------------------- Revised: August 25, 2023-#

pacman::p_load(modelsummary, MASS, pscl, glmmTMB, AICcmodavg, ggplot2, DHARMa, car, broom.mixed,
               dotwhisker, lubridate, magrittr, tidyverse, optimx, marginaleffects, boot, forcats,
               emmeans, VGAM, insight, ggeffects, dplyr, lme4)

# 1) Plots

## Showing graphically the effect of different lags per type of repression

# Disparos
predict_disparos_lag1 <- ggpredict(model_lag3, terms = "disparos_total_lag1")
predict_disparos_lag2 <- ggpredict(model_lag3, terms = "disparos_total_lag2")
predict_disparos_lag3 <- ggpredict(model_lag3, terms = "disparos_total_lag3")

predict_disparos_lag1$group <- "t-1"
predict_disparos_lag2$group <- "t-2"
predict_disparos_lag3$group <- "t-3"

predict_disparos <- rbind(predict_disparos_lag1, predict_disparos_lag2, predict_disparos_lag3)
predict_disparos$group <- as.factor(predict_disparos$group)

plot_disparos <- ggplot(predict_disparos, aes(x, predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0, linetype = "dashed") +
  labs(x = "Shootings", y = expression("Predicted Protests"["t"]), colour = "Lag (days)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(predict_disparos$x), max(predict_disparos$x), by = 5)) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), axis.line = element_line(colour = "black")
  ) +
  ylim(0, 1.5) + # Set common Y-axis limits +
  theme(axis.title.y = element_blank())

plot_disparos

# Gaseado
predict_gaseado_lag1 <- ggpredict(model_lag3, terms = "gaseado_total_lag1")
predict_gaseado_lag2 <- ggpredict(model_lag3, terms = "gaseado_total_lag2")
predict_gaseado_lag3 <- ggpredict(model_lag3, terms = "gaseado_total_lag3")

predict_gaseado_lag1$group <- "t-1"
predict_gaseado_lag2$group <- "t-2"
predict_gaseado_lag3$group <- "t-3"

predict_gaseado <- rbind(predict_gaseado_lag1, predict_gaseado_lag2, predict_gaseado_lag3)
predict_gaseado$group <- as.factor(predict_gaseado$group)

plot_gaseado <- ggplot(predict_gaseado, aes(x, predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0, linetype = "dashed") +   
  labs(x = "Gassed", y = expression("Predicted Protests"["t"])) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(predict_gaseado$x), max(predict_gaseado$x), by = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0, 1.5) +  # Set common Y-axis limits
  theme(legend.position = "none")

plot_gaseado

# Water canon
predict_mojado_lag1 <- ggpredict(model_lag3, terms = "mojado_guanaco_total_lag1")
predict_mojado_lag2 <- ggpredict(model_lag3, terms = "mojado_guanaco_total_lag2")
predict_mojado_lag3 <- ggpredict(model_lag3, terms = "mojado_guanaco_total_lag3")

predict_mojado_lag1$group <- "t-1"
predict_mojado_lag2$group <- "t-2"
predict_mojado_lag3$group <- "t-3"

predict_mojado <- rbind(predict_mojado_lag1, predict_mojado_lag2, predict_mojado_lag3)
predict_mojado$group <- as.factor(predict_mojado$group)

plot_mojado <- ggplot(predict_mojado, aes(x, predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0, linetype = "dashed") +   
  labs(x = "Gassed", y = expression("Predicted Protests"["t"])) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(predict_mojado$x), max(predict_mojado$x), by = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0, 1)   # Set common Y-axis limits

plot_mojado

# Beatings

predict_golpiza_lag1 <- ggpredict(model_lag3, terms = "golpiza_total_lag1")
predict_golpiza_lag2 <- ggpredict(model_lag3, terms = "golpiza_total_lag2")
predict_golpiza_lag3 <- ggpredict(model_lag3, terms = "golpiza_total_lag3")

predict_golpiza_lag1$group <- "t-1"
predict_golpiza_lag2$group <- "t-2"
predict_golpiza_lag3$group <- "t-3"

predict_golpiza <- rbind(predict_golpiza_lag1, predict_golpiza_lag2, predict_golpiza_lag3)
predict_golpiza$group <- as.factor(predict_golpiza$group)

plot_golpiza <- ggplot(predict_golpiza, aes(x, predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0, linetype = "dashed") +  
  labs(x = "Beatings", y = expression("Predicted Protests"["t"])) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(predict_golpiza$x), max(predict_golpiza$x), by = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0, 1) +   # Set common Y-axis limits
  theme(legend.position = "none")

plot_golpiza

# Arrests

predict_arrests_lag1 <- ggpredict(model_lag3, terms = "detencion_total_lag1")
predict_arrests_lag2 <- ggpredict(model_lag3, terms = "detencion_total_lag2")
predict_arrests_lag3 <- ggpredict(model_lag3, terms = "detencion_total_lag3")

predict_arrests_lag1$group <- "t-1"
predict_arrests_lag2$group <- "t-2"
predict_arrests_lag3$group <- "t-3"

predict_arrests <- rbind(predict_arrests_lag1, predict_arrests_lag2, predict_arrests_lag3)
predict_arrests$group <- as.factor(predict_arrests$group)

plot_arrests <- ggplot(predict_arrests, aes(x, predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0, linetype = "dashed") +
  labs(x = "Arrests", y = expression("Predicted Protests"["t"]), colour = "Lag (days)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(predict_arrests$x), max(predict_arrests$x), by = 1)) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), axis.line = element_line(colour = "black"),
    axis.title.y = element_blank()
  ) +
  ylim(0, 1)  # Set common Y-axis limits

plot_arrests

# Plot combination

# Combine both plots using cowplot
combined_plots_repression <- plot_grid(plot_golpiza, plot_arrests, plot_gaseado, plot_disparos,
                                       nrow = 2, align="hv")

# Display the combined plot
combined_plots_repression

library(patchwork)
# Combine the plots using patchwork
combined_plots <- (plot_golpiza | plot_arrests) / (plot_gaseado | plot_disparos)

# Save the combined plot using ggsave
ggsave("combined_plots.png", combined_plots, width = 22, height = 12, units = "cm")


## Autocorrelation of residuals
# Get the residuals from your model
residuals <- residuals(model_lag1)

# Define the specific lag values you want to include
selected_lags <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)  # Add more if needed

# Create the residuals vs. selected lag scatter plot
plot(selected_lags, residuals[selected_lags], xlab = "Lag", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a reference line at y = 0

# Customize the x-axis tick marks to show all selected lags
axis(1, at = selected_lags)


# 2) Table

## Descriptive Statistics

freq_table_repression_raw <- final_df %>%
  group_by() %>%
  summarise_at(vars(disparos_total, golpiza_total, amenaza_total, detencion_total,
                    atropello_total, asfixia_total, invasion_ingreso_total,
                    gaseado_total, piedrazo_total, ataque_animales_total,
                    obstruccion_total, tocaciones_total, quemado_total, 
                    estigmatizacion_total, destruccion_objetos_total, seguimiento_total,
                    desnudamiento_total, mojado_guanaco_total, otros_total), sum) #19 variables

freq_table_repression_raw %<>%
  tibble::rownames_to_column() %>% 
  gather(repression_type, value, -rowname) %>% 
  spread(rowname, value) 

## Rename rows
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
                                   invasion_ingreso_total = "Unauthorized entry/home invasion",
                                   mojado_guanaco_total = "Water impact",
                                   obstruccion_total = "Obstruction medical assistance",
                                   piedrazo_total = "Stone throwing",
                                   quemado_total = "Burned",
                                   seguimiento_total = "Follow up",
                                   tocaciones_total = "Touching",
                                   otros_total = "Other")) #19


colnames(freq_table_repression)[2] <- "frequency"

freq_table_repression %<>% mutate(percentage=100*frequency/sum(frequency))

## Group all the repressive types with less than 15 occurrences under the category "Other"
freq_table_repression %<>%
  mutate(repression_type = ifelse(frequency > 15, repression_type, "Other")) %>%
  group_by(repression_type) %>%
  summarise(frequency = sum(frequency)) %>%
  mutate(percentage=100*frequency/sum(frequency))

sum(freq_table_repression$percentage) #100
sum(freq_table_repression$frequency) #2779

## Order alphabetically by repression_type and round up to two decimals and create the row 'Total repressive acts'
freq_table_repression %<>%
  arrange(repression_type) %>%
  mutate(percentage = round(percentage, 2)) %>%
  mutate(`Total Repressive Acts` = sum(frequency))

freq_table_repression


## Frequency table by region and month

freq_table_region <- final_df %>%
  group_by(nombre_region) %>%
  summarize(total_repression = sum(repression_total),
            total_protests = sum(contentious_total)) %>%
  mutate(perc_repression = 100*total_repression/sum(total_repression),
         perc_protests = 100*total_protests/sum(total_protests)) %>%
  mutate(perc_repression = round(perc_repression, 2),
         perc_protests = round(perc_protests, 2)) %>%
  mutate(`Total Repressive Acts` = sum(total_repression),
         `Total Contentious Acts` = sum(total_protests))

freq_table_region


freq_table_month <- final_df %>% 
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(total_repression = sum(repression_total),
            total_protests = sum(contentious_total)) %>%
  mutate(perc_repression = 100*total_repression/sum(total_repression),
         perc_protests = 100*total_protests/sum(total_protests)) %>%
  mutate(perc_repression = round(perc_repression, 2),
         perc_protests = round(perc_protests, 2)) %>%
  mutate(`Total Repressive Acts` = sum(total_repression),
         `Total Contentious Acts` = sum(total_protests))

freq_table_month

