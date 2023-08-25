#----------------------------------Data Preparation--------------------------------#
#-Author: Francisca Castro -------------------------------- Created: June 30, 2023-#
#-R Version: 4.3.1 -------------------------------------- Revised: August 25, 2023-#

pacman::p_load(chilemapas, tidyr, dplyr, readr, readxl, gdata, stringr, writexl,
               lubridate, janitor, stringr, here, plm, magrittr, ggmap, geosphere,
               osmdata, sf)


# 1) Create empty dataframe

#This first stage consists on the elaboration of an empty dataframe. 
#This dataframe will serve as a vassel where ACLED and INDH data will be pasted. 
#This dataframe consists of one date per municipality (comuna), counting from 
#the first day of protest (18 October 2019) to the last date (31 March 2020).

#Load municipalities: I will use the R package chilemapas that consist of all 
#Chilean municipalities. This will serve as a standard in term of names for the 
#three administrative levels (regions, provinces, and municipalities).

codigos_territoriales <- (chilemapas::codigos_territoriales)

#Create panel dataframe

comunas_panel <- data.frame()
for(i in 1:nrow(codigos_territoriales))  {   
  dates <- data.frame(date = seq(from = as.Date("2019-10-18"), 
                                 to = as.Date("2020-03-31"), by = 1))
  comunas_panel = rbind(comunas_panel, dates)
}

comunas_panel <- comunas_panel %>%
  arrange(date)

# Extract names of municipalities, provinces, and regions
comunas <- codigos_territoriales %>%
  select(2,4,6)
comunas <- comunas %>% slice(rep(row_number(), 166))
comunas_panel_final <- cbind(comunas_panel,comunas)

# Check that every municipality has 166 observations
comunas_panel_final %>%
  count(nombre_comuna)

unique(comunas_panel_final$nombre_comuna)

# 2) ACLED data

#Original ACLED data as downloaded through the website. Event type: protests, 
#riots Actor type: protesters, rioters, civilians 
#Dates: 18 October 2019 to 31 March 2020

acled_data <- read_csv(here("01-data/acled-original.csv"), 
                       col_types = cols(event_date = 
                                          col_date(format = "%d %B %Y")))

# Modify inconsistent municipality names
acled_data$nombre_comuna <- acled_data$admin3

acled_data$nombre_comuna[is.na(acled_data$nombre_comuna)] = 
  "Santiago" #All NAs are from Santiago

acled_data$nombre_comuna[acled_data$nombre_comuna == "Coyhaique"] <- "Coihaique"
acled_data$nombre_comuna[acled_data$nombre_comuna == "Aysen"] <- "Aisen"
acled_data$nombre_comuna[acled_data$nombre_comuna == 
                           "Padre Las Casas"] <- "Padre las Casas"

acled_data$date <- acled_data$event_date
class(acled_data$date) #Date

# Generate counting variables for protests and riots
# Divide the db into two, one for protests and one for riots
acled_data_protests <- acled_data[acled_data$event_type == "Protests", ]
acled_data_protests <- acled_data_protests %>%
  group_by(nombre_comuna, date) %>%
  summarize(protest_total = n())

acled_data_riots <- acled_data[acled_data$event_type == "Riots", ]
acled_data_riots <- acled_data_riots %>%
  group_by(nombre_comuna, date) %>%
  summarize(riots_total = n())
head(acled_data_riots)

# Transfer data into comunas_panel_final

final_df <- full_join(comunas_panel_final, acled_data_protests, 
                      by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, acled_data_riots, by = 
                        c("nombre_comuna", "date")) 

length(unique(final_df$nombre_comuna)) #346

final_df$protest_total[is.na(final_df$protest_tota)] = 0
final_df$riots_total[is.na(final_df$riots_total)] = 0

# Create a new variable "contentious total" that sums protests and riots
final_df %<>% mutate(contentious_total = protest_total + riots_total)

# 3) INDH data

#Source: Mapa de Violaciones a los Derechos Humanos elaborated by the National 
#Institute of Human rights (INDH) available here: 
#<https://mapaviolacionesddhh.indh.cl/public/investigadores>

indh <- read_excel(here("data/01-raw-data/indh-original.xlsx"))
names(indh)

#Clean database to leave only the used variables, and rename:
#Folio -> id
#Fecha del delito -> date
#Comuna1 -> nombre_comuna
#Hecho1 -> repressive_type_original
#Tipo de lugar1 -> location

indh_clean <- select(indh, "Folio", "Fecha del delito", "Comuna1", 
                     "Hecho1", "Tipo de lugar1")

indh_clean <- rename.vars(indh_clean, from = c("Folio", "Fecha del delito", 
                                               "Comuna1", "Hecho1", 
                                               "Tipo de lugar1"),
                          to = c(c("id", "date", "nombre_comuna", 
                                   "repressive_type_original",
                                   "location")))

head(indh_clean)

#Drop NAs

indh_clean <- indh_clean %>% drop_na() # from 2838 obs to 2784
class(indh_clean$date)

indh_clean$date <- as.Date(indh_clean$date, format = "%Y-%m-%d")
class(indh_clean$date)

# Drop observations from 17 October 2019
indh_clean <- indh_clean[!(indh_clean$date == "2019-10-17"),] #2782 obs

#Change inconsistent municipality to standardize names
# Drop cases with no information for municipalities (value "Anonimizado")
indh_clean <- indh_clean[!(indh_clean$nombre_comuna == "Anonimazdo"),] #2779 obs

# Remove trailing white space after municipality name
indh_clean$nombre_comuna <- str_trim(indh_clean$nombre_comuna)

indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Cañete"] <- "Canete"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Chillán"] <- "Chillan"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Cañete"] <- "Canete"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Concepción"] <- "Concepcion"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Conchalí"] <- "Conchali"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Copiapó"] <- "Copiapo"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Coyhaique"] <- "Coihaique"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Curacaví"] <- "Curacavi"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Curicó"] <- "Curico"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Estación Central"] <- "Estacion Central"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "La florida"] <- "La Florida"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "La serena"] <- "La Serena"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Hualpén"] <- "Hualpen"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "La Unión"] <- "La Union"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Llolleo"] <- "San Antonio"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Los Ángeles"] <- "Los Angeles"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Machalí"] <- "Machali"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Macúl"] <- "Macul"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Maipú"] <- "Maipu"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Maullín"] <- "Maullin"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Mauillín"] <- "Maullin"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Ñuñoa"] <- "Nunoa"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Padre de las Casas"] <- "Padre las Casas"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Peñaflor"] <- "Penaflor"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Peñalolén"] <- "Penalolen"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Puente alto"] <- "Puente Alto"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Pucón"] <- "Pucon"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Pucón"] <- "Pucon"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Puerto Aysén"] <- "Aisen"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Puerto Natales"] <- "Natales"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Quellón"] <- "Quellon"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Quilpué"] <- "Quilpue"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Quinteros"] <- "Quintero"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Río Bueno"] <- "Rio Bueno"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Río Negro"] <- "Rio Negro"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "San Joaquín"] <- "San Joaquin"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "San José de Maipo"] <- "San Jose de Maipo"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "San Ramón"] <- "San Ramon"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "San Vicente de Tagua-Tagua"] <- "San Vicente"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Santa Bárbara"] <- "Santa Barbara"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Santiago Centro"] <- "Santiago"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Tomé"] <- "Tome"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Valparaíso"] <- "Valparaiso"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Ventanas"] <- "Puchuncavi"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Vicuña"] <- "Vicuna"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Villarica"] <- "Villarrica"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Viña del Mar"] <- "Vina del Mar"
indh_clean$nombre_comuna[indh_clean$nombre_comuna == "Viña del mar"] <- "Vina del Mar"


#Create categories for repressive events

tabyl(indh_clean$repressive_type_original, sort = TRUE) #23 types

# Join amenaza y amenaza de muerte
indh_clean$repressive_type_original[indh_clean$repressive_type_original == "Amenaza de muerte"] <- "Amenaza"
unique(indh_clean$repressive_type_original) # 22 categories
tabyl(indh_clean$repressive_type_original) 

# Join rotura de teléfono con destrucción de objetos personales
indh_clean$repressive_type_original[indh_clean$repressive_type_original == "Rotura de teléfono"] <- "Destrucción de objetos personales"
unique(indh_clean$repressive_type_original) # 21 categories
tabyl(indh_clean$repressive_type_original) 

# Create particular categories INDH data
disparos <- indh_clean[indh_clean$repressive_type_original == "Disparos", ]
disparos %<>%
  group_by(nombre_comuna, date) %>%
  summarize(disparos_total = n())

golpiza <- indh_clean[indh_clean$repressive_type_original == "Golpiza", ]
golpiza %<>%
  group_by(nombre_comuna, date) %>%
  summarize(golpiza_total = n())

amenaza <- indh_clean[indh_clean$repressive_type_original == "Amenaza", ]
amenaza %<>%
  group_by(nombre_comuna, date) %>%
  summarize(amenaza_total = n())

detencion <- indh_clean[indh_clean$repressive_type_original == "Detención", ]
detencion %<>%
  group_by(nombre_comuna, date) %>%
  summarize(detencion_total = n())

atropello <- indh_clean[indh_clean$repressive_type_original == "Atropello", ]
atropello %<>%
  group_by(nombre_comuna, date) %>%
  summarize(atropello_total = n())

asfixia <- indh_clean[indh_clean$repressive_type_original == "Asfixia", ]
asfixia %<>%
  group_by(nombre_comuna, date) %>%
  summarize(asfixia_total = n())

invasion_hogar <- indh_clean[indh_clean$repressive_type_original == "Invasión del hogar", ]
invasion_hogar %<>%
  group_by(nombre_comuna, date) %>%
  summarize(invasion_hogar_total = n())

ingreso <- indh_clean[indh_clean$repressive_type_original == "Ingreso no autorizado", ]
ingreso %<>%
  group_by(nombre_comuna, date) %>%
  summarize(ingreso_total = n())

otros <- indh_clean[indh_clean$repressive_type_original == "Otros", ]
otros %<>%
  group_by(nombre_comuna, date) %>%
  summarize(otros_total = n())

gaseado <- indh_clean[indh_clean$repressive_type_original == "Gaseado", ]
gaseado %<>%
  group_by(nombre_comuna, date) %>%
  summarize(gaseado_total = n())

piedrazo <- indh_clean[indh_clean$repressive_type_original == "Piedrazo", ]
piedrazo %<>%
  group_by(nombre_comuna, date) %>%
  summarize(piedrazo_total = n())

ataque_animales <- indh_clean[indh_clean$repressive_type_original == "Ataque con animales", ]
ataque_animales %<>%
  group_by(nombre_comuna, date) %>%
  summarize(ataque_animales_total = n())

guanaco <- indh_clean[indh_clean$repressive_type_original == "Impacto de chorro", ]
guanaco %<>%
  group_by(nombre_comuna, date) %>%
  summarize(guanaco_total = n())

obstruccion <- indh_clean[indh_clean$repressive_type_original == "Obstrucción de asistencia médica", ]
obstruccion %<>%
  group_by(nombre_comuna, date) %>%
  summarize(obstruccion_total = n())

tocaciones <- indh_clean[indh_clean$repressive_type_original == "Tocaciones", ]
tocaciones %<>%
  group_by(nombre_comuna, date) %>%
  summarize(tocaciones_total = n())

quemado <- indh_clean[indh_clean$repressive_type_original == "Quemado", ]
quemado %<>%
  group_by(nombre_comuna, date) %>%
  summarize(quemado_total = n())

estigmatizacion <- indh_clean[indh_clean$repressive_type_original == "Estigmatización", ]
estigmatizacion %<>%
  group_by(nombre_comuna, date) %>%
  summarize(estigmatizacion_total = n())

destruccion_objetos <- indh_clean[indh_clean$repressive_type_original == "Destrucción de objetos personales", ]
destruccion_objetos %<>%
  group_by(nombre_comuna, date) %>%
  summarize(destruccion_objetos_total = n())

seguimiento <- indh_clean[indh_clean$repressive_type_original == "Seguimiento", ]
seguimiento %<>%
  group_by(nombre_comuna, date) %>%
  summarize(seguimiento_total = n())

desnudamiento <- indh_clean[indh_clean$repressive_type_original == "Desnudamiento", ]
desnudamiento %<>%
  group_by(nombre_comuna, date) %>%
  summarize(desnudamiento_total = n())

mojado <- indh_clean[indh_clean$repressive_type_original == "Mojado con productos químicos", ]
mojado %<>%
  group_by(nombre_comuna, date) %>%
  summarize(mojado_total = n())

#20 types of repression

#Transfer new data to final_df
final_df <- full_join(final_df, disparos, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, golpiza, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, amenaza, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, detencion, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, atropello, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, asfixia, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, invasion_hogar, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, ingreso, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, gaseado, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, piedrazo, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, ataque_animales, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, guanaco, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, obstruccion, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, tocaciones, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, quemado, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, estigmatizacion, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, destruccion_objetos, by=c("nombre_comuna", "date")) 
final_df <- full_join(final_df, seguimiento, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, desnudamiento, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, mojado, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, otros, by = c("nombre_comuna", "date")) 

# Replace NAs for 0s
final_df <- final_df %>% replace(is.na(.), 0)
length(unique(final_df$nombre_comuna)) #346
length(unique(final_df$date)) #166

final_df <- final_df %>% drop_na()
length(unique(final_df$nombre_comuna)) #346
length(unique(final_df$date)) #166

#Create new variable that combines guanaco + mojado and invasion_hogar with ingresos
final_df %<>%
  rowwise() %>%
  mutate(mojado_guanaco_total = sum(c_across(guanaco_total|mojado_total)),
         invasion_ingreso_total = sum(c_across(invasion_hogar_total|ingreso_total)))

#Delete previous variables to avoid confussion
final_df %<>% dplyr::select(-c(guanaco_total,mojado_total,
                               invasion_hogar_total,ingreso_total))

#Create `repression total` that combines all the repressive types
final_df %<>%
  rowwise() %>%
  mutate(repression_total = sum(c_across(8:26), na.rm = TRUE)) %>%
  ungroup()

# 4) Final modifications

## Data with the repressive actions with less than 15 occurrences grouped in a new category
#Use the `freq_table_repression_raw` object

colnames(freq_table_repression_raw)[2] <- "frequency"

freq_table_repression_raw %<>%
  arrange(desc(frequency))

#Variables that will be categorized under `other` because they have less than 15 occurrences:
#otros_total asfixia_total desnudamiento_total obstruccion_total piedrazo_total tocaciones_total
#estigmatizacion_total destruccion_objetos_total seguimiento_total ataque_animales_total quemado_total

# Modify final_df

final_df %<>% 
  mutate(otros_aggr = otros_total + asfixia_total +
           desnudamiento_total + obstruccion_total + piedrazo_total + tocaciones_total +
           estigmatizacion_total + destruccion_objetos_total + seguimiento_total + ataque_animales_total +
           quemado_total)

# Data with Provinces as unit of analysis
# Sum only top 9, contentious total, and repression total
province_df <- final_df %>%
  group_by(nombre_region, nombre_provincia, date) %>%
  summarize(contentious_total = sum(contentious_total),
            disparos_total = sum(disparos_total),
            golpiza_total = sum(golpiza_total),
            detencion_total = sum(detencion_total),
            gaseado_total = sum(gaseado_total),
            mojado_guanaco_total = sum(mojado_guanaco_total),
            amenaza_total = sum(amenaza_total),
            atropello_total = sum(atropello_total),
            invasion_ingreso_total = sum(invasion_ingreso_total),
            otros_aggr = sum(otros_aggr),
            repression_total = sum(repression_total))

# 5) Additional control variables

final_df$day_week <- wday(final_df$date, label = TRUE)

final_df %<>%
  mutate(weekday_category = case_when(day_week %in% c("Sat", "Sun") ~ "Weekend",
                                      day_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri") ~ "Weekday"))

final_df$weekday_category <- relevel(factor(final_df$weekday_category), ref = "Weekend")

# Distance of each municipality to the regional capital using `codigos_territoriales`

# Create the `reg_cap` variable based on the `nombre_region` values
codigos_territoriales %<>%
  mutate(reg_cap = case_when(nombre_region == "Antofagasta" ~ "Antofagasta",
                             nombre_region == "Arica y Parinacota" ~ "Arica",
                             nombre_region == "Atacama" ~ "Copiapo",
                             nombre_region == "Aysen del General Carlos Ibanez del Campo" ~ "Coihaique",
                             nombre_region == "Biobio" ~ "Concepcion",
                             nombre_region == "Coquimbo" ~ "La Serena",
                             nombre_region == "La Araucania" ~ "Temuco",
                             nombre_region == "Libertador General Bernardo OHiggins" ~ "Rancagua",
                             nombre_region == "Los Lagos" ~ "Puerto Montt",
                             nombre_region == "Los Rios" ~ "Valdivia",
                             nombre_region == "Magallanes y de la Antartica Chilena" ~ "Punta Arenas",
                             nombre_region == "Maule" ~ "Talca",
                             nombre_region == "Metropolitana de Santiago" ~ "Santiago",
                             nombre_region == "Nuble" ~ "Chillan",
                             nombre_region == "Tarapaca" ~ "Iquique",
                             nombre_region == "Valparaiso" ~ "Valparaiso"))



get_distance <- function(city1, city2) {
  # Geocode the cities to obtain their coordinates
  coords1 <- geocode(city1)
  coords2 <- geocode(city2)
  
  # Calculate the distance between the coordinates
  distance <- distGeo(coords1, coords2)
  
  # Convert the distance from meters to kilometers
  distance_km <- distance / 1000
  
  # Return the distance in kilometers
  return(distance_km)
}

# Create new name that is the name of the city, region, and country
codigos_territoriales %<>%
  mutate(complete_name_comuna = paste(nombre_comuna, ", ", nombre_region, ", Chile"),
         complete_name_reg_cap = paste(reg_cap, ", ", nombre_region,", Chile"))

register_google(key = "INSERT-KEY") #insert own key

# Iterate over each row in the dataset and calculate the distance
for (i in 1:nrow(codigos_territoriales)) {
  city1 <- codigos_territoriales$complete_name_comuna[i]
  city2 <- codigos_territoriales$complete_name_reg_cap[i]
  
  # Calculate the distance between the cities
  distance <- get_distance(city1, city2)
  
  # Assign the distance value to a new variable
  codigos_territoriales$distance_km[i] <- distance
}

# Add distance_km to final_df

final_df %<>%
  left_join(codigos_territoriales %>% 
              select(nombre_comuna, distance_km),
            by = "nombre_comuna")

## Weather 

# For this part, the database created by the script weather-data is necessary

#Include the information of weather on final_df
#Particularly precipitation, temp_max and temp_mean.
#Precipitation as a binary variable, and temperatures as standard deviation of the mean temperature. 

weather_final <- merged_weather
weather_final$precipitation_binary <- ifelse(weather_final$avg_precipitation > 0, 1, 0)
weather_final$anomalous_temp <- ifelse(weather_final$avg_max_temperature > 30, 1, 0) # Temperatures over 30 degrees

# Add nombre_provincia. Aggregate 'codigos_territoriales' to remove duplicate 'codigo_provincia' entries
unique_codigos <- codigos_territoriales %>% 
  distinct(codigo_provincia, .keep_all = TRUE)

# Join 'weather_final' with 'unique_codigos' based on 'codigo_provincia'
weather_final <- left_join(weather_final, unique_codigos %>% select(codigo_provincia, nombre_provincia), 
                           by = "codigo_provincia")

# Add temperature data to the original dataset

final_df_lag_filter_cont <- left_join(final_df_lag_filter, weather_final, by = c("nombre_provincia", "date"))
