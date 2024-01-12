#-------------------------------Data Preparation-------------------------------#
#-Author: Francisca Castro ------------------------ Created: November 30, 2023-#
#-R Version: 4.3.1 --------------------------------- Revised: January 11, 2023-#

# LOAD LIBRARIES

pacman::p_load(chilemapas, tidyr, dplyr, readr, readxl, gdata, stringr, writexl,
               lubridate, janitor, stringr, here, plm, magrittr, haven, ggplot2, ggmap, 
               geosphere, osmdata, sf, censo2017)

# DATAFRAME CREATION

# - This first stage consists on the elaboration of an empty dataframe. 
# - This dataframe will serve as a vessel where ACLED and INDH data will be pasted. 
# - This dataframe consists of one date per municipality (comuna), counting from the 
# - first day of protest (18 October 2019) to the last date (31 March 2020).

# - Load municipalities: I  use the R package `chilemapas` that consist of all Chilean municipalities. 
# - This will serve as a standard in term of names for the three administrative levels 
# - (regions, provinces, and municipalities).

codigos_territoriales <- (chilemapas::codigos_territoriales)

## Create panel dataframe

comunas_panel <- data.frame()
for(i in 1:nrow(codigos_territoriales))  {   
  dates <- data.frame(date = seq(from = as.Date("2019-10-18"),
                                 to = as.Date("2020-03-31"), by = 1))
  comunas_panel = rbind(comunas_panel, dates)
}

comunas_panel <- comunas_panel %>%
  arrange(date)

# Extract names of municipalities, provinces, and regions
comunas <- codigos_territoriales %>% select(2,4,6)
comunas <- comunas %>% slice(rep(row_number(), 166))
comunas_panel_final <- cbind(comunas_panel,comunas)

# Check that every municipality has 166 observations
comunas_panel_final %>%
  count(nombre_comuna)

unique(comunas_panel_final$nombre_comuna)


# ACLED DATA

# - Original ACLED data as downloaded through the website. 
# - Event type: protests, riots Actor type: protesters, rioters, civilians 
# - Dates: 18 October 2019 to 31 March 2020

acled_data <- read_csv(here("01-data/01-raw-data/acled-original.csv"),
                       col_types = cols(event_date = col_date(format = "%d %B %Y")))

# Modify inconsistent municipality names
acled_data$nombre_comuna <- acled_data$admin3

acled_data$nombre_comuna[is.na(acled_data$nombre_comuna)] = "Santiago" #All NAs are from Santiago
acled_data$nombre_comuna[acled_data$nombre_comuna == "Coyhaique"] <- "Coihaique"
acled_data$nombre_comuna[acled_data$nombre_comuna == "Aysen"] <- "Aisen"
acled_data$nombre_comuna[acled_data$nombre_comuna == "Padre Las Casas"] <- "Padre las Casas"

acled_data$date <- acled_data$event_date
class(acled_data$date) #Date

# Generate counting variables for protests and riots
# Divide the db into two, one for protests and one for riots
acled_data_protests <- acled_data[acled_data$event_type == "Protests", ]
acled_data_protests <- acled_data_protests %>%
  group_by(nombre_comuna, date) %>%
  summarize(protest_acled = n())

acled_data_riots <- acled_data[acled_data$event_type == "Riots", ]
acled_data_riots <- acled_data_riots %>%
  group_by(nombre_comuna, date) %>%
  summarize(riots_acled = n())
head(acled_data_riots)

# Transfer data using comunas_panel_final

final_df <- full_join(comunas_panel_final, acled_data_protests, by = c("nombre_comuna", "date")) 
final_df <- full_join(final_df, acled_data_riots, by = c("nombre_comuna", "date")) 

length(unique(final_df$nombre_comuna)) #346

final_df$protest_acled[is.na(final_df$protest_acled)] = 0
final_df$riots_acled[is.na(final_df$riots_acled)] = 0

# Create a new variable "contentious_total" that sums protests and riots
final_df %<>% mutate(contentious_acled = protest_acled + riots_acled)

# Check how many protests there are per day
final_df %>%
  group_by(date) %>%
  summarise(counts = sum(contentious_acled, na.rm = TRUE)) %>%
  print(n = Inf)

# COES DATA

load("~/PhD/1_research/state-repression/r-project/01-data/01-raw-data/observatorio-conflictos.RData")

# Construct a variable for date that combines p5c (year) p5b (month) and p5a (day)

# First, modify the formats
acciones_de_protesta_2009_2019 %<>%
  mutate(
    year = p5c + 2000,  # Adjust the year
    month = sprintf("%02d", p5b),  # monthin two-digit format
    day = sprintf("%02d", p5a),  # day  in two-digit format
    date = make_date(year, month, day))


# Filter dates starting from oct 18

coes_filtered <- acciones_de_protesta_2009_2019 %>%
  filter(date >= as.Date("2019-10-18"))

# Include municipality

coes_filtered$p8 <- as.numeric(coes_filtered$p8)

coes_filtered %<>%
  mutate(codigo_comuna = if_else(nchar(as.character(p8)) <= 4,
                                 sprintf("%05d", as.numeric(p8)),
                                 as.character(p8)))

coes_filtered %<>%
  left_join(codigos_territoriales, by = "codigo_comuna")

# Count totals per date and municipality

coes_protests <- coes_filtered %>%
  group_by(codigo_comuna, nombre_comuna, codigo_provincia, nombre_provincia, 
           codigo_region, nombre_region, date) %>%
  summarise(protest_coes = n(), .groups = "drop")

# Integrate into final_df

final_df %<>%
  left_join(coes_protests %>% select(date, nombre_comuna, protest_coes), by = c("date", "nombre_comuna")) %>%
  mutate(protest_coes = replace_na(protest_coes, 0))

# Have the 31st of December 2019 as the cutoff date

final_df %<>% filter(date <= as.Date("2019-12-31"))

# Check how many protests there are per day
final_df %>%
  group_by(date) %>%
  summarise(counts = sum(contentious_acled, protest_coes, na.rm = TRUE)) %>%
  print(n = Inf)

# INDH

# - Source: Mapa de Violaciones a los Derechos Humanos elaborated by the 
# - National Institute of Human rights (INDH) available here: 
# - <https://mapaviolacionesddhh.indh.cl/public/investigadores>

indh <- read_excel(here("01-data/01-raw-data/indh-original.xlsx"))
names(indh)

# Clean database to leave only the used variables, and rename:
# Folio -> id
# Fecha del delito -> date
# Comuna1 -> nombre_comuna
# Hecho1 -> repressive_type_original
# Tipo de lugar1 -> location

indh_clean <- select(indh, "Folio", "Fecha del delito", "Comuna1", 
                     "Hecho1", "Tipo de lugar1")

indh_clean <- rename.vars(indh_clean, from = c("Folio", "Fecha del delito", 
                                               "Comuna1", "Hecho1", 
                                               "Tipo de lugar1"),
                          to = c(c("id", "date", "nombre_comuna", 
                                   "repressive_type_original",
                                   "location")))

head(indh_clean)

# Drop NAs

indh_clean <- indh_clean %>% drop_na() # from 2838 obs to 2784
class(indh_clean$date)

indh_clean$date <- as.Date(indh_clean$date, format = "%Y-%m-%d")
class(indh_clean$date)

# Drop observations from 17 October 2019 and beyond the 31 Dec 2019
indh_clean <- indh_clean[!(indh_clean$date == "2019-10-17"),] #2782 obs
indh_clean <- indh_clean[!(indh_clean$date > as.Date("2019-12-31")),]

# Change inconsistent municipality to standardize names

indh_clean %<>%
  # Drop cases with no information for municipalities (value "Anonimizado")
  filter(nombre_comuna != "Anonimazdo") %>%
  # Remove trailing white space after municipality name
  mutate(nombre_comuna = str_trim(nombre_comuna)) %>%
  # Replace specific municipality names
  mutate(nombre_comuna = case_when(
    nombre_comuna == "Cañete" ~ "Canete",
    nombre_comuna == "Chillán" ~ "Chillan",
    nombre_comuna == "Concepción" ~ "Concepcion",
    nombre_comuna == "Conchalí" ~ "Conchali",
    nombre_comuna == "Copiapó" ~ "Copiapo",
    nombre_comuna == "Coyhaique" ~ "Coihaique",
    nombre_comuna == "Curacaví" ~ "Curacavi",
    nombre_comuna == "Curicó" ~ "Curico",
    nombre_comuna == "Estación Central" ~ "Estacion Central",
    nombre_comuna == "La florida" ~ "La Florida",
    nombre_comuna == "La serena" ~ "La Serena",
    nombre_comuna == "Hualpén" ~ "Hualpen",
    nombre_comuna == "La Unión" ~ "La Union",
    nombre_comuna == "Llolleo" ~ "San Antonio",
    nombre_comuna == "Los Ángeles" ~ "Los Angeles",
    nombre_comuna == "Machalí" ~ "Machali",
    nombre_comuna == "Macúl" ~ "Macul",
    nombre_comuna == "Maipú" ~ "Maipu",
    nombre_comuna == "Maullín" ~ "Maullin",
    nombre_comuna == "Mauillín" ~ "Maullin",
    nombre_comuna == "Ñuñoa" ~ "Nunoa",
    nombre_comuna == "Padre de las Casas" ~ "Padre las Casas",
    nombre_comuna == "Peñaflor" ~ "Penaflor",
    nombre_comuna == "Peñalolén" ~ "Penalolen",
    nombre_comuna == "Puente alto" ~ "Puente Alto",
    nombre_comuna == "Pucón" ~ "Pucon",
    nombre_comuna == "Puerto Aysén" ~ "Aisen",
    nombre_comuna == "Puerto Natales" ~ "Natales",
    nombre_comuna == "Quellón" ~ "Quellon",
    nombre_comuna == "Quilpué" ~ "Quilpue",
    nombre_comuna == "Quinteros" ~ "Quintero",
    nombre_comuna == "Río Bueno" ~ "Rio Bueno",
    nombre_comuna == "Río Negro" ~ "Rio Negro",
    nombre_comuna == "San Joaquín" ~ "San Joaquin",
    nombre_comuna == "San José de Maipo" ~ "San Jose de Maipo",
    nombre_comuna == "San Ramón" ~ "San Ramon",
    nombre_comuna == "San Vicente de Tagua-Tagua" ~ "San Vicente",
    nombre_comuna == "Santa Bárbara" ~ "Santa Barbara",
    nombre_comuna == "Santiago Centro" ~ "Santiago",
    nombre_comuna == "Tomé" ~ "Tome",
    nombre_comuna == "Valparaíso" ~ "Valparaiso",
    nombre_comuna == "Ventanas" ~ "Puchuncavi",
    nombre_comuna == "Vicuña" ~ "Vicuna",
    nombre_comuna == "Villarica" ~ "Villarrica",
    nombre_comuna == "Viña del Mar" ~ "Vina del Mar",
    nombre_comuna == "Viña del mar" ~ "Vina del Mar",
    TRUE ~ nombre_comuna))

# Create categories for repressive events

tabyl(indh_clean$repressive_type_original, sort = TRUE) #23 types

# Keep only relevant categories: beatings, shootings, arrests, gas, water cannon 

indh_repression <- indh_clean %>%
  filter(repressive_type_original %in% c("Golpiza", "Disparos", "Detención", "Gaseado", "Impacto de chorro")) %>%
  mutate(repressive_type = case_when(
    repressive_type_original == "Golpiza" ~ "beatings",
    repressive_type_original == "Disparos" ~ "shootings",
    repressive_type_original == "Detención" ~ "arrests",
    repressive_type_original %in% c("Gaseado", "Impacto de chorro") ~ "crowd_control",
    TRUE ~ repressive_type_original))

# Summarize the data

indh_summary <- indh_repression %>%
  group_by(nombre_comuna, date) %>%
  summarise(
    beatings = sum(repressive_type == "beatings", na.rm = TRUE),
    shootings = sum(repressive_type == "shootings", na.rm = TRUE),
    arrests = sum(repressive_type == "arrests", na.rm = TRUE),
    crowd_control = sum(repressive_type == "crowd_control", na.rm = TRUE),
    .groups = "drop")

# Transfer new data to final_df

final_df %<>%
  left_join(indh_summary, by = c("nombre_comuna", "date")) %>%
  # Replace NA with 0 in all columns that come from indh_summary
  mutate(across(.cols = where(is.numeric), .fns = ~ replace_na(.x, 0)))

# Create `repression total` that combines all the repressive types
final_df %<>%
  rowwise() %>%
  mutate(repression_total = sum(c_across(9:11), na.rm = TRUE)) %>%
  ungroup()

# CARABINEROS DATA

# - In order to measure collective costs, a new variable of the amount of police officers
# - per 100,000 inhabitants will be included, based on information provided by Carabineros
# - via Transparency law.

dotacion_ffee <- read_excel(here("01-data/01-raw-data/dotacion-ffee/total-nacional.xlsx"))

dotacion_ffee %<>%
  mutate(date = ymd(date))

str(dotacion_ffee)

## Group the dataset into a day-province level
dotacion_ffee_prov <- dotacion_ffee %>%
  group_by(date, province) %>%
  summarise(total_police = sum(police), .groups = 'drop')

## Include province code from `codigos_territoriales`

codigos_territoriales_prov <- codigos_territoriales %>%
  group_by(nombre_provincia, codigo_provincia) %>%
  summarise(total_population = sum(population), .groups = 'drop') %>%
  distinct(nombre_provincia, codigo_provincia, .keep_all = TRUE)

dotacion_ffee_prov <- left_join(dotacion_ffee_prov, codigos_territoriales_prov, 
          by = c("province" = "nombre_provincia"))


## Create variable of amount of police per 100,000 inhabitants

dotacion_ffee_prov %<>%
  mutate(police_per_100k = (total_police / total_population) * 100000)

## Include that new information in final_df

final_df %<>%
  left_join(dotacion_ffee_prov %>% 
              select(date, province, police_per_100k), 
            by = c("nombre_provincia" = "province", "date" = "date")) %>%
  mutate(police_per_100k = replace_na(police_per_100k, 0))


# ADITIONAL VARIABLES

## Additional control variables

# Day of the week and weekend/weekday

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

register_google(key = "insert_own_google_key")

# Iterate over each row in the dataset and calculate the distance
for (i in 1:nrow(codigos_territoriales)) {
  city1 <- codigos_territoriales$complete_name_comuna[i]
  city2 <- codigos_territoriales$complete_name_reg_cap[i]
  
  # Calculate the distance between the cities
  distance <- get_distance(city1, city2)
  
  # Assign the distance value to a new variable
  codigos_territoriales$distance_km[i] <- distance
}

# Problems with Alto Hospicio 01107 and Pozo Almonte 01401
codigos_territoriales$distance_km[codigos_territoriales$codigo_comuna == "01107"] <- 10.6
codigos_territoriales$distance_km[codigos_territoriales$codigo_comuna == "01401"] <- 53.1


# Add distance_km to final_df

final_df %<>%
  left_join(codigos_territoriales %>% 
              select(nombre_comuna, distance_km),
            by = "nombre_comuna")

# Add population

# Extract data

personas_censo <- tbl(censo_conectar(), "zonas") %>%
  mutate(
    region = substr(as.character(geocodigo), 1, 2),
    comuna = substr(as.character(geocodigo), 1, 5)
  ) %>%
  dplyr::select(region, comuna, geocodigo, zonaloc_ref_id) %>%
  inner_join(select(tbl(censo_conectar(), "viviendas"),
                    zonaloc_ref_id, vivienda_ref_id), by = "zonaloc_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "hogares"), vivienda_ref_id,
                    hogar_ref_id), by = "vivienda_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "personas"), hogar_ref_id,
                    p08, p15, p18), by = "hogar_ref_id") %>%
  collect()


poblacion_censo <- personas_censo %>%
  group_by(comuna) %>%
  count()

sum(poblacion_censo$n)

# Include old codes for Region del Nuble
codigos_territoriales$codigo_comuna_antiguo <- codigos_territoriales$codigo_comuna

codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16101"] <- "08401"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16102"] <- "08402"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16103"] <- "08403"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16104"] <- "08404"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16105"] <- "08405"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16106"] <- "08406"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16107"] <- "08407"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16108"] <- "08408"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16109"] <- "08409"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16201"] <- "08410"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16202"] <- "08411"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16203"] <- "08412"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16204"] <- "08413"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16205"] <- "08414"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16206"] <- "08415"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16207"] <- "08416"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16301"] <- "08417"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16302"] <- "08418" 
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16303"] <- "08419"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16304"] <- "08420"
codigos_territoriales$codigo_comuna_antiguo[codigos_territoriales$codigo_comuna == "16305"] <- "08421"

# Join n to `codigos_territoriales`
codigos_territoriales %<>%
  left_join(poblacion_censo, by = c("codigo_comuna_antiguo" = "comuna"))

codigos_territoriales$population <- codigos_territoriales$n.y

final_df %<>%
  left_join(codigos_territoriales %>% 
              select(nombre_comuna, population),
            by = "nombre_comuna")


# LAGGING VARIABLES

## T-1

final_df_1day_lag <- final_df %>%
  group_by(nombre_comuna) %>%
  arrange(date) %>%
  mutate(across(c(protest_acled, riots_acled, contentious_acled, protest_coes, 
                  shootings, beatings, arrests, crowd_control, repression_total, 
                  police_per_100k),
                ~ dplyr::lag(., 1), .names = "{.col}_lag1")) %>%
  mutate(across(c(protest_acled, riots_acled, contentious_acled, protest_coes, 
                  shootings, beatings, arrests, crowd_control, repression_total, 
                  police_per_100k),
                ~ dplyr::lag(., 2), .names = "{.col}_lag2")) %>%
  mutate(across(c(protest_acled, riots_acled, contentious_acled, protest_coes, 
                  shootings, beatings, arrests, crowd_control, repression_total, 
                  police_per_100k),
                ~ dplyr::lag(., 3), .names = "{.col}_lag3"))


## T-3: accumulation of the three days prior

final_df_3day_lag_acc <- final_df %>%
  group_by(nombre_comuna) %>%
  arrange(nombre_comuna, date) %>%
  mutate(across(c("protest_acled", "riots_acled", "contentious_acled",
                  "protest_coes", "shootings", "beatings", "arrests", 
                  "crowd_control", "repression_total", "police_per_100k"),
                ~ zoo::rollapply(.x, width = 3, FUN = sum, fill = NA, align = "right", partial = TRUE),
                .names = "acc_3day_{.col}")) %>%
  filter(row_number() %% 3 == 0) %>%
  ungroup()

## T-7: accumulation of the seven days prior

final_df_7day_lag_acc <- final_df %>%
  group_by(nombre_comuna) %>%
  arrange(nombre_comuna, date) %>%
  mutate(across(c("protest_acled", "riots_acled", "contentious_acled",
                  "protest_coes", "shootings", "beatings", "arrests", 
                  "crowd_control", "repression_total", "police_per_100k"),
                ~ zoo::rollapply(.x, width = 7, FUN = sum, fill = NA, align = "right", partial = TRUE),
                .names = "acc_7day_{.col}")) %>%
  filter(row_number() %% 7 == 0) %>%
  ungroup()

# Save dataset

save(final_df, file = "03-outputs/01-data/state-repression-df.RData")
