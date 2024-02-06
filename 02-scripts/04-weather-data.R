### Weather data ###

# Load packages

pacman::p_load(tidyr, dplyr, readr, readxl, gdata, stringr, writexl,
               lubridate, janitor, stringr, here, plm, magrittr, geosphere,
               osmdata, sf, ggmap, tmaptools, osm, geosphere, terra, geodata, revgeo, maps)


# Set wd

setwd("C:/Users/Francisca/Documents/PhD/1_research/state-repression/state-coercion-paper/data/01-raw-data/weather-data")

# Precipitations

## Load data

### Construct the full file path using file.path()
file_path_rain <- file.path(getwd(), "precipitaciones", "cr2_prDaily_2020_ghcn.txt")

### Read the data from the constructed file path
rain <- read.csv(file_path_rain, header = TRUE)

## Clean unnecesary variables

### Filter unnecesary variables
rain %<>%
  slice(-15:-43768)

## Transform to long format

### Split the database to get character variables and dates
estacion <- rain %>%
  slice(3)

latitud <- rain %>%
  slice(5)

longitud <- rain %>%
  slice(6)

dates <- rain %>%
  slice(15:n())

estacion %<>%
  pivot_longer(cols = -codigo_estacion,
               names_to = "cod",
               values_to = "nombre_estacion")

latitud %<>%
  pivot_longer(cols = -codigo_estacion,
               names_to = "cod",
               values_to = "lat")

longitud %<>%
  pivot_longer(cols = -codigo_estacion,
               names_to = "cod",
               values_to = "lon")

dates %<>%
  rename(date = codigo_estacion) %>%
  pivot_longer(cols = -date,
               names_to = "cod",
               values_to = "precipitation")

### Merge

rain_df <- estacion %>%
  select(-codigo_estacion) %>%
  left_join(select(latitud, -codigo_estacion), by = "cod") %>%
  left_join(select(longitud, -codigo_estacion), by = "cod") %>%
  left_join(dates, by = "cod")

### Categorize -9999 and empty cells as NAs

rain_df %<>%
  mutate_all(~ ifelse(. %in% c(-9999, ""), NA, .))

# Temperature (max)

### Construct file path using
file_path_t_max <- file.path(getwd(), "temperatura-maxima", "cr2_tasmaxDaily_2020_ghcn.txt")

### Read the data from the constructed file path
temp_max <- read.csv(file_path_t_max, header = TRUE)

## Clean unnecesary variables

temp_max %<>%
  slice(-15:-43768)

## Transform to long format

### Split the database to get character variables and dates
estacion <- temp_max %>%
  slice(3)

latitud <- temp_max %>%
  slice(5)

longitud <- temp_max %>%
  slice(6)

dates <- temp_max %>%
  slice(15:n())

estacion %<>%
  pivot_longer(cols = -codigo_estacion,
               names_to = "cod",
               values_to = "nombre_estacion")

latitud %<>%
  pivot_longer(cols = -codigo_estacion,
               names_to = "cod",
               values_to = "lat")

longitud %<>%
  pivot_longer(cols = -codigo_estacion,
               names_to = "cod",
               values_to = "lon")

dates %<>%
  rename(date = codigo_estacion) %>%
  pivot_longer(cols = -date,
               names_to = "cod",
               values_to = "temp_max")


### Merge

temp_max_df <- estacion %>%
  select(-codigo_estacion) %>%
  left_join(select(latitud, -codigo_estacion), by = "cod") %>%
  left_join(select(longitud, -codigo_estacion), by = "cod") %>%
  left_join(dates, by = "cod")

### Categorize -9999 and empty cells as NAs

temp_max_df %<>%
  mutate_all(~ ifelse(. %in% c(-9999, ""), NA, .))

# Temperature (mean)

### Construct file path using
file_path_t_mean <- file.path(getwd(), "temperatura-media", "cr2_tasDaily_2020.txt")

### Read the data from the constructed file path
temp_mean <- read.csv(file_path_t_mean, header = TRUE)

## Clean unnecesary variables
temp_mean %<>%
  slice(-15:-43768)

## Transform to long format

### Split the database to get character variables and dates
estacion <- temp_mean %>%
  slice(3)

latitud <- temp_mean %>%
  slice(5)

longitud <- temp_mean %>%
  slice(6)

dates <- temp_mean %>%
  slice(15:n())

estacion %<>%
  pivot_longer(cols = -codigo_estacion,
               names_to = "cod",
               values_to = "nombre_estacion")

latitud %<>%
  pivot_longer(cols = -codigo_estacion,
               names_to = "cod",
               values_to = "lat")

longitud %<>%
  pivot_longer(cols = -codigo_estacion,
               names_to = "cod",
               values_to = "lon")

dates %<>%
  rename(date = codigo_estacion) %>%
  pivot_longer(cols = -date,
               names_to = "cod",
               values_to = "temp_mean")


### Merge

temp_mean_df <- estacion %>%
  select(-codigo_estacion) %>%
  left_join(select(latitud, -codigo_estacion), by = "cod") %>%
  left_join(select(longitud, -codigo_estacion), by = "cod") %>%
  left_join(dates, by = "cod")


### Categorize -9999 and empty cells as NAs

temp_mean_df %<>%
  mutate_all(~ ifelse(. %in% c(-9999, ""), NA, .))

# Include municipality information for each station

rain_df$lon <- as.numeric(rain_df$lon)
rain_df$lat <- as.numeric(rain_df$lat)
rain_df$location_str <- paste(rain_df$lon, rain_df$lat, sep = ", ")

temp_max_df$lon <- as.numeric(temp_max_df$lon)
temp_max_df$lat <- as.numeric(temp_max_df$lat)
temp_max_df$location_str <- paste(temp_max_df$lon, temp_max_df$lat, sep = ", ")

temp_mean_df$lon <- as.numeric(temp_mean_df$lon)
temp_mean_df$lat <- as.numeric(temp_mean_df$lat)
temp_mean_df$location_str <- paste(temp_mean_df$lon, temp_mean_df$lat, sep = ", ")

# Extract unique latitude and longitude combinations
unique_rain_df <- rain_df %>%
  select(location_str, lat, lon) %>%
  distinct()

unique_temp_max_df <- temp_max_df %>%
  select(location_str, lat, lon) %>%
  distinct()

unique_temp_mean_df <- temp_mean_df %>%
  select(location_str, lat, lon) %>%
  distinct()

unique_all <- bind_rows(unique_rain_df, unique_temp_max_df, unique_temp_mean_df) %>%
  distinct(location_str, lon, lat, .keep_all = TRUE)


# Function to get the city name from latitude and longitude using Nominatim geocoding
get_city_name <- function(lat, lon) {
  geo_info <- revgeocode(c(lon, lat), output = "all")
  
  # Check if there's a result and if the formatted address is available in the third element
  if (!is.null(geo_info$results) && length(geo_info$results) >= 3 &&
      !is.null(geo_info$results[[3]]) && !is.null(geo_info$results[[3]]$formatted_address)) {
    city_name <- geo_info$results[[3]]$formatted_address
  } else {
    city_name <- "Unknown"  # If city name is not found in the third element, set to "Unknown"
  }
  
  return(city_name)
}

# Apply the get_city_name() function to the "lat" and "lon" columns using mapply
unique_all$city_name <- mapply(get_city_name, unique_all$lat, unique_all$lon)


# Get consistent names for city_name
# Function to extract last three elements from a comma-separated string
extract_last_three <- function(city_name) {
  elements <- strsplit(city_name, ", ")[[1]]
  n_elements <- length(elements)
  
  country <- NA
  province <- NA
  municipality <- NA
  
  if (n_elements >= 1) {
    country <- elements[n_elements]
  }
  if (n_elements >= 2) {
    province <- elements[n_elements - 1]
  }
  if (n_elements >= 3) {
    municipality <- elements[n_elements - 2]
    
    # Remove leading numeric values and combinations of numbers and '+' from the municipality
    municipality <- sub("^\\d+\\s*\\+*\\s*", "", municipality)
    
    # Remove non-character characters from the municipality
    municipality <- gsub("[^[:alpha:][:space:]]", "", municipality)
    
    # Remove leading spaces from the municipality
    municipality <- trimws(municipality)
  }
  
  return(data.frame(municipality = municipality, province = province, country = country))
}


# Apply the extract_last_three() function 
unique_all %<>%
  rowwise() %>%
  mutate(
    extracted_info = extract_last_three(c_across(city_name)),
    municipality = c(extracted_info$municipality),
    province = c(extracted_info$province),
    country = c(extracted_info$country)
  ) %>%
  select(-extracted_info)  # Remove the temporary column "extracted_info"


# Function to remove non-character elements, including combinations of numbers and symbols, from a character vector
remove_non_characters <- function(text) {
  # Remove combinations of letters and digits followed by spaces at the beginning
  cleaned_text <- gsub("^[A-Za-z0-9]+\\s+", "", text)
  
  # Remove leading spaces
  cleaned_text <- trimws(cleaned_text)
  
  return(cleaned_text)
}

remove_combinations_and_spaces <- function(text) {
  # Remove combinations of letters, digits, and symbols followed by spaces at the beginning
  cleaned_text <- gsub("^\\w+\\s*\\+\\s*", "", text)
  
  # Remove leading spaces
  cleaned_text <- trimws(cleaned_text)
  
  return(cleaned_text)
}

remove_leading_combinations <- function(text) {
  # Remove combinations of letters, digits, or symbols followed by spaces at the beginning
  cleaned_text <- gsub("^\\S+\\s+", "", text)
  
  # Remove leading spaces
  cleaned_text <- trimws(cleaned_text)
  
  return(cleaned_text)
}

# Modify municipalities with NAs directly in the existing database
unique_all %<>%
  mutate(
    municipality = ifelse(is.na(municipality), province, municipality),
    municipality = remove_non_characters(municipality)
  )

unique_all %<>%
  mutate(
    municipality = ifelse(is.na(municipality), province, municipality),
    municipality = remove_combinations_and_spaces(municipality)
  )

unique_all %<>%
  mutate(
    municipality = ifelse(is.na(municipality), province, municipality),
    municipality = remove_leading_combinations(municipality)
  )


# Join information to original dataframes

# Merge unique_rain_df with rain_df, using custom suffixes for overlapping columns
rain_locations <- merge(rain_df, unique_all, by = "location_str", all.x = TRUE, suffixes = c("", "_unique"))

# Merge unique_temp_max_df with temp_max_df, using custom suffixes for overlapping columns
temp_max_locations <- merge(temp_max_df, unique_all, by = "location_str", all.x = TRUE, suffixes = c("", "_unique"))

# Merge unique_temp_mean_df with temp_mean_df, using custom suffixes for overlapping columns
temp_mean_locations <- merge(temp_mean_df, unique_all, by = "location_str", all.x = TRUE, suffixes = c("", "_unique"))

# Remove other countries

rain_locations %<>% 
  filter(country == "Chile")

temp_max_locations %<>% 
  filter(country == "Chile")

temp_mean_locations %<>% 
  filter(country == "Chile")

# Combine all three databases
weather <- bind_rows(rain_locations, temp_max_locations, temp_mean_locations)

# Correct municipality names
codigos_territoriales <- (chilemapas::codigos_territoriales)

# Remove stations outside lon lat limits. Define the latitude and longitude boundaries for Chile
chile_latitude_min <- -56.0
chile_latitude_max <- -17.5
chile_longitude_min <- -75.0
chile_longitude_max <- -66.5

# Filter the "weather" data frame to keep only the rows within Chile's boundaries
weather %<>%
  filter(lat >= chile_latitude_min,
         lat <= chile_latitude_max,
         lon >= chile_longitude_min,
         lon <= chile_longitude_max)

## Replace all tildes
weather$municipality <- gsub("[áÁ]", "a", weather$municipality)
weather$municipality <- gsub("[éÉ]", "e", weather$municipality)
weather$municipality <- gsub("[íÍ]", "i", weather$municipality)
weather$municipality <- gsub("[óÓ]", "o", weather$municipality)
weather$municipality <- gsub("[úÚ]", "u", weather$municipality)
weather$municipality <- gsub("[ñÑ]", "n", weather$municipality)

weather$municipality[weather$municipality== "Almagro"] <- "Diego de Almagro"
weather$municipality[weather$municipality== "Almonte"] <- "Pozo Almonte"
weather$municipality[weather$municipality== "Alto"] <- "Frutillar"
weather$municipality[weather$municipality== "Altos"] <- "Lo Barnechea"
weather$municipality[weather$municipality== "Amarilla"] <- "Tierra Amarilla"
weather$municipality[weather$municipality== "Andes"] <- "Los Andes"
weather$municipality[weather$municipality== "Angeles"] <- "Los Angeles"
weather$municipality[weather$municipality== "Angostura"] <- "Caldera"
weather$municipality[weather$municipality== "Antonio"] <- "San Antonio"
weather$municipality[weather$municipality== "Arenas"] <- "Punta Arenas"
weather$municipality[weather$municipality== "Atacama"] <- "San Pedro de Atacama"
weather$municipality[weather$municipality== "Aysen"] <- "Aisen"
weather$municipality[weather$municipality== "Baquedano"] <- "Sierra Gorda"
weather$municipality[weather$municipality== "Barbara"] <- "Santa Barbara"
weather$municipality[weather$municipality== "Bautista"] <- "San Juan Bautista"
weather$municipality[weather$municipality== "Barnechea"] <- "Lo Barnechea"
weather$municipality[weather$municipality== "Biobio"] <- "Alto Biobio"
weather$municipality[weather$municipality== "Blanca"] <- "Laguna Blanca"
weather$municipality[weather$municipality== "Bueno"] <- "Rio Bueno"
weather$municipality[weather$municipality== "Cabras"] <- "Las Cabras"
weather$municipality[weather$municipality== "Cachapoal"] <- "Machali"
weather$municipality[weather$municipality== "Carlos"] <- "San Carlos"
weather$municipality[weather$municipality== "Carmen"] <- "Alto del Carmen"
weather$municipality[weather$municipality== "Casas"] <- "Padre las Casas"
weather$municipality[weather$municipality== "Central"] <- "Estacion Central"
weather$municipality[weather$municipality== "Chico"] <- "Chile Chico"
weather$municipality[weather$municipality== "Claro"] <- "Rio Claro"
weather$municipality[weather$municipality== "Clemente"] <- "San Clemente"
weather$municipality[weather$municipality== "Colchagua"] <- "San Fernando"
weather$municipality[weather$municipality== "Condes"] <- "Las Condes"
weather$municipality[weather$municipality== "Costa"] <- "San Juan de la Costa"
weather$municipality[weather$municipality== "Coyhaique"] <- "Coihaique"
weather$municipality[weather$municipality== "Cruz"] <- "Santa Cruz"
weather$municipality[weather$municipality== "Domingo"] <- "Santo Domingo"
weather$municipality[weather$municipality== "Elena"] <- "Santa Elena"
weather$municipality[weather$municipality== "Esperanza"] <- "Ultima Esperanza"
weather$municipality[weather$municipality== "Esteban"] <- "San Esteban"
weather$municipality[weather$municipality== "Fabian"] <- "San Fabian"
weather$municipality[weather$municipality== "Familia"] <- "Sagrada Familia"
weather$municipality[weather$municipality== "Felipe"] <- "San Felipe"
weather$municipality[weather$municipality== "Fernando"] <- "San Fernando"
weather$municipality[weather$municipality== "Glasinovic"] <- "Antofagasta"
weather$municipality[weather$municipality== "Gorda"] <- "Sierra"
weather$municipality[weather$municipality== "Grande"] <- "Curico"
weather$municipality[weather$municipality== "Gregorio"] <- "San Gregorio"
weather$municipality[weather$municipality== "Guadal"] <- "Colchagua"
weather$municipality[weather$municipality== "Higuera"] <- "La Higuera"
weather$municipality[weather$municipality== "Hornos"] <- "Cabo de Hornos"
weather$municipality[weather$municipality== "Hurtado"] <- "Rio Hurtado"
weather$municipality[weather$municipality== "Ibanez"] <- "Rio Ibanez"
weather$municipality[weather$municipality== "Ignacio"] <- "San Ignacio"
weather$municipality[weather$municipality== "Imperial"] <- "Nueva Imperial"
weather$municipality[weather$municipality== "Island"] <- "Isla de Pascua"
weather$municipality[weather$municipality== "Javier"] <- "San Javier"
weather$municipality[weather$municipality== "Lagos"] <- "Los Lagos"
weather$municipality[weather$municipality== "Lastenia"] <- "Antofagasta"
weather$municipality[weather$municipality== "Ligua"] <- "La Ligua"
weather$municipality[weather$municipality== "Loa"] <- "Calama"
weather$municipality[weather$municipality== "Lucia"] <- "Chaiten"
weather$municipality[weather$municipality== "Maipo"] <- "San Jose de Maipo"
weather$municipality[weather$municipality== "Manzano"] <- "San José de Maipo"
weather$municipality[weather$municipality== "Mar"] <- "Vina del Mar"
weather$municipality[weather$municipality== "Maria"] <- "Santa Maria"
weather$municipality[weather$municipality== "Montt"] <- "Puerto Montt"
weather$municipality[weather$municipality== "Monturaqui"] <- "Antofagasta"
weather$municipality[weather$municipality== "Muermos"] <- "Los Muermos"
weather$municipality[weather$municipality== "Negro"] <- "Rio Negro"
weather$municipality[weather$municipality== "nombre"] <- "Antofagasta"
weather$municipality[weather$municipality== "Normal"] <- "Quinta Normal"
weather$municipality[weather$municipality== "Octay"] <- "Puerto Octay"
weather$municipality[weather$municipality== "Ollagüe"] <- "Ollague"
weather$municipality[weather$municipality== "Pablo"] <- "San Pablo"
weather$municipality[weather$municipality== "Paihuano"] <- "Paiguano"
weather$municipality[weather$municipality== "Patria"] <- "Monte Patria"
weather$municipality[weather$municipality== "Paz"] <- "San Pedro de la Paz"
weather$municipality[weather$municipality== "Province"] <- "Putre"
weather$municipality[weather$municipality== "Puyuhuapi"] <- "Cisnes"
weather$municipality[weather$municipality== "Quenes"] <- "Romeral"
weather$municipality[weather$municipality== "Quillen"] <- "Perquenco"
weather$municipality[weather$municipality== "Rafael"] <- "San Rafael"
weather$municipality[weather$municipality== "Ranco"] <- "Lago Ranco"
weather$municipality[weather$municipality== "Reina"] <- "La Reina"
weather$municipality[weather$municipality== "Rivadavia"] <- "Vicuna"
weather$municipality[weather$municipality== "Rodelillo"] <- "Valparaiso"
weather$municipality[weather$municipality== "Ruta"] <- "Coquimbo"
weather$municipality[weather$municipality== "Santa Elena"] <- "Maria Elena"
weather$municipality[weather$municipality== "Schmidt"] <- "Teodoro Schmidt"
weather$municipality[weather$municipality== "Serena"] <- "La Serena"
weather$municipality[weather$municipality== "Sierra"] <- "Sierra Gorda"
weather$municipality[weather$municipality== "snumero"] <- "Antofagasta"
weather$municipality[weather$municipality== "Tabo"] <- "El Tabo"
weather$municipality[weather$municipality== "Tamarugal"] <- "Pica"
weather$municipality[weather$municipality== "Transito"] <- "Alto del Carmen"
weather$municipality[weather$municipality== "Ultima Esperanza"] <- "Natales"
weather$municipality[weather$municipality== "Union"] <- "La Union"
weather$municipality[weather$municipality== "Varas"] <- "Puerto Varas"
weather$municipality[weather$municipality== "Verde"] <- "Lago Verde"
weather$municipality[weather$municipality== "El Vergel"] <- "Valparaiso"
weather$municipality[weather$municipality== "Vicente"] <- "San Vicente"
weather$municipality[weather$municipality== "Viejo"] <- "Chillan Viejo"
weather$municipality[weather$municipality== "Vilos"] <- "Los Vilos"
weather$municipality[weather$municipality== "Vina"] <- "Vina del Mar"
weather$municipality[weather$municipality== "Vergel"] <- "Valparaiso"
weather$municipality[weather$nombre_estacion== "Islas Huichas"] <- "Aisen"

weather$municipality[weather$municipality== "Antofagasta Andres Sabella  Camino a mejillones snumero"] <- "Antofagasta"
weather$municipality[weather$municipality== "B"] <- "Antofagasta"
weather$municipality[weather$municipality== "Bio Province"] <- "Biobio"
weather$municipality[weather$municipality== "Colchagua"] <- "Chile Chico"
weather$municipality[weather$municipality== "de la Costa"] <- "San Juan de la Costa"
weather$municipality[weather$municipality== "de Atacama"] <- "San Pedro de Atacama"
weather$municipality[weather$municipality== "de la Paz"] <- "San Pedro de la Paz"
weather$municipality[weather$municipality== "de Maipo"] <- "San Jose de Maipo"
weather$municipality[weather$municipality== "del Mar"] <- "Vina del Mar"
weather$municipality[weather$municipality== "Santa Lucia"] <- "Chaiten"
weather$municipality[weather$municipality== "Fuego Province"] <- "Tierra del Fuego"
weather$municipality[weather$municipality== "Prat Province"] <- "Aisen"
weather$municipality[weather$municipality== "sin nombre"] <- "Antofagasta"
weather$municipality[weather$municipality== "Tierra del Fuego"] <- "Porvenir"
weather$municipality[weather$municipality== "Biobio"] <- "Alto Biobio"
weather$municipality[weather$municipality== "Maitenes Altos"] <- "Lo Barnechea"

## Perform the left join based on matching columns
weather_check <- left_join(weather, codigos_territoriales, by = c("municipality" = "nombre_comuna"))

## Find rows with missing codigo_comuna
municipalities_without_codigo <- weather_check[is.na(weather_check$codigo_comuna), "municipality"]

## Get unique municipalities without a codigo_comuna
unique_municipalities_without_codigo <- unique(municipalities_without_codigo)

## Convert to a data frame
municipalities_df <- data.frame(municipality = unique_municipalities_without_codigo)
table(municipalities_df$municipality)

# Construct final weather database
weather_df <- weather_check

# Check for weird temperatures
weather_df %<>%
  filter(cod != "X01020017" & cod != "X01020016" & cod != "X10123004")

# Get mean precipitations, mean max temperature and mean median temperature per municipality
colnames(weather_df)

class(weather_df$precipitation)
class(weather_df$temp_mean)
class(weather_df$temp_max)
class(weather_df$cod)

weather_df$precipitation <- as.numeric(weather_df$precipitation)
weather_df$temp_mean <- as.numeric(weather_df$temp_mean)
weather_df$temp_max <- as.numeric(weather_df$temp_max)

weather_avg <- weather_df %>%
  group_by(date, codigo_comuna) %>%
  summarize(
    station_count = n(),  # Count the number of stations per codigo_comuna per day
    weighted_precipitation = round(sum(precipitation * (1 / station_count), na.rm = TRUE), 3),
    weighted_temp_mean = round(sum(temp_mean * (1 / station_count), na.rm = TRUE), 3),
    weighted_temp_max = round(sum(temp_max * (1 / station_count), na.rm = TRUE), 3)
  )


# Save dataset
save(weather_df, file = "weather_df.RData")

# Get province averages
weather_df_province <- weather_df %>%
  group_by(codigo_provincia, date) %>%
  summarize(avg_precipitation = mean(precipitation, na.rm = TRUE),
            avg_max_temperature = mean(temp_max, na.rm = TRUE),
            avg_temp = mean(temp_mean, na.rm = TRUE)) %>%
  mutate(avg_precipitation = ifelse(is.nan(avg_precipitation), NA, avg_precipitation),
         avg_max_temperature = ifelse(is.nan(avg_max_temperature), NA, avg_max_temperature),
         avg_temp = ifelse(is.nan(avg_temp), NA, avg_temp))

# Remove NAs

weather_df_province %<>%
  filter(!is.na(codigo_provincia))

###########################################################################################
##### With manually obtained data

weather_manual <- read_excel("data/01-raw-data/weather-data/weather_df.xlsx")

weather_manual$Municipality[weather_manual$Municipality== "Coyhaique"] <- "Coihaique"
weather_manual$Municipality[weather_manual$Municipality== "Puerto Aisen"] <- "Aisen"
weather_manual$Municipality[weather_manual$Municipality== "La Junta"] <- "Cochrane"
weather_manual$Municipality[weather_manual$Municipality== "Villa O'Higgins"] <- "Aisen"
weather_manual$Municipality[weather_manual$Municipality== "Ollagüe"] <- "Ollague"
weather_manual$Municipality[weather_manual$Municipality== "Chañaral"] <- "Chanaral"
weather_manual$Municipality[weather_manual$Municipality== "Cañete"] <- "Canete"
weather_manual$Municipality[weather_manual$Municipality== "Padre Las Casas"] <- "Padre las Casas"
weather_manual$Municipality[weather_manual$Municipality== "Reinaco"] <- "Renaico"
weather_manual$Municipality[weather_manual$Municipality== "Timuakel"] <- "Timaukel"

## Include codigo_provincia and codigo_comuna

weather_manual <- left_join(weather_manual, codigos_territoriales, by = c("Municipality" = "nombre_comuna"))

# Check NAs

weather_manual %>%
  filter(is.na(codigo_comuna)) %>%
  distinct(Municipality)

# Convert date

weather_manual$Date <- dmy(weather_manual$Date)

# Get averages for each province

weather_province <- weather_manual %>%
  group_by(codigo_provincia, Date) %>%
  summarize(avg_precipitation = mean(Precipitations, na.rm = TRUE),
            avg_max_temperature = mean(Temperature, na.rm = TRUE)) %>%
  mutate(avg_precipitation = ifelse(is.nan(avg_precipitation), NA, avg_precipitation),
         avg_max_temperature = ifelse(is.nan(avg_max_temperature), NA, avg_max_temperature))

# Check NAs

weather_province %>%
  filter(is.na(avg_precipitation)) %>%
  distinct(codigo_provincia)

weather_province %>%
  filter(is.na(avg_max_temperature)) %>%
  distinct(codigo_provincia)

# Include information from weather_df_province

weather_province$avg_temp <- NA
weather_province$date <- weather_province$Date

class(weather_province$date)
weather_df_province$date <- as.Date(weather_df_province$date)

# Join the two data frames based on 'codigo_provincia' and 'date'
merged_weather <- left_join(weather_province, weather_df_province, by = c("codigo_provincia", "date"))

# Replace NA values with corresponding values from weather_df_province
merged_weather <- merged_weather %>%
  mutate(avg_precipitation = ifelse(is.na(avg_precipitation.x), avg_precipitation.y, avg_precipitation.x),
         avg_max_temperature = ifelse(is.na(avg_max_temperature.x), avg_max_temperature.y, avg_max_temperature.x),
         avg_temp = ifelse(is.na(avg_temp.x), avg_temp.y, avg_temp.x)) %>%
  select(-ends_with(".x"), -ends_with(".y"))  # Remove duplicated columns from the join


# Create dummy variables for precipitations and temperatures over 30 degrees