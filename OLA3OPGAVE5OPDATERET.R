### OPAGVE 5 ####
library(httr)
library(jsonlite)
library(openair)

api_key <- "df80f145-972c-44c1-958f-fac98738f9b0"

# tilgå API med observationer
# endpoint
url <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items"

# Send GET-anmodning til API'et
response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
response$status_code

# parse indhold
data <- fromJSON(content(response, "text"))

observationer <- data$features

### OPGAVE 5.3

# tilgå API for vejrudsigt
api_key_forecast <- "df80f145-972c-44c1-958f-fac98738f9b0"

# endpoint
url_forecast <- "https://dmigw.govcloud.dk/v1/forecastedr/collections"

response_forecast <- GET(url_forecast, add_headers("X-Gravitee-Api-Key" = api_key_forecast))
response_forecast$status_code

data_forecast <- fromJSON(content(response_forecast, "text"))

observationer_forecast <- data_forecast$collections # 16 collections
length(observationer_forecast) #16

# ID lillebælt - dkss_lb


### OPGAVE 5.4 ###

# århus
api_key <- "df80f145-972c-44c1-958f-fac98738f9b0"

url_aarhus <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                     "parameterId=wind_dir&datetime=2022-11-01T00:00:00Z/2022-11-30T23:59:59Z&limit=30000&stationId=06074")

response1 <- GET(url_aarhus, add_headers("X-Gravitee-Api-Key" = api_key))
response1$status_code

data_aarhus <- fromJSON(content(response1, "text"))
test <- data_aarhus$features[1,]

data_aarhus <- as.data.frame(data_aarhus$features)
colnames(data_aarhus)
data_aarhus <- data_aarhus$properties
str(data_aarhus)

library(dplyr)
data_aarhus$observed <- substr(data_aarhus$observed, 1, 10) # hent de første 10 tegn
daily_wind_dir_obs_aarhus <- data_aarhus %>%
  group_by(observed) %>%
  summarise(avg_wind_dir = mean(value, na.rm = TRUE))


daily_wind_dir_obs_aarhus <- daily_wind_dir_obs_aarhus %>%
  mutate(wind_direction = case_when(
    (avg_wind_dir >= 348.75 | avg_wind_dir < 11.25) ~ "N",
    (avg_wind_dir >= 11.25 & avg_wind_dir < 33.75)  ~ "N/NE",
    (avg_wind_dir >= 33.75 & avg_wind_dir < 56.25)  ~ "NE",
    (avg_wind_dir >= 56.25 & avg_wind_dir < 78.75)  ~ "E/NE",
    (avg_wind_dir >= 78.75 & avg_wind_dir < 101.25) ~ "E",
    (avg_wind_dir >= 101.25 & avg_wind_dir < 123.75)~ "E/SE",
    (avg_wind_dir >= 123.75 & avg_wind_dir < 146.25)~ "SE",
    (avg_wind_dir >= 146.25 & avg_wind_dir < 168.75)~ "S/SE",
    (avg_wind_dir >= 168.75 & avg_wind_dir < 191.25)~ "S",
    (avg_wind_dir >= 191.25 & avg_wind_dir < 213.75)~ "S/SW",
    (avg_wind_dir >= 213.75 & avg_wind_dir < 236.25)~ "SW",
    (avg_wind_dir >= 236.25 & avg_wind_dir < 258.75)~ "W/SW",
    (avg_wind_dir >= 258.75 & avg_wind_dir < 281.25)~ "W",
    (avg_wind_dir >= 281.25 & avg_wind_dir < 303.75)~ "W/NW",
    (avg_wind_dir >= 303.75 & avg_wind_dir < 326.25)~ "NW",
    (avg_wind_dir >= 326.25 & avg_wind_dir < 348.75)~ "N/NW",
    TRUE ~ "Unknown"  # hvis der er værdier der ikke passer
  ))


url_wind_speed_aarhus <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                                "parameterId=wind_speed&datetime=2022-11-01T00:00:00Z/2022-11-30T23:59:59Z&limit=30000&stationId=06074")

response_wind_speed_aarhus <- GET(url_wind_speed_aarhus, add_headers("X-Gravitee-Api-Key" = api_key))
response_wind_speed_aarhus$status_code

data_wind_speed_aarhus <- fromJSON(content(response_wind_speed_aarhus, "text"))
data_wind_speed_aarhus <- as.data.frame(data_wind_speed_aarhus$features)
data_wind_speed_aarhus <- data_wind_speed_aarhus$properties

max(data_wind_speed_aarhus$value)

data_wind_speed_aarhus$observed <- substr(data_wind_speed_aarhus$observed, 1, 10) # hent de første 10 tegn
daily_wind_speed_obs_aarhus <- data_wind_speed_aarhus %>%
  group_by(observed) %>%
  summarise(avg_wind_speed = mean(value, na.rm = TRUE))

aarhus_daily_wind_obs <- cbind(daily_wind_speed_obs_aarhus, daily_wind_dir_obs_aarhus[,2:3])


#anholt
url_anholt <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                     "parameterId=wind_dir&datetime=2022-11-01T00:00:00Z/2022-11-30T23:59:59Z&limit=30000&stationId=06079")

response2 <- GET(url_anholt, add_headers("X-Gravitee-Api-Key" = api_key))
response2$status_code

data_anholt <- fromJSON(content(response2, "text"))
data_anholt <- as.data.frame(data_anholt$features)
data_anholt <- data_anholt$properties
str(data_anholt)

library(dplyr)
data_anholt$observed <- substr(data_anholt$observed, 1, 10) # hent de første 10 tegn
daily_wind_dir_anholt <- data_anholt %>%
  group_by(observed) %>%
  summarise(avg_wind_dir = mean(value, na.rm = TRUE))


daily_wind_dir_anholt <- daily_wind_dir_anholt %>%
  mutate(wind_direction = case_when(
    (avg_wind_dir >= 348.75 | avg_wind_dir < 11.25) ~ "N",
    (avg_wind_dir >= 11.25 & avg_wind_dir < 33.75)  ~ "N/NE",
    (avg_wind_dir >= 33.75 & avg_wind_dir < 56.25)  ~ "NE",
    (avg_wind_dir >= 56.25 & avg_wind_dir < 78.75)  ~ "E/NE",
    (avg_wind_dir >= 78.75 & avg_wind_dir < 101.25) ~ "E",
    (avg_wind_dir >= 101.25 & avg_wind_dir < 123.75)~ "E/SE",
    (avg_wind_dir >= 123.75 & avg_wind_dir < 146.25)~ "SE",
    (avg_wind_dir >= 146.25 & avg_wind_dir < 168.75)~ "S/SE",
    (avg_wind_dir >= 168.75 & avg_wind_dir < 191.25)~ "S",
    (avg_wind_dir >= 191.25 & avg_wind_dir < 213.75)~ "S/SW",
    (avg_wind_dir >= 213.75 & avg_wind_dir < 236.25)~ "SW",
    (avg_wind_dir >= 236.25 & avg_wind_dir < 258.75)~ "W/SW",
    (avg_wind_dir >= 258.75 & avg_wind_dir < 281.25)~ "W",
    (avg_wind_dir >= 281.25 & avg_wind_dir < 303.75)~ "W/NW",
    (avg_wind_dir >= 303.75 & avg_wind_dir < 326.25)~ "NW",
    (avg_wind_dir >= 326.25 & avg_wind_dir < 348.75)~ "N/NW",
    TRUE ~ "Unknown"  # For værdier, der ikke passer
  ))


url_wind_speed_anholt <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                                "parameterId=wind_speed&datetime=2022-11-01T00:00:00Z/2022-11-30T23:59:59Z&limit=30000&stationId=06079")

response_wind_speed_anholt <- GET(url_wind_speed_anholt, add_headers("X-Gravitee-Api-Key" = api_key))
response_wind_speed_anholt$status_code

data_wind_speed_anholt <- fromJSON(content(response_wind_speed_anholt, "text"))
data_wind_speed_anholt <- as.data.frame(data_wind_speed_anholt$features)
data_wind_speed_anholt <- data_wind_speed_anholt$properties

max(data_wind_speed_anholt$value)

data_wind_speed_anholt$observed <- substr(data_wind_speed_anholt$observed, 1, 10) # hent de første 10 tegn
daily_wind_speed_anholt <- data_wind_speed_anholt %>%
  group_by(observed) %>%
  summarise(avg_wind_speed = mean(value, na.rm = TRUE))

max(data_wind_speed_anholt$value)

daily_wind_obs_anholt <- cbind(daily_wind_speed_anholt, daily_wind_dir_anholt[,2:3])


# lav graf for stormen i oktober 2023

# Definer dine stationId'er Aarhus og Anholt
station_ids <- c("06074", "06079")  # Tilføj flere stationer, hvis nødvendigt

# Opret en tom liste til at gemme data fra hver station
data_list <- list()

# API nøgle
api_key <- "2b0c3d28-a837-4b01-a4b5-5947848f934c"

# Loop igennem hver station og hent data for vindretning
for (station in station_ids) {
  # Definer URL med den aktuelle stationId
  url <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                "parameterId=wind_dir_past1h&datetime=2023-10-01T00:00:00Z/2023-10-31T23:59:59Z&stationId=", station)
  
  # Send GET-anmodning
  response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
  
  # Parse JSON-indhold og gem i listen
  data <- fromJSON(content(response, "text"))
  data_list[[station]] <- as.data.frame(data$features)
}

# Kombiner alle data frames i én samlet data frame
combined_data <- bind_rows(data_list)
combined_data <- combined_data$properties

# Fjern klokkeslæt fra kolonne observed
combined_data$observed <- substr(combined_data$observed, 1, 10)

# Beregn gennemsnitlig vindretning per dag og per station
daily_wind_dir <- combined_data %>%
  group_by(observed, stationId) %>%
  summarise(avg_wind_dir = mean(value, na.rm = TRUE))

# Loop igennem hver station og hent data for vindstyrke
for (station in station_ids) {
  # Definer URL med den aktuelle stationId
  url <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                "parameterId=wind_max_per10min_past1h&datetime=2023-10-01T00:00:00Z/2023-10-31T23:59:59Z&stationId=", station)
  
  # Send GET-anmodning
  response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
  
  # Parse JSON-indhold og gem i listen
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  data_list[[station]] <- as.data.frame(data$features)
}

# Opret en ny data frame til vindstyrke ved at kombinere data for alle stationer fra data_list
combined_data_wind_speed <- bind_rows(data_list)
combined_data_wind_speed <- combined_data_wind_speed$properties

max(combined_data_wind_speed$value)

# Fjern klokkeslet fra observed
combined_data_wind_speed$observed <- substr(combined_data_wind_speed$observed, 1, 10)

# Beregn gennemsnitlig vindstyrke per dag og per station
daily_wind_speed <- combined_data_wind_speed %>%
  group_by(observed, stationId) %>%
  summarise(avg_wind_speed = mean(value, na.rm = TRUE))

# sæt de to df sammen
combined_daily_wind <- cbind(daily_wind_speed, daily_wind_dir[,3])


# plot m linje og barplot

ggplot(combined_daily_wind, aes(x = observed)) +
  # Søjlediagram for gennemsnitlig vindretning (sekundær akse)
  geom_bar(aes(y = avg_wind_dir / 18), stat = "identity", fill = "black", alpha = 0.5) +
  
  # Linjediagram for gennemsnitlig vindhastighed (primær akse)
  geom_line(aes(y = avg_wind_speed, group = stationId, color = stationId), size = 0.8) +
  
  # Primær y-akse for vindhastighed og sekundær y-akse for vindretning
  scale_y_continuous(
    name = "Gns vind hastighed (m/s)",                           # Titel for primær y-akse
    limits = c(0, 20),                                           # Skalaen for avg_wind_speed
    sec.axis = sec_axis(~ . * 18, name = "Gns vin retning (grader)")  # Sekundær y-akse titel og skala
  ) +
  
  labs(
    title = "Kraftigst vind på Anholt",
    x = "tidsperiode 1/10/23 - 31/10/23"
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

# windrose

# Definer dine stationId'er Aarhus og Anholt
station_ids <- c("06074", "06079")  # Tilføj flere stationer, hvis nødvendigt

# Opret en tom liste til at gemme data fra hver station
data_list <- list()

# API nøgle
api_key <- "2b0c3d28-a837-4b01-a4b5-5947848f934c"

# Loop igennem hver station og hent data for vindstyrke
for (station in station_ids) {
  # Definer URL med den aktuelle stationId
  url <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                "parameterId=wind_max_past1h&datetime=2023-10-01T00:00:00Z/2023-10-31T23:59:59Z&stationId=", station)
  
  # Send GET-anmodning
  response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
  
  # Parse JSON-indhold og gem i listen
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  data_list[[station]] <- as.data.frame(data$features)
}



library(openair)

#Create the wind rose plot using openairs windRose
#Adding custom settings to display degrees for wind direction

windRose(combined_daily_wind, ws = "avg_wind_speed", wd = "avg_wind_dir",
         paddle = FALSE,
         breaks = seq(0, 30, 5), # Adjust for desired speed intervals
         angle.scale = 45, # Optional: Set intervals in degrees on plot
         key.footer = "Wind Speed (m/s)", 
         key.position = "right",
         main = "Wind Rose: Direction and Speed",
         type = "default")


# plot m pile
combined_data_plot <- combined_daily_wind
combined_data_plot$observed <- as.numeric(as.Date(combined_data_plot$observed))

# Bestem længden af pilene
arrow_length <- 0.5

# Sørg for, at `angle_rad` er i radianer
combined_data_plot <- combined_data_plot %>%
  mutate(angle_rad = avg_wind_dir * pi / 180)  # Konverter `avg_wind_dir` fra grader til radianer

# Plot af vindstyrke og vindretning
ggplot(combined_data_plot, aes(x = observed, color = factor(stationId))) +
  # Linjegraf for gennemsnitlig vindstyrke
  geom_line(aes(y = avg_wind_speed), linewidth = 1) +
  
  # Tilføj pile for vindretningen (ændring i både x- og y-retning)
  geom_segment(
    aes(
      x = observed,  # Startpunkt for pilen
      y = avg_wind_speed,
      xend = observed + arrow_length * cos(angle_rad),  # Endepunkt x baseret på cosinus af retningen
      yend = avg_wind_speed + arrow_length * sin(angle_rad),  # Endepunkt y baseret på sinus af retningen
      color = factor(stationId)
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    size = 0.6,
    lineend = "round"
  ) +
  
  # Titel og akse-labels
  labs(
    title = "Kraftigst vindstyrke på Anholt",
    x = "Dato - Oktober 2023",
    y = "Vindstyrke (m/s)"
  ) +
  
  # Æstetik
  scale_color_manual(values = c("06074" = "blue", "06079" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "Station ID")) +
  
  # Skaler x-aksen og formatter som datoer
  scale_x_continuous(
    labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%Y-%m-%d"),
    breaks = scales::pretty_breaks(n = 10)
  )