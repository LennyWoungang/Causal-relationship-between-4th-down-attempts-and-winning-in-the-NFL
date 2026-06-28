### Loading Packages
install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
install.packages("gt")
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(dplyr)
library(stringr)

### Loading nflfastR Datasets for seasons 2010-2024
years <- 2011:2024
for (year in years) {
  pbp_name <- paste0("pbp", year)  
  assign(pbp_name, load_pbp(year), envir = .GlobalEnv)  
}

### Only keeping 4 teams of interest when exposure = 1
for (year in years) {
  pbp_name <- paste0("pbp", year)     
  fourth_name <- paste0("fourth", year) 
  if (exists(pbp_name)) { 
    df <- get(pbp_name) %>%
      filter(
        down == 4, 
        season_type == "REG", 
        penalty == 0, 
        play_type %in% c("run", "pass"), 
        posteam %in% c("IND", "JAX", "KC", "LA")
      )
    assign(fourth_name, df, envir = .GlobalEnv) 
  }
}
# Keeping certain variables of interest
datasets <- paste0("fourth", years)
columns_to_keep <- c("game_id", "posteam", "posteam_type", "defteam", "week", "game_date", "season", "down", "play_type", "result", "penalty", "weather", "wind", "temp", "surface", "roof", "spread_line")  
filtered_datasets <- lapply(datasets, function(name) {
  if (exists(name)) {
    df <- get(name)  
    df <- df %>% select(all_of(columns_to_keep)) 
    assign(name, df, envir = .GlobalEnv) 
  }
})
# Removing duplicate games
filtered_datasets <- lapply(datasets, function(name) {
  if (exists(name)) {
    df <- get(name)  
    df <- df %>% distinct(week, posteam, .keep_all = TRUE)
    assign(name, df, envir = .GlobalEnv)  
  }
})

### Only keeping 4 teams of interest when exposure = 0
for (year in years) {
  pbp_name <- paste0("pbp", year)     
  fourth_name <- paste0("nofourth", year) 
  if (exists(pbp_name)) { 
    df <- get(pbp_name) %>%
      filter(
        down == 4, 
        season_type == "REG", 
        penalty == 0, 
        posteam %in% c("IND", "JAX", "KC", "LA")
      )
    assign(fourth_name, df, envir = .GlobalEnv) 
  }
}
# Keeping certain variables of interest
datasets <- paste0("nofourth", years)
columns_to_keep <- c("game_id", "posteam", "posteam_type", "defteam", "week", "game_date", "season", "down", "play_type", "result", "penalty", "weather", "wind", "temp", "surface", "roof", "spread_line")  
filtered_datasets <- lapply(datasets, function(name) {
  if (exists(name)) {
    df <- get(name)  
    df <- df %>% select(all_of(columns_to_keep)) 
    assign(name, df, envir = .GlobalEnv) 
  }
})
# Removing duplicate games
for (year in years) {
  fourth_name <- paste0("nofourth", year)
  if (exists(fourth_name)) {
    df <- get(fourth_name) %>%
      group_by(week, posteam) %>%
      filter(!any(play_type %in% c("run", "pass"))) %>%  
      slice(1) %>%
      ungroup()
    assign(fourth_name, df, envir = .GlobalEnv)
  }
}



#### Combining datasets of exposure = 1 and exposure = 0 for each season
for (year in years) {
  fourth_name <- paste0("fourth", year)     
  nofourth_name <- paste0("nofourth", year)
  combined_name <- paste0("combined", year) 
  df <- bind_rows(get(fourth_name), get(nofourth_name))
  assign(combined_name, df, envir = .GlobalEnv)
}
# 2021 chiefs vs eagles had no fourth down plays so that season has 1 less game


### Adding defensive and offensive ratings for each season, where 1 = top half defense/offense, 2 = bottom half defense/offense
combined2024 <- combined2024 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("PHI", "LAC", "DEN", "MIN", "GB", "KC", "PIT", "BAL", "MIA", "HOU", "DET", "BUF", "SEA", "CHI", "LA", "ARI") ~ 1,  
    TRUE ~ 2  
  ))
combined2024 <- combined2024 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 1,
    posteam == "LA" ~ 2,
    posteam == "JAX" ~ 2,
    posteam == "KC" ~ 1
  ))

combined2023 <- combined2023 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("GB", "KC", "PIT", "BAL", "HOU", "BUF", "SF", "NO", "TB", "LV", "NYJ", "NE", "TEN", "JAX", "DAL", "MIN") ~ 1,  
    TRUE ~ 2  
  ))
combined2023 <- combined2023 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 1,
    posteam == "LA" ~ 1,
    posteam == "JAX" ~ 1,
    posteam == "KC" ~ 1
  ))

combined2022 <- combined2022 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("SF","NYJ","BAL", "BUF","CIN", "DAL", "PHI", "WAS", "NO", "PIT", "NE", "TEN", "DEN", "JAX", "TB", "GB") ~ 1,  
    TRUE ~ 2  
  ))
combined2022 <- combined2022 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 2,
    posteam == "LA" ~ 2,
    posteam == "JAX" ~ 1,
    posteam == "KC" ~ 1
  ))

combined2021 <- combined2021 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("BUF", "DEN", "NE", "NO", "SF", "TEN", "TB", "DAL", "LA", "GB", "IND", "SEA", "CIN", "CLE", "MIA", "ARI") ~ 1,  
    TRUE ~ 2  
  ))
combined2021 <- combined2021 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 1,
    posteam == "LA" ~ 1,
    posteam == "JAX" ~ 2,
    posteam == "KC" ~ 1
  ))

combined2020 <- combined2020 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("DET", "JAX", "LV", "MIN", "DAL", "HOU", "NYJ", "DEN", "TEN", "LAC", "CLE", "CIN", "PHI", "ATL", "CAR", "SF") ~ 2,  
    TRUE ~ 1  
  ))
combined2020 <- combined2020 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 1,
    posteam == "LA" ~ 2,
    posteam == "JAX" ~ 2,
    posteam == "KC" ~ 1
  ))

combined2019 <- combined2019 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("NE", "BUF","BAL", "CHI", "PIT", "MIN", "SF", "DEN", "DAL", "KC", "TEN", "GB", "LAC", "NO", "PHI", "NYJ", "LA") ~ 1,  
    TRUE ~ 2  
  ))
combined2019 <- combined2019 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 1,
    posteam == "LA" ~ 1,
    posteam == "JAX" ~ 2,
    posteam == "KC" ~ 1
  ))

combined2018 <- combined2018 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("CHI", "BAL", "TEN", "JAX", "NE", "DAL", "IND", "PHI", "MIN", "LAC","DEN", "SEA","NO","WAS","PIT","DET") ~ 1,  
    TRUE ~ 2  
  ))
combined2018 <- combined2018 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 1,
    posteam == "LA" ~ 1,
    posteam == "JAX" ~ 2,
    posteam == "KC" ~ 1
  ))

combined2017 <- combined2017 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("LAC","MIN","JAX","PHI","BAL","ATL","NE","CHI","DAL","SEA","PIT","LA","CAR","NO","KC","BUF") ~ 1,  
    TRUE ~ 2  
  ))
combined2017 <- combined2017 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 2,
    posteam == "LA" ~ 1,
    posteam == "JAX" ~ 1,
    posteam == "KC" ~ 1
  ))

combined2016 <- combined2016 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("NE", "SEA","DEN","NYG","MIN","KC","CIN","DAL","BAL","PIT","PHI","HOU","DET","ARI","TB","BUF") ~ 1,  
    TRUE ~ 2 
  ))
combined2016 <- combined2016 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 1,
    posteam == "LA" ~ 2,
    posteam == "JAX" ~ 2,
    posteam == "KC" ~ 1
  ))

combined2015 <- combined2015 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("KC","CIN","SEA","DEN","MIN","CAR","NYJ","NE","PIT","HOU","GB","LAR","ARI","ATL","BUF","DAL") ~ 1,  
    TRUE ~ 2  
  ))
combined2015 <- combined2015 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 2,
    posteam == "LA" ~ 2,
    posteam == "JAX" ~ 1,
    posteam == "KC" ~ 1
  ))

combined2014 <- combined2014 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("SEA","KC","DET","BUF","ARI","HOU","BAL","NE","CLE","SF","MIN","LAC","CIN","GB","DAL","LA") ~ 1,  
    TRUE ~ 2
  ))
combined2014 <- combined2014 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 1,
    posteam == "LA" ~ 2,
    posteam == "JAX" ~ 2,
    posteam == "KC" ~ 1
  ))

combined2013 <- combined2013 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("SEA","CAR","SF","NO","CIN","ARI","KC","MIA","LAC","NE","BAL","LA","PIT","IND","DET","TEN") ~ 1,  
    TRUE ~ 2  
  ))
combined2013 <- combined2013 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 1,
    posteam == "LA" ~ 2,
    posteam == "JAX" ~ 2,
    posteam == "KC" ~ 1
  ))

combined2012 <- combined2012 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("SEA","CHI","SF","DEN","PIT","ATL","MIA","CIN","BAL","HOU","NYG","NE","GB","LA","LAC","MIN") ~ 1,  
    TRUE ~ 2
  ))
combined2012 <- combined2012 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 2,
    posteam == "LA" ~ 2,
    posteam == "JAX" ~ 2,
    posteam == "KC" ~ 2
  ))

combined2011 <- combined2011 %>%
  mutate(def_team_rating = case_when(
    defteam %in% c("PIT","SF","BAL","HOU","CLE","MIA","SEA","TEN","PHI","JAX","NE","CIN","KC","CHI","DAL","ARI") ~ 1,  
    TRUE ~ 2  
  ))
combined2011 <- combined2011 %>%
  mutate(off_team_rating = case_when(
    posteam == "IND" ~ 2,
    posteam == "LA" ~ 2,
    posteam == "JAX" ~ 2,
    posteam == "KC" ~ 2
  ))


#### Combine all seasons into one dataset
Combinedallseasons <- bind_rows(combined2011, combined2012, combined2013, combined2014, combined2015, combined2016,
                                combined2017, combined2018, combined2019, combined2020, combined2021, combined2022,
                                combined2023, combined2024)

#### Adjust variables of interest
# Fixing weather variable so I can have proper temp and wind
# Play_type (Exposure): run/pass (exposed) = 1, other (unexposed) = 0

Combinedallseasons <- Combinedallseasons %>%
  mutate(
    temp_new = as.numeric(str_extract(weather, "(?<=Temp: )\\d+")), 
    wind_new = as.numeric(str_extract(weather, "Wind: [A-Z]*\\s*(\\d+) mph") %>% str_extract("\\d+")), 
    weather_new = str_extract(weather, "Wind: .*") %>% str_extract("\\d+"),  
    wind_new = weather_new,  
    surface_new = case_when(
      surface %in% c("matrixturf", "fieldturf", "a_turf", "astroturf", "sportturf", "astroplay") ~ "turf",  
      TRUE ~ surface 
    ),
    play_type_binary = case_when(
      play_type %in% c("run", "pass") ~ 1,  
      TRUE ~ 0 
    )
  ) %>%
  drop_na(temp_new, wind_new, weather_new, surface_new) %>%  
  filter(surface_new %in% c("turf", "grass"))  

# Spread_line: The closing spread line for the game. A positive number means the home team was favored by that many points, 
# a negative number means the away team was favored by that many points. 
# Spread_line: Changing variable so that positive spread line means favored, and negative spread line means un-favored to win game

# Result: Equals home_score - away_score and means the game outcome from the perspective of the home team.
# Result (Outcome): win = 1, loss = 0

Combinedallseasons <- Combinedallseasons %>%
  mutate(
    spread_line_new = case_when(
      posteam_type == "away" ~ spread_line * (-1), 
      TRUE ~ spread_line  
    )
  )

Combinedallseasons <- Combinedallseasons %>%
  mutate(
    result_new = case_when(
      posteam_type == "away" & result > 0 ~ 0, 
      posteam_type == "away" & result < 0 ~ 1, 
      posteam_type == "home" & result > 0 ~ 1,  
      posteam_type == "home" & result < 0 ~ 0   
    )
  )


# Surface: Grass = 1, Turf = 0
# Field Location: Home = 1, Away = 0
Combinedallseasons <- Combinedallseasons %>%
  mutate(
    surface_new = case_when(
      surface_new == "grass" ~ 1,   
      surface_new == "turf" ~ 0,    
    ),
    posteam_type_new = case_when(
      posteam_type == "away" ~ 0, 
      posteam_type == "home" ~ 1,  
    )
  )

Combinedallseasons <- Combinedallseasons %>%
  rename(
    exposure = play_type_binary,
    outcome = result_new,
    field_location = posteam_type_new
  )


# Roof -> Closed/Dome = 0, Outdoor/Open = 1
Combinedallseasons <- Combinedallseasons %>%
  mutate(
    roof_new = case_when(
      roof %in% c("closed", "dome") ~ 0,  # Assign 0 if roof is "closed" or "dome"
      TRUE ~ 1  # Assign 1 for all other cases
    )
  )

### Seperating Dataset for each team
# For IND dataset
IND <- Combinedallseasons %>% 
  filter(posteam == "IND") %>%
  select(game_id, exposure, outcome, field_location,
         wind_new, temp_new, surface_new, roof_new, def_team_rating, 
         off_team_rating, spread_line_new)
IND <- na.omit(IND)
# removing 2022_01_IND_HOU because game was a tie

# For JAX dataset
JAX <- Combinedallseasons %>% 
  filter(posteam == "JAX") %>%
  select(game_id, exposure, outcome, field_location,
         wind_new, temp_new, surface_new, roof_new, def_team_rating, 
         off_team_rating, spread_line_new)

# For KC dataset
KC <- Combinedallseasons %>% 
  filter(posteam == "KC") %>%
  select(game_id, exposure, outcome, field_location,
         wind_new, temp_new, surface_new, roof_new, def_team_rating, 
         off_team_rating, spread_line_new)

# For LAR dataset
LAR <- Combinedallseasons %>% 
  filter(posteam == "LA") %>%
  select(game_id, exposure, outcome, field_location,
         wind_new, temp_new, surface_new, roof_new, def_team_rating, 
         off_team_rating, spread_line_new)
LAR <- na.omit(LAR)
# removing 2022_01_IND_HOU because game was a tiebecause game was a tie

# Export Datsets
write.csv(IND, "IND_dataset.csv", row.names = FALSE)
write.csv(JAX, "JAX_dataset.csv", row.names = FALSE)
write.csv(KC, "KC_dataset.csv", row.names = FALSE)
write.csv(LAR, "LAR_dataset.csv", row.names = FALSE)

