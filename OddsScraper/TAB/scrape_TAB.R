# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(httr)
library(jsonlite)

# Function to fix team names
source("Scripts/fix_team_names.R")

main_tab <- function() {

# Get response body
tab_response <- fromJSON("OddsScraper/TAB/tab_response.json")
  
# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
    
    # Market info
    markets_name = markets$betOption
    market_propositions = markets$propositions
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions)
}

# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$name
    match_start_time = matches$startTime
    
    # Market info
    market_info = map(matches$markets, get_market_info) |> bind_rows()
    
    # Output Tibble
    tibble(
        match = match_name,
        start_time = match_start_time,
        market_name = market_info$market,
        propositions = market_info$propositions
    )
}

# List of matches
matches <- map(1:nrow(tab_response$matches), ~ tab_response$matches[., ])

# Map functions to data
all_tab_markets <-
  map(matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
  all_tab_markets |>
  unnest(cols = c(propositions)) |> 
  select(any_of(c("match",
                  "round",
                  "start_time",
                  "market_name")),
         prop_id = id,
         prop_name = name,
         price = returnWin)

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = price) |> 
    select(-prop_name, -prop_id)

# Away teams
away_teams <-
  all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = price) |> 
  select(-prop_name, -prop_id)

# Combine
tab_head_to_head_markets <-
    home_teams |>
    left_join(away_teams) |> 
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TAB")

# Fix team names
tab_head_to_head_markets <-
    tab_head_to_head_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(tab_head_to_head_markets, "Data/scraped_odds/tab_h2h.csv")

#===============================================================================
# Line markets
#===============================================================================

# Home teams
home_team_lines <-
  all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Line" | market_name == "Pick Your Own Line") |> 
  rename(home_win = price) |>
  mutate(team = str_remove(prop_name, " [\\+\\-].*$")) |>
  filter(team == home_team) |>
  mutate(home_line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |>
  select(-prop_name, -prop_id, -team) |> 
  mutate(line = abs(home_line)) |> 
  distinct(match, home_line, home_win, .keep_all = TRUE)

# Away teams
away_team_lines <-
  all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Line" | market_name == "Pick Your Own Line") |> 
  rename(away_win = price) |>
  mutate(team = str_remove(prop_name, " [\\+\\-].*$")) |>
  filter(team == away_team) |>
  mutate(away_line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |>
  select(-prop_name, -prop_id, -team) |> 
  mutate(line = abs(away_line)) |> 
  distinct(match, away_line, away_win, .keep_all = TRUE)

# Combine
tab_line_markets <-
  home_team_lines |>
  left_join(away_team_lines, relationship = "many-to-many") |> 
  mutate(market_name = "Line") |> 
  filter(home_line != away_line) |> 
  select(match, start_time, market_name, home_team, home_line, home_win, away_team, away_line, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "TAB")

# Fix team names
tab_line_markets <-
  tab_line_markets |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(tab_line_markets, "Data/scraped_odds/tab_line.csv")

#===============================================================================
# Totals
#===============================================================================

# Total Overs
totals_overs <-
all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Total Points Over/Under" |
           market_name == "Pick Your Own Total") |> 
  filter(str_detect(prop_name, "Over")) |>
  mutate(line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |> 
  mutate(market_name = "Total Points") |>
  select(match, start_time, market_name, home_team, away_team, line, over_price = price) |> 
  distinct(match, line, over_price, .keep_all = TRUE)

# Total Unders
totals_unders <-
all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Total Points Over/Under" |
           market_name == "Pick Your Own Total") |> 
  filter(str_detect(prop_name, "Under")) |>
  mutate(line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |> 
  mutate(market_name = "Total Points") |>
  select(match, start_time, market_name, home_team, away_team, line, under_price = price) |> 
  distinct(match, line, under_price, .keep_all = TRUE)

# Combine
tab_totals_markets <-
  totals_overs |>
  left_join(totals_unders) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(match, start_time, market_name, home_team, away_team, line, over_price, under_price) |> 
  mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |> 
  mutate(agency = "TAB") |> 
  arrange(start_time, match, line)

# Write to csv
write_csv(tab_totals_markets, "Data/scraped_odds/tab_totals.csv")
 
#===============================================================================
# Total Match Tries
#===============================================================================

# Total Tries Overs
tries_overs <-
all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Total Tries Over/Under" |
           market_name == "Pick Your Own Tries") |> 
  filter(str_detect(prop_name, "Over")) |>
  mutate(line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |> 
  mutate(market_name = "Total Tries") |>
  select(match, start_time, market_name, home_team, away_team, line, over_price = price) |> 
  distinct(match, line, over_price, .keep_all = TRUE)

# Total Tries Unders
tries_unders <-
all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Total Tries Over/Under" |
           market_name == "Pick Your Own Tries") |> 
  filter(str_detect(prop_name, "Under")) |>
  mutate(line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |> 
  mutate(market_name = "Total Tries") |>
  select(match, start_time, market_name, home_team, away_team, line, under_price = price) |> 
  distinct(match, line, under_price, .keep_all = TRUE)

# Combine
tab_tries_markets <-
  tries_overs |>
  left_join(tries_unders) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(match, start_time, market_name, home_team, away_team, line, over_price, under_price) |> 
  mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |> 
  mutate(agency = "TAB") |> 
  arrange(start_time, match, line)

# Write to csv
write_csv(tab_tries_markets, "Data/scraped_odds/tab_total_tries.csv")

#===============================================================================
# Player Tries
#===============================================================================

# Filter to player tries markets
player_try_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Try$|Tries$"))

# Alternate Player Tries
alternate_player_try_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "To Score a Try$|2\\+ Tries$")) |>
    mutate(market_name = str_replace(market_name, "a Try", "1+ Tries")) |> 
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Tries", player_name, line, over_price = price, prop_id)


# Fix team names for tries
player_try_markets <-
  alternate_player_try_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(player_name = case_when(
        player_name == "Nasiah W-Milera" ~ "Nasiah Wanganeen-Milera",
        .default = player_name
    )) |> 
  arrange(over_price)

# Write to csv
write_csv(player_try_markets, "Data/scraped_odds/tab_player_tries.csv")
  
#===============================================================================
# First Try Scorer
#===============================================================================

# Filter to first try scorer markets
first_try_scorer_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "1st Try Scorer$"))

# Extract player names and clean data
first_try_scorer_markets <-
  first_try_scorer_markets |> 
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(market_name = "First Try Scorer") |>
  select(match, market_name, player_name, price, prop_id) |> 
  rename(over_price = price)

# Fix team names for first try scorer
first_try_scorer <-
  first_try_scorer_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = case_when(
      player_name == "Nasiah W-Milera" ~ "Nasiah Wanganeen-Milera",
      .default = player_name
  )) |> 
  arrange(over_price)

# Write to csv
write_csv(first_try_scorer, "Data/scraped_odds/tab_first_try_scorer.csv")
  
#===============================================================================
# Time of First Try
#===============================================================================
  
# Time of First Try markets
time_of_first_try_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "Time of First Try"))

# Extract time and clean data
time_of_first_try_markets <-
  time_of_first_try_markets |> 
  mutate(market_name = "Time of First Try") |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  # Create a pivot format for over/under prices
  group_by(match, start_time, market_name) |>
  summarize(
    home_team = first(home_team),
    away_team = first(away_team),
    # Extract the time threshold from the "or later" prop_name
    line = str_extract(prop_name[str_detect(prop_name, "or later")], "\\d+:\\d+"),
    over_price = price[str_detect(prop_name, "or Earlier")],
    under_price = price[str_detect(prop_name, "or later")],
    .groups = "drop"
  ) |>
  # Add margin and agency
  mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |> 
  mutate(agency = "TAB") |>
  # Final column selection and arrangement
  select(match, start_time, market_name, home_team, away_team, line, over_price, under_price, margin, agency)

# Write to csv
write_csv(time_of_first_try_markets, "Data/scraped_odds/tab_time_of_first_try.csv")

}

#===============================================================================
# Run safe function
#===============================================================================

safe_main_tab <- safely(main_tab)
safe_main_tab()
