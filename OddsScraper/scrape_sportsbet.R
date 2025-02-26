# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/rugby-league/nrl"

# Function to fix team names
source("Functions/fix_team_names.R")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {

# Get data from main market page
matches <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".White_fqa53j6")
    
# Function to get team names
get_team_names <- function(match) {
    team_names <-
        match |>
        html_nodes(".participantText_fivg86r") |>
        html_text()
    
    # Home team and Away Team
    home_team <- team_names[1]
    away_team <- team_names[2]
    
    # Output
    tibble(home_team, away_team)
}

# Function to get odds
get_odds <- function(match) {
    odds <-
        match |>
        html_nodes(".priceTextSize_frw9zm9") |>
        html_text() |>
        as.numeric()
    
    # Home team
    home_win <- odds[1]
    away_win <- odds[2]
    
    # Output
    tibble(home_win, away_win)
}

# Function to get start time
get_start_time <- function(match) {
    start_time <-
        match |>
        html_nodes(".oneLine_f15ay66x") |>
        html_text()
    
    # Output
    tibble(start_time)
}

# Map functions to each match and combine together
all_main_market_data <-
bind_cols(
    map(matches, get_team_names) |> bind_rows(),
    map(matches, get_odds) |> bind_rows(),
    map(matches, get_start_time) |> bind_rows()
)

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

sportsbet_h2h <-
all_main_market_data |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           market_name,
           home_team,
           home_win,
           away_team,
           away_win,
           start_time) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Sportsbet")

# Write to csv
write_csv(sportsbet_h2h, "Data/scraped_odds/sportsbet_h2h.csv")

}

##%######################################################%##
#                                                          #
####                    Market Props                    ####
#                                                          #
##%######################################################%##

market_props_function <- function() {

# Function to get team names
get_team_names <- function(match) {
    team_names <-
        match |>
        html_nodes(".participantText_fivg86r") |>
        html_text()

    # Home team and Away Team
    home_team <- team_names[1]
    away_team <- team_names[2]

    # Output
    tibble(home_team, away_team)
}


# Get match links
match_links <-
sportsbet_url |>
    read_html() |>
    html_nodes(".linkMultiMarket_fcmecz0") |>
    html_attr("href")

# Get match IDs from links
match_ids <-
match_links |>
    str_extract("\\d{4,10}$") |>
    as.numeric()

# Get data from main market page
matches <-
    sportsbet_url |>
    read_html() |>
    html_nodes(".White_fqa53j6")

# Get team names that correspond to each match link
team_names <-
    map_dfr(matches, get_team_names) |>
    bind_cols("match_id" = match_ids) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team))

# Match info links
match_info_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/SportCard?displayWinnersPriceMkt=true&includeLiveMarketGroupings=true&includeCollection=true")

# Main Markets
main_market_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/101/Markets")

# Line markets links
line_market_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/313/Markets")

# Total Points links
total_points_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/108/Markets")

# Total Tries links
total_tries_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/110/Markets")

# Player Tries links
player_tries_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/112/Markets")

# Time of First Try links
time_of_first_try_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/205/Markets")

# Get IDs needed for SGM engine-------------------------------------------------
read_prop_url_metadata <- function(url) {

    # Make request and get response
    sb_response <-
        request(url) |>
          req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |> 
          req_headers("Referer" = "https://www.sportsbet.com.au") |>
          req_perform() |> 
          resp_body_json()

    # Empty vectors to append to
    class_external_id = c()
    competition_external_id = c()
    event_external_id = c()

    # Append to vectors
    class_external_id = c(class_external_id, sb_response$classExternalId)
    competition_external_id = c(competition_external_id, sb_response$competitionExternalId)
    event_external_id = c(event_external_id, sb_response$externalId)

    # Output
    tibble(class_external_id,
           competition_external_id,
           event_external_id,
           url) |>
        mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
        rename(match_id = url) |>
        mutate(match_id = as.numeric(match_id))
}

# Safe version that just returns NULL if there is an error
safe_read_prop_metadata <- safely(read_prop_url_metadata, otherwise = NULL)

# Map function to player points urls
market_prop_metadata <-
    map(match_info_links, safe_read_prop_metadata)

# Get just result part from output
market_prop_metadata <-
    market_prop_metadata |>
    map("result") |>
    map_df(bind_rows)

# Function to read a url and get the props-------------------------------

read_prop_url <- function(url) {

    # Make request and get response
    sb_response <-
        request(url) |>
        req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |> 
        req_headers("Referer" = "https://www.sportsbet.com.au") |>
        req_perform() |> 
        resp_body_json()

    # Empty vectors to append to
    prop_market_name = c()
    selection_name_prop = c()
    prop_market_selection = c()
    prop_market_price = c()
    player_id = c()
    market_id = c()
    handicap = c()

    # Loop through each market
    for (market in sb_response) {
        for (selection in market$selections) {

            # Append to vectors
            prop_market_name = c(prop_market_name, market$name)
            selection_name_prop = c(selection_name_prop, selection$name)
            prop_market_selection = c(prop_market_selection, selection$resultType)
            prop_market_price = c(prop_market_price, selection$price$winPrice)
            player_id = c(player_id, selection$externalId)
            market_id = c(market_id, market$externalId)
            if (is.null(selection$unformattedHandicap)) {
                selection$unformattedHandicap = NA
                handicap = c(handicap, selection$unformattedHandicap)
            } else {
                selection$unformattedHandicap = as.numeric(selection$unformattedHandicap)
                handicap = c(handicap, selection$unformattedHandicap)
            }
        }
    }

    # Output
    tibble(prop_market_name,
           selection_name_prop,
           prop_market_selection,
           prop_market_price,
           player_id,
           market_id,
           handicap,
           url)
}

# Safe version that just returns NULL if there is an error
safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)

#===============================================================================
# Line Markets
#===============================================================================

  # Map function to line market urls
line_market_data <-
  map(line_market_links, safe_read_prop_url)

# Get just result part from output
line_market_data <-
  line_market_data |>
  map("result") |>
  map_df(bind_rows)

# Process the line market data
processed_line_data <- 
  line_market_data |>
  filter(str_detect(prop_market_name, "Line|Pick Your Own Line")) |>
  # Extract team name and handicap from selection_name_prop
  mutate(
      team_name = str_remove(selection_name_prop, " \\(.*\\)$"),
      handicap_extracted = as.numeric(str_extract(selection_name_prop, "-?\\d+\\.\\d+")),
      team_name = fix_team_names(team_name)
  ) |>
  # Add match_id for joining
  mutate(match_id = as.numeric(str_extract(url, "[0-9]{6,8}")))

# Join with team names to determine home/away
line_data_with_teams <-
  processed_line_data |>
  left_join(team_names, by = "match_id") |>
  mutate(
      is_home = team_name == home_team,
      is_away = team_name == away_team
  )

# Get all lines for home teams
home_team_lines <-
  line_data_with_teams |>
  filter(is_home) |>
  transmute(
      match_id,
      home_team = team_name,
      home_line = handicap_extracted, 
      home_win = prop_market_price
  )

# Get all lines for away teams
away_team_lines <-
  line_data_with_teams |>
  filter(is_away) |>
  transmute(
      match_id,
      away_team = team_name,
      away_line = handicap_extracted,
      away_win = prop_market_price
  )

# For each match, combine all possible line combinations
sportsbet_line_markets <- list()

# Process each match separately
for (id in unique(home_team_lines$match_id)) {
  # Get team names for this match
  match_teams <- team_names |> filter(match_id == id)
  match_name <- paste(match_teams$home_team, "v", match_teams$away_team)
  
  # Get all home and away lines for this match
  home_lines <- home_team_lines |> filter(match_id == id)
  away_lines <- away_team_lines |> filter(match_id == id)
  
  # For each match, find pairs where home_line + away_line = 0 (balanced lines)
  # This ensures we pair up corresponding handicaps
  paired_lines <- NULL
  for (i in 1:nrow(home_lines)) {
      for (j in 1:nrow(away_lines)) {
          # Find pairs where handicaps offset each other (or are close to it)
          if (abs(home_lines$home_line[i] + away_lines$away_line[j]) < 0.1) {
              paired_lines <- bind_rows(
                  paired_lines,
                  tibble(
                      match = match_name,
                      market_name = "Line",
                      home_team = home_lines$home_team[i],
                      home_line = home_lines$home_line[i],
                      home_win = home_lines$home_win[i],
                      away_team = away_lines$away_team[j],
                      away_line = away_lines$away_line[j],
                      away_win = away_lines$away_win[j]
                  )
              )
          }
      }
  }
  
  # Add to the list of markets
  sportsbet_line_markets[[length(sportsbet_line_markets) + 1]] <- paired_lines
}

# Combine all matches
sportsbet_line_markets <- bind_rows(sportsbet_line_markets) |>
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |>
  mutate(agency = "Sportsbet")

# Write to csv
write_csv(sportsbet_line_markets, "Data/scraped_odds/sportsbet_line.csv")

#===============================================================================
# Total Points Over/Under
#===============================================================================

# Map function to total points urls
total_points_data <-
    map(total_points_links, safe_read_prop_url)

# Get just result part from output
total_points_data <-
    total_points_data |>
    map("result") |>
    map_df(bind_rows)

# Get total points over/under data
total_points_over <-
    total_points_data |>
    filter(str_detect(prop_market_name, "Total Match Points")) |>
    filter(str_detect(selection_name_prop, "Over$")) |>
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Over ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(line = handicap) |>
    rename(over_price = prop_market_price) |>
    select(line, over_price, url)

total_points_under <-
    total_points_data |>
    filter(str_detect(prop_market_name, "Total Match Points")) |>
    filter(str_detect(selection_name_prop, "Under$")) |>
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Under ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(line = handicap) |>
    rename(under_price = prop_market_price) |>
    select(line, under_price, url)

# Combine
total_points_markets <-
    total_points_over |>
    left_join(total_points_under) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Total Points") |> 
    transmute(match, market_name, home_team, away_team, line, over_price, under_price) |> 
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |> 
    mutate(agency = "Sportsbet")

# Write to csv
write_csv(total_points_markets, "Data/scraped_odds/sportsbet_total_points.csv")

#===============================================================================
# Total Tries Over/Under
#===============================================================================

# Map function to total tries urls
total_tries_data <-
    map(total_tries_links, safe_read_prop_url)

# Get just result part from output
total_tries_data <-
    total_tries_data |>
    map("result") |>
    map_df(bind_rows)

# Get total tries over/under data
total_tries_over <-
    total_tries_data |>
    filter(str_detect(prop_market_name, "Total Match Tries")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Over ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(line = handicap) |>
    rename(over_price = prop_market_price) |>
    select(line, over_price, url)

total_tries_under <-
    total_tries_data |>
    filter(str_detect(prop_market_name, "Total Match Tries")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Under ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(line = handicap) |>
    rename(under_price = prop_market_price) |>
    select(line, under_price, url)

# Combine
total_tries_markets <-
    total_tries_over |>
    left_join(total_tries_under) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Total Tries") |> 
    transmute(match, market_name, home_team, away_team, line, over_price, under_price) |> 
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |> 
    mutate(agency = "Sportsbet")

# Write to csv
write_csv(total_tries_markets, "Data/scraped_odds/sportsbet_total_tries.csv")

#===============================================================================
# Player Tries
#===============================================================================

# Map function to player tries urls
player_tries_data <-
    map(player_tries_links, safe_read_prop_url)

# Get just result part from output
player_tries_data <-
    player_tries_data |>
    map("result") |>
    map_df(bind_rows)

# Add match data
player_tries_data <-
    player_tries_data |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team)) |>
    left_join(market_prop_metadata)

# Get player tries alternate lines
player_tries_alternate <-
    player_tries_data |>
    filter(str_detect(prop_market_name, "To Score")) |>
    filter(!str_detect(prop_market_name, "First")) |>
    mutate(line = str_extract(prop_market_name, "\\d+")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |>
    # Clean player names if needed
    rename(over_price = prop_market_price) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Tries",
        player_name,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id
    )

# Get player tries over/under
player_tries_over <-
    player_tries_data |>
    filter(str_detect(selection_name_prop, "Over")) |>
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Over ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(player_name = name_2) |>
    rename(line = handicap) |>
    rename(over_price = prop_market_price) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Tries",
        player_name,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id
    )

player_tries_under <-
    player_tries_data |>
    filter(str_detect(selection_name_prop, "Under")) |>
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Under ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(player_name = name_2) |>
    rename(line = handicap) |>
    rename(under_price = prop_market_price) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Tries",
        player_name,
        line,
        under_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id_unders = player_id
    )

# Combine over/under
player_tries_over_under <-
    player_tries_over |>
    left_join(player_tries_under)

# Combine all player tries markets
player_tries_all <-
    player_tries_alternate |>
    bind_rows(player_tries_over_under) |>
    distinct()

# Write to csv
write_csv(player_tries_all, "Data/scraped_odds/sportsbet_player_tries.csv")

#===============================================================================
# First Try Scorer
#===============================================================================

# Map function to first try scorer urls
first_try_scorer_data <-
    map(first_try_scorer_links, safe_read_prop_url)

# Get just result part from output
first_try_scorer_data <-
    first_try_scorer_data |>
    map("result") |>
    map_df(bind_rows)

# Process first try scorer data
first_try_scorer <-
    first_try_scorer_data |>
    filter(str_detect(prop_market_name, "First Try Scorer")) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "First Try Scorer",
        player_name = selection_name_prop,
        over_price = prop_market_price,
        agency = "Sportsbet",
        player_id,
        market_id
    )

# Write to csv
write_csv(first_try_scorer, "Data/scraped_odds/sportsbet_first_try_scorer.csv")

#===============================================================================
# Time of First Try
#===============================================================================

# Map function to time of first try urls
time_of_first_try_data <-
    map(time_of_first_try_links, safe_read_prop_url)

# Get just result part from output
time_of_first_try_data <-
    time_of_first_try_data |>
    map("result") |>
    map_df(bind_rows)

# Process time of first try data
time_of_first_try <-
    time_of_first_try_data |>
    filter(str_detect(prop_market_name, "Time of First Try")) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team)) |>
    # Create a pivot format for over/under prices
    group_by(match, match_id, prop_market_name) |>
    summarize(
        home_team = first(home_team),
        away_team = first(away_team),
        # Extract the minute threshold from selection names
        line = case_when(
            any(str_detect(selection_name_prop, "0-")) ~ str_extract(selection_name_prop[str_detect(selection_name_prop, "0-")], "0-\\d+")[1],
            TRUE ~ NA_character_
        ),
        over_price = prop_market_price[str_detect(selection_name_prop, "0-")][1],
        under_price = prop_market_price[str_detect(selection_name_prop, "No Try")][1],
        .groups = "drop"
    ) |>
    # Clean up line values
    mutate(
        line = str_replace(line, "0-", ""),
        line = paste0(line, ":00")
    ) |>
    # Add margin and agency
    transmute(
        match,
        market_name = "Time of First Try",
        home_team,
        away_team,
        line,
        over_price,
        under_price,
        margin = round((1/over_price + 1/under_price), digits = 3),
        agency = "Sportsbet"
    ) |>
    filter(!is.na(line))

# Write to csv
write_csv(time_of_first_try, "Data/scraped_odds/sportsbet_time_of_first_try.csv")

}

##%######################################################%##
#                                                          #
####                Run functions safely                ####
#                                                          # ▋