fix_team_names <- function(team_name_vector) {
  map_chr(team_name_vector, ~ case_when(
    str_detect(., "(^Brisbane)|(Broncos)") ~ "Brisbane Broncos",
    str_detect(., "(^Canberra)|(Raiders)") ~ "Canberra Raiders",
    str_detect(., "(^Canterbury)|(Bulldogs)") ~ "Canterbury-Bankstown Bulldogs",
    str_detect(., "(^Cronulla)|(Sharks)") ~ "Cronulla Sharks",
    str_detect(., "(^Dolphins)") ~ "Dolphins",
    str_detect(., "(^Gold Coast)|(Titans)") ~ "Gold Coast Titans",
    str_detect(., "(^Manly)|(Sea Eagles)") ~ "Manly Warringah Sea Eagles",
    str_detect(., "(^Melbourne)|(Storm)") ~ "Melbourne Storm",
    str_detect(., "(^Newcastle)|(Knights)") ~ "Newcastle Knights",
    str_detect(., "(^New Zealand Warriors)|(Warriors)") ~ "New Zealand Warriors",
    str_detect(., "(^North Queensland)|(Cowboys)") ~ "North Queensland Cowboys",
    str_detect(., "(^Parramatta)|(Eels)") ~ "Parramatta Eels",
    str_detect(., "(^Penrith)|(Panthers)") ~ "Penrith Panthers",
    str_detect(., "(^South Sydney)|(Rabbitohs)") ~ "South Sydney Rabbitohs",
    str_detect(., "(^St George Illawarra)|(Dragons)") ~ "St George Illawarra Dragons",
    str_detect(., "(^Sydney Roosters)|(Roosters)") ~ "Sydney Roosters",
    str_detect(., "(^Wests Tigers)|(Tigers)") ~ "Wests Tigers",
    TRUE ~ . # Default case to return the original team name
  ))
}
