library(httr)
library(jsonlite)
library(tidyr)
library(xml2)
library(dplyr)
library(gt)
library(png)
library(webshot2)
library(lubridate)

api <- "d72d888a7e2831439aa64a8ac1525f71"
base <- "https://api.the-odds-api.com"
sport <- "baseball_mlb"
markets <- "h2h,totals" #,btts,draw_no_bet"
endpoint <- paste0("/v4/sports/", sport, "/odds/?apiKey=", api, "&regions=us&markets=", markets, "&bookmakers=draftkings,fanduel&oddsFormat=american")

url <- paste0(base, endpoint)


# Make the GET request
response <- GET(url)

# Check the response status
content <- fromJSON(content(response, "text")) %>%
  unnest(., cols = c(bookmakers)) %>%
  unnest(., cols = c(markets), names_sep = "_") %>%
  unnest(., cols = c(markets_outcomes), names_sep = "_")

american_to_probability <- function(american_odds) {
  ifelse(is.na(american_odds), NA,
         ifelse(american_odds > 0, 100 / (american_odds + 100),
                -american_odds / (-american_odds + 100)))
}

prices_totals <- content %>% filter(markets_key == "totals") %>%
  select(id, home_team, away_team, commence_time, title, markets_outcomes_name, markets_outcomes_point, markets_outcomes_price) %>%
  pivot_wider(names_from = title, values_from = markets_outcomes_price) %>%
  mutate(dk_prob = american_to_probability(DraftKings),
         fd_prob = american_to_probability(FanDuel),
         bet = paste0(markets_outcomes_name, " ", markets_outcomes_point),
         start_time = format(with_tz(ymd_hms(commence_time, tz = "UTC"), tzone = "America/New_York"), "%b %d, %I:%M")) %>%
  select(-c(markets_outcomes_name, markets_outcomes_point, commence_time, id))

prices_h2h <- content %>% filter(markets_key == "h2h") %>%
  select(id, home_team, away_team, commence_time, title, markets_outcomes_name, markets_outcomes_price) %>%
  pivot_wider(names_from = title, values_from = markets_outcomes_price) %>%
  rename(dk_price = DraftKings, fd_price = FanDuel) %>%
  mutate(dk_prob = american_to_probability(dk_price),
         fd_prob = american_to_probability(fd_price),
         bet = markets_outcomes_name,
         start_time = str_remove(format(with_tz(ymd_hms(commence_time, tz = "UTC"), tzone = "America/New_York"), "%I:%M"), "^0+")) %>%
  select(-c(markets_outcomes_name, commence_time, id))


#ballpark pal
bp_games_url <- "https://ballparkpal.com/index.php"

# Read in the HTML from the webpage
bp_games_webpage <- read_html(bp_games_url)

# game list

game_list <- list()


get_game_hrefs <- function(bp_games_webpage) {
  game_hrefs <- list()

  for (i in 1:30) {
    tryCatch({
      # This code may generate an error
      value <- xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(bp_games_webpage, 2), 5), i), 3), 2), 1))[["href"]]
      game_hrefs[[i]] <- value
    }, error = function(e) {
      # If an error occurs, store NA
      game_hrefs[[i]] <- NA
    })
  }

  return(game_hrefs)
}

game_hrefs <- get_game_hrefs(bp_games_webpage)

filtered_game_hrefs <- as.vector(unlist(game_hrefs))

bp_game_df <- data.frame(
  home_team = character(),
  away_team = character(),
  home_total = numeric(),
  away_total = numeric(),
  game_total = numeric(),
  home_prob = numeric(),
  away_prob = numeric(),
  yrfi_prob = numeric(),
  nrfi_prob = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(filtered_game_hrefs)){
  url <- filtered_game_hrefs[i]
  #home_team
  home_team_xpath <- "/html/body/div/div[2]/div/div[2]/p/text()"
  home_team <- xml_text(xml_find_all(read_html(url), home_team_xpath))

  #away_team
  away_team_xpath <- "/html/body/div/div[2]/div/div[1]/p/text()"
  away_team <- xml_text(xml_find_all(read_html(url), away_team_xpath))

  #home_score
  home_total_xpath <- "/html/body/div/div[3]/div/div[3]/p"
  home_total <- as.numeric(xml_text(xml_find_all(read_html(url), home_total_xpath)))

  #away_score
  away_total_xpath <- "/html/body/div/div[3]/div/div[1]/p"
  away_total <- as.numeric(xml_text(xml_find_all(read_html(url), away_total_xpath)))

  #home_prob
  home_prob_xpath <- "/html/body/div/div[4]/div/div[3]/p"
  home_prob <- as.numeric(gsub("([0-9.]+)%.*", "\\1", xml_text(xml_find_all(read_html(url), home_prob_xpath))))/100

  #away_prob
  away_prob_xpath <- "/html/body/div/div[4]/div/div[1]/p"
  away_prob <- as.numeric(gsub(".*\\s([0-9.]+)%.*", "\\1", xml_text(xml_find_all(read_html(url), away_prob_xpath))))/100

  #runs first inning
  yrfi_xpath <- "/html/body/div/div[11]/div[3]/p/text()[1]"
  yrfi_prob <- as.numeric(gsub("([0-9.]+)%.*", "\\1", xml_text(xml_find_all(read_html(url), yrfi_xpath))))/100
  nrfi_xpath <- "/html/body/div/div[11]/div[3]/p/text()[2]"
  nrfi_prob <- as.numeric(gsub("([0-9.]+)%.*", "\\1", xml_text(xml_find_all(read_html(url), nrfi_xpath))))/100

  new_row <- data.frame(
    home_team = home_team,
    away_team = away_team,
    home_total = home_total,
    away_total = away_total,
    game_total = home_total + away_total,
    home_prob = home_prob,
    away_prob = away_prob,
    yrfi_prob = yrfi_prob,
    nrfi_prob = nrfi_prob
  )

  bp_game_df <- rbind(bp_game_df, new_row)

  #return(bp_game_df)

}

team_names_with_city <- c(
  "Arizona Diamondbacks",
  "Atlanta Braves",
  "Baltimore Orioles",
  "Boston Red Sox",
  "Chicago White Sox",
  "Chicago Cubs",
  "Cincinnati Reds",
  "Cleveland Guardians",
  "Colorado Rockies",
  "Detroit Tigers",
  "Houston Astros",
  "Kansas City Royals",
  "Los Angeles Angels",
  "Los Angeles Dodgers",
  "Miami Marlins",
  "Milwaukee Brewers",
  "Minnesota Twins",
  "New York Yankees",
  "New York Mets",
  "Oakland Athletics",
  "Philadelphia Phillies",
  "Pittsburgh Pirates",
  "San Diego Padres",
  "San Francisco Giants",
  "Seattle Mariners",
  "St. Louis Cardinals",
  "Tampa Bay Rays",
  "Texas Rangers",
  "Toronto Blue Jays",
  "Washington Nationals"
)

team_names_without_city <- c(
  "Diamondbacks",
  "Braves",
  "Orioles",
  "Red Sox",
  "White Sox",
  "Cubs",
  "Reds",
  "Guardians",
  "Rockies",
  "Tigers",
  "Astros",
  "Royals",
  "Angels",
  "Dodgers",
  "Marlins",
  "Brewers",
  "Twins",
  "Yankees",
  "Mets",
  "Athletics",
  "Phillies",
  "Pirates",
  "Padres",
  "Giants",
  "Mariners",
  "Cardinals",
  "Rays",
  "Rangers",
  "Blue Jays",
  "Nationals"
)

teams <- data.frame(team_with_city = team_names_with_city, team_wo_city = team_names_without_city)


bp_game_df_adjusted <- bp_game_df %>%
  left_join(., teams, by = c("home_team" = "team_wo_city")) %>%
  mutate(home_team = team_with_city) %>%
  left_join(., teams, by = c("away_team" = "team_wo_city")) %>%
  mutate(away_team = team_with_city.y) %>%
  select(c(1, 2, 3, 4, 5, 6, 7))

final_df <- bp_game_df_adjusted %>%
  left_join(., prices_h2h, by = join_by(home_team, away_team)) %>%
  mutate(home_value_dk = ifelse(home_team == bet, home_prob - dk_prob, NA),
         home_value_fd = ifelse(home_team == bet, home_prob - fd_prob, NA),
         away_value_dk = ifelse(away_team == bet, away_prob - dk_prob, NA),
         away_value_fd = ifelse(away_team == bet, away_prob - fd_prob, NA),
         home_value_flag_dk = ifelse(home_value_dk > .05, 1, 0),
         home_value_flag_fd = ifelse(home_value_fd > .05, 1, 0),
         away_value_flag_dk = ifelse(away_value_dk > .05, 1, 0),
         away_value_flag_fd = ifelse(away_value_fd > .05, 1, 0),
         any_value_flag = ifelse(home_value_flag_dk == 1 | away_value_flag_dk == 1 | home_value_flag_fd == 1 | away_value_flag_fd == 1, 1, 0)) %>%
  filter(any_value_flag == 1) %>%
  left_join(., teams, by = c("home_team" = "team_with_city")) %>%
  mutate(home_team = team_wo_city) %>%
  left_join(., teams, by = c("away_team" = "team_with_city")) %>%
  mutate(away_team = team_wo_city.y) %>%
  select(-c(team_wo_city.x, team_wo_city.y)) %>%
  left_join(., teams, by = c("bet" = "team_with_city")) %>%
  mutate(bet = team_wo_city) %>%
  select(-c(team_wo_city)) %>%
  mutate(game = paste0(away_team, " @ ", home_team),
         dk_value = ifelse(bet == home_team, home_value_dk, away_value_dk),
         fd_value = ifelse(bet == home_team, home_value_fd, away_value_fd),
         #test = ifelse(home_team == bet, pmax(home_value_dk, home_value_fd, na.rm = FALSE), 0))
         bet_prob = ifelse(home_team == bet, home_prob, away_prob)) %>%
  select(c(game, start_time, bet, bet_prob, dk_price, dk_value, fd_price, fd_value)) %>%
  gt() %>%
  tab_spanner(label = "Pick", columns = c(bet, bet_prob)) %>%
  tab_spanner(label = "DraftKings", columns = c(dk_price, dk_value)) %>%
  tab_spanner(label = "FanDuel", columns = c(fd_price, fd_value)) %>%
  cols_label(game ~ "Game",
             bet ~ "Side",
             bet_prob ~ "Win %",
             dk_price ~ "Price",
             fd_price ~ "Price",
             dk_value ~ "Value",
             fd_value ~ "Value",
             start_time ~ "Time") %>%
  fmt_number(columns = c(dk_price, fd_price),
             force_sign = TRUE,
             decimals = 0) %>%
  fmt_percent(columns = c(bet_prob, dk_value, fd_value),
              decimals = 0) %>%
  data_color(columns = bet_prob,
             fn = scales::col_numeric(palette = c("red", "white", "green"), domain = c(0, 1)),
             apply_to = "fill") %>%
  cols_align(columns = c(game, start_time), align = "left") %>%
  cols_align(columns = c(bet, dk_price, dk_value, fd_price, fd_value), align = "center") %>%
  tab_style(style = cell_borders(sides = "right"),
            locations = cells_body(columns = 2)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_spanners()) %>%
  tab_header(
    title = md("**Ballpark Pal Moneyline Values**")) %>%
  tab_source_note(source_note = "Data obtained from ballparkpal.com")

gtsave(final_df, expand = 100,
       filename = "ML_plays.png",
       #path = file.path(paste0("H:/My Drive/R_playground/mlbvalues/")),
       vheight = 100, vwidth =1000)

