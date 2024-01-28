# Login + load utilities 
source(here::here("R/cbbdata_login.R"))

source(here::here("R/utils.R"))

# Fetch the non-conference only game types for this season and add quads
non_con_data <-
  cbbdata::cbd_torvik_game_stats(year = 2024, type = "nc") |> 
  cbbdata::cbd_add_net_quad()

# Find the head to head conference records 
hth_recs <- non_con_data |> 
  dplyr::group_by(conf, opp_conf) |>
  dplyr::summarise(
    games = dplyr::n(),
    wins = sum(result == "W"),
    losses = sum(result == "L"),
    win_pct = wins / (wins + losses),
    q1_wins = sum(result == "W" & quad == "Quadrant 1"),
    q1_losses = sum(result == "L" & quad == "Quadrant 1"),
    q2_wins = sum(result == "W" & quad == "Quadrant 2"),
    q2_losses = sum(result == "L" & quad == "Quadrant 2"),
    q3_wins = sum(result == "W" & quad == "Quadrant 3"),
    q3_losses = sum(result == "L" & quad == "Quadrant 3"),
    q4_wins = sum(result == "W" & quad == "Quadrant 4"),
    q4_losses = sum(result == "L" & quad == "Quadrant 4"),
    ) |> 
  dplyr::mutate(opp_conf = conf_name_lookup(opp_conf)) |>
  dplyr::mutate(conf = conf_name_lookup(conf)) |>
  dplyr::mutate(overall_rec = paste0(wins, "-", losses),
                quad1 = paste0(q1_wins, "-", q1_losses),
                quad2 = paste0(q2_wins, "-", q2_losses),
                quad3 = paste0(q3_wins, "-", q3_losses),
                quad4 = paste0(q4_wins, "-", q4_losses)) |> 
  dplyr::select(conf, opp_conf, games, overall_rec, win_pct, quad1, quad2,
                quad3, quad4) 

