# Login + load utilities 
source(here::here("R/cbbdata_login.R"))

source(here::here("R/utils.R"))

# Fetch the non-conference only game types for this season 
# Not using this code right now because it's quite slow to load
# Exploring ways to speed it up 
non_con_data <-
  cbbdata::cbd_torvik_game_stats(year = 2024, type = "nc")

# Find the head to head conference records 
hth_recs <- non_con_data |> 
  cbbdata::cbd_add_net_quad() |>
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

# Add a function to fetch the h2h records by conference 
hth_conf_records <- function(conf) {
  hth_recs |>
    dplyr::filter(conf == {
      {
        conf
      }
    }) |>
    gt::gt() |>
    gt::cols_hide(columns = conf) |> 
    gt::cols_label(
      opp_conf = "Opponent Conference",
      games = "Games",
      overall_rec = "W-L",
      win_pct = "Win %",
      quad1 = "Q1",
      quad2 = "Q2",
      quad3 = "Q3", 
      quad4 = "Q4"
    ) |>
    gt::tab_spanner(label = "Quadrant Record",
                    columns = c(quad1, quad2, quad3, quad4)) |>
    gt::fmt_percent(columns = c(win_pct), decimals = 1) |> 
    gtExtras::gt_hulk_col_numeric(columns = c(win_pct)) |> 
    gtExtras::gt_theme_dot_matrix() |>
    gt::tab_header(title = "2023-24 Head-to-Head Conference Records",
                   subtitle = "Win/loss and quadrant records against each conference") |>
    gt::tab_source_note(source_note = "Bless your chart | data: cbbdata") |>
    gt::tab_options (
      source_notes.font.size = gt::px(10),
      row.striping.background_color = '#EEEEEE',
      table.font.size = gt::px(12),
      column_labels.text_transform = 'capitalize'
    ) |>
    gt::tab_style(style = list(
      gt::cell_borders(
        sides = c("left"),
        color = "#c1c1c1",
        weight = gt::px(2)
      )
    ),
    locations = list(gt::cells_body(
      columns = c(
        games, 
        overall_rec,
        win_pct,
        quad1,
        quad2,
        quad3,
        quad4
      )
    ))) -> hth_table
  
  hth_table
}

cached_hth_records <- memoise::memoise(hth_conf_records)


# This is the code to add to the shiny app if needed 

### Head-to-Head

# ```{r hth-records}
# fluidPage(
#  fluidRow(
#    column(
#      4,
#      selectInput(
#        'conf',
#        'Select a conference',
#        choices = c(as.character(unique(hth_recs$conf))),
#        selected = "ACC"
#      )
#    )
#  ),
#  fluidRow(
#    column(
#      12,
#      gt::gt_output("hthtable")
#    )
#  )
# )
# ```

# output$hthtable <- gt::render_gt({
#  cached_hth_records(conf = input$conf)
# })