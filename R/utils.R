# Function to update conference abbreviations to be more friendly 
conf_name_lookup <- function(conf_var) {
  conf_var = dplyr::case_match(
    conf_var,
    "B12" ~ "Big 12",
    "BE" ~ "Big East",
    "P12" ~ "Pac-12",
    "B10" ~ "Big Ten",
    "Amer" ~ "American",
    "SB" ~ "Sun Belt",
    "Slnd" ~ "Southland",
    "BW" ~ "Big West",
    "SC" ~ "Southern",
    "AE" ~ "America East",
    "BSth" ~ "Big South",
    "ASun" ~ "Atlantic Sun",
    "Pat" ~ "Patriot",
    "Horz" ~ "Horizon",
    "BSky" ~ "Big Sky",
    "OVC" ~ "Ohio Valley",
    "Sum" ~ "Summit",
    "A10" ~ "Atlantic 10",
    "MWC" ~ "Mountain West",
    "MVC" ~ "Missouri Valley",
    "NEC" ~ "Northeast",
    "MAC" ~ "Mid-American",
    "MAAC" ~ "Metro Atlantic",
    "ind" ~ "Independent",
    conf_var ~ conf_var
  )
}