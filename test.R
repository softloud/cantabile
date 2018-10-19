library(tidyverse)
seq_by <- 10

# generate data for given substring
get_obs_df <- function(obs) {
  tibble(p = seq(0, 1, by = 1 / seq_by)) %>%
    mutate(
      n = stringr::str_length(obs),
      obs = obs,
      water_count = stringr::str_count(obs, "W"),
      plausibility = dbinom(x = water_count, size = n, prob = p)
    )
}

# generate for all substrings
get_plot_df <- function(all_obs) {
  df <- seq(1, stringr::str_length(all_obs)) %>%
    map(
      .f = function(x) {
        stringr::str_sub(all_obs, start = 1, end = x)
      }
    ) %>%
    map_df(get_obs_df) %>%
    mutate(fn = "posterior")
  
  all <- df %>%
    filter(obs != all_obs) %>%
    mutate(obs = df$obs[-c(1:(seq_by + 1))]) %>%
    mutate(fn = "prior")
  
  df[1:(seq_by + 1),] %>%
    mutate(plausibility = 0.5,
           fn = "prior") %>%
    rbind(df) %>%
    rbind(all) %>%
    arrange(obs)
}


# plot

grid_plot <- function(obs_all) {
  obs_all %>%
    get_plot_df() %>%
    group_by(fn) %>%
    ggplot(aes(x = p, y = plausibility)) +
    geom_line(aes(linetype = fn)) +
    facet_wrap( ~ obs)
} 

grid_plot("WWW")
grid_plot("WWWL")
grid_plot("LWWLWWW")