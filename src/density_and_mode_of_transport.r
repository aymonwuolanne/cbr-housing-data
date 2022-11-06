library(readr)
library(tidyr)
library(forcats)
library(dplyr)
library(ggplot2)

sa2mtw <- read_csv("data/SA2_MTW06P_2021.csv",
    col_names = c("sa2", "mtw", "n"),
    col_types = "ffn",
    skip = 11L
)

sa2strd <- read_csv("data/SA2_STRD_2021.csv",
    col_names = c("sa2", "strd", "n"),
    col_types = "ffn",
    skip = 11L
)

sa2_totals <- read_csv("data/SA2_dwellings_2021.csv",
    col_names = c("sa2", "n"),
    col_types = "fn",
    skip = 11L
)

sa2mtw <- sa2mtw %>%
    select(sa2, mtw, n) %>%
    filter(!is.na(n)) %>%
    fill(sa2)

sa2strd <- sa2strd %>%
    select(sa2, strd, n) %>%
    filter(!is.na(n)) %>%
    fill(sa2) %>%
    mutate(strd = fct_collapse(strd,
        detached = "Separate house",
        semi_detached = grep("Semi-detached", levels(strd), value = TRUE),
        unit = grep("Flat or apartment", levels(strd), value = TRUE),
        other_level = "other"
    )) %>%
    group_by(sa2, strd) %>%
    summarise(n = sum(n))

sa2_totals <- sa2_totals %>%
    select(sa2, n) %>%
    filter(!is.na(n)) %>%
    fill(sa2)

# density index
sa2_density <- sa2strd %>%
    filter(strd == "unit") %>%
    left_join(sa2_totals, by = "sa2", suffix = c("_unit", "_sa2")) %>%
    mutate(density_index = n_unit / n_sa2)

# compare density with active travel
sa2mtw %>%
    left_join(sa2_density) %>%
    filter(
        mtw %in% c(
            "Public Transport",
            "Vehicle", "Active Transport", "Other Mode"
        ),
        n_sa2 >= 50
    ) %>%
    group_by(sa2) %>%
    mutate(prop = n / sum(n)) %>%
    filter(mtw != "Other Mode") %>%
    ggplot(aes(x = density_index, y = prop, colour = mtw)) +
    geom_point() +
    geom_smooth(method = "lm")

ggsave("plots/density_by_mtw.png", scale = 0.6)
