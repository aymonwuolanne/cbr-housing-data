library(readr)
library(tidyr)
library(forcats)
library(dplyr)
library(ggplot2)

raw_data <- read_csv(
    file = "data/SA3_STRD_RNTRD_2021.csv",
    col_names = c("sa3", "strd", "rntrd", "n"),
    col_types = list(
        sa3 = col_factor(),
        strd = col_factor(),
        rntrd = col_factor(ordered = TRUE),
        n = col_number()
    ),
    skip = 11
)

tidy_data <- raw_data %>%
    select(sa3, strd, rntrd, n) %>%
    filter(!is.na(rntrd)) %>%
    fill(sa3, strd, rntrd) %>%
    droplevels() %>%
    mutate(
        strd = fct_collapse(strd,
            detached = "Separate house",
            semi_detached = grep("Semi-detached",
                levels(strd),
                value = TRUE
            ),
            unit = grep("Flat", levels(strd), value = TRUE),
            other_level = "other"
        ),
        rntrd = as.ordered(rntrd)
    )

tidy_data %>%
    filter(
        !rntrd %in% c("Not applicable", "Not stated"),
        strd != "other"
    ) %>%
    group_by(sa3, strd) %>%
    summarise(
        median_rent = levels(rntrd)[median(rep(as.numeric(rntrd), n))]
    ) %>%
    drop_na() %>%
    ggplot() +
    geom_col(
        aes(x = sa3, y = median_rent, fill = strd),
        position = "dodge"
        )

ggsave(filename = "plots/sa3_median_rents.png")
