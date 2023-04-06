library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

read_and_prepare <- function(path, term, cities, skip = 10) {
    num_sheets <- length(excel_sheets(path))

    sheet_list <- lapply(2:num_sheets, read_excel, path = path, skip = skip)

    sheet_colnames <- lapply(2:num_sheets, read_excel, path = path, n_max = 1)

    for (i in seq_along(sheet_list)) {
        names(sheet_list[[i]]) <- names(sheet_colnames[[i]])
    }

    search_terms <- paste0(term, ".*", cities)
    df_names <- paste0(term, "_", cities)

    df <- data.frame(date = as.Date(sheet_list[[1]][[1]]))

    for (sheet in sheet_list) {
        for (i in seq_along(search_terms)) {
            indices <- grep(search_terms[i], names(sheet), ignore.case = TRUE)
            if (length(indices) > 0) {
                df[, df_names[i]] <- sheet[, indices[1]]
            }
        }
    }

    long_df <- pivot_longer(
        data = df,
        cols = all_of(df_names),
        names_to = "city",
        values_to = term,
        names_prefix = paste0(term, "_")
    )

    return(long_df)
}

create_index_at_base <- function(data, base_date) {
    data %>%
        filter(date >= base_date) %>%
        arrange(date) %>%
        group_by(city) %>%
        mutate(
            rent_index = 100 * rent / rent[1]
        )
}


df <- read_and_prepare(
    path = "data/cpi_items_capitals.xlsx",
    term = "rent",
    cities = c(
        "canberra", "sydney", "melbourne",
        "brisbane", "hobart", "adelaide",
        "perth", "australia"
    )
)

df_indexed <- create_index_at_base(df, as.Date("2017-12-01")) %>%
    filter(city %in% c(
        "canberra", "sydney",
        "melbourne", "australia"
    ))

ggplot(df_indexed) +
    geom_line(aes(x = date, y = rent_index, colour = city), size = 1.5) +
    labs(
        x = "Quarter",
        y = "Rents Index (Dec 2017 = 100)",
        colour = "City"
    ) +
    theme_bw()

ggsave("plots/cpi_rents.png", width = 16, height = 10, scale = 0.4)
