# calculating the proportion of land devoted to different
# land use zones in the ACT

library(dplyr)

blocks <- read.csv("data/ACTGOV_TP_LAND_USE_ZONE.csv")

relevant_zones <- c(
    "RZ1", "RZ2", "RZ3", "RZ4", "RZ5",
    "CF", "CZ1", "CZ2", "CZ3", "CZ4",
    "CZ5", "CZ6"
)

blocks %>%
    filter(
        LAND_USE_ZONE_CODE_ID %in% relevant_zones
    ) %>%
    group_by(LAND_USE_ZONE_CODE_ID) %>%
    summarise(area = sum(Shape__Area)) %>%
    ungroup() %>%
    mutate(area = area / sum(area))
