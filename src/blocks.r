# calculating the proportion of land devoted to different
# land use zones in the ACT

library(dplyr)

blocks <- read.csv("data/ACTGOV_TP_LAND_USE_ZONE.csv")
names(blocks) <- tolower(names(blocks))

relevant_zones <- c(
    "RZ1", "RZ2", "RZ3", "RZ4", "RZ5",
    "CF", "CZ1", "CZ2", "CZ3", "CZ4",
    "CZ5", "CZ6"
)


res <- blocks %>%
    filter(
        land_use_zone_code_id %in% relevant_zones
    ) %>%
    group_by(
        land_use_zone_code_id,
        land_use_policy_desc
    ) %>%
    summarise(area = sum(shape__area)) %>%
    ungroup() %>%
    mutate(area_proportion = area / sum(area)) %>%
    arrange(land_use_zone_code_id)

res

write.csv(res, "tables/zone_areas.csv", row.names = FALSE)
