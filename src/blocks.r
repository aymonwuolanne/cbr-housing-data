library(dplyr)

blocks <- read.csv("C:/Users/Aymon/Downloads/ACTGOV_TP_LAND_USE_ZONE.csv")

blocks %>%
    group_by(LAND_USE_ZONE_CODE_ID) %>%
    summarise(area = sum(Shape__Area)) %>%
    print(n = Inf)

blocks %>%
    filter(LAND_USE_ZONE_CODE_ID %in% c("RZ1", "RZ2", "RZ3", "RZ4", "RZ5")) %>%
    group_by(LAND_USE_ZONE_CODE_ID) %>%
    summarise(area = sum(Shape__Area)) %>%
    ungroup() %>%
    mutate(area = area / sum(area))
