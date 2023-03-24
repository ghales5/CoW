a <- shp_wphu_mb %>%
    select(centroids) %>%
    sample_n(10) %>%
    as_tibble() %>%
    select(centroids) %>%
    pull() %>%
    st_coordinates() %>%
    as_tibble() %>%
    mutate(
        longitude = X + rnorm(mean = 0, sd = 0.1, n=n()),
        latitude = Y + rnorm(mean = 0, sd = 0.01, n=n()),
        person_weight = 1
    )

write.csv(a, "person_weights_test2.csv")


b <- shp_wphu_lga %>%
    select(LGA_NAME22, centroids) %>%
    mutate(
        longitude = st_coordinates(centroids)[, 1],
        latitude = st_coordinates(centroids)[, 2]
    ) %>%
    as_tibble() %>%
    select(-centroids, -geometry)

write.csv(
    b %>% mutate(person_weight = round(runif(n = n(), min = 1, max = 100))),
    "person_weights_lga.csv"
)
write.csv(
    b %>% mutate(person_weight = round(runif(n = n(), min = 1, max = 100))),
    "person_weights_lga2.csv"
)
