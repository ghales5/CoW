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

write.csv(a, "person_weights_test.csv")
