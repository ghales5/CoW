example_lga_person_weights <- list(
    read.csv("data-raw/person_weights_lga.csv") |>
        dplyr::select(-1),
    read.csv("data-raw/person_weights_lga2.csv") |>
        dplyr::select(-1)
)


usethis::use_data(example_lga_person_weights, overwrite = TRUE)
