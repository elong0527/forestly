#' tidy_ae_table
#'
#'
#' @param population_from
#' @param observation_from
#' @param population_where
#' @param observation_where
#' @param treatment_var
#' @param treatment_order
#' @param ae_var
#' @param stratum_var
#' @param listing_var
#'
#' @examples
#'
#' library(dplyr)
#' library(tidyr)
#' db <- adsl %>% rename(TRTA = TRT01A)
#' tidy_ae_table(population_from  = db,
#'               observation_from = adae,
#'               treatment_var = "TRTA",
#'               treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"))

tidy_ae_table <- function(population_from,
                          observation_from,
                          population_where = "ITTFL=='Y'",
                          observation_where = NULL,
                          treatment_var = "TRTA",
                          treatment_order = unique(population_from[[treatment_var]]),
                          ae_var = "AEDECOD",
                          stratum_var = NULL,
                          listing_var = names(observation_from)){

  # Population Level Tidy Data
  pop <- tidy_population(population_from  = population_from,
                         population_where = population_where,
                         treatment_var    = treatment_var,
                         treatment_order  = treatment_order,
                         stratum_var      = stratum_var,
                         baseline_var     = NULL)

  # Select the Desired Observation
  db <- tidy_observation(observation_from = observation_from,
                         observation_where = observation_where,
                         treatment_var    = treatment_var,
                         treatment_order  = treatment_order)

  db[["ae"]] <- db[[ae_var]]
  db <- subset(db, USUBJID %in% pop$USUBJID)

  # Yilong: will remove dplyr, tiyer dependency
  db_N <- count(pop, treatment, stratum, name = "N")

  res <- db %>% group_by(treatment, ae) %>%
    summarise(n = n()) %>%
    left_join(db_N) %>%
    mutate(pct = n / N * 100) %>%
    ungroup() %>%
    mutate(trtn = as.numeric(treatment)) %>%
    select(- treatment) %>%
    pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0) %>%
    mutate(across(starts_with("N", ignore.case = FALSE), ~ max(.x)))

  listing_var <- unique(c("USUBJID", "ae", "treatment", listing_var))
  list(table = res, listing = db[, listing_var])
}
