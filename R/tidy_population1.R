#' tidy_population1
#'
#' This function is used to prepare the observation datasets for the use in function `tidy_ae_table()`.
#' The function filter the `adsl` dataset, and define proper treatment order in the dataset.
#'
#' @param population_from A data frame to obtain population level variables.
#'                        Typically an 'adsl' dataset.
#'                        It is the source of variables mentioned in
#'                        `population_where`, `treatment_var`, `stratum_var` and `covariate_var`.
#' @param population_where A character string to define the criteria to select analysis population.
#' @param treatment_var  A character string to define the variable of new column called "treatment".
#' @param treatment_order A vector of character strings that tells the
#'                function which rows of the table should be select,
#'                only if the values in "treatment" is in this vector.
#'                        It also provide the label names of the treatments after turning them into factors.
#' @param stratum_var A character string to define the variable of baseline stratum in 'population_from'.
#'                    Only one 'stratum_var' is allowed.
#' @param baseline_var Tells the function which column of the table to be selected as the final output.
#'
#' @return an output data frame of population level information.
#'         USUBJID: Unique subject identifier.
#'         treatment: Actual treatment.
#'         stratum: baseline stratum in 'population_from'.
#'
#' @export
#'
#' @examples
#' pop <- tidy_population(population_from  = adsl %>% rename(TRTA = TRT01A),
#'  population_where = "ITTFL=='Y'",
#'  treatment_var    = "TRTA",
#'  treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
#'  stratum_var      = NULL,
#'  baseline_var     = NULL)
#'
#' tidy_population(population_from = adsl)
#' tidy_population(population_from = adsl, stratum_var = "RACE")
tidy_population1 <- function(population_from,
                            population_where = NULL,
                            treatment_var = "TRT01A",
                            treatment_order = unique(population_from[[treatment_var]]),
                            stratum_var = NULL,
                            baseline_var = NULL){

  ##################################
  # End of Input Checking
  ##################################

  if(is.null(population_where)){ population_where <- TRUE}

  # Select the Desired Population
  pop <- eval(parse(text = paste0("subset(population_from,", population_where, ")")))

  # Define treatment group
  pop[["treatment"]] <- pop[[treatment_var]]
  pop <- subset(pop, treatment %in% treatment_order)  ## here is the difference between tidy_population()
  
  # Define treatment label
  if (!is.null(names(treatment_order))) {
    label_name = names(treatment_order)
  } else {label_name = treatment_order}

  pop[["treatment"]] <- factor(pop[["treatment"]], levels = treatment_order, labels = label_name)

  # Define stratum
  if(length(stratum_var) == 0){
    pop[["stratum"]] <- "All"
  }else{
    pop[["stratum"]] <- pop[[stratum_var]]
  }

  pop <- pop[, unique(c("USUBJID", "treatment", "stratum", baseline_var))]

  pop
}
