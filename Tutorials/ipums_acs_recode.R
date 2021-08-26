ipums_recode_acs5 <- function(dataframe){
  dataframe <- dataframe %>%
    mutate(
    # hh_id = paste0(SAMPLE, SERIAL),
    hh_type = case_when(
      HHTYPE == 1 ~ "family",
      HHTYPE %in% 2:8 ~ "non-family"
    ),
    tenure = case_when(
      OWNERSHP == 1 ~ "owner",
      OWNERSHP == 2 ~ "renter"
    ),
    household = case_when(
      GQ == 0 ~ "vacant",
      GQ %in% c(1:2,5) ~ "household",
      GQ %in% c(3:4) ~ "gq"
    ),
    multigen = case_when(
      MULTGEN == 1 ~ "1 generation",
      MULTGEN == 2 ~ "2 generations",
      MULTGEN == 3 ~ "3+ generations",
    )
  )
}
