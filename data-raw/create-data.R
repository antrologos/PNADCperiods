# Script to create package data files

# Exception quarters - quarters with non-standard IBGE timing rules
exception_quarters <- c("2016t3", "2016t4", "2017t2", "2022t3", "2023t2")

# Save to data/
usethis::use_data(exception_quarters, overwrite = TRUE)
