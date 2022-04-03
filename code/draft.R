library(here)
library(stringr)
data <- load(here::here("raw_data/moduletwo.rda"))
l <- moduletwo[1,]
nameOfQuestion <- names(l)
dependentVariablesNames <- c( 
                            nameOfQuestion[str_detect(nameOfQuestion, regex("Ben", ignore_case= TRUE))],
                            nameOfQuestion[str_detect(nameOfQuestion, regex("Risky", ignore_case= TRUE))]
                            )
