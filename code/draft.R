library(here)
library(stringr)
library(dplyr)
data <- load(here::here("raw_data/moduletwo.rda"))
l <- moduletwo[1,]
nameOfQuestion <- names(l)
dependentVariablesNames <- c( 
                            nameOfQuestion[str_detect(nameOfQuestion, regex("Ben", ignore_case= TRUE))],
                            nameOfQuestion[str_detect(nameOfQuestion, regex("Risky", ignore_case= TRUE))]
                            )
KehenScaleNames <- nameOfQuestion[str_detect(nameOfQuestion, regex("K_", ignore_case= TRUE))]
likert <- c("Rather not say/ Don't know", )
dataUsed %>% sapply(function(x) { mapvalues(x,
                                            c("Rather not say/ Don't know"),
                                            1)})
r <- nrow(moduletwo)
dataUsed <- moduletwo[3:r,]