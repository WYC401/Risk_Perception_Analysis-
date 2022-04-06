library(here)
library(stringr)
library(dplyr)
library(plyr)
data <- load(here::here("raw_data/moduletwo.rda"))
l <- moduletwo[1,]
nameOfQuestion <- names(l)
dependentVariablesNames <- c( 
                            nameOfQuestion[str_detect(nameOfQuestion, regex("Ben", ignore_case= TRUE))],
                            nameOfQuestion[str_detect(nameOfQuestion, regex("Risky", ignore_case= TRUE))]
                            )
KehenScaleNames <- nameOfQuestion[str_detect(nameOfQuestion, regex("K_", ignore_case= TRUE))]
r <- nrow(moduletwo)
dataUsed <- moduletwo[3:r,]
beneficialLikert <- c(
            "Not at all beneficial",
            "Slightly beneficial",
            "Moderately beneficial",
            "Very beneficial",
            "Extremely beneficial"
            )

agreeLikert <- c(
            "Strongly disagree",
            "Somewhat disagree",
            "Neither agree nor disagree",
            "Somewhat agree",
            "Strongly agree"
            )

riskyLikert <- c("Not at all risky",
                 "Slightly risky",
                 "Moderately risky",
                 "Very risky",
                 "Extremely risky")

dataUsed <- dataUsed %>% sapply(
  function(x) { mapvalues(x, c(beneficialLikert, agreeLikert, riskyLikert, "Rather not say/Don't know"), c( 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, NA) )}
)

temp <- dataUsed  %>% mutate_at(vars(starts_with("Risky")), funs(case_when(. =="Not at all risky" ~ 1, 
                                                                . =="Slightly risky" ~ 2, 
                                                                . =="Moderately risky" ~ 3, 
                                                                . =="Very risky" ~ 4, 
                                                                . =="Extremely risky" ~ 5))) %>%
  
  # replace beneficial likert scale with numbers  
  mutate_at(vars(starts_with("Ben")), funs(case_when(. =="Not at all beneficial" ~ 1,
                                                     . =="Slightly beneficial" ~ 2,
                                                     . =="Moderately beneficial" ~ 3,
                                                     . =="Very beneficial" ~ 4,
                                                     . =="Extremely beneficial" ~ 5 ))) %>%
  
  # replace nuclear acceptance likert scale with numbers
  mutate_at(vars(! (starts_with("Ben") | starts_with("Risk"))), funs(case_when(. == "Strongly disagree" ~ 1, 
                                                                        . == "Somewhat disagree" ~ 2,
                                                                        . == "Neither agree nor disagree" ~3,
                                                                        . == "Somewhat agree" ~ 4,
                                                                        . == "Strongly agree" ~ 5)))
dataUsed <- dataUsed %>% sapply( function(x) {
  mapvalues(x, agreeLikert, c(1, 2, 3, 4, 5))
})
