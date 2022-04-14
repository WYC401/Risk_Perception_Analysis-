library(here)
library(stringr)
library(dplyr)
library(plyr)
load(here::here("raw_data/moduletwo.rda"))
load(here::here("raw_data/moduleone.rda"))
l <- moduletwo[1,]

nameOfQuestion <- names(l)
dependentVariablesNames <- c( 
                            nameOfQuestion[str_detect(nameOfQuestion, regex("Ben", ignore_case= TRUE))],
                            nameOfQuestion[str_detect(nameOfQuestion, regex("Risky", ignore_case= TRUE))],
                            nameOfQuestion[str_detect(nameOfQuestion, regex("N_", ignore_case= FALSE))]
                            )
KehenScaleNames <- nameOfQuestion[str_detect(nameOfQuestion, regex("K_", ignore_case= TRUE))]
governanceScaleNames <- names(l[str_detect(l, regex("Q2.8", ignore_case= TRUE))])
economyScaleNames <- names(l[str_detect(l, regex("Q2.9", ignore_case= TRUE))])
characterRiskScaleNames <- names(l[str_detect(l, regex("Q2.1[0-5]", ignore_case= TRUE))])

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
moduletwodata_final <- temp %>% select(c(dependentVariablesNames, KehenScaleNames, governanceScaleNames, economyScaleNames, characterRiskScaleNames))


for(i in dependentVariablesNames) {
  formulaK <- as.formula(paste(i, " ~ ."))
  tempK <- moduletwodata_final %>% select(c(i, KehenScaleNames)) %>% lm(formula = formulaK, data = .)
  rK <- summary(tempK)$r.squared
  
  formulaK <- as.formula(paste(i, " ~ ."))
  tempK <- moduletwodata_final %>% select(c(i, )) %>% lm(formula = formulaK, data = .)
  
}



