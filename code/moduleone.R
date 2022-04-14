
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(dplyr))

moduleone <- read_excel(here::here("raw_data/livelihood_insecurity_complete.xlsx"))
save(moduleone, file = "moduleone.rda")
module1 <- data.frame(moduleone)
module1 %>%
  filter(!row_number() %in% c(1,2))
nameOfQuestion <- names(moduleone)
dependentVariablesNames <- c( 
  nameOfQuestion[str_detect(nameOfQuestion, regex("Ben", ignore_case= TRUE))],
  nameOfQuestion[str_detect(nameOfQuestion, regex("Risky", ignore_case= TRUE))],
  nameOfQuestion[str_detect(nameOfQuestion, regex("N_", ignore_case= FALSE))]
)



standardize = function(x){
  z <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  return( z)
}

#This chunk contains the coding schemes of various scales used in survey one: eco-political factors, kahan scale and acceptance scale
codedmodule1 <- module1 %>%
  
  #remove row 1
  filter(!row_number() %in% c(1,2)) %>% 
  
  # replace risky likert scale with numbers
  mutate_at(vars(starts_with("Risky")), funs(case_when(. =="Not at all risky" ~ 1, 
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
  mutate_at(vars(N_accept,N_reluctantlyaccept,N_reject), funs(case_when(. == "Strongly disagree" ~ 1, 
                                                                        . == "Somewhat disagree" ~ 2,
                                                                        . == "Neither agree nor disagree" ~3,
                                                                        . == "Somewhat agree" ~ 4,
                                                                        . == "Strongly agree" ~ 5))) %>%
  
  
  
  mutate_at(vars(Livelihood_Income), funs(case_when(. == "(a) own farming/fishing/animal rearing/business" ~ "LSI", 
                                                    . == "(b) employed for wages/salary" ~ "ISI",
                                                    . == "(c) student" ~"student",
                                                    . == "(d) currently unemployed" ~ "unemployed",
                                                    . == "(e) homemaker" ~ "homemaker",
                                                    . == "(f) retired" ~ "retired",
                                                    . == "(g) unable to work" ~ "unable to work" ))) %>%
  
  # coding education
  mutate_at(vars(education), funs(case_when(. == "No schooling completed" ~ 0, 
                                            . == "Primary school" ~ 1,
                                            . == "Grade 10th" ~ 2,
                                            . == "Grade 12th" ~ 3,
                                            . == "Some college, no degree" ~ 4,
                                            . == "Trade/technical/vocational training" ~ 5,
                                            . == "Bachelor's degree" ~ 6,
                                            . == "Master's degree" ~ 7,
                                            . == "Professional degree" ~ 8,
                                            . == "Doctorate degree" ~ 9 ))) %>%
  # coding yes and no questions
  mutate_at(vars(Iassociation, govtscheme, wateravail, polposition, polparty), funs(case_when(.== "Yes" ~ 1,
                                                                                              .== "No" ~ 0))) %>%
  # reverse coding yes and no questions
  mutate_at(vars(illness,severeillness), funs(case_when(.== "Yes" ~ 0,
                                                        .== "No" ~ 1))) %>%
  
  # coding How confident are you that you can continue your current livelihood if you want to? 
  mutate_at(vars(Lconfidence), funs(case_when(. == "Not at all confident" ~ 1, 
                                              . == "Not much confident" ~ 2,
                                              . == "Neutral" ~3,
                                              . == "Somewhat confident" ~ 4,
                                              . == "Very confident" ~ 5))) %>%
  
  mutate_at(vars(Lboat, houseown), funs(case_when(.== "Own it" ~ 1,
                                                  .== "Rent it" ~ 0)))%>%
  
  
  # J security index coding
  
  mutate_at(vars(JISRkeep), funs(case_when(. == "Strongly disagree" ~ 1, 
                                           . == "Somewhat disagree" ~ 2,
                                           . == "Neither agree nor disagree" ~3,
                                           . == "Somewhat agree" ~ 4,
                                           . == "Strongly agree" ~ 5))) %>%
  
  
  # reverse code JIS 
  mutate_at(vars(JISlosefuture, JISlose, JISfuture), funs(case_when(. == "Strongly disagree" ~ 5, 
                                                                    . == "Somewhat disagree" ~ 4,
                                                                    . == "Neither agree nor disagree" ~3,
                                                                    . == "Somewhat agree" ~ 2,
                                                                    . == "Strongly agree" ~ 1))) %>%
  
  mutate_at(vars(totalincome, otherincome), funs(case_when(. == "Less than Rs 1,00,000" ~ 1,
                                                           . == "Rs 1,00,000 to Rs 2,50,000"  ~ 2,
                                                           . == "Rs 2,50,001 to Rs 5,00,000" ~ 3,
                                                           . == "Rs 5,00,001 to Rs 10,00,000" ~ 4,
                                                           . == "Rs 10,00,001 and above"  ~ 5))) %>%
  
  mutate_at(vars(workknowledge, subsidyknow), funs(case_when(. == "Very poor" ~ 1, 
                                                             . == "Poor" ~ 2,
                                                             . == "Fair" ~3,
                                                             . == "Good" ~ 4,
                                                             . == "Very good" ~ 5))) %>%
  
  mutate_at(vars(drinkingwater, cookingwater), funs(case_when(. == "Bottled water/ Filtered Water" ~ 1,                                       
                                                              .  == "Piped into dwelling" ~ 2,                                                  
                                                              .  == "Piped to neighbour" ~ 3,                                                   
                                                              .  == "Piped to yard/plot" ~ 4, 
                                                              .  == "Protected well" ~ 5, 
                                                              .  == "Public tap/standpipe" ~6,
                                                              .  == "Surface Water  (River/Dam/Lake/Pond/Stream/Canal/Irrigation Channel)" ~7, 
                                                              .  == "Tanker Truck" ~ 8,
                                                              .  == "Tubewell/borehole" ~ 9,
                                                              .  == "Unprotected Spring" ~ 10))) %>%
  
  
  mutate_at(vars(toilet), funs(case_when(. == "No facility/bush/field" ~ 1,                                       
                                         .  == "Hanging toilet/hanging latrine" ~ 2,                                             
                                         .  == "Bucket toilet"  ~ 3,                                                   
                                         .  == "Composting toilet"  ~ 4, 
                                         .  == "Pit latrine without slab/open pit" ~ 5, 
                                         .  == "Pit latrine with slab" ~6,
                                         .  == "Ventilated improved pit laterine"  ~7, 
                                         .  == "Flush to somewhere else"  ~ 8,
                                         .  == "Flush to pit latrine"  ~ 9,
                                         .  == "Flush to septic tank" ~ 10,
                                         .  == "Flush to piped sewer system" ~ 11))) %>%
  
  mutate_at(vars(house), funs(case_when(. == "Hut made of mud, bamboo, straw etc. (kutcha house)" ~ 1, 
                                        . == "Permanent single story house (pukka house)" ~ 2,
                                        . == "Apartment/Flat in a multi-story building"  ~3,
                                        . == "Multi-story house" ~ 4))) %>% 
  
  mutate_at(vars(foodtrouble), funs(case_when(. == "8 or more months" ~ 1,                                       
                                              .  == "6-8 months"  ~ 2,                                                  
                                              .  == "4-6 months"  ~ 3,                                                   
                                              .  == "2-4 months"  ~ 4, 
                                              .  == "1-2 months" ~ 5, 
                                              .  == "0 months"  ~6))) %>%
  
  mutate_at(vars(foodadequacy), funs(case_when(. == "Extremely inadequate" ~ 1, 
                                               . == "Moderately inadequate" ~ 2,
                                               . == "Slightly inadequate" ~3,
                                               . == "Neither adequate nor inadequate" ~ 4,
                                               . == "Slightly adequate" ~ 5,
                                               . == "Moderately adequate" ~ 6,
                                               . == "Extremely adequate" ~ 7)))%>%
  
  separate( col = cookstove, into = c("cook1", "cook2", "cook3", "cook4"), sep = ",") %>%
  separate( col = vehicle, into = c("vehicle1", "vehicle2", "vehicle3", "vehicle4"), sep = ",")%>%
  separate( col = foodsource, into = c("food1", "food2", "food3", "food4"), sep = ",") 
  
namesModuleMap <- moduleone[1,]
temp_codedmodule1 <- codedmodule1 %>% select(where(is.numeric) & -dependentVariablesNames) %>%
  apply(2, standardize) 



temp_namesToReplace <- colnames(temp_codedmodule1)

temp_colLeft <- codedmodule1 %>% select( colnames(.) [! (colnames(.) %in% temp_namesToReplace)])

codedmodule1 <- cbind(temp_colLeft, temp_codedmodule1)


codedmodule1$Llenght = as.numeric(codedmodule1$Llenght) %>% standardize()
codedmodule1$Llenght[is.na(codedmodule1$Llenght)]=0
codedmodule1$Ilenght = as.numeric(codedmodule1$Ilenght) %>% standardize()
codedmodule1$Ilenght[is.na(codedmodule1$Ilenght)]=0
temp <- codedmodule1$Llenght + codedmodule1$Ilenght
humanAsset <- c("Q5.4", "Q5.5", "Q5.6", "I_Llenght")
known_sum <- codedmodule1 %>% 
  select( names(namesModuleMap[namesModuleMap %in% humanAsset])) %>%
  apply(1, sum)
humanAssetIndex_a <- (known_sum + temp) / (length(humanAsset)+1) 

known_prod <- codedmodule1 %>% 
  select( names(namesModuleMap[namesModuleMap %in% humanAsset])) %>%
  apply(1, prod) 
humanAssetIndex_g <- (known_prod * temp)^(1 / (length(humanAsset)+1)) 
  

physicalAsset <- c("Q5.6", "Q5.8", "Q5.9_1_TEXT", "Q5.10", "Q5.11", "Q5.12", "Q5.13") #find 

codedmodule1$farwatermin <- as.numeric(codedmodule1$farwatermin)  %>% standardize()
physicalAssetIndex_a <- codedmodule1 %>% 
  select( names(namesModuleMap[, namesModuleMap %in% physicalAsset])) %>%
  apply(1, mean)

physicalAssetIndex_g <- codedmodule1 %>% 
  select( names(namesModuleMap[, namesModuleMap %in% physicalAsset])) %>%
  apply(1, function(x) (prod(x))^(1.0 / length(x)))




socialAsset <- c("Q5.16", "Q5.17")

known_sum <- codedmodule1 %>% 
  select(names(namesModuleMap[namesModuleMap %in% socialAsset])) %>%
  apply(1, sum) 

known_prod <- codedmodule1 %>% 
  select(names(namesModuleMap[namesModuleMap %in% socialAsset])) %>%
  apply(1, function(x) (prod(x))^(1.0 / length(x)))

codedmodule1$Lassociation[codedmodule1$Lassociation == "Yes"] <- "1"
codedmodule1$Lassociation[codedmodule1$Lassociation == "No"] <- "0"
codedmodule1$Lassociation <- as.numeric(codedmodule1$Lassociation)  %>% standardize()
codedmodule1$Lassociation[is.na(codedmodule1$Lassociation)] <- 0
codedmodule1$Iassociation[is.na(codedmodule1$Iassociation)] <- 0

socialAssetIndex_a <- (known_sum + codedmodule1$Lassociation + codedmodule1$Iassociation) / 2
socialAssetIndex_g <- (known_sum * codedmodule1$Lassociation * codedmodule1$Iassociation)^(0.5)


financialAsset <- c("Q5.1", "Q5.2", "Q5.3")

financialAssetIndex_a <- codedmodule1 %>% 
  select(names(namesModuleMap[namesModuleMap %in% financialAsset])) %>%
  apply(1, mean)

financialAssetIndex_g <- codedmodule1 %>% 
  select(names(namesModuleMap[namesModuleMap %in% financialAsset])) %>%
  apply(1, function(x) (prod(x))^(1.0 / length(x)))


naturalAsset <- c("Q3.4_1_TEXT", "Q3.5", "Q3.6", "Q5.18","Q4.8_1_TEXT")

codedmodule1$totalincome <- as.numeric(codedmodule1$totalincome)  %>% standardize()
codedmodule1$totalincome[is.na(codedmodule1$totalincome)] <- 0
codedmodule1$Lflandarea <- as.numeric(codedmodule1$Lflandarea)  %>% standardize()
codedmodule1$Lflandarea[is.na(codedmodule1$Lflandarea)] <- 0
codedmodule1$Lboat[is.na(codedmodule1$Lboat)] <- 0
codedmodule1$Ipropvalue <- as.numeric(codedmodule1$Ipropvalue)  %>% standardize()
codedmodule1$Ipropvalue[is.na(codedmodule1$Iprovalue)] <- 0
naturalAssetIndex_a <- (codedmodule1$totalincome +codedmodule1$Lflandarea + codedmodule1$Lboat + codedmodule1$Ipropvalue + codedmodule1$polparty) /4
naturalAssetIndex_g <- (codedmodule1$totalincome * codedmodule1$Lflandarea * codedmodule1$Lboat * codedmodule1$Ipropvalue *  codedmodule1$polparty)^(0.25)


livehoodOutcomes <- c("Q5.18", "Q5.20", "Q5.21", "Q5.23")

livehoodOutcomesIndex_a <- codedmodule1 %>% 
  select(names(namesModuleMap[namesModuleMap %in% livehoodOutcomes])) %>%
  apply(1, mean)

livehoodOutcomesIndex_g <- codedmodule1 %>% 
  select(names(namesModuleMap[namesModuleMap %in% livehoodOutcomes])) %>%
  apply(1, function(x) (prod(x))^(1.0 / length(x)))

arithmetic_columns <- c("socialAssetIndex_a", "humanAssetIndex_a", "socialAssetIndex_a",
                     "financialAssetIndex_a", "naturalAssetIndex_a", "livehoodOutcomesIndex_a")

geometric_columns <- c("socialAssetIndex_g", "humanAssetIndex_g", "socialAssetIndex_g",
                        "financialAssetIndex_g", "naturalAssetIndex_g", "livehoodOutcomesIndex_g")


dependentVariablesNames <- c( 
  nameOfQuestion[str_detect(nameOfQuestion, regex("Ben", ignore_case= TRUE))],
  nameOfQuestion[str_detect(nameOfQuestion, regex("Risky", ignore_case= TRUE))]
)

data_final <- codedmodule1 %>% 
  select(dependentVariablesNames) %>% 
  mutate(
    socialAssetIndex_a = socialAssetIndex_a,
    humanAssetIndex_a = humanAssetIndex_a,
    physicalAssetIndex_a = physicalAssetIndex_a,
    financialAssetIndex_a = financialAssetIndex_a,
    naturalAssetIndex_a = naturalAssetIndex_a,
    livehoodOutcomesIndex_a = livehoodOutcomesIndex_a,
    socialAssetIndex_g = socialAssetIndex_g,
    humanAssetIndex_g = humanAssetIndex_g,
    physicalAssetIndex_g = physicalAssetIndex_g,
    financialAssetIndex_g = financialAssetIndex_g,
    naturalAssetIndex_g = naturalAssetIndex_g,
    livehoodOutcomesIndex_g = livehoodOutcomesIndex_g
  )


R_Square_A <- rep(0, length(dependentVariablesNames))
R_Square_G <- rep(0, length(dependentVariablesNames))
AIC <- rep(0, length(dependentVariablesNames))

for(i in 1:length(dependentVariablesNames)) {
  formula_A <- as.formula(paste(dependentVariablesNames[i], " ~ ."))
  tempA <- data_final %>% select(c(dependentVariablesNames[i], arithmetic_columns)) %>% lm(formula = formula_A, data = .)
  R_Square_A[i] <- summary(tempA)$r.squared
  mutLogistic <- nnet::multinom(formula = tempA, data = data_final)
  AIC[i] <- mutLogistic$AIC
  
  formula_G <- as.formula(paste(dependentVariablesNames[i], " ~ ."))
  tempG <- data_final %>% select(c(dependentVariablesNames[i], geometric_columns)) %>% lm(formula = formula_G, data = .)
  R_Square_G[i] <- summary(tempG)$r.squared
}



