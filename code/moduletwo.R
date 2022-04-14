suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(hablar))
suppressPackageStartupMessages(library(tsibble))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(readr))


moduletwo <- read_excel(here::here("raw_data/political_factors_complete.xlsx"))
module2 <- data.frame(moduletwo)
module2[module2 == "Rather not say/ Don't know"] <- "Rather not say/Don't know"
module2 %>%
  filter(!row_number() %in% c(1,2))

module2%>%
  filter(!row_number() %in% c(1,2)) %>% 
  select(starts_with("RISKY"))%>%
  gather(Technology, Perceived_Risk, Risky_Hydro:Risky_INDLPG, factor_key = TRUE)%>%
  separate(Technology, c("Risky", "Technology"), extra = "merge", fill = "left")%>%
  select(-Risky) %>%
  group_by(Technology, Perceived_Risk) %>%
  dplyr::summarise(n=n())
colorsequence<- c("#8C4646","#D9695F","#F2A679","#F2D091","#5D8C7B","#808080")
df_barplot<-module2%>%
  filter(!row_number() %in% c(1,2)) %>% 
  select(starts_with("RISKY"))%>%
  gather(Technology, Perceived_Risk, Risky_Hydro:Risky_INDLPG, factor_key = TRUE)%>%
  separate(Technology, c("Risky", "Technology"), extra = "merge", fill = "left")%>%
  select(-Risky) %>% 
  group_by(Technology, Perceived_Risk) %>%
  dplyr::summarise(n=n())
df_barplot %>% ggplot(aes(fill= fct_rev(factor(Perceived_Risk, levels=c("Extremely risky", "Very risky","Moderately risky","Slightly risky","Not at all risky","Rather not say/Don't know"))), y=n , x=Technology)) + 
  geom_bar(position="fill",stat="identity")+
  scale_x_discrete(limits = c("Solar", "INDSolar", "INDFirewoodetc" ,"Wind" , "Hydro", "INDWind", "INDHydro", "INDDiesel" , "INDBiogas","INDKerosene", "Oil", "Coal","Gas", "INDLPG","Nuclear"))+
  scale_fill_manual("legend", values = c("Extremely risky" = "#8C4646", "Very risky" = "#D9695F", "Moderately risky" = "#F2A679", "Slightly risky" = "#F2D091","Not at all risky" = "#5D8C7B", "Rather not say/Don't know" = "grey94"))+
  coord_flip()+
  theme_classic()

codedmodule2 <- module2 %>%
  
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
  
  # code likert scale for variables for Kahan scale into numbers
  mutate_at(vars(starts_with (c("K_I","K_H","DISPLACE", "POLLUTE", "HEALTH", "JOBS", "BEAUTY", "PRIDE", "NPRIDE", "DEV", "PROSPER", "RELY"))), funs(case_when(. == "Strongly disagree" ~ 1, 
                                                                                                                                                              . == "Somewhat disagree" ~ 2,
                                                                                                                                                              . == "Neither agree nor disagree" ~3,
                                                                                                                                                              . == "Somewhat agree" ~ 4,
                                                                                                                                                              . == "Strongly agree" ~ 5))) %>%
  
  # reverse code for likert scale for variables for Kahan scale into numbers
  mutate_at(vars(starts_with (c("K_S","K_E"))), funs(case_when(. == "Strongly disagree" ~ 5, 
                                                               . == "Somewhat disagree" ~ 4,
                                                               . == "Neither agree nor disagree" ~3,
                                                               . == "Somewhat agree" ~ 2,
                                                               . == "Strongly agree" ~ 1))) %>%
  # code eco-pol scale variables into numbers
  mutate_at(vars(SYSTEMDEMO,SYSTEMRELIGION,SYSTEMTECHNO,SYSTEMTOTAL,WEALTHLIM,MECHANISATION,DECISIONDECEN,INDUSTRYLARGE,ECONOMYGLOBAL,OWNERPVT,OWNERNOREG), funs(case_when(. == "Strongly disagree" ~ 1, 
                                                                                                                                                                           . == "Somewhat disagree" ~ 2,
                                                                                                                                                                           . == "Neither agree nor disagree" ~3,
                                                                                                                                                                           . == "Somewhat agree" ~ 4,
                                                                                                                                                                           . == "Strongly agree" ~ 5))) %>%
  # reverse code eco-pol scale variables into numbers 
  mutate_at(vars(DECISIONCEN, INDUSTRYSMALL, ECONOMYLOCAL, ENVOVERDEV,OWNERPUB, OWNERREG), funs(case_when(. == "Strongly disagree" ~ 5, 
                                                                                                          . == "Somewhat disagree" ~ 4,
                                                                                                          . == "Neither agree nor disagree" ~3,
                                                                                                          . == "Somewhat agree" ~ 2,
                                                                                                          . == "Strongly agree" ~ 1)))
PerrnaScaleNames <- codedmodule2 %>% select((which(names(.) == "N_reject") + 1):last_col()) %>%
  select(where(is.numeric)) %>% names()


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

Kahan <- rep(0, length(dependentVariablesNames))
Prerna<- rep(0, length(dependentVariablesNames))
AIC_K <- rep(0, length(dependentVariablesNames))
AIC_P <- rep(0, length(dependentVariablesNames))

for(i in 1:length(dependentVariablesNames)) {
  formulaK <- as.formula(paste(dependentVariablesNames[i], " ~ ."))
  tempK <- moduletwodata_final %>% select(c(dependentVariablesNames[i], KehenScaleNames)) %>% lm(formula = formulaK, data = .)
  Kahan[i] <- summary(tempK)$r.squared
  mutLogistic_K <- nnet::multinom(formula = tempK, data = moduletwodata_final)
  AIC_K[i] <- mutLogistic_K$AIC
  
  formulaP <- as.formula(paste(dependentVariablesNames[i], " ~ ."))
  tempP <- moduletwodata_final %>% select(c(dependentVariablesNames[i], PerrnaScaleNames)) %>% lm(formula = formulaK, data = .)
  Prerna[i] <- summary(tempP)$r.squared
  mutLogistic_G <- nnet::multinom(formula = tempP, data = moduletwodata_final)
  AIC_P[i] <- mutLogistic_G$AIC
  
}

rSquareResult <- data.frame(Kahan, Prerna) %>% pivot_longer(cols = everything(), names_to = "Scale")
colnames(rSquareResult) <- c("Scale","R_Square")
ggplot(data = rSquareResult , aes(y = R_Square, color =Scale)) + geom_boxplot() + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) 

AICResult <- data.frame(AIC_K, AIC_P) %>% pivot_longer(cols = everything(), names_to = "Scale")
colnames(AICResult) <- c("Scale","AIC")
ggplot(data = AICResult , aes(y = AIC, color =Scale)) + geom_boxplot() + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) 

