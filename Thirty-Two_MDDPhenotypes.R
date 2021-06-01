###################################################################################################################
##################  GLOSSARY OF CODES - ALL FIELDS REQUIRED TO DEFINE PHENOTYPES       ############################
###################################################################################################################
### UK Biobank Code     Descriptive                                                                             ###
### f.eid	        ID										                                            col number = 1
### f.20446.0.0		Ever.had.prolonged.feelings.of.sadness.or.depression			   		  col number = 12963
### f.20441.0.0		Ever.had.prolonged.loss.of.interest.in.normal.activities				  col number = 12960
### f.20532.0.0		Did.your.sleep.change									                            col number = 13027
### f.20435.0.0		Difficulty.concentrating.during.worst.depression					        col number = 12954
### f.20536.0.0		Weight.change.during.worst.episode.of.depression					        col number = 13031
### f.20437.0.0		Thoughts.of.death.during.worst.depression						              col number = 12956
### f.20449.0.0		Feelings.of.tiredness.during.worst.episode.of.depression				  col number = 12966
### f.20450.0.0		Feelings.of.worthlessness.during.worst.period.of.depression				col number = 12967
### f.20438.0.0		Duration.of.worst.depression								                      col number = 12957
### f.20439.0.0		Frequency.of.depressed.days.during.worst.episode.of.depression		col number = 12958		
### f.20440.0.0		Impact.on.normal.roles.during.worst.period.of.depression				  col number = 12959
### f.20442.0.0		Lifetime.number.of.depressed.periods							                col number = 12961
### f.20436.0.0		Fraction.of.day.affected.during.worst.episode.of.depression				col number = 12955
###################################################################################################################

### Load dependencies
library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)
library(polycor)

#Read in the dataset
MHQ <- fread('decomposition_study_pheno_file', data.table=FALSE)

colnames(MHQ) <- c('ID', 'Ever.had.prolonged.feelings.of.sadness.or.depression', 'Ever.had.prolonged.loss.of.interest.in.normal.activities', 'Did.your.sleep.change', 'Difficulty.concentrating.during.worst.depression', 
'Weight.change.during.worst.episode.of.depression', 'Thoughts.of.death.during.worst.depression', 'Feelings.of.tiredness.during.worst.episode.of.depression', 'Feelings.of.worthlessness.during.worst.period.of.depression', 
'Duration.of.worst.depression', 'Frequency.of.depressed.days.during.worst.episode.of.depression', 'Impact.on.normal.roles.during.worst.period.of.depression', 'Lifetime.number.of.depressed.periods', 'Fraction.of.day.affected.during.worst.episode.of.depression')

#Remove NAs such that only individuals who have responded to the MHQ are retained. Sample size = 157358  
MHQ <- MHQ[!is.na(MHQ$Ever.had.prolonged.feelings.of.sadness.or.depression), ]

#Check there are no duplicated IDs 
sum(duplicated(MHQ$ID)) 

#NOTE: When creating these phenotypes - the number of the phenotype in the field correspond to the phenotypes specified within the appendix of the pre-registration document

#Phenotype 1: Cardinal symptoms assessed only - All participants either endorse depressed mood or anhedonia get a 1, otherwise classed as 0. Prefer not to answers where the response to the second question in 
#the set is a 0 or another prefer not to answer are reclassified as NA. Checked against data showcase this should return a phenotype of 89,040 endorsements. It did.  
MHQ$Pheno1 <- case_when(
  MHQ$Ever.had.prolonged.feelings.of.sadness.or.depression == 0 & MHQ$Ever.had.prolonged.loss.of.interest.in.normal.activities == 0 ~ 0,
  MHQ$Ever.had.prolonged.feelings.of.sadness.or.depression == 1 | MHQ$Ever.had.prolonged.loss.of.interest.in.normal.activities == 1 ~ 1,
  TRUE ~ 999
  )

#Recode components such that NAs are changed to 999

#Recode the variables so they are suitable for the subsequent phenotypes, i.e. make them binary - Rules followed in accordance with the pre-registration
MHQ$BINARY_Impact.on.normal.roles.during.worst.period.of.depression <- case_when(
  MHQ$Impact.on.normal.roles.during.worst.period.of.depression == 2 | MHQ$Impact.on.normal.roles.during.worst.period.of.depression == 3 ~ 1,
  MHQ$Impact.on.normal.roles.during.worst.period.of.depression == 0 | MHQ$Impact.on.normal.roles.during.worst.period.of.depression == 1 ~ 0,
  TRUE ~ 999
  )

MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression <- case_when(
  MHQ$Frequency.of.depressed.days.during.worst.episode.of.depression == 2 | MHQ$Frequency.of.depressed.days.during.worst.episode.of.depression == 3 ~ 1,
  MHQ$Frequency.of.depressed.days.during.worst.episode.of.depression == 1 ~ 0,
  TRUE ~ 999
  )

MHQ$BINARY_Duration.of.worst.depression <- case_when(
  MHQ$Duration.of.worst.depression == 4 | MHQ$Duration.of.worst.depression == 5 | MHQ$Duration.of.worst.depression == 6 ~ 1,
  MHQ$Duration.of.worst.depression == 1 | MHQ$Duration.of.worst.depression == 2 | MHQ$Duration.of.worst.depression == 3 ~ 0,
  TRUE ~ 999
  )

MHQ$BINARY_Lifetime.number.of.depressed.periods <- case_when(
  MHQ$Lifetime.number.of.depressed.periods > 1 | MHQ$Lifetime.number.of.depressed.periods == -999 ~ 1,
  MHQ$Lifetime.number.of.depressed.periods == 1 ~ 0,
  TRUE ~ 999
  )

#If people have responded don't know or prefer not to answer for individual symptoms, recode this to 0 for the sum of items. 
MHQ$Ever.had.prolonged.feelings.of.sadness.or.depression[MHQ$Ever.had.prolonged.feelings.of.sadness.or.depression==-818 | MHQ$Ever.had.prolonged.feelings.of.sadness.or.depression==-121] <- NA

MHQ$Ever.had.prolonged.loss.of.interest.in.normal.activities[MHQ$Ever.had.prolonged.loss.of.interest.in.normal.activities==-818 | MHQ$Ever.had.prolonged.loss.of.interest.in.normal.activities==-121] <- NA

MHQ$Did.your.sleep.change[MHQ$Did.your.sleep.change==-818 | MHQ$Did.your.sleep.change==-121] <- NA 

MHQ$Difficulty.concentrating.during.worst.depression[MHQ$Difficulty.concentrating.during.worst.depression==-818 | MHQ$Difficulty.concentrating.during.worst.depression==-121] <- NA 

MHQ$Weight.change.during.worst.episode.of.depression_Updated <- case_when(
  MHQ$Weight.change.during.worst.episode.of.depression == 1 | MHQ$Weight.change.during.worst.episode.of.depression == 2 | MHQ$Weight.change.during.worst.episode.of.depression == 3 ~ 1,
  MHQ$Weight.change.during.worst.episode.of.depression == 0 ~ 0,
  TRUE ~ 999
  )

MHQ$Weight.change.during.worst.episode.of.depression_Updated[MHQ$Weight.change.during.worst.episode.of.depression_Updated==999] <- NA

MHQ$Thoughts.of.death.during.worst.depression[MHQ$Thoughts.of.death.during.worst.depression==-818 | MHQ$Thoughts.of.death.during.worst.depression==-121] <- NA 

MHQ$Feelings.of.tiredness.during.worst.episode.of.depression[MHQ$Feelings.of.tiredness.during.worst.episode.of.depression==-818 | MHQ$Feelings.of.tiredness.during.worst.episode.of.depression==-121] <- NA 

MHQ$Feelings.of.worthlessness.during.worst.period.of.depression[MHQ$Feelings.of.worthlessness.during.worst.period.of.depression==-818 | MHQ$Feelings.of.worthlessness.during.worst.period.of.depression==-121] <- NA 

MHQ$SymptomNAs <- apply(MHQ[,c("Ever.had.prolonged.feelings.of.sadness.or.depression", "Ever.had.prolonged.loss.of.interest.in.normal.activities", "Did.your.sleep.change", "Difficulty.concentrating.during.worst.depression", 
                                   "Weight.change.during.worst.episode.of.depression_Updated", "Thoughts.of.death.during.worst.depression", "Feelings.of.tiredness.during.worst.episode.of.depression", "Feelings.of.worthlessness.during.worst.period.of.depression")], MARGIN = 1, function(x) sum(is.na(x)))

MHQ$SymptomCount <- rowSums(MHQ[,c("Ever.had.prolonged.feelings.of.sadness.or.depression", "Ever.had.prolonged.loss.of.interest.in.normal.activities", "Did.your.sleep.change", "Difficulty.concentrating.during.worst.depression", 
"Weight.change.during.worst.episode.of.depression_Updated", "Thoughts.of.death.during.worst.depression", "Feelings.of.tiredness.during.worst.episode.of.depression", "Feelings.of.worthlessness.during.worst.period.of.depression")], na.rm=TRUE)

################################################################################################################################################################################
###### Cardinal symptoms + 1 extra component: Next set of phenotypes are a subset of phenotype 1 as they endorse the phenotype and another core component of MDD   #############
################################################################################################################################################################################

#Phenotype 2: Cardinal symptoms + impairment

MHQ$Pheno2 <- case_when(
  MHQ$Pheno1 == 1 & MHQ$BINARY_Impact.on.normal.roles.during.worst.period.of.depression == 1 ~ 1,
  MHQ$Pheno1 == 0 | ((MHQ$Pheno1 == 1 | MHQ$Pheno1 == 999) & MHQ$BINARY_Impact.on.normal.roles.during.worst.period.of.depression == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 3: Cardinal symptoms + frequency of days during episode
MHQ$Pheno3 <- case_when(
  MHQ$Pheno1 == 1 & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 1 ~ 1,
  MHQ$Pheno1 == 0 | ((MHQ$Pheno1 == 1 | MHQ$Pheno1 == 999) & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 4: Cardinal symptoms + duration of episode
MHQ$Pheno4 <- case_when(
  MHQ$Pheno1 == 1 & MHQ$BINARY_Duration.of.worst.depression == 1 ~ 1,
  MHQ$Pheno1 == 0 | ((MHQ$Pheno1 == 1 | MHQ$Pheno1 == 999) & MHQ$BINARY_Duration.of.worst.depression == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 5: Cardinal symptoms + number of episodes
MHQ$Pheno5 <- case_when(
  MHQ$Pheno1 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno1 == 0 | ((MHQ$Pheno1 == 1 | MHQ$Pheno1 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 6: Cardinal symptoms + 5 or more symptoms
MHQ$Pheno6 <- case_when(
  MHQ$Pheno1 == 1 & MHQ$SymptomCount > 4 ~ 1,
  MHQ$Pheno1 == 1 & (MHQ$SymptomCount + MHQ$SymptomNAs >= 5) ~ 999,
  MHQ$Pheno1 == 0 | ((MHQ$Pheno1 == 1 | MHQ$Pheno1 == 999) & MHQ$SymptomCount <= 4) ~ 0
  )

################################################################################################################################################################################
###### Cardinal symptoms + 2 extra component: Next set of phenotypes are a subset of the previous phenotypes as they endorse a further component of MDD  #######################
################################################################################################################################################################################

#Phenotype 7: (Cardinal symptoms + impairment) + frequency of days during episode
MHQ$Pheno7 <- case_when(
  MHQ$Pheno2 == 1 & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 1 ~ 1,
  MHQ$Pheno2 == 0 | ((MHQ$Pheno2 == 1 | MHQ$Pheno2 == 999) & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 8: (Cardinal symptoms + impairment) + duration of episode
MHQ$Pheno8 <- case_when(
  MHQ$Pheno2 == 1 & MHQ$BINARY_Duration.of.worst.depression == 1 ~ 1,
  MHQ$Pheno2 == 0 | ((MHQ$Pheno2 == 1 | MHQ$Pheno2 == 999) & MHQ$BINARY_Duration.of.worst.depression == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 9: (Cardinal symptoms + impairment) + number of episodes
MHQ$Pheno9 <- case_when(
  MHQ$Pheno2 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno2 == 0 | ((MHQ$Pheno2 == 1 | MHQ$Pheno2 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 10: (Cardinal symptoms + impairment) + 5 or more symptoms
MHQ$Pheno10 <- case_when(
  MHQ$Pheno6 == 1 & MHQ$BINARY_Impact.on.normal.roles.during.worst.period.of.depression == 1 ~ 1,
  MHQ$Pheno6 == 0 | ((MHQ$Pheno6 == 1 | MHQ$Pheno6 == 999) & MHQ$BINARY_Impact.on.normal.roles.during.worst.period.of.depression == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 11: (Cardinal symptoms + frequency of days during episode) + duration of episode
MHQ$Pheno11 <- case_when(
  MHQ$Pheno4 == 1 & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 1 ~ 1,
  MHQ$Pheno4 == 0 | ((MHQ$Pheno4 == 1 | MHQ$Pheno4 == 999) & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 0) ~ 0,
  TRUE ~ 999
  )
  
#Phenotype 12: (Cardinal symptoms + frequency of days during episode) + number of episodes 
MHQ$Pheno12 <- case_when(
  MHQ$Pheno3 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno3 == 0 | ((MHQ$Pheno3 == 1 | MHQ$Pheno3 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 13: (Cardinal symptoms + frequency of days during episode) + 5 or more symptoms
MHQ$Pheno13 <- case_when(
  MHQ$Pheno6 == 1 & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 1 ~ 1,
  MHQ$Pheno6 == 0 | ((MHQ$Pheno6 == 1 | MHQ$Pheno6 == 999) & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 14: (Cardinal symptoms + duration of episode) + number of episodes
MHQ$Pheno14 <- case_when(
  MHQ$Pheno4 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno4 == 0 | ((MHQ$Pheno4 == 1 | MHQ$Pheno4 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 15: (Cardinal symptoms + duration of episode) + 5 or more symptoms
MHQ$Pheno15 <- case_when(
  MHQ$Pheno6 == 1 & MHQ$BINARY_Duration.of.worst.depression == 1 ~ 1,
  MHQ$Pheno6 == 0 | ((MHQ$Pheno6 == 1 | MHQ$Pheno6 == 999) & MHQ$BINARY_Duration.of.worst.depression == 0) ~ 0,
  TRUE ~ 999
  )
  
#Phenotype 16: (Cardinal symptoms + number of episodes) + 5 or more symptoms
MHQ$Pheno16 <- case_when(
  MHQ$Pheno6 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno6 == 0 | ((MHQ$Pheno6 == 1 | MHQ$Pheno6 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

################################################################################################################################################################################
###### Cardinal symptoms + 3 extra components: Next set of phenotypes are a subset of the previous phenotypes as they endorse a further component of MDD      ##################
################################################################################################################################################################################

#Phenotype 17: (Cardinal symptoms + impairment + frequency of days during episode) + duration of episode
MHQ$Pheno17 <- case_when(
  MHQ$Pheno8 == 1 & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 1 ~ 1,
  MHQ$Pheno8 == 0 | ((MHQ$Pheno8 == 1 | MHQ$Pheno8 == 999) & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 18: (Cardinal symptoms + impairment + frequency of days during episode) + number of episodes
MHQ$Pheno18 <- case_when(
  MHQ$Pheno7 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno7 == 0 | ((MHQ$Pheno7 == 1 | MHQ$Pheno7 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 19: (Cardinal symptoms + impairment + frequency of days during episode) + 5 or more symptoms
MHQ$Pheno19 <- case_when(
  MHQ$Pheno10 == 1 & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 1 ~ 1,
  MHQ$Pheno10 == 0 | ((MHQ$Pheno10 == 1 | MHQ$Pheno10 == 999) & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 20: (Cardinal symptoms + frequency of days during episode + duration of episode) + number of episodes
MHQ$Pheno20 <- case_when(
  MHQ$Pheno11 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno11 == 0 | ((MHQ$Pheno11 == 1 | MHQ$Pheno11 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 21: (Cardinal symptoms + frequency of days during episode + duration of episode) + 5 or more symptoms
MHQ$Pheno21 <- case_when(
  MHQ$Pheno15 == 1 & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 1 ~ 1,
  MHQ$Pheno15 == 0 | ((MHQ$Pheno15 == 1 | MHQ$Pheno15 == 999) & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 0) ~ 0,
  TRUE ~ 999
  )
  
#Phenotype 22: (Cardinal symptoms + frequency of days during episode + number of episodes) + 5 or more symptoms
MHQ$Pheno22 <- case_when(
  MHQ$Pheno13 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno13 == 0 | ((MHQ$Pheno13 == 1 | MHQ$Pheno13 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 23: (Cardinal symptoms + duration of episode + number of episodes) + 5 or more symptoms
MHQ$Pheno23 <- case_when(
  MHQ$Pheno15 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno15 == 0 | ((MHQ$Pheno15 == 1 | MHQ$Pheno15 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 24: (Cardinal symptoms + number of episodes + duration of episode) + impairment
MHQ$Pheno24 <- case_when(
  MHQ$Pheno8 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno8 == 0 | ((MHQ$Pheno8 == 1 | MHQ$Pheno8 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 25: (Cardinal symptoms + number of episodes + 5 or more symptoms) + impairment
MHQ$Pheno25 <- case_when(
  MHQ$Pheno10 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno10 == 0 | ((MHQ$Pheno10 == 1 | MHQ$Pheno10 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )
  
#Phenotype 26: (Cardinal symptoms + 5 or more symptoms + impairment) + duration of episode
MHQ$Pheno26 <- case_when(
  MHQ$Pheno10 == 1 & MHQ$BINARY_Duration.of.worst.depression == 1 ~ 1,
  MHQ$Pheno10 == 0 | ((MHQ$Pheno10 == 1 | MHQ$Pheno10 == 999) & MHQ$BINARY_Duration.of.worst.depression == 0) ~ 0,
  TRUE ~ 999
  )

################################################################################################################################################################################
###### Cardinal symptoms + 4 extra components: Next set of phenotypes are a subset of the previous phenotypes as they endorse a further component of MDD      ##################
################################################################################################################################################################################

#Phenotype 27: (Cardinal symptoms + impairment + frequency of days during episode + duration of episode) + number of episodes
MHQ$Pheno27 <- case_when(
  MHQ$Pheno17 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno17 == 0 | ((MHQ$Pheno17 == 1 | MHQ$Pheno17 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 28: (Cardinal symptoms + impairment + frequency of days during episode + duration of episode) + 5 or more symptoms
MHQ$Pheno28 <- case_when(
  MHQ$Pheno26 == 1 & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 1 ~ 1,
  MHQ$Pheno26 == 0 | ((MHQ$Pheno26 == 1 | MHQ$Pheno26 == 999) & MHQ$BINARY_Frequency.of.depressed.days.during.worst.episode.of.depression == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 29: (Cardinal symptoms + frequency of days during episode + duration of episode + number of episodes) + 5 or more symptoms
MHQ$Pheno29 <- case_when(
  MHQ$Pheno21 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno21 == 0 | ((MHQ$Pheno21 == 1 | MHQ$Pheno21 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 30: (Cardinal symptoms + frequency of days during episode + number of episodes + 5 or more symptoms) + impairment
MHQ$Pheno30 <- case_when(
  MHQ$Pheno19 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno19 == 0 | ((MHQ$Pheno19 == 1 | MHQ$Pheno19 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Phenotype 31: (Cardinal symptoms + duration of episode + number of episodes + 5 or more symptoms) + impairment
MHQ$Pheno31 <- case_when(
  MHQ$Pheno26 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno26 == 0 | ((MHQ$Pheno26 == 1 | MHQ$Pheno26 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

################################################################################################################################################################################
######################################################################## Everything is endorsed ################################################################################
################################################################################################################################################################################

#Phenotype 32: ALL THE THINGS!
MHQ$Pheno32 <- case_when(
  MHQ$Pheno28 == 1 & MHQ$BINARY_Lifetime.number.of.depressed.periods == 1 ~ 1,
  MHQ$Pheno28 == 0 | ((MHQ$Pheno28 == 1 | MHQ$Pheno28 == 999) & MHQ$BINARY_Lifetime.number.of.depressed.periods == 0) ~ 0,
  TRUE ~ 999
  )

#Select the phenotypes only
phenos <- MHQ %>% select(1,starts_with('Pheno'))

#write.csv(phenos, 'Phenotypes.csv')

phenos <- fread('Phenotypes.csv',  data.table=FALSE)

###################################################################################################################
################  GLOSSARY OF CODES - ALL FIELDS REQUIRED FOR EXCLUSION CRITERIA      #############################
###################################################################################################################
### UK Biobank Code     Descriptive                                                                             ###
### f.eid	              ID										                                           col number = 1
### f.20446.0.0		      Ever.had.prolonged.feelings.of.sadness.or.depression			   		 col number = 12963
### f.20499.0.0		      Ever.sought.or.received.professional.help.for.mental.distress	   col_number = 12998
### f.20126.0		        Bipolar.and.major.depression.status                              col_number = 7684
### f.20544.0		        Mental.health.problems.ever.diagnosed.by.a.professional 	       col_number = 13039 - 13054
### f.20002.0		        Non.cancer.illness.code.self.reported							               col_number = 5793 - 5821
### f.41202.0		        Diagnoses.main.ICD10								   	                         col_number = 271 - 650
### f.41204.0		        Diagnoses.secondary.ICD10								                         col_number = 679 - 1113
###################################################################################################################

#Read in the datasets
exclusion <- fread('exclusion_criteria.txt', data.table=FALSE)

antipsychotics <- fread('antipsychotics', data.table=FALSE)

phenos <- fread('Phenotypes.csv', data.table=FALSE)
phenos <- phenos[,-1]

#Remove NAs such that only individuals who have responded to the MHQ are retained. Sample size = 157365  
exclusion <- exclusion[!is.na(exclusion$Ever.had.prolonged.feelings.of.sadness.or.depression), ]

#Save space by overwriting old file with the condensed version
fwrite(exclusion, 'exclusion_criteria.txt')

#Create all necessary variables

#####Physician Diagnosed Disorders

# To the question: "Have you been diagnosed with one or more of the following", there are 16 possible responses (i.e. array=16,  see MHQ pdf for details). Therefore, an individual can report up to 16 diagnoses. So for each individual, there are 16 columns of data for this question, such that each column contains an integer representing a possible response (e.g. Social Phobia =1) or NA. 
# The r code is creating a column for social anxiety or social phobia by first looking at the column Ever.sought.or.received.professional.help.for.mental.distress and adding an NA if the response was NA, then looking at each array column for Mental.health.problems.ever.diagnosed.by.a.professional and looking for the integer associated with a phenotype (e.g. 1 = social anxiety). If the column does not contain NA and does contain the relevant integer, participant is coded as 1 for case, else 0. 

exclusion$SRSchizophrenia <- with(exclusion, ifelse(is.na(Ever.sought.or.received.professional.help.for.mental.distress), NA,
			       ifelse((!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.1) & Mental.health.problems.ever.diagnosed.by.a.professional.1 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.2) & Mental.health.problems.ever.diagnosed.by.a.professional.2 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.3) & Mental.health.problems.ever.diagnosed.by.a.professional.3 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.4) & Mental.health.problems.ever.diagnosed.by.a.professional.4 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.5) & Mental.health.problems.ever.diagnosed.by.a.professional.5 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.6) & Mental.health.problems.ever.diagnosed.by.a.professional.6 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.7) & Mental.health.problems.ever.diagnosed.by.a.professional.7 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.8) & Mental.health.problems.ever.diagnosed.by.a.professional.8 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.9) & Mental.health.problems.ever.diagnosed.by.a.professional.9 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.10) & Mental.health.problems.ever.diagnosed.by.a.professional.10 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.11) & Mental.health.problems.ever.diagnosed.by.a.professional.11 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.12) & Mental.health.problems.ever.diagnosed.by.a.professional.12 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.13) & Mental.health.problems.ever.diagnosed.by.a.professional.13 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.14) & Mental.health.problems.ever.diagnosed.by.a.professional.14 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.15) & Mental.health.problems.ever.diagnosed.by.a.professional.15 == 2) |
		                      (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.16) & Mental.health.problems.ever.diagnosed.by.a.professional.16 == 2), 1, 0)))

exclusion$SRPsychosisOther <- with(exclusion, ifelse(is.na(Ever.sought.or.received.professional.help.for.mental.distress), NA,
			        ifelse((!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.1) & Mental.health.problems.ever.diagnosed.by.a.professional.1 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.2) & Mental.health.problems.ever.diagnosed.by.a.professional.2 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.3) & Mental.health.problems.ever.diagnosed.by.a.professional.3 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.4) & Mental.health.problems.ever.diagnosed.by.a.professional.4 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.5) & Mental.health.problems.ever.diagnosed.by.a.professional.5 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.6) & Mental.health.problems.ever.diagnosed.by.a.professional.6 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.7) & Mental.health.problems.ever.diagnosed.by.a.professional.7 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.8) & Mental.health.problems.ever.diagnosed.by.a.professional.8 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.9) & Mental.health.problems.ever.diagnosed.by.a.professional.9 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.10) & Mental.health.problems.ever.diagnosed.by.a.professional.10 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.11) & Mental.health.problems.ever.diagnosed.by.a.professional.11 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.12) & Mental.health.problems.ever.diagnosed.by.a.professional.12 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.13) & Mental.health.problems.ever.diagnosed.by.a.professional.13 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.14) & Mental.health.problems.ever.diagnosed.by.a.professional.14 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.15) & Mental.health.problems.ever.diagnosed.by.a.professional.15 == 3) |
		                       (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.16) & Mental.health.problems.ever.diagnosed.by.a.professional.16 == 3), 1, 0)))

# The next line of code is creating a column for SRPsychosisAny based on the last two columns created, i.e. if participant has scz OR psychosis(other), they will be a case in this column. 

exclusion$SRPsychosisAny <- with(exclusion, ifelse(is.na(Ever.sought.or.received.professional.help.for.mental.distress), NA,
					 ifelse((!is.na(SRSchizophrenia) & SRSchizophrenia == 1) | (!is.na(SRPsychosisOther) & SRPsychosisOther == 1), 1, 0)))

exclusion$SRManiaBIP <- with(exclusion, ifelse(is.na(Ever.sought.or.received.professional.help.for.mental.distress), NA,
			  ifelse((!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.1) & Mental.health.problems.ever.diagnosed.by.a.professional.1 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.2) & Mental.health.problems.ever.diagnosed.by.a.professional.2 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.3) & Mental.health.problems.ever.diagnosed.by.a.professional.3 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.4) & Mental.health.problems.ever.diagnosed.by.a.professional.4 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.5) & Mental.health.problems.ever.diagnosed.by.a.professional.5 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.6) & Mental.health.problems.ever.diagnosed.by.a.professional.6 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.7) & Mental.health.problems.ever.diagnosed.by.a.professional.7 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.8) & Mental.health.problems.ever.diagnosed.by.a.professional.8 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.9) & Mental.health.problems.ever.diagnosed.by.a.professional.9 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.10) & Mental.health.problems.ever.diagnosed.by.a.professional.10 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.11) & Mental.health.problems.ever.diagnosed.by.a.professional.11 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.12) & Mental.health.problems.ever.diagnosed.by.a.professional.12 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.13) & Mental.health.problems.ever.diagnosed.by.a.professional.13 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.14) & Mental.health.problems.ever.diagnosed.by.a.professional.14 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.15) & Mental.health.problems.ever.diagnosed.by.a.professional.15 == 10) |
		                 (!is.na(Mental.health.problems.ever.diagnosed.by.a.professional.16) & Mental.health.problems.ever.diagnosed.by.a.professional.16 == 10), 1, 0)))

#####Interview Diagnoses At Baseline######

#Self-report diagnoses considered. 
#1289 = Schizophrenia 
#1291 = Bipolar 
#1408 = Alcohol dependency 
#1409 = Opioid dependency 
#1410 = Other dependency 

InterviewExclusion <- apply(exclusion[,grep("Non.cancer.illness.code.self.reported", colnames(exclusion))], 1, function(row) c("1289","1291","1408","1409","1410") %in% row)

#This takes the array created in the previous step and asks if a TRUE exists at least once in the 5 columns - if this is so, one of the self-reported illnesses has been endorsed. Resulted in 586 cases.
ExclusionComb <- apply(InterviewExclusion, 2, function(col) TRUE %in% col)

# takes the object created in the last step and changes FALSE to NA and TRUE to 1 

exclusion$InterviewExclusion[c(ExclusionComb)] <- 1

####HES Inpatient Diagnoses######

ICDFields <- c("F10","F100","F101","F102","F103","F104","F105","F106","F107","F108","F109",
"F11","F110","F111","F112","F113","F114","F115","F116","F117","F118","F119",
"F12","F120","F121","F122","F123","F124","F125","F126","F127","F128","F129",
"F13","F130","F131","F132","F133","F134","F135","F136","F137","F138","F139",
"F14","F140","F141","F142","F143","F144","F145","F146","F147","F148","F149",
"F15","F150","F151","F152","F153","F154","F155","F156","F157","F158","F159",
"F16","F160","F161","F162","F163","F164","F165","F166","F167","F168","F169",
"F18","F180","F181","F182","F183","F184","F185","F186","F187","F188","F189",
"F19","F190","F191","F192","F193","F194","F195","F196","F197","F198","F199",
"F20","F200","F201","F202","F203","F204","F205","F206","F207","F208","F209",
"F21",
"F22","F228","F229",
"F23","F230","F231","F230","F231","F232","F233","F238","F239",
"F24",
"F250","F251","F252","F258","F259",
"F28",
"F29",
"F300","F301","F302","F308","F309",
"F310","F311","F312","F313","F314","F315","F316","F317","F318","F319")

HES <- apply(exclusion[,grep("ICD10", colnames(exclusion))], 1, function(row) ICDFields %in% row)

# create a numeric object

HES_Combined <- numeric()

for(indiv in 1:dim(exclusion)[1]){
    HES_Combined[indiv]<-ifelse(sum(HES[(((indiv-1)*length(ICDFields))+1):(indiv*length(ICDFields))]) == 0, 0, 1)
}

exclusion$HES[c(HES_Combined==1)] <- 1

#####Drug-derived Diagnoses######

antidepcodes <- c("1140879616","1140921600","1140879540","1140867878","1140916282","1140909806","1140867888","1141152732","1141180212","1140879634","1140867876","1140882236",
                "1141190158","1141200564","1140867726","1140879620","1140867818","1140879630","1140879628","1141151946","1140867948","1140867624","1140867756","1140867884",
                "1141151978","1141152736","1141201834","1140867690","1140867640","1140867920","1140867850","1140879544","1141200570","1140867934","1140867758","1140867914",
                "1140867820","1141151982","1140882244","1140879556","1140867852","1140867860","1140917460","1140867938","1140867856","1140867922","1140910820","1140882312",
                "1140867944","1140867784","1140867812","1140867668","1140867940")

DrugDepression <- apply(exclusion[,grep("Treatment.medication.code", colnames(exclusion))], 1, function(row) antidepcodes %in% row)

DrugDepression_Combined <- numeric()

for(indiv in 1:dim(exclusion)[1]){
  DrugDepression_Combined[indiv] <- ifelse(sum(DrugDepression[(((indiv-1)*length(antidepcodes))+1):(indiv*length(antidepcodes))]) == 0, 0, 1)
}

exclusion$DrugDepression[DrugDepression_Combined==1] <- 1 

###Antipsychotics
colnames(antipsychotics)[1] <- "ID"
## List of codes for antipsychotics
exclusion <- merge(x=exclusion, y=antipsychotics, by="ID")

###Mania and mood stabilisers
bpdcodes <- c('1140867504','1140867494','1140867498','1140867520','1140917270','1140867490')

Lithium <- apply(exclusion[,grep("Treatment.medication.code", colnames(exclusion))], 1, function(row) bpdcodes %in% row)

DrugBipolar_Combined <- numeric()

for(indiv in 1:dim(exclusion)[1]){
    DrugBipolar_Combined[indiv] <- ifelse(sum(Lithium[(((indiv-1)*length(bpdcodes))+1):(indiv*length(bpdcodes))]) == 0, 0, 1)
}

exclusion$Lithium[DrugBipolar_Combined==1] <- 1 

exclusion$BPDrug <- case_when(
exclusion$Lithium == 1 & (exclusion$DrugDepression == 0 | is.na(exclusion$DrugDepression)) ~ 1,
TRUE ~ 0
) 

###Smith Depression Diagnosis 2013

# if column for Bipolar.and.major.depression.status is NA, print NA. Else, if entry is not NA and the score is either 1 or 2, mark individual as 1 for case, else 0 for control. The UKB coding for this variable is:
# 0	No Bipolar or Depression
# 1	Bipolar I Disorder
# 2 Bipolar II Disorder
# 3	Probable Recurrent major depression (severe)
# 4	Probable Recurrent major depression (moderate)
# 5	Single Probable major depression episode

exclusion$SmithMood <- with(exclusion, ifelse(is.na(Bipolar.and.major.depression.status.0.0), NA,
			 ifelse(!is.na(Bipolar.and.major.depression.status.0.0) & (Bipolar.and.major.depression.status.0.0 == 1) |
			 !is.na(Bipolar.and.major.depression.status.0.0) & (Bipolar.and.major.depression.status.0.0 == 2), 1, 0)))

#Extract relevant columns only and remove rest
relevant <- c('ID', 'InterviewExclusion','SRPsychosisAny','SRManiaBIP', 'Antipsychotics', 'HES','SmithMood', 'BPDrug')
exclusion <- exclusion[,relevant]

#Make a field such that it is the total of all exclusion criteria - anything above 0 equates to a removal - ignore NAs in this function, i.e. treated as 0.
exclusion$total <- rowSums(exclusion[,2:8], na.rm=TRUE)

#Innerjoin to phenotypes dataset
all <- merge(x=phenos, y=exclusion, by="ID")

#Remove people who haven't endorsed one of the exclusion criteria
all1 <- subset(all, total == 0)

#Remove columns relating to exclusion criteria
all1 <- all1[,-c(34:41)]

write.table(all1, "Phenotypes_with_scz_bip_sa_rm_v2.csv", sep='\t')