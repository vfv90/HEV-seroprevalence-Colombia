#Title: "Seroprevalence of Hepatitis E virus in children and adolescents living in urban Bogotá: an explorative cross-sectional study" 
#Authors: "Nathalie Fernandez_&_Barbora Kessel"
#Date: 01.06.2022
#R version: 4.1.0

# Packages
pacman::p_load("tidyverse", "MASS", "vcd", "asht", "irrCAC", "stats", "binom")

# Read database
mydata <- read.csv("database_HEV_manuscript.csv", header = T, sep = "," ,stringsAsFactors = F)

##Part I. Description of characteristics of participants (Table 1)
#age: This variable was not included in the data for data protection reasons

#age groups
xtabs(~group_age,data=mydata) #all participants 
xtabs(~group_age,data=mydata)/sum(xtabs(~ group_age,data=mydata))
xtabs(~Mikrogen_results + group_age,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + group_age,data=mydata)/rowSums(xtabs(~Mikrogen_results + group_age,data=mydata)) 
aux<-xtabs(~Mikrogen_results + group_age,data=mydata)
fisher.test(aux) #Fisher test

#sex
xtabs(~sex,data=mydata) #all participants 
xtabs(~sex,data=mydata)/sum(xtabs(~ sex,data=mydata))
xtabs(~Mikrogen_results + sex,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + sex,data=mydata)/rowSums(xtabs(~Mikrogen_results + sex,data=mydata)) 
aux<-xtabs(~Mikrogen_results + sex,data=mydata)
fisher.test(aux) #Fisher test

#School type
xtabs(~school_type,data=mydata) #all participants
xtabs(~school_type,data=mydata)/sum(xtabs(~ school_type,data=mydata))  
xtabs(~Mikrogen_results + school_type,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + school_type,data=mydata)/rowSums(xtabs(~Mikrogen_results + school_type,data=mydata))
aux<-xtabs(~Mikrogen_results + school_type,data=mydata)
fisher.test(aux) #Fisher test

#Social security affiliation
xtabs(~subsidized,data=mydata) #all participants 
xtabs(~subsidized,data=mydata)/sum(xtabs(~subsidized,data=mydata)) 
xtabs(~Mikrogen_results + subsidized,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + subsidized,data=mydata)/rowSums(xtabs(~Mikrogen_results + subsidized,data=mydata))
aux<-xtabs(~Mikrogen_results + subsidized,data=mydata)
fisher.test(aux) #Fisher test

#Socioeconomical strata
xtabs(~se_stratum,data=mydata) #all participants 
xtabs(~se_stratum,data=mydata)/sum(xtabs(~se_stratum,data=mydata))
xtabs(~Mikrogen_results + se_stratum,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + se_stratum,data=mydata)/rowSums(xtabs(~Mikrogen_results + se_stratum,data=mydata))
aux<-xtabs(~Mikrogen_results + se_stratum,data=mydata[mydata$se_stratum!=8,])
fisher.test(aux) #Fisher test

#Income
xtabs(~income,data=mydata) #all participants 
xtabs(~income,data=mydata)/sum(xtabs(~income,data=mydata))
xtabs(~Mikrogen_results + income,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + income,data=mydata)/rowSums(xtabs(~Mikrogen_results + income,data=mydata))
aux<-xtabs(~Mikrogen_results + income,data=mydata[mydata$income<4,])
fisher.test(aux) #Fisher test

#Country
xtabs(~country,data=mydata) #all participants 
xtabs(~country,data=mydata)/sum(xtabs(~country,data=mydata))
xtabs(~Mikrogen_results + country,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + country,data=mydata)/rowSums(xtabs(~Mikrogen_results + country,data=mydata))
aux<-xtabs(~Mikrogen_results + country,data=mydata)
fisher.test(aux) #Fisher test

#Occupation mother
xtabs(~mother_occupation,data=mydata) #all participants 
xtabs(~mother_occupation,data=mydata)/sum(xtabs(~mother_occupation,data=mydata))
xtabs(~Mikrogen_results + mother_occupation,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + mother_occupation,data=mydata)/rowSums(xtabs(~Mikrogen_results + mother_occupation,data=mydata))
aux<-xtabs(~Mikrogen_results + mother_occupation,data=mydata)
fisher.test(aux) #Fisher test

#Occupation father
xtabs(~father_occupation,data=mydata) #all participants 
xtabs(~father_occupation,data=mydata)/sum(xtabs(~father_occupation,data=mydata))
xtabs(~Mikrogen_results + father_occupation,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + father_occupation,data=mydata)/rowSums(xtabs(~Mikrogen_results + father_occupation,data=mydata))
aux<-xtabs(~Mikrogen_results + father_occupation,data=mydata)
fisher.test(aux) #Fisher test

#Contact with pigs
xtabs(~contact_pigs,data=mydata) #all participants 
xtabs(~contact_pigs,data=mydata)/sum(xtabs(~contact_pigs,data=mydata))
xtabs(~Mikrogen_results + contact_pigs,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + contact_pigs,data=mydata)/rowSums(xtabs(~Mikrogen_results + contact_pigs,data=mydata))
aux<-xtabs(~Mikrogen_results + contact_pigs,data=mydata[mydata$contact_pigs!=3,])
fisher.test(aux) #Fisher test

#Pork consumption
xtabs(~pork_consumption,data=mydata) #all participants 
xtabs(~pork_consumption,data=mydata)/sum(xtabs(~pork_consumption,data=mydata))
xtabs(~Mikrogen_results + pork_consumption,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + pork_consumption,data=mydata)/rowSums(xtabs(~Mikrogen_results + pork_consumption,data=mydata))
aux<-xtabs(~Mikrogen_results + pork_consumption,data=mydata)
fisher.test(aux) #Fisher test

#Pipped water
xtabs(~pipped_water,data=mydata) #all participants 
xtabs(~pipped_water,data=mydata)/sum(xtabs(~pipped_water,data=mydata))
xtabs(~Mikrogen_results + pipped_water,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + pipped_water,data=mydata)/rowSums(xtabs(~Mikrogen_results + pipped_water,data=mydata))
aux<-xtabs(~Mikrogen_results + pipped_water,data=mydata)
fisher.test(aux) #Fisher test

#Hand washing after the toilet
xtabs(~hand_washing,data=mydata) #all participants 
xtabs(~hand_washing,data=mydata)/sum(xtabs(~hand_washing,data=mydata))
xtabs(~Mikrogen_results + hand_washing,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + hand_washing,data=mydata)/rowSums(xtabs(~Mikrogen_results + hand_washing,data=mydata))
aux<-xtabs(~Mikrogen_results + hand_washing,data=mydata[mydata$hand_washing<5,])
fisher.test(aux) #Fisher test

#Hand washing befoer eating
xtabs(~washing_pre_food,data=mydata) #all participants 
xtabs(~washing_pre_food,data=mydata)/sum(xtabs(~washing_pre_food,data=mydata))
xtabs(~Mikrogen_results + washing_pre_food,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + washing_pre_food,data=mydata)/rowSums(xtabs(~Mikrogen_results + washing_pre_food,data=mydata))
aux<-xtabs(~Mikrogen_results + washing_pre_food,data=mydata[mydata$washing_pre_food<5,])
fisher.test(aux) #Fisher test

#Recreational swimming
xtabs(~recreational,data=mydata) #all participants 
xtabs(~recreational,data=mydata)/sum(xtabs(~recreational,data=mydata))
xtabs(~Mikrogen_results + recreational,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + recreational,data=mydata)/rowSums(xtabs(~Mikrogen_results + recreational,data=mydata))
aux<-xtabs(~Mikrogen_results + recreational,data=mydata)
fisher.test(aux) #Fisher test

#Transfusion
xtabs(~transfusion,data=mydata) #all participants 
xtabs(~transfusion,data=mydata)/sum(xtabs(~transfusion,data=mydata))
xtabs(~Mikrogen_results + transfusion,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + transfusion,data=mydata)/rowSums(xtabs(~Mikrogen_results + transfusion,data=mydata))
aux<-xtabs(~Mikrogen_results + transfusion,data=mydata)
fisher.test(aux) #Fisher test

#Jaundice
xtabs(~Ictericia,data=mydata) #all participants 
xtabs(~Ictericia,data=mydata)/sum(xtabs(~Ictericia,data=mydata))
xtabs(~Mikrogen_results + Ictericia,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + Ictericia,data=mydata)/rowSums(xtabs(~Mikrogen_results + Ictericia,data=mydata))
aux<-xtabs(~Mikrogen_results + Ictericia,data=mydata[mydata$Ictericia<3,])
fisher.test(aux) #Fisher test

#Viral hepatitis diagnosis
xtabs(~DX_hepatitis,data=mydata) #all participants 
xtabs(~DX_hepatitis,data=mydata)/sum(xtabs(~DX_hepatitis,data=mydata))
xtabs(~Mikrogen_results + DX_hepatitis,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + DX_hepatitis,data=mydata)/rowSums(xtabs(~Mikrogen_results + DX_hepatitis,data=mydata))
aux<-xtabs(~Mikrogen_results + DX_hepatitis,data=mydata)
fisher.test(aux) #Fisher test

# Hepatitis symptoms
xtabs(~hep_symptoms,data=mydata) #all participants 
xtabs(~hep_symptoms,data=mydata)/sum(xtabs(~hep_symptoms,data=mydata))
xtabs(~Mikrogen_results + hep_symptoms,data=mydata) # reactive and non-ractive
xtabs(~Mikrogen_results + hep_symptoms,data=mydata)/rowSums(xtabs(~Mikrogen_results + hep_symptoms,data=mydata))
aux<-xtabs(~Mikrogen_results + hep_symptoms,data=mydata)
fisher.test(aux) #Fisher test

##Part II. Seroprevalence of HEV by the two assays
#Mikrogen
#positivity by Mikrogen
xtabs(~Mikrogen_results,data=mydata)
xtabs(~Mikrogen_results,data=mydata)/sum(xtabs(~Mikrogen_results,data=mydata))

#seroprevalence using Mikrogen
binom::binom.prop.test(x=sum(mydata$Mikrogen_results),n=nrow(mydata))

#Adjustment seroprevalence Mikrogen
asht::prevSeSp(3/263, 263, 88/89, 89, 132/134, 134)

#Axiom
#positivity by Axiom
xtabs(~Axiom_results,data=mydata)
xtabs(~Axiom_results,data=mydata)/sum(xtabs(~Axiom_results,data=mydata))

#seroprevalence using Axiom
binom::binom.prop.test(x=sum(mydata$Axiom_results),n=nrow(mydata))

#Adjustment seroprevalence Axiom
asht::prevSeSp(3/263, 263, 90/91, 91, 414/418, 418)

##Part III. Agreement between assays (Table 2)
#2x2 table
xtabs(~ Mikrogen_results+Axiom_results, data=mydata)

#Range Mikrogen
range(mydata$Mikrogen_S.CO[mydata$Mikrogen_results==1])
range(mydata$Mikrogen_S.CO[mydata$Mikrogen_results==0])

#Range Axiom
range(mydata$Axiom_S.CO[mydata$Axiom_results==1])
range(mydata$Axiom_S.CO[mydata$Axiom_results==0])

#Fleiss's kappa
irrCAC::fleiss.kappa.raw(mydata[,c("Mikrogen_results","Axiom_results")]) #coeff.val (conf.int) reported as text

##Part IV. Details of positive participants (Table 3)
mydata[mydata$Mikrogen_results==1,c("sex","group_age",
                                    "contact_pigs","pork_consumption",
                                    "pipped_water","hand_washing",
                                    "washing_pre_food","recreational","Axiom_S.CO",
                                    "Mikrogen_S.CO")]

##Part V. IgM seroprevalence (Mikrogen)
#positivity
xtabs(~Mikrogen_IgM,data=mydata)
xtabs(~Mikrogen_IgM,data=mydata)/sum(xtabs(~Mikrogen_IgM,data=mydata))

#seroprevalence using Mikrogen
binom::binom.prop.test(x=sum(mydata$Mikrogen_IgM),n=nrow(mydata))

#Adjustment seroprevalence Mikrogen
asht::prevSeSp(1/263, 263, 87/89, 89, 354/359, 359)

