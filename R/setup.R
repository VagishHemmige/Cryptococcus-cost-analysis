# This file contains definitions of comorbidities, "magic numbers", etc.

# Libraries necessary for the analysis
library(tidyverse)
library(usRds)
library(strobe)

#Constants
censor_date<-as.Date("2022-01-01")
minimum_followup<-1 #Minimum followup in days

#Create list of ICD9 and ICD10 codes for cryptococcus
cryptococcus_ICD_list<-c("1175", "3210", "B450", "B451", "B452", "B453", "B457", "B458", "B459", "B45")

comorbidity_ICD_list<-list()
comorbidity_ICD_list[["cirrhosis"]]<-c("5712", # Alcoholic cirrhosis of liver
                                       "5715", # Cirrhosis of liver without mention of alcohol
                                       "5716", # Biliary cirrhosis
                                       "5722", # Hepatic encephalopathy
                                       "5723", # Portal hypertension
                                       "5724", # Hepatorenal syndrome
                                       "5728", #Other sequelae of chronic liver disease# 
                                       "K7030", # Alcoholic cirrhosis of liver with ascites
                                       "K7031", # Alcoholic cirrhosis of liver without ascites
                                       "K7460", # Unspecified cirrhosis of liver
                                       "K7469", # Other cirrhosis of liver
                                       "K743", # Primary biliary cirrhosis
                                       "K744", # Secondary biliary cirrhosis
                                       "K745", # Biliary cirrhosis, unspecified
                                       "K7290", # Hepatic failure, unspecified 
                                       "K7291", # Acute and subacute hepatic failure with coma
                                       "K766", # Portal hypertension
                                       "K767") # Hepatorenal syndrome

comorbidity_ICD_list[["CMV"]] <- c(
  "0785",   # Cytomegaloviral disease (ICD-9)
  "B250",   # CMV pneumonitis
  "B251",   # CMV hepatitis
  "B252",   # CMV pancreatitis
  "B258",   # Other CMV diseases
  "B259",   # CMV disease, unspecified
  "B2710",  # Cytomegaloviral mononucleosis without complication
  "B2711"   # Cytomegaloviral mononucleosis with complication
)

