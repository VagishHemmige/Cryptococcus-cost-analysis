#Create a cohort of kidney transplant patients with cryptococcus

library(tidyverse)
library(usRds)
library(strobe)

#Import core demographics from "patients" file
patients_raw<-usRds::load_usrds_file("patients")%>%
  select(-ZIPCODE) #This is ZIP code at time of USRDS initiation, but we want at time of crypto ds

#Initialize a STROBE cohort
patients_clean<-strobe_initialize(patients_raw, "Initial USRDS cohort")

#Remove patients who have never had a transplant
patients_clean<-patients_clean%>%
  strobe_filter("TOTTX>0", "Prior transplant", "Excluded: No prior transplant")

#Create list of USRDS ids for patients who have undergone transplant
transplant_id_list<-patients_clean%>%
  pull(USRDS_ID)
#Create list of ICD9 and ICD10 codes for cryptococcus
cryptococcus_ICD_list<-c("1175", "3210", "B450", "B451", "B452", "B453", "B457", "B458", "B459", "B45")

#Obtain cryptococcus claims from the IN and PS files
cryptococcus_IN_df<-get_IN_ICD(icd_codes = cryptococcus_ICD_list, 
                               years = 2006:2021, 
                               usrds_ids = transplant_id_list )%>%
  select(USRDS_ID, CLM_FROM, DIAG)

cryptococcus_PS_df<-get_PS_ICD(icd_codes = cryptococcus_ICD_list, 
                               years = 2006:2021, 
                               usrds_ids = transplant_id_list )%>%
  select(USRDS_ID, CLM_FROM, DIAG)%>%
  rename(CODE=DIAG)

#Combine cryptococcus from IN and PS files
cryptococcus_df<-bind_rows(cryptococcus_IN_df, cryptococcus_PS_df)%>%

  #sort by ID and date
  arrange(USRDS_ID, CLM_FROM)%>%

  #Keep first date from each patient
  group_by(USRDS_ID) %>%
  slice(1) %>%
  ungroup()%>%
  
  #Keep ID and first cryptococcus date for each patient
  rename(first_cryptococcus_date=CLM_FROM)%>%
  select(USRDS_ID, first_cryptococcus_date)


#Join cryptococcus date to patient file
patients_clean<-left_join(patients_clean, 
                          cryptococcus_df)%>%

  #Filter by whether or not the patient had a claim in the study period  
  strobe_filter("!is.na(first_cryptococcus_date)", 
                "At least one cryptococcus claim 2006-2021", 
                "No cryptococcus claim 2006-2021")%>%

  #Filter by whether or not the first claim was subsequent to the first kidney transplant date
  strobe_filter("first_cryptococcus_date>TX1DATE", 
                "Cryptococcus subsequent to first transplant ", 
                "Excluded: Cryptococcus prior to first transplant")

#Load medicare coverage history for patients still in the cohort.  We only want to keep
#patients with Medicare primary because they will have the appropriate types of claims

eligible_payers <- c(
  "MEDICARE FFS PRIMARY PAY, FOR BOTH PART A AND PART B",
  "MEDICARE FFS PRIMARY PAY, FOR OTHER"
)

#Open medicare history file
medicare_history<-load_usrds_file("payhist", 
                usrds_ids = patients_clean$USRDS_ID)%>%
  filter(PAYER %in% eligible_payers) %>%
  select(USRDS_ID, BEGDATE, ENDDATE)%>%
  arrange(USRDS_ID, BEGDATE)%>%


  #Combine lines where there is no gap between the lines and they are the same patient
  group_by(USRDS_ID) %>%
  arrange(BEGDATE, .by_group = TRUE) %>%
  mutate(
    prev_end = lag(ENDDATE),
    # start a new group if there is a gap
    new_group = if_else(
      is.na(prev_end) | BEGDATE > prev_end + days(1),
      1L,
      0L
    ),
    group_id = cumsum(new_group)
  ) %>%
  group_by(USRDS_ID, group_id) %>%
  summarise(
    BEGDATE = min(BEGDATE),
    ENDDATE = max(ENDDATE),
    .groups = "drop"
  )%>%
  select(-group_id)

#Join patient cohort to medicare history data 
patients_clean<-left_join(patients_clean,
          medicare_history,
          join_by(USRDS_ID, between(first_cryptococcus_date, BEGDATE, ENDDATE))
          )

#Filter by FFS coverage on day of claim through 365 days prior
patients_clean<-patients_clean%>%
  strobe_filter("!is.na(BEGDATE)", 
                "Medicare is primary coverage on date of claim", 
                "Other coverage on date of claim")%>%
  strobe_filter("first_cryptococcus_date-BEGDATE>=365", 
                "365+ days of coverage prior to first cryptococcus claim", 
                "Fewer than 365 days of coverage")

#Load ZIP codes
ZIP_code_df<-load_usrds_file("residenc", 
                             usrds_ids = patients_clean$USRDS_ID)%>%
  select(USRDS_ID, ZIPCODE, BEGRES, ENDRES)

#Join patient cohort to ZIP code data 
patients_clean<-left_join(patients_clean,
                          ZIP_code_df,
                          join_by(USRDS_ID, between(first_cryptococcus_date, BEGRES, ENDRES)))

#Load transplant data, and add a date for the next transplant (missing set to 1-1-2100)
transplant_df<-load_usrds_file("tx", usrds_ids = patients_clean$USRDS_ID)%>%
  select(USRDS_ID, TDATE, FAILDATE, PROVUSRD, YEAR)%>%
  arrange(USRDS_ID, TDATE)%>%
  group_by(USRDS_ID)%>%
  mutate(TDATE_lead=lead(TDATE))%>%
  ungroup()%>%
  mutate(TDATE_lead=coalesce(TDATE_lead, as.Date("2100-01-01")))

#Load facility data
facility <- load_usrds_file("facility")%>%
  select(ZIPCODE, PROVUSRD, FS_YEAR)%>%
  rename(YEAR=FS_YEAR)%>%
  mutate(YEAR=as.double(YEAR))

#Interval merge

transplant_joined<-left_join(transplant_df, facility, join_by(PROVUSRD, closest(x$YEAR<=y$YEAR )))

#Plot strobe diagram
plot_strobe_diagram(incl_fontsize = 150, 
                    excl_fontsize = 150,
                    incl_width_min = 50, 
                    excl_width_min = 50)

