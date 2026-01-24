#Create a cohort of kidney transplant patients with cryptococcus matched to controls without cryptococcus

source("R/setup.R")

#Import core demographics from "patients" file
patients_raw<-usRds::load_usrds_file("patients")%>%
  select(-ZIPCODE) #This is ZIP code at time of USRDS initiation, but we want at time of crypto dx

#Initialize a flowchart cohort
patients_clean<-patients_raw%>%
  as_fc(label="Patients in USRDS")%>%
  
  
  fc_filter(TOTTX>0, 
            label="Prior transplant", 
            label_exc = "Excluded: No prior transplant", 
            show_exc = TRUE)%>%
  
  fc_filter(TX1DATE<as.Date("2021-01-01"), 
            label="Transplant prior to 2021", 
            label_exc = "Excluded: Transplant 2021 or later", 
            show_exc = TRUE)



#Create list of USRDS ids for patients who have undergone transplant
transplant_id_list<-patients_clean$data%>%
  pull(USRDS_ID)

# We now seek to determine comorbidities by using diagnosis codes from the setup.R file

comorbidity_diagnosis_date<-list()

# Combine all ICD codes from all comorbidities into one list 
comorbidity_ICD_combined_list<-unlist(comorbidity_ICD_list, use.names=FALSE)

#Scrape files for any comorbidity claim
comorbidity_claims_df<-bind_rows(get_IN_ICD(icd_codes = comorbidity_ICD_combined_list, 
                                            years = 2006:2021, 
                                            usrds_ids = transplant_id_list ),
                                 get_PS_ICD(icd_codes = comorbidity_ICD_combined_list, 
                                            years = 2006:2021, 
                                            usrds_ids = transplant_id_list )%>%rename(CODE=DIAG))%>%
  arrange(USRDS_ID, CLM_FROM)

#Create comorbidity_diagnosis_date data frame
for (comorbidity in names(comorbidity_ICD_list)){
  
  comorbidity_diagnosis_date[[comorbidity]]<-comorbidity_claims_df%>%
    filter(CODE %in% comorbidity_ICD_list[[comorbidity]])%>%
    establish_dx_date(diagnosis_established = comorbidity)
}

#Load Medicare coverage history for all patients with transplant
medicare_history<-load_usrds_file("payhist",
                                  usrds_ids = transplant_id_list)%>%
  arrange(USRDS_ID, BEGDATE)%>%
  group_by(USRDS_ID)%>%
  mutate(lag_ENDDATE=lag(ENDDATE))%>%
  mutate(gap=as.numeric(BEGDATE-lag_ENDDATE))%>%
  arrange(desc(gap))

#Confirm no gaps (gap should always be 1 or missing)
table(medicare_history$gap)

#Add cryptococcus diagnosis date to patients_clean$data
#First, format a df with the cryptococcus dx
cryptococcus_df<-comorbidity_diagnosis_date$cryptococcus%>%
  select(-diagnosis)%>%
  rename(cryptococcus_dx_date=date_established)



#Join cryptococcus date to fc cohort
patients_clean$data<-left_join(patients_clean$data, 
                               cryptococcus_df)%>%
  mutate(cryptococcus_case=ifelse(is.na(cryptococcus_dx_date), "Potential control", "Case"))

#Continue to create analytic cohort
patients_merged<-patients_clean%>%
  
  #Remove patients with a diagnosis of cryptococcus prior to transplant
  fc_filter(cryptococcus_dx_date>TX1DATE | is.na(cryptococcus_dx_date), 
            label="No cryptococcus dx prior to transplant", 
            label_exc = "Excluded: cryptococcus dx prior to transplant", 
            show_exc = TRUE)%>%
  
  fc_split(cryptococcus_case)
  
  #Check Medicare coverage for 365-day lookback period from day of first episode of cryptococcus
patients_merged$data<-patients_merged$data%>%
  verify_medicare_primary(index_date = "cryptococcus_dx_date",
                          lookback_days = 365)%>%
  mutate(medicare_primary_TF=ifelse(cryptococcus_case=="Potential control", TRUE, medicare_primary_TF))
  
patients_merged2<-patients_merged%>%
  
  fc_filter(medicare_primary_TF==TRUE, 
            label = "365+ days of Medicare primary coverage\nprior to first cryptococcus claim", 
            label_exc = "Excluded: Fewer than 365 days of coverage",
            show_exc = TRUE)

patients_merged2$data<-patients_merged2$data%>%
  select(-medicare_primary_TF)%>%
  
  #Prepare data for cohort initialization
  mutate(terminal_date=coalesce(ENDDATE, censor_date))

patients_merged2<-patients_merged2%>%
  fc_filter((terminal_date - cryptococcus_dx_date >=minimum_followup) | is.na(cryptococcus_dx_date), 
            label = "Minimum followup exceeded", 
            label_exc= "Excluded: Minimum follow-up threshold not met",
            show_exc = TRUE)

  
patients_merged2%>%
  fc_draw()


#Now we need to construct the time-varying data set

#Ungroup
initial_cohort<-patients_merged2$data%>%
  ungroup()%>%

#Cases join when they experience cryptococcus
#Controls start on date of first transplant
  mutate(
    cohort_join_date = coalesce(
      as.Date(cryptococcus_dx_date),
      as.Date(TX1DATE)
    )
  )

#Initialize cohort
prematching_cohort<-create_usrds_cohort(df=initial_cohort,
                            start_date = "cohort_join_date",
                            end_date = "terminal_date")%>%
  
  # Add cirrhosis
  add_cohort_covariate(covariate_data_frame=comorbidity_diagnosis_date[["cirrhosis"]],
                       covariate_date="date_established",
                       covariate_variable_name="cirrhosis")%>%
  
  # Add CMV
  add_cohort_covariate(covariate_data_frame=comorbidity_diagnosis_date[["CMV"]],
                       covariate_date="date_established",
                       covariate_variable_name="CMV")%>%
  
  # Add diabetes
  add_cohort_covariate(covariate_data_frame=comorbidity_diagnosis_date[["Diabetes"]],
                       covariate_date="date_established",
                       covariate_variable_name="diabetes")%>%
  
  # Add HIV
  add_cohort_covariate(covariate_data_frame=comorbidity_diagnosis_date[["HIV"]],
                       covariate_date="date_established",
                       covariate_variable_name="HIV")%>%
  
  # Add Medicare current coverage
  add_cohort_covariate(covariate_data_frame=medicare_history,
                       covariate_date="BEGDATE",
                       covariate_variable_name="current_medicare_coverage",
                       covariate_value = "PAYER"
                       )
