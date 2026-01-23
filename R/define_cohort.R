#Create a cohort of kidney transplant patients with cryptococcus

source("R/setup.R")

#Import core demographics from "patients" file
patients_raw<-usRds::load_usrds_file("patients")%>%
  select(-ZIPCODE) #This is ZIP code at time of USRDS initiation, but we want at time of crypto dx

#Initialize a STROBE cohort
patients_clean<-strobe_initialize(patients_raw, "Initial USRDS cohort")

#Remove patients who have never had a transplant
patients_clean<-patients_clean%>%
  strobe_filter("TOTTX>0", "Prior transplant", "Excluded: No prior transplant")

#Create list of USRDS ids for patients who have undergone transplant
transplant_id_list<-patients_clean%>%
  pull(USRDS_ID)

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


#Obtain cryptococcus claims from the IN and PS files
cryptococcus_IN_df<-get_IN_ICD(icd_codes = cryptococcus_ICD_list, 
                               years = 2006:2021, 
                               usrds_ids = transplant_id_list )%>%
  select(USRDS_ID, CLM_FROM, CODE)

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

#Create a cryptococcus dx date DF that uses the USRDS package expressly written for this purpose:
cryptococcus_dx_date<-bind_rows(get_IN_ICD(icd_codes = cryptococcus_ICD_list, 
                     years = 2006:2021, 
                     usrds_ids = transplant_id_list ),
          get_PS_ICD(icd_codes = cryptococcus_ICD_list, 
                     years = 2006:2021, 
                     usrds_ids = transplant_id_list )%>%rename(CODE=DIAG)
          )%>%
  establish_dx_date(diagnosis_established = "cryptococcus")


#Join cryptococcus date to patient file
patients_clean<-left_join(patients_clean, 
                          cryptococcus_df)%>%

  #Filter by whether or not the patient had a claim in the study period  
  strobe_filter("!is.na(first_cryptococcus_date)", 
                "At least one cryptococcus claim 2006-2021", 
                "Excluded: No cryptococcus claim 2006-2021")%>%

  #Filter by whether or not the first claim was subsequent to the first kidney transplant date
  strobe_filter("first_cryptococcus_date>TX1DATE", 
                "Cryptococcus subsequent to first transplant ", 
                "Excluded: Cryptococcus prior to first transplant")%>%

    
  #Filter by Medicare coverage for 365-day lookback period from day of first episode of cryptococcus
  verify_medicare_primary(index_date = "first_cryptococcus_date",
                            lookback_days = 365)%>%
  #Load medicare coverage history for patients still in the cohort.  We only want to keep
  #patients with Medicare primary because they will have the appropriate types of claims
  
  strobe_filter("medicare_primary_TF==TRUE", 
                "365+ days of Medicare primary coverage\nprior to first cryptococcus claim", 
                "Excluded: Fewer than 365 days of coverage")%>%
  select(-medicare_primary_TF)%>%
  
  #Prepare data for cohort initialization
  mutate(terminal_date=coalesce(ENDDATE, censor_date))%>%
  
  strobe_filter("terminal_date - first_cryptococcus_date >=minimum_followup", 
                "Minimum followup exceeded", 
                "Excluded: Minimum follow-up threshold not met")


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


# We now seek to determine comorbidities by using diagnosis codes from the setup.R file

comorbidity_diagnosis_date<-list()

# Combine all ICD codes from all comorbidities into one list 
comorbidity_ICD_combined_list<-unlist(comorbidity_ICD_list, use.names=FALSE)

#Scrape files for any comorbiditity claim
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


#Initialize cohort
cohort<-create_usrds_cohort(df=patients_clean,
                    start_date = "first_cryptococcus_date",
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
  
  finalize_usrds_cohort()


#Gather raw costs for each patient
costs_raw<-list()
costs_raw[["IN_REV"]]<-get_IN_REV_costs(years = 2006:2021, usrds_ids = cohort$USRDS_ID)
costs_raw[["IN_CLM"]]<-get_IN_CLM_costs(years = 2006:2021, usrds_ids = cohort$USRDS_ID)
costs_raw[["PS_REV"]]<-get_PS_REV_costs(years = 2006:2021, usrds_ids = cohort$USRDS_ID)

#Group by ID and nest, so that each patient has a single tibble for each type of cost

#Start with the raw costs
costs_clean <- costs_raw%>% 
  
  #Use the imap function to create nested tibbles so that each USRDS ID is unique
  imap(~.x %>%
         group_by(USRDS_ID) %>%
         nest(!!paste0(.y, "_rows") := -USRDS_ID)
  )%>%

  #Join the three tibbles in the resulting list
  reduce(full_join, by = "USRDS_ID")%>%
  
  #Clean up so that patients with empty cost data tibbles do not throw an error later by providing empty columns
  mutate(IN_REV_rows=replace_na(IN_REV_rows, list(tibble(CLM_FROM = as.Date(character()),
                                                         REV_CH = numeric(),
                                                         REVPMT = numeric(),
                                                         HCFASAF = character()))))%>%
  mutate(IN_CLM_rows=replace_na(IN_CLM_rows, list(tibble(CLM_FROM = as.Date(character()),
                                                         CLM_THRU = as.Date(character()),
                                                         CLM_TOT = numeric(),
                                                         CLM_AMT = numeric(),
                                                         HCFASAF = character()))))%>%
  mutate(PS_REV_rows=replace_na(PS_REV_rows, list(tibble(CLM_FROM = as.Date(character()),
                                                         CLM_THRU = as.Date(character()),
                                                         SBMTCH = numeric(),
                                                         ALOWCH = numeric(),
                                                         PMTAMT = numeric(),
                                                         HCFASAF = character()))))


#Add cost tibbles to patients_clean data set
cost_joined<-left_join(patients_clean, 
                       costs_clean, 
                       by = join_by(USRDS_ID))

#Use the cost claims to calculate appropriate
cost_intermediate<-cost_joined%>%
  mutate(IN_REV_1yr_cost = map2_dbl(IN_REV_rows, first_cryptococcus_date, ~
                                      .x %>%
                                      filter(CLM_FROM >= .y)%>%
                                      summarise(total = sum(REVPMT, na.rm = TRUE)) %>%
                                      pull(total)
    ))%>%
  mutate(IN_CLM_1yr_cost = map2_dbl(IN_CLM_rows, first_cryptococcus_date, ~
                                      .x %>%
                                      filter(CLM_FROM >= .y)%>%
                                      summarise(total = sum(CLM_AMT, na.rm = TRUE)) %>%
                                      pull(total)
  ))%>%
  mutate(PS_REV_1yr_cost = map2_dbl(PS_REV_rows, first_cryptococcus_date, ~
                                      .x %>%
                                      filter(CLM_FROM >= .y)%>%
                                      summarise(total = sum(PMTAMT, na.rm = TRUE)) %>%
                                      pull(total)
  ))

#Now we trial code that accounts for both the start date and the end date
cost_broken_down<-cost_intermediate%>%
  
  #Define end date for purposes of cost
  mutate(end_date_analysis=first_cryptococcus_date+365)%>%
  
  #Use pmap to calculate costs between first_cryptococcus_date and end_date for IN_REV
  mutate(IN_REV_365d_cost_total=pmap(
    .l=list(IN_REV_rows, first_cryptococcus_date, end_date_analysis),
    .f=function(claims_df, s_date,e_date) {
      claims_df%>%
        filter(CLM_FROM >= s_date, CLM_FROM<=e_date)%>%
        summarise(total = sum(REVPMT, na.rm = TRUE)) %>%
        pull(total)}
    )
    )%>%
  
  #Use pmap to calculate costs between first_cryptococcus_date and end_date for IN_CLM, after prorating costs by day
  mutate(IN_CLM_365d_cost_total=pmap(
    .l=list(IN_CLM_rows, first_cryptococcus_date, end_date_analysis),
    .f=function(claims_df, s_date,e_date) {
      claims_df%>%
        usRds::prorate_costs_by_day()%>%
        filter(CLM_FROM >= s_date, CLM_FROM<=e_date)%>%
        summarise(total = sum(CLM_AMT, na.rm = TRUE)) %>%
        pull(total)}
  )
  )%>%
  
  #Use pmap to calculate costs between first_cryptococcus_date and end_date for PS_REV
  mutate(PS_REV_365d_cost_total=pmap(
    .l=list(PS_REV_rows, first_cryptococcus_date, end_date_analysis),
    .f=function(claims_df, s_date,e_date) {
      claims_df%>%
        filter(CLM_FROM >= s_date, CLM_FROM<=e_date)%>%
        summarise(total = sum(PMTAMT, na.rm = TRUE)) %>%
        pull(total)}
  )
  )

