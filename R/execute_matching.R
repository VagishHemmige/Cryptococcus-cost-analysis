#This file executes matching on the prematching_cohort object, which is assumed to already exist

unmatched_patients<-list()

unmatched_patients[["Case"]]<-prematching_cohort%>%
  filter(cryptococcus_case=="Case")

unmatched_patients[["Control"]]<-prematching_cohort%>%
  filter(cryptococcus_case=="Potential control")

unmatched_patients[["Case"]]<-unmatched_patients[["Case"]]%>%
  group_by(USRDS_ID)%>%
  slice(1)%>%
  ungroup()

Case<-unmatched_patients[["Case"]]
Control<-unmatched_patients[["Control"]]


#Define helper function for function that assesses potential number of controls per case patient
calculate_number_potential_matches<-function(control_df,
                                             USRDS_ID,
                                             birthdate,
                                             cirrhosis_status,
                                             cmv_status,
                                             hiv_status,
                                             diabetes_status,
                                             matching_date,
                                             matching_days_since_transplant) {
  
  print(paste0("Calculating number of potential controls for patient: ", USRDS_ID))
  control_df%>%
    
    #Exact match on cirrhosis, CMV, HIV, and diabetes status
    filter(cirrhosis==cirrhosis_status)%>%
  #  filter(CMV==cmv_status)%>%
    filter(HIV==hiv_status)%>%
    filter(diabetes==diabetes_status)%>%
    
    #Risk set matching
    filter(tstart<=matching_days_since_transplant)%>%
    filter(tstop>matching_days_since_transplant)%>%
    
    #Calculate date for age calculations/etc.
    mutate(.baseline_control_date=matching_days_since_transplant+most_recent_transplant_date)%>%
    
    #Make sure case and control are sampled within 3 years of each other
    filter(abs(time_length(interval(.baseline_control_date, matching_date), "years")) <=3)%>%
    
    #Date matching
  #  filter(cohort_start_date<=matching_date)%>%
  #  filter(cohort_stop_date>matching_date)%>%
    
    #Age>=18 on index date
    filter(time_length(interval(BORN, .baseline_control_date), "years") >= 18)%>%
    
    #Age difference under 10 years, calculated at sampling date
    filter(abs(time_length(interval(BORN,.baseline_control_date), "years")-
                 time_length(interval(birthdate,matching_date), "years")) <=10)%>%
    
    #Confirming 365 day Medicare lookback available for potential match
    verify_medicare_primary(index_date = matching_date, medicare_coverage_df = medicare_history, cache=TRUE)%>%
    filter(medicare_primary_TF==TRUE)%>%
    
    #Count rows after ensuring controls with mult transplants are only counted once 
    distinct(USRDS_ID)%>%
    nrow()%>%
    return()
  
}

unmatched_patients[["Case"]] <- unmatched_patients[["Case"]] %>%
  mutate(
    num_potential_controls = pmap_int(
      list(USRDS_ID, BORN,cirrhosis, CMV, HIV, diabetes, cryptococcus_dx_date, tstart),
      function(USRDS_ID, BORN, cirrhosis, CMV, HIV, diabetes, matching_date, tstart) {
        calculate_number_potential_matches(
          unmatched_patients[["Control"]],
          USRDS_ID,
          BORN, 
          cirrhosis,
          CMV,
          HIV,
          diabetes,
          matching_date,
          tstart
        )
      }
    )
  )

unmatched_patients[["Case"]] <- unmatched_patients[["Case"]] %>%
  arrange(num_potential_controls)%>%
  mutate(index_date_match=cryptococcus_dx_date)


unmatched_patients_backup<-unmatched_patients


unmatched_patients<-unmatched_patients_backup



#Time to set up the matching loop
continue<-TRUE

#Initialize the variable where the matched variables will be stored
matched_patients<-list()
matched_patients[["Case"]]<-unmatched_patients[["Case"]][0, ]%>%
  mutate(matched_control_indices = list())
matched_patients[["Control"]] <- unmatched_patients[["Control"]][0, ] %>%
  mutate(index_date_match = as.Date(character()))

set.seed(12345)
i<-0

while (continue==TRUE)
{
  i<-i+1
  print(i)

  #Select matches from controls
  eligible_controls <-unmatched_patients[["Control"]]%>%
    
    filter(cirrhosis==unmatched_patients$Case$cirrhosis[[1]])%>%
    #filter(CMV==unmatched_patients$Case$CMV[[1]])%>%
    filter(HIV==unmatched_patients$Case$HIV[[1]])%>%
    filter(diabetes==unmatched_patients$Case$diabetes[[1]])%>%
    
    #Risk set matching
    filter(tstart<=unmatched_patients$Case$tstart[[1]])%>%
    filter(tstop>unmatched_patients$Case$tstart[[1]])%>%
    
    #Calculate date for age calculations/etc.
    mutate(.baseline_control_date=unmatched_patients$Case$tstart[[1]] + most_recent_transplant_date)%>%
    
    #Make sure case and control are sampled within 3 years of each other
    filter(abs(time_length(interval(.baseline_control_date, unmatched_patients$Case$cryptococcus_dx_date[[1]]), "years")) <=3)%>%
    
    #filter(cohort_start_date<=unmatched_patients$Case$cryptococcus_dx_date[[1]])%>%
    #filter(cohort_stop_date>unmatched_patients$Case$cryptococcus_dx_date[[1]])%>%
    
    #Age>=18 on index date
    filter(time_length(interval(BORN, .baseline_control_date), "years") >= 18)%>%
    
    #Age difference under 10 years, calculated at sampling date
    filter(abs(time_length(interval(BORN,.baseline_control_date), "years")-
                 time_length(interval(unmatched_patients$Case$BORN[[1]],
                                      unmatched_patients$Case$cryptococcus_dx_date[[1]]), "years")) <=10)%>%
  
    verify_medicare_primary(index_date = ".baseline_control_date", 
                            medicare_coverage_df = medicare_history, 
                            coverage_start_variable = "medicare_coverage_start_date",
                            coverage_end_variable = "medicare_coverage_end_date",
                            cache=TRUE)%>%
    filter(medicare_primary_TF==TRUE)%>%
    
    #Move matches for cumulative number of transplants to the top
    #Note: this step could be used for propensity score matching on various variables
    #Consider minimizing date difference and age difference as well
    arrange(desc(cumulative_transplant_total==unmatched_patients$Case$cumulative_transplant_total[[1]]))
  
  
  #Number of controls is minimum of global constant number_controls_per_case and number of distinct USRDS_IDs
  k <- min(number_controls_per_case, nrow(distinct(eligible_controls, USRDS_ID)))
  
  #Initialize sampled_controls df
  sampled_controls<-eligible_controls[0, ]
    
  #Loop to select controls one at a time to ensure same patient is not sampled multiple times 
  while (k>0)
  {
  
  sampled_controls <- bind_rows(
    sampled_controls,
    eligible_controls %>%slice_head(n = 1))
    
    eligible_controls<-eligible_controls%>%
      filter(!(USRDS_ID %in% sampled_controls$USRDS_ID ))

    k<-min(k-1, nrow(distinct(eligible_controls, USRDS_ID)))   
  }
    
    sampled_controls<-sampled_controls%>%
    mutate(index_date_match = .baseline_control_date)%>%
    select(-.baseline_control_date)
  
  #Add case to matched DF
  matched_patients[["Case"]]<-unmatched_patients$Case[1,]%>%
    mutate(matched_control_indices=list(sampled_controls$USRDS_ID))%>%
    bind_rows(matched_patients[["Case"]])
  
  #Add controls to control DF
  matched_patients[["Control"]]<-matched_patients[["Control"]]%>%
    bind_rows(sampled_controls)
  
  #Remove controls
  unmatched_patients[["Control"]]<-unmatched_patients[["Control"]]%>%
    filter(!(USRDS_ID %in% sampled_controls$USRDS_ID ))
  
  #Remove cases
  unmatched_patients[["Case"]]<-unmatched_patients[["Case"]]%>%
    slice(-1)
  
 if (nrow(unmatched_patients[["Case"]])==0){
   continue<-FALSE
 } 
  
  if (i==10000){
    continue<-FALSE
  }
  
}


post_match_results<-matched_patients%>%
  bind_rows(.id = "patient_type")
