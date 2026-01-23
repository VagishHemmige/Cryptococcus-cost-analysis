#This file executes matching on the prematching_cohort object, which is assumed to already exist

unmatched_patients<-list()

#Number of controls per case
number_controls_per_case<-2

unmatched_patients[["Case"]]<-prematching_cohort%>%
  filter(cryptococcus_case=="Case")

unmatched_patients[["Control"]]<-prematching_cohort%>%
  filter(cryptococcus_case=="Potential control")

unmatched_patients[["Case"]]<-unmatched_patients[["Case"]]%>%
  group_by(USRDS_ID)%>%
  slice(1)%>%
  ungroup()

Case<-unmatched_patients[["Case"]]


calculate_number_potential_matches<-function(control_df,
                                             cirrhosis_status,
                                             cmv_status,
                                             hiv_status,
                                             diabetes_status,
                                             matching_date) {
  
  control_df%>%
    filter(cirrhosis==cirrhosis_status)%>%
    filter(CMV==cmv_status)%>%
    filter(HIV==hiv_status)%>%
    filter(diabetes==diabetes_status)%>%
    filter(cohort_start_date<=matching_date)%>%
    filter(cohort_stop_date>matching_date)%>%
    nrow()%>%
    return()
  
}

unmatched_patients[["Case"]] <- unmatched_patients[["Case"]] %>%
  mutate(
    num_potential_controls = pmap_int(
      list(cirrhosis, CMV, HIV, diabetes, cryptococcus_dx_date),
      function(cirrhosis, CMV, HIV, diabetes, matching_date) {
        calculate_number_potential_matches(
          unmatched_patients[["Control"]],
          cirrhosis,
          CMV,
          HIV,
          diabetes,
          matching_date
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
    filter(CMV==unmatched_patients$Case$CMV[[1]])%>%
    filter(HIV==unmatched_patients$Case$HIV[[1]])%>%
    filter(diabetes==unmatched_patients$Case$diabetes[[1]])%>%
    filter(cohort_start_date<=unmatched_patients$Case$cryptococcus_dx_date[[1]])%>%
    filter(cohort_stop_date>unmatched_patients$Case$cryptococcus_dx_date[[1]])
  
  k <- min(number_controls_per_case, nrow(eligible_controls))
  
  sampled_controls <- eligible_controls %>%
    slice_sample(n = k)%>%
    mutate(index_date_match = unmatched_patients$Case$cryptococcus_dx_date[[1]])
  
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
  
  if (i==10){
    continue<-FALSE
  }
  
}


post_match_results<-matched_patients%>%
  bind_rows()
