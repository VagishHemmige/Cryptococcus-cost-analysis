# Performs all postmatch processing

post_match_results%>%
  select(-matched_control_indices, -USRDS_ID)%>%
  gtsummary::tbl_summary(by=patient_type)%>%
  add_p()

patients_fc_matched<-patients_merged2%>%
  fc_filter(USRDS_ID %in% post_match_results$USRDS_ID,
            label = "Matched", 
            label_exc= "Excluded: Unmatched",
            show_exc = TRUE)

patients_fc_matched%>%
  fc_draw()












#Gather raw costs for each patient
costs_raw<-list()
costs_raw[["IN_REV"]]<-get_IN_REV_costs(years = 2006:2021, usrds_ids = post_match_results$USRDS_ID)
costs_raw[["IN_CLM"]]<-get_IN_CLM_costs(years = 2006:2021, usrds_ids = post_match_results$USRDS_ID)
costs_raw[["PS_REV"]]<-get_PS_REV_costs(years = 2006:2021, usrds_ids = post_match_results$USRDS_ID)


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
cost_joined<-left_join(post_match_results, 
                       costs_clean, 
                       by = join_by(USRDS_ID))%>%
  mutate(IN_REV_rows=map(IN_REV_rows, ~ if (is.null(.x)) tibble(CLM_FROM = as.Date(character()),
                                                                REV_CH = numeric(),
                                                                REVPMT = numeric(),
                                                                HCFASAF = character()) else .x))%>%
  mutate(IN_CLM_rows=map(IN_CLM_rows, ~ if (is.null(.x)) tibble(CLM_FROM = as.Date(character()),
                                                                CLM_THRU = as.Date(character()),
                                                                CLM_TOT = numeric(),
                                                                CLM_AMT = numeric(),
                                                                HCFASAF = character()) else .x))%>%
  mutate(PS_REV_rows=map(PS_REV_rows, ~ if (is.null(.x)) tibble(CLM_FROM = as.Date(character()),
                                                                CLM_THRU = as.Date(character()),
                                                                SBMTCH = numeric(),
                                                                ALOWCH = numeric(),
                                                                PMTAMT = numeric(),
                                                                HCFASAF = character()) else .x))



#Use the cost claims to calculate appropriate
cost_intermediate<-cost_joined%>%
  mutate(IN_REV_post_cost = map2_dbl(IN_REV_rows, index_date_match, ~
                                      .x %>%
                                      filter(CLM_FROM >= .y)%>%
                                      summarise(total = sum(REVPMT, na.rm = TRUE)) %>%
                                      pull(total)
  ))%>%
  mutate(IN_CLM_post_cost = map2_dbl(IN_CLM_rows, index_date_match, ~
                                      .x %>%
                                      filter(CLM_FROM >= .y)%>%
                                      summarise(total = sum(CLM_AMT, na.rm = TRUE)) %>%
                                      pull(total)
  ))%>%
  mutate(PS_REV_post_cost = map2_dbl(PS_REV_rows, index_date_match, ~
                                      .x %>%
                                      filter(CLM_FROM >= .y)%>%
                                      summarise(total = sum(PMTAMT, na.rm = TRUE)) %>%
                                      pull(total)
  ))

#Now we trial code that accounts for both the start date and the end date
cost_broken_down<-cost_intermediate%>%
  
  #Define end date for purposes of cost
  mutate(end_date_analysis=index_date_match+365)%>%
  
  #Use pmap to calculate costs between first_cryptococcus_date and end_date for IN_REV
  mutate(IN_REV_365d_cost_total=pmap(
    .l=list(IN_REV_rows, index_date_match, end_date_analysis),
    .f=function(claims_df, s_date,e_date) {
      claims_df%>%
        filter(CLM_FROM >= s_date, CLM_FROM<=e_date)%>%
        summarise(total = sum(REVPMT, na.rm = TRUE)) %>%
        pull(total)}
  )
  )%>%
  
  #Same, except now we group by HCFASAF
  #Use pmap to calculate costs between first_cryptococcus_date and end_date for IN_REV, grouped by HCFASAF
  mutate(IN_REV_365d_cost_grouped=pmap(
    .l=list(IN_REV_rows, index_date_match, end_date_analysis),
    .f=function(claims_df, s_date,e_date) {
      claims_df%>%
        filter(CLM_FROM >= s_date, CLM_FROM<=e_date)%>%
        mutate(HCFASAF = stringr::str_replace_all(HCFASAF, " ", ""))%>%
        mutate(HCFASAF = ifelse(HCFASAF=="Inpatient(REBUS)", "Inpatient", HCFASAF))%>%
        mutate(HCFASAF = ifelse(HCFASAF=="Non-claim/auxiliary", "Nonclaimauxiliary", HCFASAF))%>%
        group_by(HCFASAF)%>%
        summarise(total = sum(REVPMT, na.rm = TRUE)) %>%
        pivot_wider(
          names_from  = HCFASAF,
          values_from = total,
          names_prefix = "IN_REV_365d_cost_",
          values_fill = 0
        ) 
      }
  )
  )%>%
  
  #Use pmap to calculate costs between first_cryptococcus_date and end_date for IN_CLM, after prorating costs by day
  mutate(IN_CLM_365d_cost_total=pmap(
    .l=list(IN_CLM_rows, index_date_match, end_date_analysis),
    .f=function(claims_df, s_date,e_date) {
      claims_df%>%
        usRds::prorate_costs_by_day()%>%
        filter(CLM_FROM >= s_date, CLM_FROM<=e_date)%>%
        summarise(total = sum(CLM_AMT, na.rm = TRUE))%>%
        pull(total)
    }
      )
  )%>%
  
  
  #Same, except now we group by HCFASAF
  #Use pmap to calculate costs between first_cryptococcus_date and end_date for IN_CLM, after prorating costs by day
  mutate(IN_CLM_365d_cost_grouped=pmap(
    .l=list(IN_CLM_rows, index_date_match, end_date_analysis),
    .f=function(claims_df, s_date,e_date) {
      claims_df%>%
        usRds::prorate_costs_by_day()%>%
        filter(CLM_FROM >= s_date, CLM_FROM<=e_date)%>%
        mutate(HCFASAF = stringr::str_replace_all(HCFASAF, " ", ""))%>%
        mutate(HCFASAF = ifelse(HCFASAF=="Inpatient(REBUS)", "Inpatient", HCFASAF))%>%
        mutate(HCFASAF = ifelse(HCFASAF=="Non-claim/auxiliary", "Nonclaimauxiliary", HCFASAF))%>%
        group_by(HCFASAF)%>%
        summarise(total = sum(CLM_AMT, na.rm = TRUE))%>%
        pivot_wider(
          names_from  = HCFASAF,
          values_from = total,
          names_prefix = "IN_CLM_365d_cost_",
          values_fill = 0
        ) 
    }
  )
  )%>%
  
  
  
  #Use pmap to calculate costs between first_cryptococcus_date and end_date for PS_REV
  mutate(PS_REV_365d_cost_total=pmap(
    .l=list(PS_REV_rows, index_date_match, end_date_analysis),
    .f=function(claims_df, s_date,e_date) {
      claims_df%>%
        filter(CLM_FROM >= s_date, CLM_FROM<=e_date)%>%
        summarise(total = sum(PMTAMT, na.rm = TRUE)) %>%
        pull(total)}
  )
  )%>%
  
  unnest_wider(IN_REV_365d_cost_grouped)%>%
  unnest_wider(IN_CLM_365d_cost_grouped)%>%
  mutate(across(matches("_365d_cost_"), ~replace_na(., 0)))
  

