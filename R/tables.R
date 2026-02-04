# Create tables

#Create a table of the post-match results to confirm that demographics were appropriately balanced
post_match_results%>%
  select(-matched_control_indices, -USRDS_ID)%>%
  gtsummary::tbl_summary(by=patient_type)%>%
  add_p()

#Examine costs broken down by type
cost_broken_down%>%
  select(patient_type, contains(c("IN_REV_365d_cost", "IN_CLM_365d_cost", "PS_REV_365d_cost")))%>%
  gtsummary::tbl_summary(by=patient_type)%>%
  add_p()

#Examine costs broken down by type and adjusted for inflation (medians)
cost_inflated%>%
  select(patient_type, contains(c("IN_REV_365d_cost", "IN_CLM_365d_cost", "PS_REV_365d_cost")))%>%
  gtsummary::tbl_summary(by=patient_type)%>%
  add_p()

cost_inflated%>%
  select(patient_type, contains(c("IN_REV_365d_cost", "IN_CLM_365d_cost", "PS_REV_365d_cost")))%>%
  gtsummary::tbl_summary(by=patient_type,
                         statistic = all_continuous() ~ "{mean} ({sd})")%>%
  add_p()



#Total the grand difference between cases and controls over the entire period of follow up:
fit[["grand_total_cost_month"]][["gee"]][["linear"]]%>%
  emmeans(
    ~ patient_type | month
  )%>%
  contrast(
    method = "revpairwise"  # Case - Control
  )%>%
  contrast(
    list("Months -3 to 11 total" = rep(1, 15)),
    by = NULL
  )%>%
  summary(infer=TRUE)