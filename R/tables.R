# Create tables

#Create a table of the post-match results to confirm that demographics were appropriately balanced
table1<-post_match_results%>%
  mutate(age=time_length(interval(BORN, index_date_match), "years"))%>%
  dplyr::select(cirrhosis, CMV, HIV, diabetes, cumulative_transplant_total, current_graft_status,
                liver_transplant, lung_transplant, heart_transplant, heartlung_transplant,
                pancreas_transplant, intestinal_transplant,
                age, SEX, RACE, HISPANIC, RACEETH, RURALURBAN, cryptococcus_dx_date, 
         pancreas_transplant, intestinal_transplant, patient_type)%>%
  gtsummary::tbl_summary(by=patient_type)%>%
  add_p()
table1%>%
  gtsummary::as_gt() %>%
  gt::gtsave("tables/table1_baseline.png")
file.exists("tables/table1_baseline.docx") && file.remove("tables/table1_baseline.docx")
table1%>%
  gtsummary::as_gt() %>%
  gt::gtsave("tables/table1_baseline.docx")


#Table of comorbidities generated from lists created in setup.R, then exported 

table2_icd <- comorbidity_ICD_list%>%
  imap_dfr(., 
           ~tibble(
             comorbidity = .y,
             icd_codes_raw = paste(.x, collapse = ", ")
             )
           )%>%
  gt() %>%
  cols_label(
    comorbidity   = "Comorbidity",
    icd_codes_raw = "ICD Codes"
  ) %>%
  cols_width(
    comorbidity   ~ px(220),
    icd_codes_raw ~ px(500)
  ) %>%
  tab_options(
    table.font.size = px(12)
  )%>%
  tab_header(
  title = "ICD-9 and ICD-10 Codes Used to Define Comorbidities",
  subtitle = "Codes grouped by clinical condition"
)
table2_icd%>%
  gt::gtsave("tables/table2_baseline.png")
file.exists("tables/table2_baseline.docx") && file.remove("tables/table2_baseline.docx")
table2_icd%>%
  gt::gtsave("tables/table2_baseline.docx")



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
calculate_total_excess_costs(fit[["grand_total_cost_month"]][["gee"]][["linear"]])