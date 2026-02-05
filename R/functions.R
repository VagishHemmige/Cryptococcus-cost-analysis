#Define functions for use in the analysis

#Function that returns a ggplot of the mean cost (with confidence interval) for cryptococcus cases and controls by month
plot_mean_monthly_costs<-function(fitted_model, countdf=final_count_df) {

  #First, use emmeans to calculate the predicted values from the model assuming 30-day months, then convert to data frame
  emm_df <- emmeans(
    fitted_model,
    ~ patient_type | month,
    at = list(month_offset = 30),
    type = "response"
  )%>%as.data.frame()%>%
    
    #Shifts all points and error_bars 0.5 to the right to avoid the zero-line
    mutate(month=0.5+as.numeric(month))%>%
    
    #Move cases 0.1 to the right and controls 0.1 to the left to improve legibility
  mutate(month=ifelse(patient_type=="Case", month+0.1, month-0.1))%>%
    mutate(
      estimate = if ("response" %in% names(.)) response else emmean)
  
  monthly_plot <- list()
  
  # Main plot
  monthly_plot[["main"]] <- ggplot(data = emm_df) +
    geom_point(aes(x = month, y = estimate, color = patient_type)) +
    geom_errorbar(
      aes(
        x = month,
        ymin = asymp.LCL,
        ymax = asymp.UCL,
        color = patient_type
      ),
      width = 0.1
    ) +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               color = "gray50") +
    theme_classic() +
    labs(
      x = "Month",
      y = "Inflation-adjusted cost per month (dollars)",
      color = "Patient group"
    )+
    theme(legend.position = "bottom")
  
  # Risk table
  monthly_plot[["risk_table"]] <- countdf %>%
    mutate(month = 0.5 + as.numeric(month),
           y_position = ifelse(patient_type == "Case", 0.4, 0.6)) %>%
    ggplot() +
    geom_text(
      aes(x = month, y = y_position, label = n, color=patient_type),
      size = 3
    )+
    coord_cartesian(ylim = c(0.3, 0.7)) +
    theme_void() +
    theme(
      plot.margin = margin(t = -5, b = 5),
      legend.position = "none")
  
  # Combine plots
  monthly_plot[["main"]] /
    monthly_plot[["risk_table"]] +
    plot_layout(heights = c(9, 1), guides = "keep")+
    plot_annotation(
      caption = "Bottom panel shows N at risk by month"
    )
  
}


#Function that defines the number of potential control matches for a case
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
