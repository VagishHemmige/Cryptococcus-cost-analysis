# Create figures


#Final STROBE flowchart
patients_fc_matched%>%
  fc_draw()


#GGplot that is beeswarm of adjusted total costs
cost_inflated%>%
  mutate(IN_CLM_365d_cost_adjusted_total=ifelse(IN_CLM_365d_cost_adjusted_total==0, 1, IN_CLM_365d_cost_adjusted_total))%>%
  ggplot()+
  geom_quasirandom(mapping=aes(x=patient_type, y=IN_CLM_365d_cost_adjusted_total), alpha=0.1)+
  scale_y_log10()+
  theme_classic()


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
    )
  
  # Risk table
  monthly_plot[["risk_table"]] <- countdf %>%
    mutate(month = 0.5 + as.numeric(month)) %>%
    ggplot() +
    geom_text(
      aes(x = month, y = patient_type, label = n),
      size = 3
    ) +
    scale_y_discrete(limits = c("Case", "Control")) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.margin = margin(t = -5, b = 5),
      axis.text.x = element_blank()
    )
  
  # Combine plots
  monthly_plot[["main"]] /
    monthly_plot[["risk_table"]] +
    plot_layout(heights = c(4, 1), guides = "keep")
}
  
  
  


for (outcome in c("grand_total_cost_month", "IN_CLM_month_total", "PS_REV_month_total", "IN_CLM_month_groupedHomeHealth",
                  "IN_CLM_month_groupedHospice","IN_CLM_month_groupedNonclaimauxiliary","IN_CLM_month_groupedDialysis",
                  "IN_CLM_month_groupedOutpatient","IN_CLM_month_groupedInpatient","IN_CLM_month_groupedSkilledNursingFacility")){
  
  
  plot_mean_monthly_costs(fit[[outcome]][["glmmTMB"]][["tweedie"]])
  plot_mean_monthly_costs(fit[[outcome]][["glmmTMB"]][["linear"]])
  plot_mean_monthly_costs(fit[[outcome]][["gee"]][["linear"]])
  plot_mean_monthly_costs(fit[[outcome]][["glmmTMB"]][["log"]])
  plot_mean_monthly_costs(fit[[outcome]][["gee"]][["log"]])
  
  
}




#Create plots of monthly costs for cases and controls

plot_mean_monthly_costs(fit_grand_total_cost_month)
plot_mean_monthly_costs(fit_IN_CLM)
plot_mean_monthly_costs(fit_PS_REV)

plot_mean_monthly_costs(fit_IN_CLM_month_groupedHomeHealth)
plot_mean_monthly_costs(fit_IN_CLM_month_groupedHospice)
plot_mean_monthly_costs(fit_IN_CLM_month_groupedOutpatient)
plot_mean_monthly_costs(fit_IN_CLM_month_groupedInpatient)
plot_mean_monthly_costs(fit_IN_CLM_month_groupedSkilledNursingFacility)
