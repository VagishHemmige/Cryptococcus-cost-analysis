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


  
#Loop that plots the different outcomes longitudinally

for (outcome in c("grand_total_cost_month", "IN_CLM_month_total", "PS_REV_month_total", "IN_CLM_month_groupedHomeHealth",
                  "IN_CLM_month_groupedHospice","IN_CLM_month_groupedNonclaimauxiliary","IN_CLM_month_groupedDialysis",
                  "IN_CLM_month_groupedOutpatient","IN_CLM_month_groupedInpatient","IN_CLM_month_groupedSkilledNursingFacility")){
  
  
  plot_mean_monthly_costs(fit[[outcome]][["glmmTMB"]][["tweedie"]])
  ggsave(filename = paste0("figures/Longitudinal ", outcome," glmmTMB", " ", "tweedie.svg"),
         width=14,
         height=7
         )
  
  plot_mean_monthly_costs(fit[[outcome]][["glmmTMB"]][["linear"]])
  ggsave(filename = paste0("figures/Longitudinal ", outcome," glmmTMB", " ", "linear.svg"),
         width=14,
         height=7
  )
  
  
  plot_mean_monthly_costs(fit[[outcome]][["gee"]][["linear"]])
  ggsave(filename = paste0("figures/Longitudinal ", outcome," gee", " ", "linear.svg"),
         width=14,
         height=7
  )
  
  
  plot_mean_monthly_costs(fit[[outcome]][["glmmTMB"]][["log"]])
  ggsave(filename = paste0("figures/Longitudinal ", outcome," glmmTMB", " ", "log.svg"),
         width=14,
         height=7
  )
  
  
  plot_mean_monthly_costs(fit[[outcome]][["gee"]][["log"]])
  ggsave(filename = paste0("figures/Longitudinal ", outcome," gee", " ", "log.svg"),
         width=14,
         height=7
  )
  
  
}




