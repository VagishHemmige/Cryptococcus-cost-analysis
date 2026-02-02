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


#GGplot the mean cost (with confidence interval) for cryptococcus cases and controls by month

#IN_CLM first
#First, use emmeans to calculate the predicted values from the model 
emm_df_IN_CLM <- emmeans(
  fit_IN_CLM,
  ~ patient_type | month,
  type = "response"
)%>%as.data.frame()%>%
  mutate(month=0.5+as.numeric(month))

#Now, use ggplot to pl
ggplot(data=emm_df_IN_CLM)+
  geom_point(mapping=aes(x=month, y=response, color=patient_type))+
  geom_errorbar(aes(x=month, ymin = asymp.LCL, ymax = asymp.UCL, color=patient_type))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x="Month", y="Inflation-adjusted cost per month (dollars)", color = "Patient group")