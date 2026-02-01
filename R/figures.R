# Create figures


#Final STROBE flowchart
patients_fc_matched%>%
  fc_draw()


#GGplot
cost_inflated%>%
  mutate(IN_CLM_365d_cost_adjusted_total=ifelse(IN_CLM_365d_cost_adjusted_total==0, 1, IN_CLM_365d_cost_adjusted_total))%>%
  ggplot()+
  geom_quasirandom(mapping=aes(x=patient_type, y=IN_CLM_365d_cost_adjusted_total), alpha=0.1)+
  scale_y_log10()+
  theme_classic()