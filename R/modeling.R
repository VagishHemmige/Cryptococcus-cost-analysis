# Statistical modeling of cost data

fit_IN_CLM <- glmmTMB(
  IN_CLM_month_total ~ patient_type *factor(month) +
    offset(log(month_offset)) +
    (1 | USRDS_ID),
  family = tweedie(link = "log"),
  data = cost_longitudinal
)



