# Statistical modeling of cost data

for (outcome in c("grand_total_cost_month", "IN_CLM_month_total", "PS_REV_month_total", "IN_CLM_month_groupedHomeHealth",
                  "IN_CLM_month_groupedHospice","IN_CLM_month_groupedNonclaimauxiliary","IN_CLM_month_groupedDialysis",
                  "IN_CLM_month_groupedOutpatient","IN_CLM_month_groupedInpatient","IN_CLM_month_groupedSkilledNursingFacility")){

  temp_formula[["glmmTMB"]][["untransformed"]]<-as.formula(
    paste0(
      outcome, " ~ ",
      "patient_type * factor(month) + ",
      "offset(log(month_offset)) + ",
      "(1 | USRDS_ID)"
    )
  )
  temp_formula[["gee"]][["untransformed"]]<-as.formula(
    paste0(
      outcome, " ~ ",
      "patient_type * factor(month) + ",
      "offset(log(month_offset))"
    )
  )

  temp_formula[["glmmTMB"]][["logtransformed"]] <- as.formula(
    paste0(
      "log1p(", outcome, ") ~ ",
      "patient_type * factor(month) + ",
      "offset(log(month_offset)) + ",
      "(1 | USRDS_ID)"
    )
  )
  temp_formula[["gee"]][["logtransformed"]]<-as.formula(
    paste0(
      "log1p(", outcome, ") ~ ",
      "patient_type * factor(month) + ",
      "offset(log(month_offset))"
    )
  )
  
  message(
    "Fitting models to formula: ", temp_formula[["glmmTMB"]][["untransformed"]]
  )
  
  message("Fitting glmmTMB to tweedie family...")
  fit[[outcome]][["glmmTMB"]][["tweedie"]] <- glmmTMB(
    temp_formula[["glmmTMB"]][["untransformed"]],
    family = tweedie(link = "log"),
    data = final_data_set
  )
  
  message("Fitting glmmTMB to linear family...")
  fit[[outcome]][["glmmTMB"]][["linear"]] <- glmmTMB(
    temp_formula[["glmmTMB"]][["untransformed"]],
    family = gaussian(link = "identity"),
    data = final_data_set
  )
  
  message("Fitting gee to linear family...")
  fit[[outcome]][["gee"]][["linear"]] <- geeglm(
    temp_formula[["gee"]][["untransformed"]],
    id     = USRDS_ID,
    data   = final_data_set,
    corstr = "exchangeable",
    family = gaussian(link = "identity"))  
  
  message(
    "Fitting models to formula: ", temp_formula[["glmmTMB"]][["logtransformed"]]
  )
  
  message("Fitting glmmTMB to logcost family...")
  fit[[outcome]][["glmmTMB"]][["log"]] <- glmmTMB(
    temp_formula[["glmmTMB"]][["logtransformed"]],
    family = gaussian(link = "identity"),
    data = final_data_set
  )
  
  message("Fitting gee to logcost family...")
  fit[[outcome]][["gee"]][["log"]] <- geeglm(
    temp_formula[["gee"]][["logtransformed"]],
    id     = USRDS_ID,
    data   = final_data_set,
    corstr = "exchangeable",
    family = gaussian(link = "identity"))  

  
}










fit[[outcome]][["gee"]][["tweedie"]] <- geeglm(
  temp_formula[["gee"]],
  id     = USRDS_ID,
  data   = final_data_set,
  corstr = "exchangeable",
  family = statmod::tweedie(
    var.power = 1.7
  ))