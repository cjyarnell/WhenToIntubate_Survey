# translate clinician variables

translate_df <- function(translated_df, lk){

translated_df$clinician[
  translated_df$clinician %in% filter(lk, Field == "DemoQ_Clinician_Yes")[4:12]
] <- unlist(filter(lk, Field == "DemoQ_Clinician_Yes")[3])
translated_df$clinician[
  translated_df$clinician %in% filter(lk, Field == "DemoQ_Clinician_No")[4:12]
] <- unlist(filter(lk, Field == "DemoQ_Clinician_No")[3])


translated_df$role[
  translated_df$role %in% filter(lk, Field == "DemoQ_Role_Attending")[4:12]
] <- unlist(filter(lk, Field == "DemoQ_Role_Attending")[3])
translated_df$role[
  translated_df$role %in% filter(lk, Field == "DemoQ_Role_Trainee")[4:12]
] <- unlist(filter(lk, Field == "DemoQ_Role_Trainee")[3])
translated_df$role[
  translated_df$role %in% filter(lk, Field == "DemoQ_Role_Nurse")[4:12]
] <- unlist(filter(lk, Field == "DemoQ_Role_Nurse")[3])
translated_df$role[
  translated_df$role %in% filter(lk, Field == "DemoQ_Role_RT")[4:12]
] <- unlist(filter(lk, Field == "DemoQ_Role_RT")[3])
translated_df$role[
  translated_df$role %in% filter(lk, Field == "DemoQ_Role_OtherClinical")[4:12]
] <- unlist(filter(lk, Field == "DemoQ_Role_OtherClinical")[3])
translated_df$role[
  translated_df$role %in% filter(lk, Field == "DemoQ_Role_NonClinical")[4:12]
] <- unlist(filter(lk, Field == "DemoQ_Role_NonClinical")[3])

# diagnosis

translated_df$diagnosis[
  translated_df$diagnosis %in% filter(lk, Field == "Diagnosis_CAP")[4:12]
] <- unlist(filter(lk, Field == "Diagnosis_CAP")[3])
translated_df$diagnosis[
  translated_df$diagnosis %in% filter(lk, Field == "Diagnosis_COVID")[4:12]
] <- unlist(filter(lk, Field == "Diagnosis_COVID")[3])
translated_df$diagnosis[
  translated_df$diagnosis %in% filter(lk, Field == "Diagnosis_Flu")[4:12]
] <- unlist(filter(lk, Field == "Diagnosis_Flu")[3])
translated_df$diagnosis[
  translated_df$diagnosis %in% filter(lk, Field == "Diagnosis_Pancreatitis")[4:12]
] <- unlist(filter(lk, Field == "Diagnosis_Pancreatitis")[3])
translated_df$diagnosis[
  translated_df$diagnosis %in% filter(lk, Field == "Diagnosis_Sepsis")[4:12]
] <- unlist(filter(lk, Field == "Diagnosis_Sepsis")[3])

# frailty

translated_df$frailty[
  translated_df$frailty %in% filter(lk, Field == "Frailty_Independent")[4:12]
] <- unlist(filter(lk, Field == "Frailty_Independent")[3])
translated_df$frailty[
  translated_df$frailty %in% filter(lk, Field == "Frailty_IndependentComorbid")[4:12]
] <- unlist(filter(lk, Field == "Frailty_IndependentComorbid")[3])
translated_df$frailty[
  translated_df$frailty %in% filter(lk, Field == "Frailty_Assistance")[4:12]
] <- unlist(filter(lk, Field == "Frailty_Assistance")[3])

# o2_device

translated_df$o2_device[
  translated_df$o2_device %in% filter(lk, Field == "O2_Device_HFNC")[4:12]
] <- unlist(filter(lk, Field == "O2_Device_HFNC")[3])
translated_df$o2_device[
  translated_df$o2_device %in% filter(lk, Field == "O2_Device_NIV")[4:12]
] <- unlist(filter(lk, Field == "O2_Device_NIV")[3])

# work of breathing

translated_df$work_of_breathing[
  translated_df$work_of_breathing %in% filter(lk, Field == "WoB_none")[4:12]
] <- unlist(filter(lk, Field == "WoB_none")[3])
translated_df$work_of_breathing[
  translated_df$work_of_breathing %in% filter(lk, Field == "WoB_neck")[4:12]
] <- unlist(filter(lk, Field == "WoB_neck")[3])
translated_df$work_of_breathing[
  translated_df$work_of_breathing %in% filter(lk, Field == "WoB_neck_abdo")[4:12]
] <- unlist(filter(lk, Field == "WoB_neck_abdo")[3])

# norepinephrine

translated_df$norepinephrine[
  translated_df$norepinephrine %in% filter(lk, Field == "Pressor_Yes")[4:12]
] <- unlist(filter(lk, Field == "Pressor_Yes")[3])
translated_df$norepinephrine[
  translated_df$norepinephrine %in% filter(lk, Field == "Pressor_No")[4:12]
] <- unlist(filter(lk, Field == "Pressor_No")[3])

# loc

translated_df$loc[
  translated_df$loc %in% filter(lk, Field == "LOC_AO")[4:12]
] <- unlist(filter(lk, Field == "LOC_AO")[3])
translated_df$loc[
  translated_df$loc %in% filter(lk, Field == "LOC_DO")[4:12]
] <- unlist(filter(lk, Field == "LOC_DO")[3])
translated_df$loc[
  translated_df$loc %in% filter(lk, Field == "LOC_DNO")[4:12]
] <- unlist(filter(lk, Field == "LOC_DNO")[3])

# duration

translated_df$duration[
  translated_df$duration %in% filter(lk, Field == "Duration_10min")[4:12]
] <- unlist(filter(lk, Field == "Duration_10min")[3])
translated_df$duration[
  translated_df$duration %in% filter(lk, Field == "Duration_30min")[4:12]
] <- unlist(filter(lk, Field == "Duration_30min")[3])
translated_df$duration[
  translated_df$duration %in% filter(lk, Field == "Duration_1hr")[4:12]
] <- unlist(filter(lk, Field == "Duration_1hr")[3])
translated_df$duration[
  translated_df$duration %in% filter(lk, Field == "Duration_2hr")[4:12]
] <- unlist(filter(lk, Field == "Duration_2hr")[3])
translated_df$duration[
  translated_df$duration %in% filter(lk, Field == "Duration_4hr")[4:12]
] <- unlist(filter(lk, Field == "Duration_4hr")[3])

# response

translated_df$response[
  translated_df$response %in% filter(lk, Field == "ResponseQ_DefNo")[4:12]
] <- unlist(filter(lk, Field == "ResponseQ_DefNo")[3])
translated_df$response[
  translated_df$response %in% filter(lk, Field == "ResponseQ_ProbNo")[4:12]
] <- unlist(filter(lk, Field == "ResponseQ_ProbNo")[3])
translated_df$response[
  translated_df$response %in% filter(lk, Field == "ResponseQ_Uncertain")[4:12]
] <- unlist(filter(lk, Field == "ResponseQ_Uncertain")[3])
translated_df$response[
  translated_df$response %in% filter(lk, Field == "ResponseQ_ProbYes")[4:12]
] <- unlist(filter(lk, Field == "ResponseQ_ProbYes")[3])
translated_df$response[
  translated_df$response %in% filter(lk, Field == "ResponseQ_DefYes")[4:12]
] <- unlist(filter(lk, Field == "ResponseQ_DefYes")[3])

# add regions

countries_df <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
countries <- tolower(countries_df[,"alpha.2"])
countries2 <- countries_df[,"name"]

translated_df <- 
  translated_df %>%
  left_join(select(countries_df, name, alpha.3, region, sub.region),
            by = c("country" = "name"))


# code ordinal and numeric variables appropriately 

translated_df <- 
  translated_df %>%
  mutate(duration_practice = as.numeric(duration_practice),
         response = 
           factor(response, 
                  levels = c("Definite no", 
                             "Probable no", 
                             "Uncertain", 
                             "Probable yes", 
                             "Definite yes"),
                  ordered = T),
         work_of_breathing = factor(
           work_of_breathing,
           levels = c("No use of neck muscles, no abdominal paradox",
                      "Use of neck muscles, no abdominal paradox",
                      "Use of neck muscles, abdominal paradox"),
           ordered = T),
         fio2 = factor(fio2, levels = 4:10/10, ordered = T),
         spo2 = as.numeric(spo2),
         norepinephrine = factor(norepinephrine,
                                 levels = c("No","Yes"),
                                 ordered =T),
         loc = factor(loc,
                      levels = c("Alert and obeying",
                                 "Drowsy but obeying",
                                 "Drowsy, not obeying"),
                      ordered = T),
         duration = factor(duration,
                           levels = c("10 minutes",
                                      "30 minutes",
                                      "1 hour",
                                      "2 hours",
                                      "4 hours"),
                           ordered = T),
         o2_device = factor(o2_device),
         frailty = factor(frailty,
                          levels = c("Independent and fit",
                                     "Independent with well-controlled medical problems",
                                     "Assistance for shopping and heavy housework")),
         diagnosis = factor(diagnosis)
  ) %>% 
  select(record_id:id, clinician, specialty_ccm:specialty_other_text, 
         role:country, alpha.3:sub.region, diagnosis:datetime)

translated_df

}
