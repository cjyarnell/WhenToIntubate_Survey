# Analysis of survey results

################# Load packages, colours, data ##############

# colours
c_navy <- "#001A49"
c_blue = "cornflower blue"
c_ice  <- "#AAC0CE"
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

rope = log(c(0.9,1/0.9))

# working drive
setwd("C:/Git/AERONOT/Survey/Results")

library(tidyverse)
library(brms)
library(maps)
library(posterior)

source("translate_df.R")

mor <- function(v) exp(sqrt(2)*v*qnorm(0.75))

lk <- readxl::read_xlsx("C:/Git/AERONOT/Survey/Translation/Language_Key.xlsx")

start_date <- lubridate::as_datetime("2023-09-27 00:00:00 UTC")

df_raw <- read_csv("WhenToIntubate_DATA_FINAL.csv") %>%
  filter(datetime > start_date) %>%
  mutate(role = ifelse(role == "Respiratory therapise", "Respiratory therapist", role))

################# translate responses #######################

df <- translate_df(df_raw, lk) %>%
  # remove non-clinicians (per protocol)
    filter(clinician == "Yes",
         role != "Non-clinical role") %>%
  # shorten some country names  
  mutate(country = ifelse(grepl("United Kingdom", country),
                          "United Kingdom",country)) %>%
  mutate(country = ifelse(grepl("United States", country),
                          "USA",country)) %>%
  mutate(region = ifelse(grepl("R?union", country),
                         "Africa", region)) %>%
  mutate(sub.region = ifelse(grepl("R?union", country),
                         "Sub-Saharan Africa", sub.region)) %>%
  select(-clinician)
  
################# Response counts #########################

# time to complete
df %>% 
  group_by(id) %>%
  mutate(responses = max(row_number())) %>%
  select(id, responses, datetime) %>%
  filter(responses >= 2) %>%
  summarise(time = (last(datetime)-first(datetime))/(responses -1)) %>%
  ungroup() %>%
  summarise(median = median(time),
            q25 = quantile(time, 0.25),
            q75 = quantile(time, 0.75))

# time to complete
df %>%
  group_by(id) %>%
  mutate(responses = max(row_number())) %>%
  filter(responses >= 2) %>%
  summarise(
    role = first(role),
    time = (last(datetime)-first(datetime))/(responses -1)) %>%
  ungroup() %>%
  ggplot(aes(x = role, 
             fill = role,
             y = time)) +
  geom_violin() +
  theme_minimal() +
  labs(title = "Time per scenario response by role",
       y = "Time (seconds)",
       x = "")

ggsave("Figures/TimePerScenario.svg",
       width = 8, height = 7)
  
# total responses
total <- df %>% 
  distinct(id) %>%
  summarise(total = n()) %>%
  print()

# geographic distribution
df %>% 
  group_by(id) %>% 
  summarise(country = first(country)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  summarise(total_by_country = n()) %>%
  arrange(desc(total_by_country)) %>%
  print(n = 75) %>%
  write_csv("country_table.csv")

df %>% 
  group_by(id) %>% 
  summarise(country = first(country)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  summarise(total_by_country = n()) %>%
  arrange(desc(total_by_country)) %>%
  count(total_by_country)

df %>% 
  group_by(id) %>% 
  summarise(country = first(country)) %>% 
  ungroup() %>% 
  group_by(country) %>%
  summarise(total_by_country = round(100*n()/2294)) %>%
  arrange(desc(total_by_country)) %>%
  print(n = 75)

# geographic distribution
df %>% 
  group_by(id) %>% 
  summarise(sub.region = first(sub.region)) %>% 
  ungroup() %>% 
  group_by(sub.region) %>% 
  summarise(total_by_subregion = n()) %>%
  arrange(desc(total_by_subregion)) %>%
  print(n = 60)

# geographic distribution
df %>% 
  group_by(id) %>% 
  summarise(region = first(region)) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarise(total_by_region = n()) %>%
  arrange(desc(total_by_region))



# responses per respondent
df %>%
  group_by(id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  ggplot(aes(x = count)) +
  geom_histogram(bins = 10, fill = c_navy) +
  theme_minimal() +
  labs(title = "Histogram of responses per respondent",
       x = "Responses",
       y = "Number of respondents") +
  scale_x_continuous(breaks = seq(from = 1, to = 10, by = 1)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("Figures/ResponseHistogram.svg",
       width = 6, height = 6)

df %>%
  group_by(id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  summarise(median = median(count),
            mean = mean(count),
            sd = sd(count),
            propLessThanTen = mean(count < 10),
            sum10 = sum(count == 10),
            iqr25 = quantile(count, 0.25),
            iqr75 = quantile(count, 0.75))

library(RColorBrewer)
colourCount = length(unique(df$country))
getPalette = colorRampPalette(brewer.pal(9, name = "Spectral"))

df_recruitment <- 
df %>% 
  group_by(id) %>%
  summarise(starttime = first(datetime),
            Country = first(country)) %>%
  mutate(Country = ifelse(grepl("United Kingdom", Country),
                          "United Kingdom",Country)) %>%
  mutate(day = lubridate::floor_date(starttime, 
                          unit = "day")) %>%
  mutate(day = factor(day)) %>%
  count(Country, day, .drop = FALSE) %>%
  group_by(Country) %>%
  mutate(total_count = cumsum(n))

everysecond <- function(x){
  x <- sort(unique(x))
  x[seq(2, length(x), 2)] <- ""
  x
}

df_recruitment %>%
  ggplot(aes(x = day, y = total_count, fill = Country)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = getPalette(colourCount)) +
  scale_x_discrete(breaks = everysecond(df_recruitment$day)) +
  labs(title = "Survey respondents by country over time",
       y = "Total number of survey respondents",
       x = "") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank())
ggsave("WhenToIntubateSurvey_CountriesRecruitment.svg",
       width = 15, height = 6, units = "in")

# respondents by role

df %>%
  group_by(id) %>%
  summarise(role = first(role)) %>%
  ungroup() %>%
  group_by(role) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Canada

df %>%
  filter(country == "Canada") %>%
  group_by(id) %>%
  summarise(role = first(role)) %>%
  ungroup() %>%
  group_by(role) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# respondents by discipline

df %>%
  group_by(id) %>%
  summarise(ccm = mean(specialty_ccm),
            anesth = mean(specialty_anesthesia),
            em = mean(specialty_em),
            other = mean(specialty_other)) %>%
  ungroup() %>%
  mutate(specialty_num = ccm + anesth + em+ other) %>%
  summarise(ccm = mean(ccm),
            anesth = mean(anesth),
            em = mean(em),
            other = mean(other),
            multiple = mean(specialty_num > 1),
            multiple_num = sum(specialty_num > 1))

data.frame(table(df$specialty_other_text)) %>% 
  arrange(desc(Freq)) %>% 
  write_excel_csv("Tables/Specialty_Other.csv")

# years in practice

df %>%
  group_by(id) %>%
  summarise(duration_practice = first(duration_practice)) %>%
  ungroup() %>%
  filter(as.numeric(duration_practice)<60) %>%
  ggplot(aes(x = as.numeric(duration_practice))) +
  geom_histogram(fill = c_ice, binwidth = 5,
                 boundary = 0) +
  theme_minimal() +
  labs(title = "Duration in practice",
       y = "Count",
       x = "Years in practice") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("Figures/DurationPracticeHistorgram.svg",
       width = 6, height = 6)


median(df$duration_practice, na.rm = T)

quantile(df$duration_practice, 0.25, na.rm = T)
quantile(df$duration_practice, 0.75, na.rm = T)

# roles in Canada vs non Canada

df %>%
  group_by(id) %>%
  summarise(role = first(role),
            Canada = (first(country) == "Canada")) %>%
  ungroup() %>%
  group_by(role, Canada) %>%
  summarise(count = n()) %>%
  arrange(desc(Canada), desc(count))



################# Identify language  ##############

# identify language of response

# Install and load the cld2 package
#install.packages("cld2")
library(cld2)

# Your data frame with text columns
# For example, assuming you have columns named text1, text2, text3, ...
text_columns <- c("clinician","role", "age","diagnosis", "frailty", "rr", 
                  "work_of_breathing", "duration", "loc","norepinephrine", 
                  "spo2", "fio2","response")

# Function to detect language in concatenated text from all columns
assign_language <- function(row) {
  combined_text <- paste(row, collapse = " ")
  cld2::detect_language(combined_text)
}

# Apply the function to each row of your data frame
df_raw$language <- apply(df_raw[text_columns], 1, assign_language)

# gets all but some spanish / portuguese responses

df_raw <- 
df_raw %>%
  mutate(language = case_when(
    is.na(language) & clinician == "S?" ~ "es",
    is.na(language) & clinician == "Sim" ~ "pt",
    TRUE ~ language))


####################### Translate and organize qualitative text ##########

# translate qualitative text
#install.packages("gtranslate")
library(gtranslate)

translate_row <- function(x){
  if(!is.na(x[1]) & x[2] != "en"){
  translate(x[1],
            from = x[2],
            to = "en")}
  else {return(x[1])}
}

translations_other <- apply(df_raw[,c("additional_info_other",
                             "language")],
                      1, translate_row)

translations_specialty <- apply(df_raw[,c("specialty_other_text",
                                      "language")],
                            1, translate_row)

df_raw$translations_other <- translations_other
df_raw$translations_specialty <- translations_specialty

# text for qualitative translation

select(df_raw, language, specialty_other_text, 
       translations_specialty,
       additional_info_other,
       translations_other) %>%
  filter(!is.na(specialty_other_text) | !is.na(additional_info_other)) %>%
  filter(language != "en") %>%
  distinct() %>%
  arrange(language, is.na(specialty_other_text)) %>%
  readr::write_excel_csv("responses_for_translation.csv")

# output all qualitative responses and their data to a dataset

df_raw %>%
  filter(!is.na(additional_info_other)) %>%
  mutate(additional_info_other =
           ifelse(is.na(translations_other),
                  additional_info_other,
                  translations_other),
         specialty_other_text = 
           ifelse(!is.na(specialty_other_text) & language == "en",
                  specialty_other_text,
                  translations_specialty)
         ) %>%
  select(record_id, language, specialty_other_text, additional_info_other) %>%
  right_join(select(
    filter(df,!is.na(additional_info_other)),
           -specialty_other_text,-additional_info_other),
    by = "record_id") %>%
#  saveRDS("qualitative_translated_df.rds")
  write_excel_csv("qualitative_translated_df.csv")

########################## Tables #########################



df_table <- left_join(df,
                select(df_raw,record_id, language),
                by = "record_id")

# by sub region

labelmake <- function(x,y){
  paste(x, " (", 
        round(100*x/y), "%)",
        sep = "")
}

labelmake2 <- function(x, y){
  paste(x, " (", 
        round(100*y), "%)",
        sep = "")
}

N <- length(unique(df$id))
duration_practice_cat <- c(0, 6, 11, 20, 60)

df_table_whole <- df_table %>%
  mutate(region = "Total")

#t1 <- 
  df_table %>% 
  mutate(region = case_when(sub.region == "Northern America" ~ "Northern America",
                            region == "Americas" ~ "Latin America and the Caribbean",
                            T ~ region)) %>%
  rbind(df_table_whole) %>%
  select(id:sub.region, -specialty_other_text, language) %>%
  distinct() %>%
  mutate(val = T) %>%
  pivot_wider(names_from = role,
              values_from = val) %>%
  mutate(duration_practice = cut(duration_practice,
                                 duration_practice_cat,
                                 include.lowest = T,
                                 right = F,
                                 ordered_result = T),
         val = T) %>%
  pivot_wider(names_from = duration_practice,
              values_from = val,
              names_prefix = "duration") %>%
  mutate(val = T) %>%
  pivot_wider(names_from = language,
              values_from = val) %>%
  group_by(region) %>%
  select(-id, -country, -alpha.3, -sub.region) %>%
  mutate(n = 1) %>%
  select(n, everything()) %>%
  summarise_all(sum, na.rm = T) %>%
  mutate(across(c(-region, -n), labelmake, n)) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column() %>%
  rbind(data.frame(
    rowname = c("Language","Specialty",
                 "Role",
                 "Duration in practice"),
    X1 = "",
    X2 = "",
    X3 = "",
    X4 = "",
    X5 = "",
    X6 = "",
    X7 = ""
  )) %>%
  mutate(rowname = factor(rowname,
                          levels = c("region", 
                    "n",
                    "Language",
                    "ms",
                    "zh",
                    "en",
                    "de",
                    "fr",
                    "it",
                    "ja",
                    "pt",
                    "es",
                    "th",
                    "Specialty",
                    "specialty_ccm",
                    "specialty_anesthesia",
                    "specialty_em",
                    "specialty_other",
                    "Role",
                    "Nurse",
                    'Attending physician',
                    'Other clinical role',
                    'Trainee physician',
                    'Respiratory therapist',
                    "Duration in practice",
                    'duration[0,6)',
                    'duration[6,11)',
                    'duration[11,20)',
                    'duration[20,60]',
                    'durationNA'),
         labels = c("  Region", 
                    "  Total",
                    "Survey language",
                    "  Bahasa Indonesia",
                    "  Chinese (simplified)",
                    "  English",
                    "  German",
                    "  French",
                    "  Italian",
                    "  Japanese",
                    "  Portuguese",
                    "  Spanish",
                    "  Thai",
                    "Specialty",
                    "  Critical care medicine",
                    "  Anesthesia",
                    "  Emergency medicine",
                    "  Other",
                    "Role",
                    "  Nurse",
                    '  Attending physician',
                    '  Other clinical role',
                    '  Trainee physician',
                    '  Respiratory therapist',
                    "Duration in practice",
                    '  0-5 years',
                    '  6-10 years',
                    '  11-20 years',
                    '  20-60 years',
                    '  Missing'),
         ordered = T)) %>%
    arrange(rowname) %>%
    select(rowname,
           X7, X5, X3, X2, X6, X4, X1) %>%
  write_excel_csv("Tables/table1.csv")

  
t2a <- 
  df_table %>%
  mutate(region = case_when(sub.region == "Northern America" ~ "Northern America",
                              region == "Americas" ~ "Latin America and the Caribbean",
                              T ~ region)) %>%
  rbind(df_table_whole) %>%
  select(id, region, response:additional_info_other) %>%
  group_by(region) %>%
  mutate(total_by_group = max(n()),
         abg = labelmake2(sum(arterial_blood_gas),
                         mean(arterial_blood_gas)),
         peep = labelmake2(sum(peep_if_on_niv),
                                     mean(peep_if_on_niv)),
         vt = labelmake2(sum(tidal_volume_if_on_niv),
                           mean(tidal_volume_if_on_niv)),
         cxr = labelmake2(sum(chest_x_ray),
                           mean(chest_x_ray)),
         esop = labelmake2(sum(esophageal_pressure),
                           mean(esophageal_pressure)),
         obstime = labelmake2(sum(more_observation_time),
                           mean(more_observation_time))) %>%
  group_by(region, response) %>%
  summarise(count = n(),
            prop = n()/first(total_by_group),
            tbg = first(total_by_group),
            abg = first(abg),
            peep = first(peep),
            vt = first(vt),
            cxr = first(cxr),
            esop = first(esop),
            obstime = first(obstime)) %>%
  mutate(lbl = labelmake2(count, prop)) %>%
  select(region, response, tbg, lbl, abg:obstime) %>%
  pivot_wider(id_cols = c(region:tbg, abg:obstime),
              names_from = response,
              values_from = lbl) %>%
    t() %>%
    data.frame() %>%
    rownames_to_column() %>%
    rbind(
      data.frame(
        rowname = c("Recommendation",
                    "Response",
                    "Additional information"),
        X1 = "",
        X2 = "",
        X3 = "",
        X4 = "",
        X5 = "",
        X6 = "",
        X7 = ""
      ) 
    ) %>%
    select(rowname,
           X7, X5, X3, X2, X6, X4, X1) 

  allten<-  df_table %>%
    group_by(id) %>%
    summarise(allten = max(row_number())==10)
  
#t2b <-  
  df_table %>%
    mutate(region = case_when(sub.region == "Northern America" ~ "Northern America",
                              region == "Americas" ~ "Latin America and the Caribbean",
                              T ~ region)) %>%
    rbind(df_table_whole) %>%
    left_join(allten, by = "id") %>%
    select(id, region, allten) %>%
    group_by(region, id) %>%
    summarise(allten = first(allten),
              count = n()) %>%
    group_by(region) %>%
    summarise(sum = n(),
              mean = round(mean(count),1),
              allten = labelmake2(sum(allten),
                                  mean(allten))) %>%
    t() %>%
    data.frame() %>%
    rownames_to_column() %>%
    rbind(filter(t2a, rowname != "region")) %>%
    mutate(
    rowname = 
      factor(rowname,
             levels = c("region",
                        "Response",
                        "tbg",
                        "sum",
                        "mean",
                        "allten",
                        "Recommendation",
                        "Definite yes",
                        "Probable yes",
                        "Uncertain",
                        "Probable no",
                        "Definite no",
                        "Additional information",
                        "abg",
                        "cxr",
                        "obstime",
                        "peep",
                        "vt",
                        "esop"),
             labels = c("Region",
                        "Responses",
                        "  Total",
                        "  Respondents",
                        "  Mean responses per respondent",
                        "  Respondents with 10 responses",
                        "Recommendation",
                        "  Definite yes",
                        "  Probable yes",
                        "  Uncertain",
                        "  Probable no",
                        "  Definite no",
                        "Additional information",
                        "  Arterial blood gas",
                        "  Chest x-ray",
                        "  More observation time",
                        "  PEEP (if on NIV)",
                        "  Tidal volume (if on NIV)",
                        "  Esophageal pressure"),
             ordered = T)) %>%
  arrange(rowname) %>%
    select(rowname,
           X7, X5, X3, X2, X6, X4, X1) %>%
  write_excel_csv("Tables/table2.csv")

left_join(t1, t2, by = "variable")

# Scenario variables table

names(df)

# such that category is b1 <= category < b2
duration_practice_cat <- c(0, 6, 11, 20, 60)
age_cat  <- c(20, 40, 50, 60, 70)
spo2_cat <- c(85, 89, 92, 95, 97)
rr_cat   <- c(15, 21, 25, 28, 33, 40)


df %>% 
  select(diagnosis:duration) %>%
  mutate(age  = cut(age, age_cat, include.lowest = T, right = F,
                    ordered_result = T),
         spo2 = cut(spo2, spo2_cat, 
                    include.lowest = T, 
                    right = F,
                    ordered_result = T),
         rr   = cut(rr, rr_cat, 
                    include.lowest = T,
                    right = F,
                    ordered_result = T)) %>%
  mutate(across(everything(), as.character)) %>%
  group_by(diagnosis) %>%
  mutate(total = "Total") %>%
  pivot_longer(age:total, names_to = "Variable") %>%
  count(diagnosis, Variable, value) %>%
  group_by(diagnosis, Variable) %>%
  mutate(perc = round(100*n/sum(n))) %>%
  mutate(text = paste0(n, " (", perc, "%)")) %>%
  select(-n, -perc) %>%
  pivot_wider(names_from = diagnosis, 
              values_from = text,
              values_fill = "0 (0%)") %>%
  bind_rows(data.frame(Variable = c("age",
                         "duration",
                         "fio2",
                         "frailty",
                         "loc",
                         "norepinephrine",
                         "o2_device",
                         "rr",
                         "spo2",
                         "work_of_breathing"),
            value = c("Age",
                      "Duration",
                      "Inspired oxygen fraction",
                      "Pre-morbid functional status",
                      "Level of consciousness",
                      "Norepinephrine use",
                      "Oxygen device",
                      "Respiratory rate",
                      "Oxygen saturation",
                      "Breathing pattern"),
            `Community-acquired pneumonia` = "",
            `COVID pneumonia` = "",
            `Influenza pneumonia` = "",
            Pancreatitis = "",
            Sepsis = "")) %>%
  ungroup() %>%
  mutate(
    value = 
      factor(
        value,
        levels = c(
          "Total",
          "Age",
          "[20,40)",
          "[40,50)",
          "[50,60)",
          "[60,70]",
          "Inspired oxygen fraction",
          "0.4",
          "0.5",
          "0.6",
          "0.7",
          "0.8",
          "0.9",
          "1",
          "Pre-morbid functional status",
          "Independent and fit",
          "Independent with well-controlled medical problems",
          "Assistance for shopping and heavy housework",
           "Oxygen device",
         "High-flow nasal oxygen",
          "Non-invasive ventilation",
          "Respiratory rate",
          "[15,21)",
          "[21,25)",
          "[25,28)",
          "[28,33)",
          "[33,40]",
          "Oxygen saturation",
          "[85,89)",
          "[89,92)",
          "[92,95)",
          "[95,97]",
          "Norepinephrine use",
          "No",
          "Yes",
"Breathing pattern",
          "No use of neck muscles, no abdominal paradox",
          "Use of neck muscles, abdominal paradox",
          "Use of neck muscles, no abdominal paradox",
          "Level of consciousness",
          "Alert and obeying",
          "Drowsy but obeying",
          "Drowsy, not obeying",
          "Duration",
"10 minutes",
"30 minutes",
"1 hour",
"2 hours",
"4 hours"
        ),
labels = c(
  "Total",
  "Age",
  "  [20,40)",
  "  [40,50)",
  "  [50,60)",
  "  [60,70]",
  "Inspired oxygen fraction",
  "  0.4",
  "  0.5",
  "  0.6",
  "  0.7",
  "  0.8",
  "  0.9",
  "  1",
  "Pre-morbid functional status",
  "  Independent and fit",
  "  Independent with well-controlled medical problems",
  "  Assistance for shopping and heavy housework",
  "Oxygen device",
  "  High-flow nasal oxygen",
  "  Non-invasive ventilation",
  "Respiratory rate",
  "  15 to 20",
  "  21 to 24",
  "  25 to 27",
  "  28 to 32",
  "  33 to 40",
  "Oxygen saturation",
  "  85 to 88",
  "  89 to 91",
  "  92 to 94",
  "  95 to 97",
  "Norepinephrine use",
  "  No",
  "  Yes",
  "Breathing pattern",
  "  No use of neck muscles, no abdominal paradox",
  "  Use of neck muscles, abdominal paradox",
  "  Use of neck muscles, no abdominal paradox",
  "Level of consciousness",
  "  Alert and obeying",
  "  Drowsy but obeying",
  "  Drowsy, not obeying",
  "Duration",
  "  10 minutes",
  "  30 minutes",
  "  1 hour",
  "  2 hours",
  "  4 hours"
),
ordered = T
      )) %>%
  arrange(value)  %>%
  write_excel_csv("Tables/ScenarioTable.csv")


############### Primary BRMS model ############

# prepare data frame
apply(is.na(df), 2, sum) # duration_practice is only one with missing


# categorizations of continuous variables

# lower boundary b of each category
# such that category is b1 <= category < b2
duration_practice_cat <- c(0, 6, 11, 20, 60)
age_cat  <- c(20, 40, 50, 60, 70)
spo2_cat <- c(85, 89, 92, 95, 97)
rr_cat   <- c(15, 21, 25, 28, 33, 40)

brm_df <-
  df %>%
    # remove other text, datetime, additional info
    dplyr::select(id:specialty_other, role:response) %>%
    # categorize according  to prespecified scheme
    mutate(duration_practice = cut(duration_practice,
                                   duration_practice_cat,
                                   include.lowest = T,
                                   right = F,
                                   ordered_result = T),
           age  = cut(age, age_cat, include.lowest = T, right = F,
                      ordered_result = T),
           spo2 = cut(spo2, spo2_cat, 
                      include.lowest = T, 
                      right = F,
                      ordered_result = T),
           rr   = cut(rr, rr_cat, 
                      include.lowest = T,
                      right = F,
                      ordered_result = T)) %>%
  # remove 135 (double-check #) responses with duration_practice = NA
  filter(!is.na(duration_practice)) %>%
  # make the factors unordered to avoid R making orthogonal polynomials for the 
  # coefficients...
  mutate(across(id:duration, factor, ordered = F)) %>%
  # reverse the spo2 factor to set 95-97 to be the baseline
  mutate(spo2 = fct_rev(spo2))
  
# everything categorical, all interactions for patient variables, horseshoe prior
# N_test <- 100
# test_respondents <- sample(unique(brm_df$id), size = N_test)
# test_df <- filter(brm_df, id %in% test_respondents)

fit_data <- brm_df # set to brm_df for full analysis 

model <- brm(
  data = fit_data,
  formula = response ~
    # respondent characteristics
    ## random effects
    (1 | id) + (1 | country) + (1 | region) + #(1 | A) + (1 | B) = (1 | A/B) if A labels vary by B clusters
    ## fixed effects
    specialty_ccm + specialty_anesthesia + specialty_em + specialty_other +
    role + duration_practice +
    # clinical scenario
    ## fixed effects single variable
    diagnosis +
    age + frailty +
    spo2 + fio2 + o2_device + rr +
    work_of_breathing + norepinephrine + loc +
    duration +
    # interactions
    (diagnosis +
       age + frailty +
       spo2 + fio2 + o2_device + rr +
       work_of_breathing + norepinephrine + loc +
       duration):(diagnosis +
                    age + frailty +
                    spo2 + fio2 + o2_device + rr +
                    work_of_breathing + norepinephrine + loc +
                    duration),
  prior =  c(set_prior(horseshoe(df = 3, par_ratio = 0.15), class = "b"),
             set_prior("normal(0, 0.5)", class = "sd")),
  chains = 0,
  family = cumulative("logit")
)

saveRDS(model, file = "model_horseshoe.rds")
model <- readRDS("model_horseshoe.rds")

# simulate from priors
prior_sim <- update(
              object = model,
              newdata = fit_data,
              chains = 4,
              cores = 4,
              iter = 1000,
              sample_prior = "only",
              family = cumulative("logit")
            )

# model fit
fit <- update(
  object = model,
  newdata = fit_data,
  chains = 4,
  cores = 4,
  iter = 1000,
  control = list(adapt_delta = 0.9),
  family = cumulative("logit")
)

saveRDS(fit, file = "full_fit.rds")

############### Load fitted primary model ##################
fit <- readRDS("full_fit.rds")
# post <- as_draws_df(prior_sim)
post <- as_draws_df(fit)

summ <- posterior::summarise_draws(fit)

############### Prior figures ##############
#post_fe <- 
  select(post, starts_with("b_")) %>%
    pivot_longer(everything()) %>%
    mutate(name = gsub("b_","",name)) %>%
    filter(!grepl("Intercept",name)) %>%
    group_by(name) %>%
    summarise(post_mean = mean(value),
              post_lb95 = quantile(value, 0.025),
              post_ub95 = quantile(value, 0.975),
              prob_lessthan_rope = mean(value < rope[1]),
              prob_morethan_rope = mean(value > rope[2])) %>%
 #   filter(post_lb95 > 0 | post_ub95 < 0) %>%
    mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
    arrange(post_mean) %>%
    mutate(name = factor(name, levels = name, ordered = T),
           ORlabel = paste0(round(post_mean, 2), " (",
                            round(post_lb95,2), " to ",
                            round(post_ub95,2), ")")) %>%
    ggplot(aes(x = name, y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
    geom_pointrange() + 
    geom_hline(yintercept = 1) +
    theme_minimal() +
    coord_flip() +
    scale_y_continuous(trans = "log") +
   # theme(axis.text.y = element_blank()) +
    labs(y = "Posterior mean odds ratio (95% credible interval)",
         x = "Variable (n = 497)",
         title = "Prior distribution of odds ratios")
  ggsave("Figures/PriorORs.svg", width = 7, height = 9)
    
post_pred <- posterior_predict(prior_sim)

post_pred %>%
  table() %>%
  as.data.frame() %>%
  rename(Response = ".",
         Count = Freq) %>%
  mutate(Count = 100*Count/length(post_pred)) %>%
  rename(Percentage = Count) %>%
  ggplot(aes(x = Response, y = Percentage)) +
  geom_col(fill = c_navy) +
  theme_minimal() +
  scale_x_discrete(labels = c("Definite no",
                              "Probable no",
                              "Uncertain",
                              "Probably yes",
                              "Definite yes")) +
  labs(title = "Distribution of responses simulated from prior distribution",
       x = "") 
ggsave("Figures/PriorResponses.svg", width = 6, height = 6)

############### Geographic figures #####################

# regions
post_regions <- 
  select(post, starts_with("r_region")) %>%
  mutate(iter = row_number()) %>%
  pivot_longer(cols = -iter) %>%
  mutate(name = gsub("r_region\\[","",name)) %>%
  mutate(name = gsub(",Intercept\\]", "", name)) %>%
  mutate(name = gsub("\\.", " ", name)) %>%
  rename(region = name)

post_countries <- 
  select(post, starts_with("r_country")) %>%
  mutate(iter = row_number()) %>%
  pivot_longer(cols = -iter) %>%
  mutate(name = gsub("r_country\\[","",name)) %>%
    mutate(name = gsub(",Intercept\\]", "", name)) %>%
  mutate(name = gsub("\\.", " ", name)) %>%
  rename(country = name)

# country-region pairs

country_region <- 
  df %>% 
  distinct(country, region)

post_country_region <-
  post_countries %>%
  left_join(country_region, by = "country") %>%
  left_join(post_regions, by = c("region","iter")) %>%
  mutate(coef = value.x + value.y) 
  
gap_size = 1

# # more conventional forest plot by country
# post_country_region %>%
#   group_by(country, region) %>%
#   summarise(post_mean = mean(coef),
#             post_lb95 = quantile(coef, 0.025),
#             post_ub95 = quantile(coef, 0.975)) %>%
#   mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
#   group_by(region) %>%
#   arrange(post_mean, .by_group = T) %>%
#   mutate(r_row_number = row_number()) %>%
#   mutate(n_in_region = max(r_row_number)) %>%
#   ungroup() %>%
#   mutate(g_row_number = row_number()) %>%
#   mutate(g_row_number = ifelse(
#     region == "Africa", g_row_number,
#     ifelse(region == "Americas", g_row_number + gap_size,
#            ifelse(region == "Asia", g_row_number + 2*gap_size,
#                   ifelse(region == "Europe", g_row_number + 3*gap_size,
#                          g_row_number + 4*gap_size))))) %>%
# 
#   mutate(ORlabel = paste0(round(post_mean, 2), " (",
#                           round(post_lb95,2), " to ",
#                           round(post_ub95,2), ")"),
#          equiv = factor(case_when(post_ub95 < 1 ~ 1,
#                            post_lb95 < 1 ~ 2,
#                            post_lb95 > 1 ~ 3))) %>%
# # shaded rows?
#   ggplot(aes(x = g_row_number, y = post_mean, ymin = post_lb95, ymax = post_ub95)) +
#   geom_hline(yintercept = 1, color = "gray") +
#   geom_rect(aes(xmin = g_row_number - 0.5,
#                   xmax = g_row_number + 0.5), 
#               fill = c_ice,
#               alpha = 0.25) +
#     geom_point(size = 2, aes(color = equiv)) +
#   theme_minimal() + 
#   scale_y_continuous(breaks = c(1/4,1/2,1,2,4), 
#                        limits = c(1/6,8),
#                        trans = "log") +
#   theme(panel.grid = element_blank(),
#         axis.text.y = element_blank()) +
#   geom_text(aes(label = country, y = post_mean*0.95,
#                 color = equiv), 
#             hjust = 1, 
#             vjust = 0.25,
#             size = 2) +
#   scale_color_manual(values = c(c_navy, "black", c_dark),
#                      labels = c("Less likely",
#                                 "Equivalent",
#                                 "More likely"),
#                      name = "Odds ratio") +
#   coord_flip() +
#   labs(x = "",
#        y = "<< Less likely      Odds ratio of recommending intubation      More likely >>")

#  circular plot by country

gap_size <- 2
total_gaps <- 73+gap_size*6

or_limits <- c(0.35, 2.25)

 post_region_summarised <-
    post_country_region %>%
    group_by(region) %>%
    summarise(region_mean = exp(mean(coef)))

post_country_region_summarised <- 
  post_country_region %>%
  group_by(country, region) %>%
  summarise(post_mean = mean(coef),
            post_lb95 = quantile(coef, 0.025),
            post_ub95 = quantile(coef, 0.975)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  mutate(region = factor(region,
                         levels = c("Europe",
                                    "Oceania",
                                    "Asia",
                                    "Africa",
                                    "Americas"
                                    ),
                         ordered = T)) %>%
  group_by(region) %>%
  arrange(post_mean, .by_group = T) %>%
  mutate(r_row_number = row_number()) %>%
  mutate(n_in_region = max(r_row_number)) %>%
  ungroup() %>%
  mutate(g_row_number = row_number()) %>%
  mutate(g_row_number = ifelse(
    region == "Europe", g_row_number+gap_size,
    ifelse(region == "Oceania", g_row_number + 2*gap_size,
           ifelse(region == "Asia", g_row_number + 3*gap_size,
                  ifelse(region == "Africa", g_row_number + 4*gap_size,
                         g_row_number + 5*gap_size))))) %>%
  mutate(country = case_when(grepl("Venezuela", country) ~ "Venezuela",
                             grepl("Taiwan", country) ~ "Taiwan",
                             grepl("Tanzania", country) ~ "Tanzania",
                             T ~ country)) %>%  
  mutate(ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")"),
         equiv = factor(case_when(post_ub95 < 1 ~ 1,
                                  post_lb95 < 1 ~ 2,
                                  post_lb95 > 1 ~ 3))) %>%
  bind_rows(data.frame(
    country = NA,
    region = NA,
    post_mean = NA,
    post_lb95 = NA,
    post_ub95 = NA,
    r_row_number = NA,
    n_in_region = NA,
    g_row_number = c(1:gap_size,(total_gaps+1-gap_size):total_gaps),
    ORlabel = NA,
    equiv = NA
  )) %>%
  left_join(post_region_summarised, by = "region")

post_country_region_summarised %>%
  mutate(country = ifelse(grepl("Emirates", country), "UAE",country)) %>%
  ggplot(aes(x = g_row_number, y = post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_rect(aes(xmin = g_row_number-0.5,
                xmax = g_row_number+0.5),
            fill = "#eaeff6") +
  geom_segment(y = 0, yend = 0, x = 1.5, xend = total_gaps-0.5, color = "gray65") +
  geom_segment(y = log(0.5), yend = log(0.5), x = 1.5, xend = total_gaps-0.5, color = "gray65") +
  geom_segment(y = log(2), yend = log(2), x = 1.5, xend = total_gaps-0.5, color = "gray65") +
  geom_point(size = 2, color= "white") +
  geom_point(size = 1.25, color= "black") +
  theme_minimal() + 
  scale_y_continuous(breaks = c(1/4,1/2,1,2), 
                     limits = c(1/1000,12),
                     trans = "log") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank()) +
  geom_text(aes(label = country, y = max(post_mean*1.5, na.rm=T),
                angle = ifelse(g_row_number < total_gaps/2, 
                               90-360*g_row_number/total_gaps,
                               270-360*g_row_number/total_gaps),
                hjust = ifelse(g_row_number < total_gaps/2, 
                               0,1)), 
            vjust = 0.25,
            size = 2.25) +
  geom_text(y = -0.02, x = 0.5, label = "1.0", size = 3, color = "gray65") +
  geom_text(y = log(1.95), x = 0.5, label = "2.0", size = 3, color = "gray65") +
  geom_text(y = log(0.48), x = 0.5, label = "0.5", size = 3,color = "gray65") +
  scale_color_manual(values = c(c_dark, "black", c_dark),
                     guide = "none") +
  coord_polar() +
  theme(axis.text.x = element_blank())+
  labs(x = "",
       y = "")
ggsave("Figures/WtI_CircularCountries.svg", height = 12, width = 12)


# map by country

world <- map_data("world")

# fix names
unique(post_country_region$country)[
  which(!(unique(post_country_region$country) %in% world$region))] 

post_country_region %>%
  mutate(country = as.character(country)) %>%
  mutate(
    country = case_when(
      grepl("Czechia", country) ~ "Czech Republic",
      grepl("Macedonia", country) ~ "Macedonia",
      grepl("Russia", country) ~ "Russia",
      grepl("Taiwan", country) ~ "Taiwan",
      grepl("Tanzania", country) ~ "Tanzania",
      grepl("United Kingdom", country) ~ "UK",
      grepl("Venezuela", country) ~ "Venezuela",
      grepl("Viet Nam", country) ~ "Vietnam",
      T ~ country)) %>%
  group_by(country) %>%
  summarise(post_mean = mean(coef),
            post_lb95 = quantile(coef, 0.025),
            post_ub95 = quantile(coef, 0.975)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  mutate(post_or = cut(post_mean,
                       breaks = c(0.3,0.8,0.88,0.96,
                                  1.04, 1.14, 1.3, 2.5),
                       include.lowest = T)) %>%
  right_join(world, by = c("country" = "region")) %>%
  filter(!grepl("Antarctica", country)) %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = post_mean),
               color = "gray50",
               linewidth = 0.1) +
  scale_fill_gradient2(low = "#1a9850",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "gray95",
                       trans = "log",
                       breaks = c(0.5, 1, 2),
                       name = "Odds ratio for recommending intubation",
                       limits = or_limits) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top"))
ggsave("Figures/WtI_WorldMapRYG.svg", width = 7, height = 5)




############### Posterior OR  Figure, fixed effects ##################

posterior_or_figure <- function(post){

post_ors <- 
  select(post, starts_with("b_")) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_","",name)) %>%
  filter(!grepl("Intercept",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  arrange(post_mean)
  

# Main effects forest plot

or_categories <- data.frame(
  name = c("Role",
           "Specialty",
           "Duration in practice",
           "Patient age",
           "Frailty",
           "Diagnosis",
           "Inspired oxygen fraction (FiO2)",
           "Saturation",
           "Oxygen device",
           "Respiratory rate",
           "Breathing pattern",
           "Level of consciousness",
           "Norepinephrine",
           "Duration"),
  post_mean = NA,
  post_lb95 = NA,
  post_ub95 = NA,
  ORlabel = NA
)

or_reference <- data.frame(
  name = c("Attending physician",
           "0-5 years",
           "20-40",
           "Independent",
           "Community-acquired pneumonia",
           "0.4",
           "95-97",
           "High-flow nasal cannula",
           "15-20",
           "Normal",
           "Alert and obeying",
           "No",
           "10 minutes"),
  post_mean = 1,
  post_lb95 = 1,
  post_ub95 = 1,
  ORlabel = "Reference"
)


post_or_fe <- 
post_ors %>%
  filter(!grepl(":", name)) %>%
  mutate(name = factor(name, levels = name, ordered = T),
         ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) %>%
  bind_rows(or_categories) %>%
  bind_rows(or_reference) %>%
  mutate(
    name = factor(
      name,
      levels = c(
        "duration4hours", 
        "duration2hours", 
        "duration1hour", 
        "duration30minutes", 
        "10 minutes",
        "Duration", 
        
        "norepinephrineYes", 
        "No", 
        "Norepinephrine", 

        "locDrowsynotobeying", 
        "locDrowsybutobeying",
        "Alert and obeying",
        "Level of consciousness",
        
        "work_of_breathingUseofneckmusclesabdominalparadox", 
        "work_of_breathingUseofneckmusclesnoabdominalparadox", 
        "Normal",
        "Breathing pattern",
        
        "rr3340", 
        "rr2833", 
        "rr2528", 
        "rr2125",
        "15-20",
        "Respiratory rate", 

        "o2_deviceNonMinvasiveventilation", 
        "High-flow nasal cannula", 
        "Oxygen device",
        
        "fio21", 
        "fio20.9", 
        "fio20.8", 
        "fio20.7", 
        "fio20.6", 
        "fio20.5",
        "0.4",
        "Inspired oxygen fraction (FiO2)", 

        "spo28589", 
        "spo28992", 
        "spo29295", 
        "95-97", 
        "Saturation",

        "diagnosisPancreatitis", 
        "diagnosisSepsis", 
         "diagnosisInfluenzapneumonia", 
       "Community-acquired pneumonia", 
        "diagnosisCOVIDpneumonia", 
        "Diagnosis", 

        "frailtyAssistanceforshoppingandheavyhousework", 
        "frailtyIndependentwithwellMcontrolledmedicalproblems", 
        "Independent",
        "Frailty",
        
        "age6070", 
        "age5060", 
        "age4050", 
        "20-40", 
        "Patient age", 

        "duration_practice2060", 
        "duration_practice1120", 
        "duration_practice611", 
        "0-5 years", 
        "Duration in practice", 
        
        "specialty_ccmTRUE", 
        "specialty_anesthesiaTRUE", 
        "specialty_otherTRUE", 
        "specialty_emTRUE", 
        "Specialty", 
        
        "roleTraineephysician", 
        "Attending physician", 
        "roleRespiratorytherapist", 
        "roleOtherclinicalrole", 
         "roleNurse", 
       "Role"),
      labels = c(
        "4 hours", 
        "2 hours", 
        "1 hour", 
        "30 minutes", 
        "10 minutes",
        "Duration", 
        
        "Yes", 
        "No", 
        "Norepinephrine use", 
        
        "Drowsy, not obeying", 
        "Drowsy but obeying",
        "Alert and obeying",
        "Level of consciousness",
        
        "Neck muscle use and abdominal paradox", 
        "Neck muscle use, no abdominal paradox", 
        "Normal",
        "Breathing pattern",
        
        "33-40", 
        "28-32", 
        "25-27", 
        "21-24",
        "15-20",
        "Respiratory rate", 
        
        "Non-inasive ventilation", 
        "High-flow nasal cannula", 
        "Oxygen device",
        
        "1.0", 
        "0.9", 
        "0.8", 
        "0.7", 
        "0.6", 
        "0.5",
        "0.4",
        "Inspired oxygen fraction", 
        
        "85-88", 
        "89-91", 
        "92-94", 
        "95-97", 
        "Saturation",
        
        "Pancreatitis", 
        "Sepsis", 
        "Influenza pneumonia", 
        "Community-acquired pneumonia", 
        "COVID-19 pneumonia", 
        "Diagnosis", 
        
        "Assistance for some IADLs", 
        "Independent with medical problems", 
        "Independent",
        "Frailty",
        
        "60-70 years", 
        "50-59 years", 
        "40-49 years", 
        "20-39 years", 
        "Patient age", 
        
        "21 or more years", 
        "11 to 20 years", 
        "6 to 10 years", 
        "5 or fewer years", 
        "Duration in practice", 
        
        "Critical care medicine", 
        "Anesthesia", 
        "Other", 
        "Emergency medicine", 
        "Specialty", 
        
        "Trainee physician", 
        "Attending physician", 
        "Respiratory therapist", 
        "Other clinical role", 
        "Nurse", 
        "Role"),
      ordered = T
      )) %>%
    arrange(name) %>%
  mutate(header = is.na(ORlabel)) %>%
  mutate(group = case_when(
    name %in% c(
    "4 hours", 
    "2 hours", 
    "1 hour", 
    "30 minutes", 
    "10 minutes",
    "Duration") ~ "Duration",
    name %in% c(
    "Yes", 
    "No", 
    "Norepinephrine use") ~ "Norepinephrine",
    name %in% c(
    "Drowsy, not obeying", 
    "Drowsy but obeying",
    "Alert and obeying",
    "Level of consciousness") ~ "LOC",
    name %in% c(
    "Neck muscle use and abdominal paradox", 
    "Neck muscle use, no abdominal paradox", 
    "Normal",
    "Breathing pattern") ~ "BreathingPattern",
    name %in% c(
    "33-40", 
    "28-32", 
    "25-27", 
    "21-24",
    "15-20",
    "Respiratory rate") ~ "RR",
    name %in% c(
    "Non-inasive ventilation", 
    "High-flow nasal cannula", 
    "Oxygen device") ~ "o2device",
    name %in% c(
    "1.0", 
    "0.9", 
    "0.8", 
    "0.7", 
    "0.6", 
    "0.5",
    "0.4",
    "Inspired oxygen fraction") ~ "FiO2",
    name %in% c(
    "85-88", 
    "89-91", 
    "92-94", 
    "95-97", 
    "Saturation") ~ "SpO2",
    name %in% c(
    "Pancreatitis", 
    "Sepsis", 
    "Influenza pneumonia", 
    "Community-acquired pneumonia", 
    "COVID-19 pneumonia", 
    "Diagnosis") ~ "Diagnosis", 
    name %in% c(
    "Assistance for some IADLs", 
    "Independent with medical problems", 
    "Independent",
    "Frailty") ~ "Frailty",
    name %in% c(
    "60-70 years", 
    "50-59 years", 
    "40-49 years", 
    "20-39 years", 
    "Patient age") ~ "Age", 
    name %in% c(
    "21 or more years", 
    "11 to 20 years", 
    "6 to 10 years", 
    "5 or fewer years", 
    "Duration in practice") ~ "DurationPractice",
    name %in% c(
    "Critical care medicine", 
    "Anesthesia", 
    "Other", 
    "Emergency medicine", 
    "Specialty") ~ "Specialty", 
    name %in% c(
    "Trainee physician", 
    "Attending physician", 
    "Respiratory therapist", 
    "Other clinical role", 
    "Nurse", 
    "Role") ~ "Role")) %>%
  mutate(group = factor(group,
                        levels = c("Role",
                                   "Specialty",
                                   "DurationPractice",
                                   "Age",
                                   "Frailty",
                                   "Diagnosis",
                                   "SpO2",
                                   "FiO2",
                                   "o2device",
                                   "RR",
                                   "BreathingPattern",
                                   "LOC",
                                   "Norepinephrine",
                                   "Duration"),
                        ordered = T,
                        labels = c("Role",
                                   "Specialty",
                                   "Duration in practice",
                                   "Patient age",
                                   "Patient frailty",
                                   "Diagnosis",
                                   "Oxygen saturation",
                                   "Inspired oxygen fraction",
                                   "Oxygen device",
                                   "Respiratory rate",
                                   "Breathing Pattern",
                                   "Level of consciousness",
                                   "Norepinephrine use",
                                   "Duration in current state")))


ylim = c(0.02, 12.4)


post_or_fe %>%
  filter(header == F) %>%
  ggplot(aes(x = as.integer(name), y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_rect(aes(xmin = as.integer(name)-0.5,
                xmax = as.integer(name)+0.5,
                ymin = ylim[1],
                ymax = ylim[2],
                fill = factor(as.integer(name)%%2)))+
  geom_hline(yintercept = 1, color = "gray") +
  geom_pointrange(size = 0.25) + 
  geom_text(
    data = filter(post_or_fe, header == F),
    aes(label = name,
        x = as.integer(name)), 
    y = log(ylim[1]+0.001),
    hjust = 0,
    vjust = 0.5,
    size = 2) +
  geom_text(
    aes(label = ORlabel,
        x = as.integer(name)), 
    y = log(0.5),
    hjust = 1,
    vjust = 0.5,
    size = 2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = 
          element_text(margin = 
                         margin(
                           t = 10, 
                           r = 0, 
                           b = 0, 
                           l = 0))) +
  coord_flip() +
  scale_fill_manual(values = c("grey95","white"),
                    guide = "none") +
  scale_y_continuous(trans = "log",
                     limits = ylim,
                     breaks = c(0.5, 1, 2, 4, 8)) +
  labs(y = "Posterior mean odds ratio (95% credible interval)",
       x = "",
       caption = "Odds ratios less than 1 = less likely to recommend intubation \nOdds ratios more than 1 = more likely to recommend intubation",
       title = "Posterior odds ratios for recommending intubation, by category of variable") +
  facet_wrap(.~group,
             scales = "free_y",
             ncol = 2)
}

posterior_or_figure(post)

ggsave("Figures/PosteriorOR_fixed.svg",
       width = 10, height = 8)
ggsave("Figures/PosteriorOR_fixed.pdf",
       width = 10, height = 8)
ggsave("Figures/PosteriorOR_ppt.svg",
       width = 12, height = 8)
ggsave("Figures/PosteriorOR_fixed.png",
       width = 10, height = 8, dpi = 300)

# Posterior OR figure, 1 CrI filter

select(post, starts_with("b_")) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_","",name)) %>%
  filter(!grepl("Intercept",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  arrange(post_mean) %>%
  filter(post_lb95 > 1 | post_ub95 < 1) %>%
  mutate(name = factor(name, levels = name, ordered = T),
         ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) %>%
  ggplot(aes(x = name, y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_pointrange() + 
  geom_hline(yintercept = 1) +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(trans = "log") +
  labs(y = "Posterior mean odds ratio (95% credible interval)",
       x = "Variable",
       title = "Posterior odds ratios")


# Posterior OR figure, ROPE mean effect filter

post_summarised_notROPE <-
select(post, starts_with("b_")) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_","",name)) %>%
  filter(!grepl("Intercept",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  arrange(post_mean) %>%
  filter(post_mean > exp(rope[2]) | post_mean < exp(rope[1])) %>%
  mutate(name = factor(name, levels = name, ordered = T),
         ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) 

post_summarised_notROPE %>%
  filter(grepl(":",name))

  ggplot(aes(x = name, y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_pointrange() + 
  geom_hline(yintercept = 1) +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(trans = "log") +
  labs(y = "Posterior mean odds ratio (95% credible interval)",
       x = "Variable",
       title = "Posterior odds ratios")

############### Posterior OR figure, interactions ################

# heat map

# find all interaction coefficients

#heatmap_df <-
int_lessthanrope <-
  post %>%
  select(contains(":")) %>% 
  summarise(across(
    cols = everything,
    .fns = ~mean(.x < rope[1])))
int_morethanrope <-
  post %>%
  select(contains(":")) %>% 
  summarise(across(
    cols = everything,
    .fns = ~mean(.x > rope[2])))
int_mean <-
  post %>%
  select(contains(":")) %>% 
  summarise(across(
    cols = everything,
    .fns = mean))
int_lb95 <-
  post %>%
  select(contains(":")) %>% 
  summarise(across(
    cols = everything,
    .fns = ~quantile(.x, 0.025)))
int_ub95 <-
  post %>%
  select(contains(":")) %>% 
  summarise(across(
    cols = everything,
    .fns = ~quantile(.x, 0.975)))

heatmap_df <- 
bind_rows(mean = int_mean, 
      lb95 = int_lb95, 
      ub95 = int_ub95,
      morethanrope = int_morethanrope,
      lessthanrope = int_lessthanrope,
      .id = "id") %>%
pivot_longer(cols = -id) %>%
  mutate(name1 = sub(".+:",
                     "",
                     name),
         name2 = sub(":.+",
                     "",
                     name)) %>%
  select(-name) %>%
  mutate(name2 = sub("b_","", name2)) %>%
  filter(grepl("rope",id)) %>%
  mutate(value = cut(value,
                     seq(from = 0, to = 1, by = 0.1),
                     include.lowest = T)) %>%
  mutate(name1 = factor(
    name1,
    levels = c(
      "duration4hours", 
      "duration2hours", 
      "duration1hour", 
      "duration30minutes", 
      "10 minutes",
      "Duration", 
      
      "norepinephrineYes", 
      "No", 
      "Norepinephrine", 
      
      "locDrowsynotobeying", 
      "locDrowsybutobeying",
      "Alert and obeying",
      "Level of consciousness",
      
      "work_of_breathingUseofneckmusclesabdominalparadox", 
      "work_of_breathingUseofneckmusclesnoabdominalparadox", 
      "Normal",
      "Breathing pattern",
      
      "rr3340", 
      "rr2833", 
      "rr2528", 
      "rr2125",
      "15-20",
      "Respiratory rate", 
      
      "o2_deviceNonMinvasiveventilation", 
      "High-flow nasal cannula", 
      "Oxygen device",
      
      "fio21", 
      "fio20.9", 
      "fio20.8", 
      "fio20.7", 
      "fio20.6", 
      "fio20.5",
      "0.4",
      "Inspired oxygen fraction (FiO2)", 
      
      "spo28589", 
      "spo28992", 
      "spo29295", 
      "95-97", 
      "Saturation",
      
      "diagnosisPancreatitis", 
      "diagnosisSepsis", 
      "diagnosisInfluenzapneumonia", 
      "Community-acquired pneumonia", 
      "diagnosisCOVIDpneumonia", 
      "Diagnosis", 
      
      "frailtyAssistanceforshoppingandheavyhousework", 
      "frailtyIndependentwithwellMcontrolledmedicalproblems", 
      "Independent",
      "Frailty",
      
      "age6070", 
      "age5060", 
      "age4050", 
      "20-40", 
      "Patient age", 
      
      "duration_practice2060", 
      "duration_practice1120", 
      "duration_practice611", 
      "0-5 years", 
      "Duration in practice", 
      
      "specialty_ccmTRUE", 
      "specialty_anesthesiaTRUE", 
      "specialty_otherTRUE", 
      "specialty_emTRUE", 
      "Specialty", 
      
      "roleTraineephysician", 
      "Attending physician", 
      "roleRespiratorytherapist", 
      "roleOtherclinicalrole", 
      "roleNurse", 
      "Role"),
    labels = c(
      "Duration: 4 hours", 
      "Duration: 2 hours", 
      "Duration: 1 hour", 
      "Duration: 30 minutes", 
      "Duration: 10 minutes",
      "Duration", 
      
      "Norepinephrine: Yes", 
      "Norepinephrine: No", 
      "Norepinephrine use", 
      
      "LOC: Drowsy, not obeying", 
      "LOC: Drowsy but obeying",
      "LOC: Alert and obeying",
      "Level of consciousness",
      
      "Neck muscle use and abdominal paradox", 
      "Neck muscle use, no abdominal paradox", 
      "Normal",
      "Breathing pattern",
      
      "RR: 33-40", 
      "RR: 28-32", 
      "RR: 25-27", 
      "RR: 21-24",
      "RR: 15-20",
      "Respiratory rate", 
      
      "Non-inasive ventilation", 
      "High-flow nasal cannula", 
      "Oxygen device",
      
      "FiO2 1.0", 
      "FiO2 0.9", 
      "FiO2 0.8", 
      "FiO2 0.7", 
      "FiO2 0.6", 
      "FiO2 0.5",
      "FiO2 0.4",
      "Inspired oxygen fraction", 
      
      "SpO2 85-88", 
      "SpO2 89-91", 
      "SpO2 92-94", 
      "SpO2 95-97", 
      "Saturation",
      
      "Pancreatitis", 
      "Sepsis", 
      "Influenza pneumonia", 
      "Community-acquired pneumonia", 
      "COVID-19 pneumonia", 
      "Diagnosis", 
      
      "Assistance for some IADLs", 
      "Independent with medical problems", 
      "Independent",
      "Frailty",
      
      "60-70 years", 
      "50-59 years", 
      "40-49 years", 
      "20-39 years", 
      "Patient age", 
      
      "21 or more years", 
      "11 to 20 years", 
      "6 to 10 years", 
      "5 or fewer years", 
      "Duration in practice", 
      
      "Critical care medicine", 
      "Anesthesia", 
      "Other", 
      "Emergency medicine", 
      "Specialty", 
      
      "Trainee physician", 
      "Attending physician", 
      "Respiratory therapist", 
      "Other clinical role", 
      "Nurse", 
      "Role"),
    ordered = T
  ),
  name2 = factor(
    name2,
    levels = c(
      "duration4hours", 
      "duration2hours", 
      "duration1hour", 
      "duration30minutes", 
      "10 minutes",
      "Duration", 
      
      "norepinephrineYes", 
      "No", 
      "Norepinephrine", 
      
      "locDrowsynotobeying", 
      "locDrowsybutobeying",
      "Alert and obeying",
      "Level of consciousness",
      
      "work_of_breathingUseofneckmusclesabdominalparadox", 
      "work_of_breathingUseofneckmusclesnoabdominalparadox", 
      "Normal",
      "Breathing pattern",
      
      "rr3340", 
      "rr2833", 
      "rr2528", 
      "rr2125",
      "15-20",
      "Respiratory rate", 
      
      "o2_deviceNonMinvasiveventilation", 
      "High-flow nasal cannula", 
      "Oxygen device",
      
      "fio21", 
      "fio20.9", 
      "fio20.8", 
      "fio20.7", 
      "fio20.6", 
      "fio20.5",
      "0.4",
      "Inspired oxygen fraction (FiO2)", 
      
      "spo28589", 
      "spo28992", 
      "spo29295", 
      "95-97", 
      "Saturation",
      
      "diagnosisPancreatitis", 
      "diagnosisSepsis", 
      "diagnosisInfluenzapneumonia", 
      "Community-acquired pneumonia", 
      "diagnosisCOVIDpneumonia", 
      "Diagnosis", 
      
      "frailtyAssistanceforshoppingandheavyhousework", 
      "frailtyIndependentwithwellMcontrolledmedicalproblems", 
      "Independent",
      "Frailty",
      
      "age6070", 
      "age5060", 
      "age4050", 
      "20-40", 
      "Patient age", 
      
      "duration_practice2060", 
      "duration_practice1120", 
      "duration_practice611", 
      "0-5 years", 
      "Duration in practice", 
      
      "specialty_ccmTRUE", 
      "specialty_anesthesiaTRUE", 
      "specialty_otherTRUE", 
      "specialty_emTRUE", 
      "Specialty", 
      
      "roleTraineephysician", 
      "Attending physician", 
      "roleRespiratorytherapist", 
      "roleOtherclinicalrole", 
      "roleNurse", 
      "Role"),
    labels = c(
      "Duration: 4 hours", 
      "Duration: 2 hours", 
      "Duration: 1 hour", 
      "Duration: 30 minutes", 
      "Duration: 10 minutes",
      "Duration", 
      
      "Norepinephrine: Yes", 
      "Norepinephrine: No", 
      "Norepinephrine use", 
      
      "LOC: Drowsy, not obeying", 
      "LOC: Drowsy but obeying",
      "LOC: Alert and obeying",
      "Level of consciousness",
      
      "Neck muscle use and abdominal paradox", 
      "Neck muscle use, no abdominal paradox", 
      "Normal",
      "Breathing pattern",
      
      "RR: 33-40", 
      "RR: 28-32", 
      "RR: 25-27", 
      "RR: 21-24",
      "RR: 15-20",
      "Respiratory rate", 
      
      "Non-inasive ventilation", 
      "High-flow nasal cannula", 
      "Oxygen device",
      
      "FiO2 1.0", 
      "FiO2 0.9", 
      "FiO2 0.8", 
      "FiO2 0.7", 
      "FiO2 0.6", 
      "FiO2 0.5",
      "FiO2 0.4",
      "Inspired oxygen fraction", 
      
      "SpO2 85-88", 
      "SpO2 89-91", 
      "SpO2 92-94", 
      "SpO2 95-97", 
      "Saturation",
      
      "Pancreatitis", 
      "Sepsis", 
      "Influenza pneumonia", 
      "Community-acquired pneumonia", 
      "COVID-19 pneumonia", 
      "Diagnosis", 
      
      "Assistance for some IADLs", 
      "Independent with medical problems", 
      "Independent",
      "Frailty",
      
      "60-70 years", 
      "50-59 years", 
      "40-49 years", 
      "20-39 years", 
      "Patient age", 
      
      "21 or more years", 
      "11 to 20 years", 
      "6 to 10 years", 
      "5 or fewer years", 
      "Duration in practice", 
      
      "Critical care medicine", 
      "Anesthesia", 
      "Other", 
      "Emergency medicine", 
      "Specialty", 
      
      "Trainee physician", 
      "Attending physician", 
      "Respiratory therapist", 
      "Other clinical role", 
      "Nurse", 
      "Role"),
    ordered = T
  )) %>% 
  mutate(id = factor(id,
                     levels = c(
                       "morethanrope",
                       "lessthanrope"
                     ),
                     labels = c(
                       "Increased odds",
                       "Decreased odds"
                     )))
#  pivot_wider(names_from = id,
#              values_from = value) %>%
heatmap_plot <- function(heatmap_df, title){
  
heatmap_df %>%
  ggplot(aes(xmin = as.integer(name1), 
             xmax = as.integer(name1)+1,
             ymin = as.integer(name2), 
             ymax = as.integer(name2) + 1,
        fill = value)) +
  
    geom_rect(color = "grey90",
              linewidth = 0.25) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(hjust = 1)) + 
  scale_fill_brewer(palette = "RdYlBu",
                    direction = -1,
                    na.value = "grey85",
                    name = "Probability") +
#  facet_grid(.~id) +
  geom_text(aes(label = name1,
                x = as.integer(name1), 
                y = 54),
            angle = 90,
            hjust = 0,
            vjust = 0.75,
            size = 2
            ) +
  geom_text(aes(label = name2,
                y = as.integer(name2), 
                x = 0),
            hjust = 1,
            vjust = 0,
            size = 2
  ) +
  scale_x_continuous(limits = c(-17, 38)) +
  scale_y_continuous(limits = c(7, 70)) +
  labs(x = "", y = "",
       title = title) +
  coord_fixed() 
}

heatmap_plot(filter(heatmap_df, id == "Increased odds"),
             "Probability of odds ratio greater than ROPE")
ggsave("Figures/interaction_heatmap_greaterthanROPE.svg",
       width = 8, height = 8)

heatmap_plot(filter(heatmap_df, id == "Decreased odds"),
             "Probability of odds ratio less than ROPE")
ggsave("Figures/interaction_heatmap_lessthanROPE.svg",
       width = 8, height = 8)

  
############### Median odds ratios ###############################

mean_95CI <- function(x){
  c(mean(x),
    quantile(x, 0.025),
    quantile(x, 0.975))
}

# individual-level
mean_95CI(mor(post$sd_id__Intercept))

# country-level
mean_95CI(mor(post$sd_country__Intercept))

# region-level
mean_95CI(mor(post$sd_region__Intercept))

# country and region combined 
# (sd of the sum of two Gaussian random variables)

combined_cr_sd <- sqrt(post$sd_country__Intercept^2 + post$sd_region__Intercept^2)
mean_95CI(mor(combined_cr_sd))


############### Requests for additional information: function ##########

# Model

 fit_data$outcome <- 0

 fit_data <- select(fit_data, -response)

# additional_info_model <- brm(
#   data = fit_data,
#   formula = outcome ~
#     # respondent characteristics
#     ## random effects
#     (1 | id) + (1 | country) + (1 | region) + #(1 | A) + (1 | B) = (1 | A/B) if A labels vary by B clusters
#     ## fixed effects
#     specialty_ccm + specialty_anesthesia + specialty_em + specialty_other +
#     role + duration_practice +
#     # clinical scenario
#     ## fixed effects single variable
#     diagnosis +
#     age + frailty +
#     spo2 + fio2 + o2_device + rr +
#     work_of_breathing + norepinephrine + loc +
#     duration +
#     # interactions
#     (diagnosis +
#        age + frailty +
#        spo2 + fio2 + o2_device + rr +
#        work_of_breathing + norepinephrine + loc +
#        duration):(diagnosis +
#                     age + frailty +
#                     spo2 + fio2 + o2_device + rr +
#                     work_of_breathing + norepinephrine + loc +
#                     duration),
#   prior =  c(set_prior(horseshoe(df = 3, par_ratio = 0.15), class = "b"),
#              set_prior("normal(0, 0.5)", class = "sd")),
#   chains = 0,
#   family = bernoulli("logit")
# )
# 
# saveRDS(additional_info_model, "additional_info_model.rds")

additional_info_model2 <- brm(
  data = select(fit_data,-o2_device),
  formula = outcome ~
    # respondent characteristics
    ## random effects
    (1 | id) + (1 | country) + (1 | region) + #(1 | A) + (1 | B) = (1 | A/B) if A labels vary by B clusters
    ## fixed effects
    specialty_ccm + specialty_anesthesia + specialty_em + specialty_other +
    role + duration_practice +
    # clinical scenario
    ## fixed effects single variable
    diagnosis +
    age + frailty +
    spo2 + fio2 +  rr +
    work_of_breathing + norepinephrine + loc +
    duration +
    # interactions
    (diagnosis +
       age + frailty +
       spo2 + fio2 + rr +
       work_of_breathing + norepinephrine + loc +
       duration):(diagnosis +
                    age + frailty +
                    spo2 + fio2 + rr +
                    work_of_breathing + norepinephrine + loc +
                    duration),
  prior =  c(set_prior(horseshoe(df = 3, par_ratio = 0.15), class = "b"),
             set_prior("normal(0, 0.5)", class = "sd")),
  chains = 0,
  family = bernoulli("logit")
)

saveRDS(additional_info_model2, "additional_info_model2.rds")


additional_info_model <- readRDS("additional_info_model.rds")
additional_info_model2 <- readRDS("additional_info_model2.rds")

additional_info_analysis <- function(fit_data,
                                     additional_info_model,
                                     outcome){
  
  fit <- update(
    object = additional_info_model,
    newdata = fit_data,
    chains = 4,
    cores = 4,
    iter = 1000,
    family = bernoulli("logit")
  )
  
  saveRDS(fit, file = paste0(outcome, "_fit.rds"))
  fit
}

############### Requests for additional information: outputs ##########

additional_info_data <- 
  df %>%
  # remove other text, datetime, additional info
  dplyr::select(id:specialty_other, role:duration,
                arterial_blood_gas:more_observation_time) %>%
  # categorize according  to prespecified scheme
  mutate(duration_practice = cut(duration_practice,
                                 duration_practice_cat,
                                 include.lowest = T,
                                 right = F,
                                 ordered_result = T),
         age  = cut(age, age_cat, include.lowest = T, right = F,
                    ordered_result = T),
         spo2 = cut(spo2, spo2_cat, 
                    include.lowest = T, 
                    right = F,
                    ordered_result = T),
         rr   = cut(rr, rr_cat, 
                    include.lowest = T,
                    right = F,
                    ordered_result = T)) %>%
  # remove 135 (double-check #) responses with duration_practice = NA
  filter(!is.na(duration_practice)) %>%
  # make the factors unordered to avoid R making orthogonal polynomials for the 
  # coefficients...
  mutate(across(id:duration, factor, ordered = F)) %>%
  # reverse the spo2 factor to set 95-97 to be the baseline
  mutate(spo2 = fct_rev(spo2))


# additional_info_data %>%
#   select(#-arterial_blood_gas,
#          -peep_if_on_niv,
#          -tidal_volume_if_on_niv,
#          -chest_x_ray,
#          -esophageal_pressure,
#          -more_observation_time) %>%
#   rename(outcome = arterial_blood_gas) %>%
#   additional_info_analysis(additional_info_model,
#                            outcome = "abg")

abg <- readRDS("abg_fit.rds")
post <- as_draws_df(abg)

ylim = c(0.01, 10)

abg_plot <- 
  select(post, starts_with("b_")) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_","",name)) %>%
  filter(!grepl("Intercept",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975),
            prob_lessthan_rope = mean(value < rope[1]),
            prob_morethan_rope = mean(value > rope[2])) %>%
  filter(post_lb95 > 0 | post_ub95 < 0) %>%
  mutate(name = factor(name,
                       levels = 
                         c("work_of_breathingUseofneckmusclesabdominalparadox:locDrowsynotobeying",
                           "diagnosisSepsis:spo28992",
                           "locDrowsybutobeying",
                           "locDrowsynotobeying",
                           "roleRespiratorytherapist",
                           "roleNurse"),
                       labels = c("Use of neck muscles, abdominal paradox, drowsy, not obeying*",
                                  "Sepsis and oxygen saturation 89 to 91*",
                                  "Drowsy but obeying (vs alert)",
                                  "Drowsy, not obeying (vs alert)",
                                  "Respiratory therapist (vs attending physician)",
                                  "Nurse (vs attending physician)"),
                       ordered = T)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  arrange(post_mean) %>%
  mutate(name = factor(name, levels = name, ordered = T),
         ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) %>%
    ggplot(aes(x = as.integer(name), y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
    geom_rect(aes(xmin = as.integer(name)-0.5,
                  xmax = as.integer(name)+0.5,
                  ymin = ylim[1],
                  ymax = ylim[2],
                  fill = factor(as.integer(name)%%2)))+
    geom_hline(yintercept = 1, color = "gray") +
    geom_pointrange(size = 0.25) + 
  geom_hline(yintercept = 1) +
    geom_text(
      aes(label = name,
          x = as.integer(name) + 0.25), 
      y = log(ylim[1]+0.001),
      hjust = 0,
      vjust = 0.5,
      size = 3) +
    geom_text(
      aes(label = ORlabel,
          x = as.integer(name)), 
      y = log(0.44),
      hjust = 1,
      vjust = 0.5,
      size = 3) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("grey95","white"),
                      guide = "none") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_continuous(trans = "log",
                       limits = ylim,
                       breaks = c(0.5, 1, 2, 4, 8)) +
    # theme(axis.text.y = element_blank()) +
  labs(y = "",
       title = "Arterial blood gas",
       x = "")

# PEEP

# peep <-
#   additional_info_data %>%
#   filter(o2_device == "Non-invasive ventilation") %>%
#   select(-arterial_blood_gas,
#     #-peep_if_on_niv,
#     -tidal_volume_if_on_niv,
#     -chest_x_ray,
#     -esophageal_pressure,
#     -more_observation_time) %>%
#   rename(outcome = peep_if_on_niv) %>%
#   additional_info_analysis(additional_info_model2,
#                            outcome = "niv")

peep <- readRDS("niv_fit.rds")
post <- as_draws_df(peep)

peep_plot <- 
  select(post, starts_with("b_")) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_","",name)) %>%
  filter(!grepl("Intercept",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975),
            prob_lessthan_rope = mean(value < rope[1]),
            prob_morethan_rope = mean(value > rope[2])) %>%
  filter(post_lb95 > 0 | post_ub95 < 0) %>%
  mutate(name = factor(name,
                       levels = 
                         c(
                           "locDrowsynotobeying",
                           "roleRespiratorytherapist"),
                       labels = c("Drowsy, not obeying (vs alert)",
                                  "Respiratory therapist (vs attending physician)"),
                       ordered = T)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  arrange(post_mean) %>%
  mutate(name = factor(name, levels = name, ordered = T),
         ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) %>%
  ggplot(aes(x = as.integer(name), y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_rect(aes(xmin = as.integer(name)-0.5,
                xmax = as.integer(name)+0.5,
                ymin = ylim[1],
                ymax = ylim[2],
                fill = factor(as.integer(name)%%2)))+
  geom_hline(yintercept = 1, color = "gray") +
  geom_pointrange(size = 0.25) + 
  geom_hline(yintercept = 1) +
  geom_text(
    aes(label = name,
        x = as.integer(name)+0.25), 
    y = log(ylim[1]+0.001),
    hjust = 0,
    vjust = 0.5,
    size = 3) +
  geom_text(
    aes(label = ORlabel,
        x = as.integer(name)), 
    y = log(0.44),
    hjust = 1,
    vjust = 0.5,
    size = 3) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("grey95","white"),
                    guide = "none") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_continuous(trans = "log",
                     limits = ylim,
                     breaks = c(0.5, 1, 2, 4, 8)) +
  # theme(axis.text.y = element_blank()) +
  labs(y = "",
       title = "PEEP value (if on NIV)",
       x = "")

# # Vt
# vt <- 
#   additional_info_data %>%
#   filter(o2_device == "Non-invasive ventilation") %>%
#   select(-arterial_blood_gas,
#     -peep_if_on_niv,
#     #-tidal_volume_if_on_niv,
#     -chest_x_ray,
#     -esophageal_pressure,
#     -more_observation_time) %>%
#   rename(outcome = tidal_volume_if_on_niv) %>%
#   additional_info_analysis(additional_info_model2,
#                            outcome = "vt")

vt <- readRDS("vt_fit.rds")
post <- as_draws_df(vt)
prelim_plot(post)

# chest xray
# cxr <- 
#   additional_info_data %>%
#   select(-arterial_blood_gas,
#     -peep_if_on_niv,
#     -tidal_volume_if_on_niv,
# #    -chest_x_ray,
#     -esophageal_pressure,
#     -more_observation_time) %>%
#   rename(outcome = chest_x_ray) %>%
#   additional_info_analysis(additional_info_model,
#                            outcome = "cxr")

cxr <- readRDS("cxr_fit.rds")
post <- as_draws_df(cxr)


cxr_plot <- 
  select(post, starts_with("b_")) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_","",name)) %>%
  filter(!grepl("Intercept",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975),
            prob_lessthan_rope = mean(value < rope[1]),
            prob_morethan_rope = mean(value > rope[2])) %>%
  filter(post_lb95 > 0 | post_ub95 < 0) %>%
  mutate(name = factor(name,
                       levels = 
                         c(
                           "diagnosisCOVIDpneumonia",
                           "roleNurse",
                           "roleRespiratorytherapist"),
                       labels = c("COVID-19 pneumonia",
                                  "Nurse (vs attending physician)",
                                  "Respiratory therapist (vs attending physician)"),
                       ordered = T)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  arrange(post_mean) %>%
  mutate(name = factor(name, levels = name, ordered = T),
         ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) %>%
  ggplot(aes(x = as.integer(name), y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_rect(aes(xmin = as.integer(name)-0.5,
                xmax = as.integer(name)+0.5,
                ymin = ylim[1],
                ymax = ylim[2],
                fill = factor(as.integer(name)%%2)))+
  geom_hline(yintercept = 1, color = "gray") +
  geom_pointrange(size = 0.25) + 
  geom_hline(yintercept = 1) +
  geom_text(
    aes(label = name,
        x = as.integer(name)+0.25), 
    y = log(ylim[1]+0.001),
    hjust = 0,
    vjust = 0.5,
    size = 3) +
  geom_text(
    aes(label = ORlabel,
        x = as.integer(name)), 
    y = log(0.44),
    hjust = 1,
    vjust = 0.5,
    size = 3) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("grey95","white"),
                    guide = "none") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_continuous(trans = "log",
                     limits = ylim,
                     breaks = c(0.5, 1, 2, 4, 8)) +
  # theme(axis.text.y = element_blank()) +
  labs(y = "",
       title = "Chest x-ray",
       x = "")

# esophageal pressure
# esop <- 
#   additional_info_data %>%
#   select(-arterial_blood_gas,
#     -peep_if_on_niv,
#     -tidal_volume_if_on_niv,
#     -chest_x_ray,
#     #-esophageal_pressure,
#     -more_observation_time) %>%
#   rename(outcome = esophageal_pressure) %>%
#   additional_info_analysis(additional_info_model,
#                            outcome = "esop")

esop <- readRDS("esop_fit.rds")
post <- as_draws_df(esop)
prelim_plot(post)


# more observation time
# obs_time <- 
#   additional_info_data %>%
#   select(-arterial_blood_gas,
#     -peep_if_on_niv,
#     -tidal_volume_if_on_niv,
#     -chest_x_ray,
#     -esophageal_pressure) %>%
#     #-more_observation_time) %>%
#   rename(outcome = more_observation_time) %>%
#   additional_info_analysis(additional_info_model,
#                            outcome = "obs_time")

obs <- readRDS("obs_time_fit.rds")
post <- as_draws_df(obs)

obstime_plot <-
  select(post, starts_with("b_")) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_","",name)) %>%
  filter(!grepl("Intercept",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975),
            prob_lessthan_rope = mean(value < rope[1]),
            prob_morethan_rope = mean(value > rope[2])) %>%
  filter(post_lb95 > 0 | post_ub95 < 0) %>%
  mutate(name = factor(name,
                       levels = 
                         c("spo28589:fio21",
                           "duration1hour",
                           "fio21:locDrowsynotobeying",
                           "locDrowsynotobeying",
                           "duration2hours",
                           "duration4hours"),
                       labels = c("Oxygen saturation 85 to 88 on FiO2 1.0*",
                                  "Duration in current state: 1 hour",
                                  "Drowsy and not obeying on FiO2 1.0*",
                                  "Drowsy and not obeying",
                                  "Duration in current state: 2 hours",
                                  "Duration in current state: 4 hours"),
                       ordered = T)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  arrange(post_mean) %>%
  mutate(name = factor(name, levels = name, ordered = T),
         ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) %>%
  ggplot(aes(x = as.integer(name), y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_rect(aes(xmin = as.integer(name)-0.5,
                xmax = as.integer(name)+0.5,
                ymin = ylim[1],
                ymax = ylim[2],
                fill = factor(as.integer(name)%%2)))+
  geom_hline(yintercept = 1, color = "gray") +
  geom_pointrange(size = 0.25) + 
  geom_hline(yintercept = 1) +
  geom_text(
    aes(label = name,
        x = as.integer(name)+0.25), 
    y = log(ylim[1]+0.001),
    hjust = 0,
    vjust = 0.5,
    size = 3) +
  geom_text(
    aes(label = ORlabel,
        x = as.integer(name)), 
    y = log(7),
    hjust = 1,
    vjust = 0.5,
    size = 3) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("grey95","white"),
                    guide = "none") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_continuous(trans = "log",
                     limits = ylim,
                     breaks = c(0.5, 1, 2, 4, 8)) +
  # theme(axis.text.y = element_blank()) +
  labs(y = "",
       title = "More observation time",
       x = "")


library(ggpubr)

ggarrange(abg_plot, peep_plot, cxr_plot, obstime_plot,
          label.x = "Posterior mean odds ratio (95% credible interval)")
ggsave("Figures/AdditionalInfo_ORs.svg",
       width = 10, height = 8)

############### Sensitivity analysis - all 10 responses ############

# prepare data frame
apply(is.na(df), 2, sum) # duration_practice is only one with missing


# categorizations of continuous variables

# lower boundary b of each category
# such that category is b1 <= category < b2
duration_practice_cat <- c(0, 6, 11, 20, 60)
age_cat  <- c(20, 40, 50, 60, 70)
spo2_cat <- c(85, 89, 92, 95, 97)
rr_cat   <- c(15, 21, 25, 28, 33, 40)

allten <- df %>%
  group_by(id) %>%
  summarise(allten = n() == 10) %>%
  filter(allten == T)

brm_df2 <-
  df %>%
  #filter to those with 10 responses
  filter(id %in% allten$id) %>%
  # remove other text, datetime, additional info
  dplyr::select(id:specialty_other, role:response) %>%
  # categorize according  to prespecified scheme
  mutate(duration_practice = cut(duration_practice,
                                 duration_practice_cat,
                                 include.lowest = T,
                                 right = F,
                                 ordered_result = T),
         age  = cut(age, age_cat, include.lowest = T, right = F,
                    ordered_result = T),
         spo2 = cut(spo2, spo2_cat, 
                    include.lowest = T, 
                    right = F,
                    ordered_result = T),
         rr   = cut(rr, rr_cat, 
                    include.lowest = T,
                    right = F,
                    ordered_result = T)) %>%
  # remove 135 (double-check #) responses with duration_practice = NA
  filter(!is.na(duration_practice)) %>%
  # make the factors unordered to avoid R making orthogonal polynomials for the 
  # coefficients...
  mutate(across(id:duration, factor, ordered = F)) %>%
  # reverse the spo2 factor to set 95-97 to be the baseline
  mutate(spo2 = fct_rev(spo2))

# everything categorical, all interactions for patient variables, horseshoe prior
# N_test <- 100
# test_respondents <- sample(unique(brm_df$id), size = N_test)
# test_df <- filter(brm_df, id %in% test_respondents)

fit_data <- brm_df2 # set to brm_df2 for full analysis 

model <- readRDS("model_horseshoe.rds")


# model fit
fit2 <- update(
  object = model,
  newdata = fit_data,
  chains = 4,
  cores = 4,
  iter = 1000,
  family = cumulative("logit")
)

saveRDS(fit2, file = "full_fit_allten.rds")
fit2 <- readRDS("full_fit_allten.rds")

post <- as_draws_df(fit2)




############### Sensitivity analysis - Geographic figures #####################

# regions
post_regions <- 
  select(post, starts_with("r_region")) %>%
  mutate(iter = row_number()) %>%
  pivot_longer(cols = -iter) %>%
  mutate(name = gsub("r_region\\[","",name)) %>%
  mutate(name = gsub(",Intercept\\]", "", name)) %>%
  mutate(name = gsub("\\.", " ", name)) %>%
  rename(region = name)

post_countries <- 
  select(post, starts_with("r_country")) %>%
  mutate(iter = row_number()) %>%
  pivot_longer(cols = -iter) %>%
  mutate(name = gsub("r_country\\[","",name)) %>%
  mutate(name = gsub(",Intercept\\]", "", name)) %>%
  mutate(name = gsub("\\.", " ", name)) %>%
  rename(country = name)

# country-region pairs

country_region <- 
  df %>% 
  distinct(country, region)

post_country_region <-
  post_countries %>%
  left_join(country_region, by = "country") %>%
  left_join(post_regions, by = c("region","iter")) %>%
  mutate(coef = value.x + value.y) 

gap_size = 1

# more conventional forest plot by country
post_country_region %>%
  group_by(country, region) %>%
  summarise(post_mean = mean(coef),
            post_lb95 = quantile(coef, 0.025),
            post_ub95 = quantile(coef, 0.975)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  group_by(region) %>%
  arrange(post_mean, .by_group = T) %>%
  mutate(r_row_number = row_number()) %>%
  mutate(n_in_region = max(r_row_number)) %>%
  ungroup() %>%
  mutate(g_row_number = row_number()) %>%
  mutate(g_row_number = ifelse(
    region == "Africa", g_row_number,
    ifelse(region == "Americas", g_row_number + gap_size,
           ifelse(region == "Asia", g_row_number + 2*gap_size,
                  ifelse(region == "Europe", g_row_number + 3*gap_size,
                         g_row_number + 4*gap_size))))) %>%
  
  mutate(ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")"),
         equiv = factor(case_when(post_ub95 < 1 ~ 1,
                                  post_lb95 < 1 ~ 2,
                                  post_lb95 > 1 ~ 3))) %>%
  # shaded rows?
  ggplot(aes(x = g_row_number, y = post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_hline(yintercept = 1, color = "gray") +
  geom_rect(aes(xmin = g_row_number - 0.5,
                xmax = g_row_number + 0.5), 
            fill = c_ice,
            alpha = 0.25) +
  geom_point(size = 2, aes(color = equiv)) +
  theme_minimal() + 
  scale_y_continuous(breaks = c(1/4,1/2,1,2,4), 
                     limits = c(1/6,8),
                     trans = "log") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank()) +
  geom_text(aes(label = country, y = post_mean*0.95,
                color = equiv), 
            hjust = 1, 
            vjust = 0.25,
            size = 2) +
  scale_color_manual(values = c(c_navy, "black", c_dark),
                     labels = c("Less likely",
                                "Equivalent",
                                "More likely"),
                     guide = "none") +
  coord_flip() +
  labs(x = "",
       y = "<< Less likely      Odds ratio of recommending intubation      More likely >>")
ggsave("Figures/Sensitivity_Countries.svg",
       width = 7, height = 8)

############### Sensitivity analysis - Posterior OR  Figure, fixed effects ##################

posterior_or_figure(post)

ggsave("Figures/Sensitivity_PosteriorOR_fixed.svg",
       width = 10, height = 8)
ggsave("Figures/Sensitivity_PosteriorOR_fixed.pdf",
       width = 10, height = 8)

# Posterior OR figure, 1 CrI filter

select(post, starts_with("b_")) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_","",name)) %>%
  filter(!grepl("Intercept",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  arrange(post_mean) %>%
  filter(post_lb95 > 1 | post_ub95 < 1) %>%
  mutate(name = factor(name, levels = name, ordered = T),
         ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) %>%
  ggplot(aes(x = name, y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_pointrange() + 
  geom_hline(yintercept = 1) +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(trans = "log") +
  labs(y = "Posterior mean odds ratio (95% credible interval)",
       x = "Variable",
       title = "Posterior odds ratios")


# Posterior OR figure, ROPE mean effect filter

select(post, starts_with("b_")) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_","",name)) %>%
  filter(!grepl("Intercept",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  arrange(post_mean) %>%
  filter(post_mean > exp(rope[2]) | post_mean < exp(rope[1])) %>%
  mutate(name = factor(name, levels = name, ordered = T),
         ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) %>%
  ggplot(aes(x = name, y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_pointrange() + 
  geom_hline(yintercept = 1) +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(trans = "log") +
  labs(y = "Posterior mean odds ratio (95% credible interval)",
       x = "Variable",
       title = "Posterior odds ratios")


############### Sensitivity analysis - Median odds ratios ###############################

mean_95CI <- function(x){
  c(mean(x),
    quantile(x, 0.025),
    quantile(x, 0.975))
}

# individual-level
mean_95CI(mor(post$sd_id__Intercept))

# country-level
mean_95CI(mor(post$sd_country__Intercept))

# region-level
mean_95CI(mor(post$sd_region__Intercept))



############### Sensitivity analysis - Diagnosis groups ####################

# prepare data frame
apply(is.na(df), 2, sum) # duration_practice is only one with missing


# categorizations of continuous variables

# lower boundary b of each category
# such that category is b1 <= category < b2
duration_practice_cat <- c(0, 6, 11, 20, 60)
age_cat  <- c(20, 40, 50, 60, 70)
spo2_cat <- c(85, 89, 92, 95, 97)
rr_cat   <- c(15, 21, 25, 28, 33, 40)

brm_df <-
  df %>%
  # remove other text, datetime, additional info
  dplyr::select(id:specialty_other, role:response) %>%
  # categorize according  to prespecified scheme
  mutate(duration_practice = cut(duration_practice,
                                 duration_practice_cat,
                                 include.lowest = T,
                                 right = F,
                                 ordered_result = T),
         age  = cut(age, age_cat, include.lowest = T, right = F,
                    ordered_result = T),
         spo2 = cut(spo2, spo2_cat, 
                    include.lowest = T, 
                    right = F,
                    ordered_result = T),
         rr   = cut(rr, rr_cat, 
                    include.lowest = T,
                    right = F,
                    ordered_result = T)) %>%
  # remove 135 (double-check #) responses with duration_practice = NA
  filter(!is.na(duration_practice)) %>%
  # make the factors unordered to avoid R making orthogonal polynomials for the 
  # coefficients...
  mutate(across(id:duration, factor, ordered = F)) %>%
  # reverse the spo2 factor to set 95-97 to be the baseline
  mutate(spo2 = fct_rev(spo2))

# everything categorical, all interactions for patient variables, horseshoe prior
# N_test <- 100
# test_respondents <- sample(unique(brm_df$id), size = N_test)
# test_df <- filter(brm_df, id %in% test_respondents)

fit_data <- brm_df # set to brm_df for full analysis 

model2 <- brm(
  data = fit_data,
  formula = response ~
    # respondent characteristics
    ## random effects
    (1 | id) + (1 | country) + (1 | region) + #(1 | A) + (1 | B) = (1 | A/B) if A labels vary by B clusters
    ## fixed effects
    specialty_ccm + specialty_anesthesia + specialty_em + specialty_other +
    role + duration_practice +
    # clinical scenario
    ## fixed effects single variable
    #diagnosis +
    age + frailty +
    spo2 + fio2 + o2_device + rr +
    work_of_breathing + norepinephrine + loc +
    duration +
    # interactions
    (age + frailty +
       spo2 + fio2 + o2_device + rr +
       work_of_breathing + norepinephrine + loc +
       duration):(age + frailty +
                    spo2 + fio2 + o2_device + rr +
                    work_of_breathing + norepinephrine + loc +
                    duration),
  prior =  c(set_prior(horseshoe(df = 3, par_ratio = 0.15), class = "b"),
             set_prior("normal(0, 0.5)", class = "sd")),
  chains = 0,
  family = cumulative("logit")
)

model3 <- brm(
  data = fit_data,
  formula = response ~
    # respondent characteristics
    ## random effects
    (1 | id) + (1 | country) + (1 | region) + #(1 | A) + (1 | B) = (1 | A/B) if A labels vary by B clusters
    ## fixed effects
    specialty_ccm + specialty_anesthesia + specialty_em + specialty_other +
    role + duration_practice +
    # clinical scenario
    ## fixed effects single variable
    #diagnosis +
    age + frailty +
    spo2 + fio2 + o2_device + rr +
    work_of_breathing + #norepinephrine + 
    loc +
    duration +
    # interactions
    (age + frailty +
       spo2 + fio2 + o2_device + rr +
       work_of_breathing  + loc +
       duration):(age + frailty +
                    spo2 + fio2 + o2_device + rr +
                    work_of_breathing + loc +
                    duration),
  prior =  c(set_prior(horseshoe(df = 3, par_ratio = 0.15), class = "b"),
             set_prior("normal(0, 0.5)", class = "sd")),
  chains = 0,
  family = cumulative("logit")
)
saveRDS(model3, file = "model_covid.rds")
model3 <- readRDS("model_covid.rds")


# model fit
sepsis <- update(
  object = model2,
  newdata = filter(brm_df, diagnosis == "Sepsis"),
  chains = 4,
  cores = 4,
  iter = 1000,
  control = list(adapt_delta = 0.9),
  family = cumulative("logit")
)

saveRDS(sepsis, file = "sepsis_fit.rds")
sepsis <- readRDS("sepsis_fit.rds")
# post <- as_draws_df(prior_sim)
post_sepsis <- as_draws_df(sepsis)

# model2 fit
CAP <- update(
  object = model2,
  newdata = filter(brm_df, diagnosis == "Community-acquired pneumonia"),
  chains = 4,
  cores = 4,
  iter = 1000,
  control = list(adapt_delta = 0.9),
  family = cumulative("logit")
)

saveRDS(CAP, file = "CAP_fit.rds")
CAP <- readRDS("CAP_fit.rds")
post_CAP <- as_draws_df(CAP)

# model2 fit
Flu <- update(
  object = model2,
  newdata = filter(brm_df, diagnosis == "Influenza pneumonia"),
  chains = 4,
  cores = 4,
  iter = 1000,
  control = list(adapt_delta = 0.9),
  family = cumulative("logit")
)

saveRDS(Flu, file = "Flu_fit.rds")
Flu <- readRDS("Flu_fit.rds")
post_Flu <- as_draws_df(Flu)

# model2 fit
COVID <- update(
  object = model3,
  newdata = filter(brm_df, diagnosis == "COVID pneumonia"),
  chains = 4,
  cores = 4,
  iter = 1000,
  control = list(adapt_delta = 0.9),
  family = cumulative("logit")
)

saveRDS(COVID, file = "COVID_fit.rds")
COVID <- readRDS("COVID_fit.rds")
post_COVID <- as_draws_df(COVID)

# model2 fit
Panc <- update(
  object = model2,
  newdata = filter(brm_df, diagnosis == "Pancreatitis"),
  chains = 4,
  cores = 4,
  iter = 1000,
  control = list(adapt_delta = 0.9),
  family = cumulative("logit")
)

saveRDS(Panc, file = "Panc_fit.rds")
Panc <- readRDS("Panc_fit.rds")
post_Panc <- as_draws_df(Panc)

############### Sensitivity analysis - Figure function code ##############
diagnosis_OR_figure <- function(post, diagnosis){
  
  post_ors <- 
    select(post, starts_with("b_")) %>%
    pivot_longer(everything()) %>%
    mutate(name = gsub("b_","",name)) %>%
    filter(!grepl("Intercept",name)) %>%
    group_by(name) %>%
    summarise(post_mean = mean(value),
              post_lb95 = quantile(value, 0.025),
              post_ub95 = quantile(value, 0.975)) %>%
    mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
    arrange(post_mean)
  
  
  # Main effects forest plot
  
  or_categories <- data.frame(
    name = c("Role",
             "Specialty",
             "Duration in practice",
             "Patient age",
             "Frailty",
             "Diagnosis",
             "Inspired oxygen fraction (FiO2)",
             "Saturation",
             "Oxygen device",
             "Respiratory rate",
             "Breathing pattern",
             "Level of consciousness",
             "Norepinephrine",
             "Duration"),
    post_mean = NA,
    post_lb95 = NA,
    post_ub95 = NA,
    ORlabel = NA
  )
  
  or_reference <- data.frame(
    name = c("Attending physician",
             "0-5 years",
             "20-40",
             "Independent",
             diagnosis,
             "0.4",
             "95-97",
             "High-flow nasal cannula",
             "15-20",
             "Normal",
             "Alert and obeying",
             "No",
             "10 minutes"),
    post_mean = 1,
    post_lb95 = 1,
    post_ub95 = 1,
    ORlabel = "Reference"
  )
  
  
  post_or_fe <- 
    post_ors %>%
    filter(!grepl(":", name)) %>%
    mutate(name = factor(name, levels = name, ordered = T),
           ORlabel = paste0(round(post_mean, 2), " (",
                            round(post_lb95,2), " to ",
                            round(post_ub95,2), ")")) %>%
    bind_rows(or_categories) %>%
    bind_rows(or_reference) %>%
    mutate(
      name = factor(
        name,
        levels = c(
          "duration4hours", 
          "duration2hours", 
          "duration1hour", 
          "duration30minutes", 
          "10 minutes",
          "Duration", 
          
          "norepinephrineYes", 
          "No", 
          "Norepinephrine", 
          
          "locDrowsynotobeying", 
          "locDrowsybutobeying",
          "Alert and obeying",
          "Level of consciousness",
          
          "work_of_breathingUseofneckmusclesabdominalparadox", 
          "work_of_breathingUseofneckmusclesnoabdominalparadox", 
          "Normal",
          "Breathing pattern",
          
          "rr3340", 
          "rr2833", 
          "rr2528", 
          "rr2125",
          "15-20",
          "Respiratory rate", 
          
          "o2_deviceNonMinvasiveventilation", 
          "High-flow nasal cannula", 
          "Oxygen device",
          
          "fio21", 
          "fio20.9", 
          "fio20.8", 
          "fio20.7", 
          "fio20.6", 
          "fio20.5",
          "0.4",
          "Inspired oxygen fraction (FiO2)", 
          
          "spo28589", 
          "spo28992", 
          "spo29295", 
          "95-97", 
          "Saturation",
          
          "diagnosisPancreatitis", 
          "diagnosisSepsis", 
          "diagnosisInfluenzapneumonia", 
          diagnosis, 
          "diagnosisCOVIDpneumonia", 
          "Diagnosis", 
          
          "frailtyAssistanceforshoppingandheavyhousework", 
          "frailtyIndependentwithwellMcontrolledmedicalproblems", 
          "Independent",
          "Frailty",
          
          "age6070", 
          "age5060", 
          "age4050", 
          "20-40", 
          "Patient age", 
          
          "duration_practice2060", 
          "duration_practice1120", 
          "duration_practice611", 
          "0-5 years", 
          "Duration in practice", 
          
          "specialty_ccmTRUE", 
          "specialty_anesthesiaTRUE", 
          "specialty_otherTRUE", 
          "specialty_emTRUE", 
          "Specialty", 
          
          "roleTraineephysician", 
          "Attending physician", 
          "roleRespiratorytherapist", 
          "roleOtherclinicalrole", 
          "roleNurse", 
          "Role"),
        labels = c(
          "4 hours", 
          "2 hours", 
          "1 hour", 
          "30 minutes", 
          "10 minutes",
          "Duration", 
          
          "Yes", 
          "No", 
          "Norepinephrine use", 
          
          "Drowsy, not obeying", 
          "Drowsy but obeying",
          "Alert and obeying",
          "Level of consciousness",
          
          "Neck muscle use and abdominal paradox", 
          "Neck muscle use, no abdominal paradox", 
          "Normal",
          "Breathing pattern",
          
          "33-40", 
          "28-32", 
          "25-27", 
          "21-24",
          "15-20",
          "Respiratory rate", 
          
          "Non-inasive ventilation", 
          "High-flow nasal cannula", 
          "Oxygen device",
          
          "1.0", 
          "0.9", 
          "0.8", 
          "0.7", 
          "0.6", 
          "0.5",
          "0.4",
          "Inspired oxygen fraction", 
          
          "85-88", 
          "89-91", 
          "92-94", 
          "95-97", 
          "Saturation",
          
          "Pancreatitis", 
          "Sepsis", 
          "Influenza pneumonia", 
          diagnosis,
          "COVID-19 pneumonia", 
          "Diagnosis", 
          
          "Assistance for some IADLs", 
          "Independent with medical problems", 
          "Independent",
          "Frailty",
          
          "60-70 years", 
          "50-59 years", 
          "40-49 years", 
          "20-39 years", 
          "Patient age", 
          
          "21 or more years", 
          "11 to 20 years", 
          "6 to 10 years", 
          "5 or fewer years", 
          "Duration in practice", 
          
          "Critical care medicine", 
          "Anesthesia", 
          "Other", 
          "Emergency medicine", 
          "Specialty", 
          
          "Trainee physician", 
          "Attending physician", 
          "Respiratory therapist", 
          "Other clinical role", 
          "Nurse", 
          "Role"),
        ordered = T
      )) %>%
    arrange(name) %>%
    mutate(header = is.na(ORlabel)) %>%
    mutate(group = case_when(
      name %in% c(
        "4 hours", 
        "2 hours", 
        "1 hour", 
        "30 minutes", 
        "10 minutes",
        "Duration") ~ "Duration",
      name %in% c(
        "Yes", 
        "No", 
        "Norepinephrine use") ~ "Norepinephrine",
      name %in% c(
        "Drowsy, not obeying", 
        "Drowsy but obeying",
        "Alert and obeying",
        "Level of consciousness") ~ "LOC",
      name %in% c(
        "Neck muscle use and abdominal paradox", 
        "Neck muscle use, no abdominal paradox", 
        "Normal",
        "Breathing pattern") ~ "BreathingPattern",
      name %in% c(
        "33-40", 
        "28-32", 
        "25-27", 
        "21-24",
        "15-20",
        "Respiratory rate") ~ "RR",
      name %in% c(
        "Non-inasive ventilation", 
        "High-flow nasal cannula", 
        "Oxygen device") ~ "o2device",
      name %in% c(
        "1.0", 
        "0.9", 
        "0.8", 
        "0.7", 
        "0.6", 
        "0.5",
        "0.4",
        "Inspired oxygen fraction") ~ "FiO2",
      name %in% c(
        "85-88", 
        "89-91", 
        "92-94", 
        "95-97", 
        "Saturation") ~ "SpO2",
      name %in% c(
        "Pancreatitis", 
        "Sepsis", 
        "Influenza pneumonia", 
        "Community-acquired pneumonia", 
        "COVID-19 pneumonia", 
        "Diagnosis") ~ "Diagnosis", 
      name %in% c(
        "Assistance for some IADLs", 
        "Independent with medical problems", 
        "Independent",
        "Frailty") ~ "Frailty",
      name %in% c(
        "60-70 years", 
        "50-59 years", 
        "40-49 years", 
        "20-39 years", 
        "Patient age") ~ "Age", 
      name %in% c(
        "21 or more years", 
        "11 to 20 years", 
        "6 to 10 years", 
        "5 or fewer years", 
        "Duration in practice") ~ "DurationPractice",
      name %in% c(
        "Critical care medicine", 
        "Anesthesia", 
        "Other", 
        "Emergency medicine", 
        "Specialty") ~ "Specialty", 
      name %in% c(
        "Trainee physician", 
        "Attending physician", 
        "Respiratory therapist", 
        "Other clinical role", 
        "Nurse", 
        "Role") ~ "Role")) %>%
    mutate(group = factor(group,
                          levels = c("Role",
                                     "Specialty",
                                     "DurationPractice",
                                     "Age",
                                     "Frailty",
                                     "Diagnosis",
                                     "SpO2",
                                     "FiO2",
                                     "o2device",
                                     "RR",
                                     "BreathingPattern",
                                     "LOC",
                                     "Norepinephrine",
                                     "Duration"),
                          ordered = T,
                          labels = c("Role",
                                     "Specialty",
                                     "Duration in practice",
                                     "Patient age",
                                     "Patient frailty",
                                     "Diagnosis",
                                     "Oxygen saturation",
                                     "Inspired oxygen fraction",
                                     "Oxygen device",
                                     "Respiratory rate",
                                     "Breathing Pattern",
                                     "Level of consciousness",
                                     "Norepinephrine use",
                                     "Duration in current state")))
  
  
  ylim = c(0.02, 12.4)
  
  
  post_or_fe %>%
    filter(header == F) %>%
    ggplot(aes(x = as.integer(name), y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
    geom_rect(aes(xmin = as.integer(name)-0.5,
                  xmax = as.integer(name)+0.5,
                  ymin = ylim[1],
                  ymax = ylim[2],
                  fill = factor(as.integer(name)%%2)))+
    geom_hline(yintercept = 1, color = "gray") +
    geom_pointrange(size = 0.25) + 
    geom_text(
      data = filter(post_or_fe, header == F),
      aes(label = name,
          x = as.integer(name)), 
      y = log(ylim[1]+0.001),
      hjust = 0,
      vjust = 0.5,
      size = 2) +
    geom_text(
      aes(label = ORlabel,
          x = as.integer(name)), 
      y = log(0.5),
      hjust = 1,
      vjust = 0.5,
      size = 2) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = 
            element_text(margin = 
                           margin(
                             t = 10, 
                             r = 0, 
                             b = 0, 
                             l = 0))) +
    coord_flip() +
    scale_fill_manual(values = c("grey95","white"),
                      guide = "none") +
    scale_y_continuous(trans = "log",
                       limits = ylim,
                       breaks = c(0.5, 1, 2, 4, 8)) +
    labs(y = "Posterior mean odds ratio (95% credible interval)",
         x = "",
         caption = "Odds ratios less than 1 = less likely to recommend intubation \nOdds ratios more than 1 = more likely to recommend intubation",
         title = paste0("Sensitivity analysis: ",
                        diagnosis,
                        ", Posterior odds ratios for recommending intubation")) +
    facet_wrap(.~group,
               scales = "free_y",
               ncol = 2)
}
############### Sensitivity analysis - Diagnosis groups plots ############

sepsis <- readRDS("sepsis_fit.rds")
post_sepsis <- as_draws_df(sepsis)

CAP <- readRDS("CAP_fit.rds")
post_CAP <- as_draws_df(CAP)

COVID <- readRDS("COVID_fit.rds")
post_COVID <- as_draws_df(COVID)

Flu <- readRDS("Flu_fit.rds")
post_Flu <- as_draws_df(Flu)

Panc <- readRDS("Panc_fit.rds")
post_Panc <- as_draws_df(Panc)

diagnosis_OR_figure(post_sepsis, "Sepsis")
  ggsave("Figures/sepsis_postORs.svg",
         width = 10, height = 7)

diagnosis_OR_figure(post_CAP, "Community-acquired pneumonia")
  ggsave("Figures/CAP_postORs.svg",
         width = 10, height = 7)

diagnosis_OR_figure(post_Flu, "Influenza pneumonia")
  ggsave("Figures/flu_postORs.svg",
         width = 10, height = 7)

diagnosis_OR_figure(post_COVID, "COVID-19 pneumonia")
  ggsave("Figures/COVID_postORs.svg",
         width = 10, height = 7)

diagnosis_OR_figure(post_Panc, "Pancreatitis")
  ggsave("Figures/Panc_postORs.svg",
         width = 10, height = 7)
  
############### Sensitivity analysis - Test proportional odds assumption ######
  
# form brm_df as above 

  # prepare data frame
  apply(is.na(df), 2, sum) # duration_practice is only one with missing
  
  
  # categorizations of continuous variables
  
  # lower boundary b of each category
  # such that category is b1 <= category < b2
  duration_practice_cat <- c(0, 6, 11, 20, 60)
  age_cat  <- c(20, 40, 50, 60, 70)
  spo2_cat <- c(85, 89, 92, 95, 97)
  rr_cat   <- c(15, 21, 25, 28, 33, 40)
  
  brm_df <-
    df %>%
    # remove other text, datetime, additional info
    dplyr::select(id:specialty_other, role:response) %>%
    # categorize according  to prespecified scheme
    mutate(duration_practice = cut(duration_practice,
                                   duration_practice_cat,
                                   include.lowest = T,
                                   right = F,
                                   ordered_result = T),
           age  = cut(age, age_cat, include.lowest = T, right = F,
                      ordered_result = T),
           spo2 = cut(spo2, spo2_cat, 
                      include.lowest = T, 
                      right = F,
                      ordered_result = T),
           rr   = cut(rr, rr_cat, 
                      include.lowest = T,
                      right = F,
                      ordered_result = T)) %>%
    # remove 135 (double-check #) responses with duration_practice = NA
    filter(!is.na(duration_practice)) %>%
    # make the factors unordered to avoid R making orthogonal polynomials for the 
    # coefficients...
    mutate(across(id:duration, factor, ordered = F)) %>%
    # reverse the spo2 factor to set 95-97 to be the baseline
    mutate(spo2 = fct_rev(spo2))
  
  # everything categorical, all interactions for patient variables, horseshoe prior
  # N_test <- 100
  # test_respondents <- sample(unique(brm_df$id), size = N_test)
  # test_df <- filter(brm_df, id %in% test_respondents)
  
fit_data <- 
    brm_df %>%
      mutate(outcome = ifelse(response == "Definite yes", 1, 0))
  
  model_propoddstest <- brm(
    data = fit_data,
    formula = outcome ~
      # respondent characteristics
      ## random effects
      (1 | id) + (1 | country) + (1 | region) + #(1 | A) + (1 | B) = (1 | A/B) if A labels vary by B clusters
      ## fixed effects
      specialty_ccm + specialty_anesthesia + specialty_em + specialty_other +
      role + duration_practice +
      # clinical scenario
      ## fixed effects single variable
      diagnosis +
      age + frailty +
      spo2 + fio2 + o2_device + rr +
      work_of_breathing + norepinephrine + loc +
      duration +
      # interactions
      (diagnosis +
         age + frailty +
         spo2 + fio2 + o2_device + rr +
         work_of_breathing + norepinephrine + loc +
         duration):(diagnosis +
                      age + frailty +
                      spo2 + fio2 + o2_device + rr +
                      work_of_breathing + norepinephrine + loc +
                      duration),
    prior =  c(set_prior(horseshoe(df = 3, par_ratio = 0.15), class = "b"),
               set_prior("normal(0, 0.5)", class = "sd")),
    chains = 0,
    family = bernoulli("logit")
  )
  
  saveRDS(model_propoddstest, file = "model_propoddstest.rds")
  model_propoddstest <- readRDS("model_propoddstest.rds")
  

  propoddstest_list = list()
  
for (i in 1:5){
  
fit_data <- 
    brm_df %>%
    mutate(outcome = ifelse(as.integer(response)> 5-i, 1, 0))
  
  propoddstest_list[[i]] <- 
    update(
      object = model_propoddstest,
      newdata = fit_data,
      save_ranef = F,
      chains = 4,
      cores = 4,
      iter = 1000
    )
}

saveRDS(propoddstest_list, "propoddstest_list.rds")

############### Plots for prop-odds assumption testing ############
propoddstest_list <- readRDS("propoddstest_list.rds")

posterior_or_figure(as_draws_df(propoddstest_list[[1]])) +
  labs(title = "Posterior odds ratios: Definite yes vs probable yes / uncertain / probable no / definite no")
ggsave("Figures/Propoddstest_DefYes.svg",
       width = 10, height = 6.5)

posterior_or_figure(as_draws_df(propoddstest_list[[2]])) +
  labs(title = "Posterior odds ratios: Definite yes / probable yes vs uncertain / probable no / definite no")
ggsave("Figures/Propoddstest_DefYesProbYes.svg",
       width = 10, height = 6.5)

posterior_or_figure(as_draws_df(propoddstest_list[[3]])) +
  labs(title = "Posterior odds ratios: Definite yes / probable yes / uncertain vs probable no / definite no")
ggsave("Figures/Propoddstest_DefYesProbYesUncertain.svg",
       width = 10, height = 6.5)

posterior_or_figure(as_draws_df(propoddstest_list[[4]])) +
  labs(title = "Posterior odds ratios: Definite yes / probable yes / uncertain / probable no vs definite no")
ggsave("Figures/Propoddstest_DefYesProbYesUncertainProbNo.svg",
       width = 10, height = 6.5)


############### Odds to probability for discussion ######################

# baseline probability of recommending definite yes 
# in patients with normal work of breathing

odds2prob <- function(o){o/(1+o)}
prob2odds <- function(p){p/(1-p)}

df %>%
  filter(work_of_breathing == "No use of neck muscles, no abdominal paradox") %>%
  summarise(mean(response == "Definite yes" | response == "Probable yes"))

df %>%
  filter(work_of_breathing == "Use of neck muscles, no abdominal paradox") %>%
  summarise(mean(response == "Definite yes" | response == "Probable yes"))

df %>%
  filter(work_of_breathing == "Use of neck muscles, abdominal paradox") %>%
  summarise(mean(response == "Definite yes" | response == "Probable yes"))

odds2prob(prob2odds(0.33)*2.32)
odds2prob(prob2odds(0.33)*6.39)

############################### How many vignettes? ##################

survey_variables <- list(Diagnosis = c("Community-acquired pneumonia",
                                       "COVID pneumonia",
                                       "Influenza pneumonia",
                                       "Pancreatitis",
                                       "Sepsis"),
                         Age = as.character(c(20:60, 40:70, 55:65)),
                         Frailty = c("Independent and fit", # CFS 1-2
                                     "Independent with well-controlled medical problems", # CFS 3
                                     "Independent with well-controlled medical problems", # CFS 3
                                     "Assistance for shopping and heavy housework"#, # CFS 5
                                     #"Assistance for housework and bathing", #CFS 6
                                     #"Dependent for personal care" # CFS 7+
                         ),
                         SpO2 = as.character(c(85:100, 88:92, 90:97)),
                         FiO2 = c("0.4",
                                  "0.5",
                                  "0.6",
                                  "0.7",
                                  "0.8",
                                  "0.9",
                                  "1.0"),
                         O2device = c("High-flow nasal oxygen",
                                      "Non-invasive ventilation"),
                         RR = as.character(c(15:30, 20:40, 20:35)),
                         #HR = c("90"),
                         WorkOfBreathing = c("No use of neck muscles, no abdominal paradox",
                                             "No use of neck muscles, no abdominal paradox",
                                             "Use of neck muscles, no abdominal paradox",
                                             "Use of neck muscles, abdominal paradox"),
                         #BP = c("100/55"),
                         Norepinephrine = c("No","No", "No","No", "Yes"),
                         LOC = c("Alert and obeying",
                                 "Alert and obeying",
                                 "Drowsy but obeying",
                                 "Drowsy, not obeying"),
                         #Flow = c("60"),
                         Duration = c("10 minutes",
                                      "30 minutes",
                                      "1 hour",
                                      "2 hours",
                                      "4 hours"))

unique_items <- lapply(survey_variables, unique)

items <- unlist(lapply(unique_items, length))

prod(items)

# covariance function
count_vignettes <- 
  function(survey_variables, n=1){
    qc <- data.frame(sapply(survey_variables, sample, n, replace = T))
    qc <-
      qc %>%
        mutate(include = 1) %>%
        mutate(include = ifelse(Age < 40 & !grepl("Independent", Frailty),
                              0,include)) %>%
        mutate(include = ifelse(Age < 55 & grepl("shopping", Frailty),
                                0, include)) %>%
        mutate(include = ifelse(Diagnosis == "COVID" & Norepinephrine == "Yes",
                                0, include)) %>%
      mutate(Age  = cut(as.numeric(Age), age_cat, include.lowest = T, right = F,
                        ordered_result = T),
             SpO2 = cut(as.numeric(SpO2), spo2_cat, 
                        include.lowest = T, 
                        right = F,
                        ordered_result = T),
             RR   = cut(as.numeric(RR), rr_cat, 
                        include.lowest = T,
                        right = F,
                        ordered_result = T))
    qc
  }

temp <- count_vignettes(survey_variables, n = 1000000)

temp %>%
  distinct() %>%
  summarise(mean(include))

# 0.8267

# Using BRM_DF

items_brmdf <- unlist(lapply(apply(brm_df, 2, unique), length))

prod(items_brmdf[12:22])*0.8267
