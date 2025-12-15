library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(viridis)


# read data - add data to folder called "data"
dataDir <- getwd()
dataDF <- read.csv(file.path(dataDir, "data\\SurveyData2025-12-08.csv"), sep = ";")



################## role ####################

# role
# if dataDF$VAR00 == 1 -> Markpersonal, else Chef  
dataDF$VAR00 <- ifelse(dataDF$VAR00 == 1, "Markpersonal", "Chef")

# recode roles VAR01, VAR02
dataDF <- dataDF |>
  mutate(
    VAR01_1 = recode(VAR01_1, `1` = "Lastare/Sorterare", `2` = NULL, .default = NULL),
    VAR01_2 = recode(VAR01_2, `1` = "Flygplanstankare", `2` = NULL, .default = NULL),
    VAR01_3 = recode(VAR01_3, `1` = "Flygplatstekniker", `2` = NULL, .default = NULL),
    VAR01_4 = recode(VAR01_4, `1` = "Skyddsombud/facklig", `2` = NULL, .default = NULL),
    VAR01_5 = recode(VAR01_5, `1` = "Annat", `2` = NULL, .default = NULL),
    VAR02_1 = recode(VAR02_1, `1` = "Flygplatschef", `2` = NULL, .default = NULL),
    VAR02_2 = recode(VAR02_2, `1` = "Teknisk chef", `2` = NULL, .default = NULL),
    VAR02_3 = recode(VAR02_3, `1` = "Operativt ansvarig", `2` = NULL, .default = NULL),
    VAR02_4 = recode(VAR02_4, `1` = "Arbetsmiljöansvarig", `2` = NULL, .default = NULL),
    VAR02_5 = recode(VAR02_5, `1` = "Skyddsombud/facklig", `2` = NULL, .default = NULL),
    VAR02_6 = recode(VAR02_6, `1` = "Annat", `2` = NULL, .default = NULL),
    VAR01_6 = na_if(VAR01_6, ""),
    VAR02_7 = na_if(VAR02_7, "")
  )


#subset
#RoleDF <- subset(dataDF, select = ID:VAR02_7)

#longDF <- RoleDF %>%
#  pivot_longer(
#    cols = matches("VAR01_|VAR02_"),
#    names_to = c(".value", "type"),
#    names_pattern = "(VAR\\d{2})_(\\d+)",
#    values_drop_na = TRUE
#  )
#longDF<-subset(longDF, select = -c(type))

#longDF$VAR01_VAR02 <- longDF$VAR01 
#longDF$VAR01_VAR02 <- ifelse(!is.na(longDF$VAR01_VAR02), longDF$VAR01_VAR02<-longDF$VAR01_VAR02, longDF$VAR01_VAR02<-longDF$VAR02)

# merge longDF back into dataDF
#dataDF <- subset(dataDF, select=-c(VAR01_1, VAR01_2,VAR01_3, VAR01_4, VAR01_5, VAR02_1, VAR02_2, VAR02_3, VAR02_4, VAR02_5, VAR02_6))
#PH_DF <- merge(dataDF, longDF, by = "ID")
#dataDF <- PH_DF



################## demographics ###############

# VAR03 - age
dataDF <- dataDF %>%
  mutate(VAR03 = case_when(
    VAR03 == 1 ~ "Under 25 år",
    VAR03 == 2 ~ "25-34 år",
    VAR03 == 3 ~ "35-44 år",
    VAR03 == 4 ~ "45-54 år",
    VAR03 == 5 ~ "55+ år",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR04 - gender
dataDF <- dataDF %>%
  mutate(VAR04 = case_when(
    VAR04 == 1 ~ "Man",
    VAR04 == 2 ~ "Kvinna",
    VAR04 == 3 ~ "Annat",
    VAR04 == 4 ~ "Vill inte uppge",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR05 - Storlek på flygplatsen
dataDF <- dataDF %>%
  mutate(VAR05 = case_when(
    VAR05 == 1 ~ "Liten (mindre än 50 anställda)",
    VAR05 == 2 ~ "Medelstor (mellan 50-150 anställda)",
    VAR05 == 3 ~ "Stor (över 150 anställda)",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR06 - Hur länge har du arbetat på den flygplats där du jobbar nu?
dataDF <- dataDF %>%
  mutate(VAR06 = case_when(
    VAR06 == 1 ~ "Mindre än 1 år",
    VAR06 == 2 ~ "1–5 år",
    VAR06 == 3 ~ "6–10 år",
    VAR06 == 4 ~ "11–15 år",
    VAR06 == 5 ~ "Mer än 15 år",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR07 - Hur länge har du totalt arbetat på flygplats(er)?
dataDF <- dataDF %>%
  mutate(VAR07 = case_when(
    VAR07 == 1 ~ "Mindre än 1 år",
    VAR07 == 2 ~ "1–5 år",
    VAR07 == 3 ~ "6–10 år",
    VAR07 == 4 ~ "11–15 år",
    VAR07 == 5 ~ "Mer än 15 år",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_ - Hur ofta använder du de tekniska hjälpmedlen?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR12_", "useFrequency_", .x), matches("VAR12_")) %>%
  mutate(across(starts_with("useFrequency_"), ~ case_when(
    .x == 1 ~ "Aldrig",
    .x == 2 ~ "Mindre än varje månad",
    .x == 3 ~ "Varje månad",
    .x == 4 ~ "Varje vecka",
    .x == 5 ~ "Dagligen",
    .x == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_
  )))



################## NASA TLX ###############

# VAR13 - mental
dataDF <- dataDF %>%
  rename_with(~ sub("VAR13_", "TLX_Mental_", .x), matches("VAR13_"))
# VAR14 - fysical
dataDF <- dataDF %>%
  rename_with(~ sub("VAR14_", "TLX_Physical_", .x), matches("VAR14_"))
# VAR15 - Temporal Demand 
dataDF <- dataDF %>%
  rename_with(~ sub("VAR15_", "TLX_Temporal_", .x), matches("VAR15_"))
# VAR16 - Performance
dataDF <- dataDF %>%
  rename_with(~ sub("VAR16_", "TLX_Performance_", .x), matches("VAR16_"))
# VAR17 - Effort
dataDF <- dataDF %>%
  rename_with(~ sub("VAR17_", "TLX_Effort_", .x), matches("VAR17_"))
# VAR18 - Frustration
dataDF <- dataDF %>%
  rename_with(~ sub("VAR18_", "TLX_Frustration_", .x), matches("VAR18_"))
# Nasa-TLX total
for (i in 1:16) {
  dataDF[[paste0("TLX_Sum_", i)]] <-
    dataDF[[paste0("TLX_Frustration_", i)]] +
    dataDF[[paste0("TLX_Effort_", i)]] +
    dataDF[[paste0("TLX_Performance_", i)]] +
    dataDF[[paste0("TLX_Temporal_", i)]] +
    dataDF[[paste0("TLX_Physical_", i)]] +
    dataDF[[paste0("TLX_Mental_", i)]] 
}


############## Tech ######################

# function to recode tech
RecodeMulti <- function(df, prefix, labels) {
  cols <- grep(paste0("^", prefix, "_\\d+$"), names(df), value = TRUE)
  df %>%
    mutate(across(
      all_of(cols),
      ~ {
        idx <- as.numeric(sub(paste0(prefix, "_"), "", cur_column()))
        recode(.x,
               `1` = labels[idx],
               `2` = NULL,
               .default = NULL)
      }
    ))
}

labels_shared <- c(
  "iPad", "Smartphone", "BRS", "De-icing bil", "Bagagetransport",
  "Räddningsfordon", "Power Stow", "Lastband", "Tankbil", "Water/Waste",
  "Fingerscanner", "Lyfthjälpmedel", "Pushback",
  "Stationär eller bärbar dator", "Digitalisering (i allmänhet)",
  "Gater", "Inget ovanstående"
)

# recode VAR08 - Vilka av följande tekniska hjälpmedel finns tillgängliga i din arbetsroll på den flygplats där du arbetar?
dataDF <- RecodeMulti(dataDF, "VAR08", labels_shared)

# VAR09 - Vilka av följande tekniska hjälpmedel finns tillgängliga för markpersonalen på flygplatsen där du arbetar?
dataDF <- RecodeMulti(dataDF, "VAR09", labels_shared)

# VAR10 - Välj max två tekniska hjälpmedel som du anser har betydelse för arbetsmiljön i ditt arbete.
dataDF <- RecodeMulti(dataDF, "VAR10", labels_shared)

# VAR11 - Du kommer att få svara på frågor om hur du upplevde processen med att införa tekniska hjälpmedel, samt vilken roll du hade i införandet.
dataDF <- RecodeMulti(dataDF, "VAR11", labels_shared)


# turn to long df
toLongDF <- function(df, var){
  sub_df <- df %>% select(matches(var), ID)
  # trn to long
  long_df <- sub_df %>%
    pivot_longer(
      cols = matches(var),
      values_to = var,
      values_drop_na = TRUE
    )
  # drop names column
  long_df <- subset(long_df, select= -c(name))
  return(long_df)
}

#VAR08_DF <- toLongDF(dataDF, "VAR08")
#VAR09_DF <- toLongDF(dataDF, "VAR09")
#VAR10_DF <- toLongDF(dataDF, "VAR10")
#VAR11_DF <- toLongDF(dataDF, "VAR11")

# merge back into dataDF
#dataDF <- dataDF %>% select(-contains("VAR08"), -contains("VAR09"), -contains("VAR10"), -contains("VAR11"))

#dataDF2 <- merge(dataDF, VAR08_DF, by = "ID", all = TRUE)
#dataDF3 <- merge(dataDF2, VAR09_DF, by = "ID", all = TRUE)
#dataDF4 <- merge(dataDF3, VAR10_DF, by = "ID", all = TRUE)
#dataDF5 <- merge(dataDF4, VAR11_DF, by = "ID", all = TRUE)



############ Export DF ###################

write.csv(dataDF, file.path(dataDir, "data\\Long_SurveyData2025-12-08.csv"))



