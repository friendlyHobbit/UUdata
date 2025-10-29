library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)


# read data - add data to folder called "data"
dataDir <- getwd()
dataDF <- read.csv(file.path(dataDir, "data\\testdata.csv"), sep = ",")
metaDataDF <- read.csv(file.path(dataDir, "data\\testdata_meta.csv"), sep = ",")


#------------- Restructucture data -------------------
# new DF
UU_DF <- data.frame()

# role
# if dataDF$VAR00 == 1 -> Markpersonal, else Chef  
dataDF$role <- ifelse(dataDF$VAR00 == 1, "Markpersonal", "Chef")

# VAR01 - markpersonal role - FIX MULTIMPLE ROLES
dataDF <- dataDF %>%
  mutate(
    mark_role = pmap_chr(
      list(VAR01_1, VAR01_2, VAR01_3, VAR01_4, VAR01_5),
      function(v1, v2, v3, v4, v5) {
        roles <- c()
        if (isTRUE(v1 == 1)) roles <- c(roles, "Lastare/Sorterare")
        if (isTRUE(v2 == 1)) roles <- c(roles, "Flygplanstankare")
        if (isTRUE(v3 == 1)) roles <- c(roles, "Flygplatstekniker")
        if (isTRUE(v4 == 1)) roles <- c(roles, "Skyddsombud/facklig")
        if (isTRUE(v5 == 1)) roles <- c(roles, "Annat")
        if (length(roles) == 0) NA_character_ else str_c(roles, collapse = ", ")
      }
    )
  )

dataDF <- dataDF %>%
  mutate(
    mark_role = case_when(
      VAR01_1 == 1 ~ "Lastare/Sorterare",
      VAR01_2 == 1 ~ "Flygplanstankare",
      VAR01_3 == 1 ~ "Flygplatstekniker",
      VAR01_4 == 1 ~ "Skyddsombud/facklig",
      VAR01_5 == 1 ~ "Annat",
      TRUE ~ NA_character_
    )
  )
dataDF$mark_role_annat <- dataDF$VAR01_6

# VAR02 - Chef role - FIX MULTIMPLE ROLES
dataDF <- dataDF %>%
  mutate(
    chef_role = pmap_chr(
      list(VAR02_1, VAR02_2, VAR02_3, VAR02_4, VAR02_5, VAR02_6),
      function(v1, v2, v3, v4, v5, v6) {
        roles <- c()
        if (isTRUE(v1 == 1)) roles <- c(roles, "Flygplatschef")
        if (isTRUE(v2 == 1)) roles <- c(roles, "Teknisk chef")
        if (isTRUE(v3 == 1)) roles <- c(roles, "Operativt ansvarig")
        if (isTRUE(v4 == 1)) roles <- c(roles, "Arbetsmiljöansvarig")
        if (isTRUE(v5 == 1)) roles <- c(roles, "Skyddsombud/facklig")
        if (isTRUE(v6 == 1)) roles <- c(roles, "Annat")
        if (length(roles) == 0) NA_character_ else str_c(roles, collapse = ", ")
      }
    )
  )
dataDF$chef_rol_annat <- dataDF$VAR02_7

# VAR03 - age
dataDF <- dataDF %>%
  mutate(age = case_when(
    VAR03 == 1 ~ "Under 25 år",
    VAR03 == 2 ~ "25-34 år",
    VAR03 == 3 ~ "35-44 år",
    VAR03 == 4 ~ "45-54 år",
    VAR03 == 5 ~ "55+ år",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR04 - gender
dataDF <- dataDF %>%
  mutate(gender = case_when(
    VAR04 == 1 ~ "Man",
    VAR04 == 2 ~ "Kvinna",
    VAR04 == 3 ~ "Annat",
    VAR04 == 4 ~ "Vill inte uppge",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR05 - Storlek på flygplatsen
dataDF <- dataDF %>%
  mutate(airport_size = case_when(
    VAR05 == 1 ~ "Liten (mindre än 50 anställda)",
    VAR05 == 2 ~ "Medelstor (mellan 50-150 anställda)",
    VAR05 == 3 ~ "Stor (över 150 anställda)",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR06 - Hur länge har du arbetat på den flygplats där du jobbar nu?
dataDF <- dataDF %>%
  mutate(VAR06_worktime = case_when(
    VAR06 == 1 ~ "Mindre än 1 år",
    VAR06 == 2 ~ "1–5 år",
    VAR06 == 3 ~ "6–10 år",
    VAR06 == 4 ~ "11–15 år",
    VAR06 == 5 ~ "Mer än 15 år",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR07 - Hur länge har du totalt arbetat på flygplats(er)?
dataDF <- dataDF %>%
  mutate(VAR07_worktime = case_when(
    VAR07 == 1 ~ "Mindre än 1 år",
    VAR07 == 2 ~ "1–5 år",
    VAR07 == 3 ~ "6–10 år",
    VAR07 == 4 ~ "11–15 år",
    VAR07 == 5 ~ "Mer än 15 år",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR08 - Vilka av följande tekniska hjälpmedel finns tillgängliga i din arbetsroll på den flygplats där du arbetar?
dataDF <- dataDF %>%
  mutate(
    VAR08_tech = pmap_chr(
      list(VAR08_1, VAR08_2, VAR08_3, VAR08_4, VAR08_5, VAR08_6, VAR08_7, VAR08_8, VAR08_9, VAR08_10, VAR08_11, VAR08_12, VAR08_13, VAR08_14, VAR08_15, VAR08_16, VAR08_17),
      function(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) {
        tech <- c()
        if (isTRUE(v1 == 1)) tech <- c(tech, "iPad")
        if (isTRUE(v2 == 1)) tech <- c(tech, "Smartphone")
        if (isTRUE(v3 == 1)) tech <- c(tech, "BRS")
        if (isTRUE(v4 == 1)) tech <- c(tech, "De-icing bil")
        if (isTRUE(v5 == 1)) tech <- c(tech, "Bagagetransport")
        if (isTRUE(v6 == 1)) tech <- c(tech, "Bagagetransport")
        if (isTRUE(v7 == 1)) tech <- c(tech, "Power Stow")
        if (isTRUE(v8 == 1)) tech <- c(tech, "Lastband")
        if (isTRUE(v9 == 1)) tech <- c(tech, "Tankbil")
        if (isTRUE(v10 == 1)) tech <- c(tech, "Water/Waste")
        if (isTRUE(v11 == 1)) tech <- c(tech, "Fingerscanner")
        if (isTRUE(v12 == 1)) tech <- c(tech, "Lyfthjälpmedel")
        if (isTRUE(v13 == 1)) tech <- c(tech, "Pushback")
        if (isTRUE(v14 == 1)) tech <- c(tech, "Stationär eller bärbar dator")
        if (isTRUE(v15 == 1)) tech <- c(tech, "Digitalisering (i allmänhet)")
        if (isTRUE(v16 == 1)) tech <- c(tech, "Gater")
        if (isTRUE(v17 == 1)) tech <- c(tech, "Inget ovanstående")
        if (length(tech) == 0) NA_character_ else str_c(tech, collapse = ", ")
      }
    )
  )

# VAR09 - Vilka av följande tekniska hjälpmedel finns tillgängliga för markpersonalen på flygplatsen där du arbetar?
dataDF <- dataDF %>%
  mutate(
    VAR09_tech = pmap_chr(
      list(VAR09_1, VAR09_2, VAR09_3, VAR09_4, VAR09_5, VAR09_6, VAR09_7, VAR09_8, VAR09_9, VAR09_10, VAR09_11, VAR09_12, VAR09_13, VAR09_14, VAR09_15, VAR09_16, VAR09_17),
      function(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) {
        tech <- c()
        if (isTRUE(v1 == 1)) tech <- c(tech, "iPad")
        if (isTRUE(v2 == 1)) tech <- c(tech, "Smartphone")
        if (isTRUE(v3 == 1)) tech <- c(tech, "BRS")
        if (isTRUE(v4 == 1)) tech <- c(tech, "De-icing bil")
        if (isTRUE(v5 == 1)) tech <- c(tech, "Bagagetransport")
        if (isTRUE(v6 == 1)) tech <- c(tech, "Räddningsfordon")
        if (isTRUE(v7 == 1)) tech <- c(tech, "Power Stow")
        if (isTRUE(v8 == 1)) tech <- c(tech, "Lastband")
        if (isTRUE(v9 == 1)) tech <- c(tech, "Tankbil")
        if (isTRUE(v10 == 1)) tech <- c(tech, "Water/Waste")
        if (isTRUE(v11 == 1)) tech <- c(tech, "Fingerscanner")
        if (isTRUE(v12 == 1)) tech <- c(tech, "Lyfthjälpmedel")
        if (isTRUE(v13 == 1)) tech <- c(tech, "Pushback")
        if (isTRUE(v14 == 1)) tech <- c(tech, "Stationär eller bärbar dator")
        if (isTRUE(v15 == 1)) tech <- c(tech, "Digitalisering (i allmänhet)")
        if (isTRUE(v16 == 1)) tech <- c(tech, "Gater")
        if (isTRUE(v17 == 1)) tech <- c(tech, "Inget ovanstående")
        if (length(tech) == 0) NA_character_ else str_c(tech, collapse = ", ")
      }
    )
  )

# VAR10 - Välj max två tekniska hjälpmedel som du anser har betydelse för arbetsmiljön i ditt arbete.
dataDF <- dataDF %>%
  mutate(
    job_tech = pmap_chr(
      list(VAR10_1, VAR10_2, VAR10_3, VAR10_4, VAR10_5, VAR10_6, VAR10_7, VAR10_8, VAR10_9, VAR10_10, VAR10_11, VAR10_12, VAR10_13, VAR10_14, VAR10_15, VAR10_16, VAR10_17),
      function(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) {
        tech <- c()
        if (isTRUE(v1 == 1)) tech <- c(tech, "iPad")
        if (isTRUE(v2 == 1)) tech <- c(tech, "Smartphone")
        if (isTRUE(v3 == 1)) tech <- c(tech, "BRS")
        if (isTRUE(v4 == 1)) tech <- c(tech, "De-icing bil")
        if (isTRUE(v5 == 1)) tech <- c(tech, "Bagagetransport")
        if (isTRUE(v6 == 1)) tech <- c(tech, "Räddningsfordon")
        if (isTRUE(v7 == 1)) tech <- c(tech, "Power Stow")
        if (isTRUE(v8 == 1)) tech <- c(tech, "Lastband")
        if (isTRUE(v9 == 1)) tech <- c(tech, "Tankbil")
        if (isTRUE(v10 == 1)) tech <- c(tech, "Water/Waste")
        if (isTRUE(v11 == 1)) tech <- c(tech, "Fingerscanner")
        if (isTRUE(v12 == 1)) tech <- c(tech, "Lyfthjälpmedel")
        if (isTRUE(v13 == 1)) tech <- c(tech, "Pushback")
        if (isTRUE(v14 == 1)) tech <- c(tech, "Stationär eller bärbar dator")
        if (isTRUE(v15 == 1)) tech <- c(tech, "Digitalisering (i allmänhet)")
        if (isTRUE(v16 == 1)) tech <- c(tech, "Gater")
        if (isTRUE(v17 == 1)) tech <- c(tech, "Inget ovanstående")
        if (length(tech) == 0) NA_character_ else str_c(tech, collapse = ", ")
      }
    )
  )

# VAR11 - Du kommer att få svara på frågor om hur du upplevde processen med att införa tekniska hjälpmedel, samt vilken roll du hade i införandet.
dataDF <- dataDF %>%
  mutate(
    mark_tech = pmap_chr(
      list(VAR11_1, VAR11_2, VAR11_3, VAR11_4, VAR11_5, VAR11_6, VAR11_7, VAR11_8, VAR11_9, VAR11_10, VAR11_11, VAR11_12, VAR11_13, VAR11_14, VAR11_15, VAR11_16, VAR11_17),
      function(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) {
        tech <- c()
        if (isTRUE(v1 == 1)) tech <- c(tech, "iPad")
        if (isTRUE(v2 == 1)) tech <- c(tech, "Smartphone")
        if (isTRUE(v3 == 1)) tech <- c(tech, "BRS")
        if (isTRUE(v4 == 1)) tech <- c(tech, "De-icing bil")
        if (isTRUE(v5 == 1)) tech <- c(tech, "Bagagetransport")
        if (isTRUE(v6 == 1)) tech <- c(tech, "Räddningsfordon")
        if (isTRUE(v7 == 1)) tech <- c(tech, "Power Stow")
        if (isTRUE(v8 == 1)) tech <- c(tech, "Lastband")
        if (isTRUE(v9 == 1)) tech <- c(tech, "Tankbil")
        if (isTRUE(v10 == 1)) tech <- c(tech, "Water/Waste")
        if (isTRUE(v11 == 1)) tech <- c(tech, "Fingerscanner")
        if (isTRUE(v12 == 1)) tech <- c(tech, "Lyfthjälpmedel")
        if (isTRUE(v13 == 1)) tech <- c(tech, "Pushback")
        if (isTRUE(v14 == 1)) tech <- c(tech, "Stationär eller bärbar dator")
        if (isTRUE(v15 == 1)) tech <- c(tech, "Digitalisering (i allmänhet)")
        if (isTRUE(v16 == 1)) tech <- c(tech, "Gater")
        if (isTRUE(v17 == 1)) tech <- c(tech, "Inget ovanstående")
        if (length(tech) == 0) NA_character_ else str_c(tech, collapse = ", ")
      }
    )
  )


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


# NASA TLX
# VAR13 - mental
dataDF <- dataDF %>%
  rename_with(~ sub("VAR13_", "NASA-TLX_Mental_", .x), matches("VAR13_"))
# VAR14 - fysical
dataDF <- dataDF %>%
  rename_with(~ sub("VAR14_", "NASA-TLX_PhysicalDemand_", .x), matches("VAR14_"))
# VAR15 - Temporal Demand 
dataDF <- dataDF %>%
  rename_with(~ sub("VAR15_", "NASA-TLX_TemporalDemand_", .x), matches("VAR15_"))
# VAR16 - Performance
dataDF <- dataDF %>%
  rename_with(~ sub("VAR16_", "NASA-TLX_Performance_", .x), matches("VAR16_"))
# VAR17 - Effort
dataDF <- dataDF %>%
  rename_with(~ sub("VAR17_", "NASA-TLX_Effort_", .x), matches("VAR17_"))
# VAR18 - Frustration
dataDF <- dataDF %>%
  rename_with(~ sub("VAR18_", "NASA-TLX_Frustration_", .x), matches("VAR18_"))


# change var names of VAR19_X to sun (Tekniken blir svårare att använda i starkt solsken.)
dataDF <- dataDF %>%
  rename_with(~ sub("VAR19_", "sun_", .x), matches("VAR19_"))

# change var names of VAR20_X to snow (Tekniken blir svårare att använda i snö.)
dataDF <- dataDF %>%
  rename_with(~ sub("VAR20_", "snow_", .x), matches("VAR20_"))

# change var names of VAR21_X to cold (Tekniken blir svårare att använda i kyla.)
dataDF <- dataDF %>%
  rename_with(~ sub("VAR21_", "cold_", .x), matches("VAR21_"))

# change var names of VAR22_X to rain (Tekniken blir svårare att använda i regn.)
dataDF <- dataDF %>%
  rename_with(~ sub("VAR22_", "rain_", .x), matches("VAR22_"))

# change var names of VAR23_X to dark (Tekniken blir svårare att använda i mörker.)
dataDF <- dataDF %>%
  rename_with(~ sub("VAR23_", "dark_", .x), matches("VAR23_"))

# change var names of VAR24_X to mist (Tekniken blir svårare att använda i dimma.)
dataDF <- dataDF %>%
  rename_with(~ sub("VAR24_", "mist_", .x), matches("VAR24_"))


# VAR25_x - I vilken utsträckning har du medverkat i arbetet att införa det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR25_", "introduce_", .x), matches("VAR25_"))

# VAR26_x - I vilken utsträckning har du medverkat i att utveckla hur ni jobbar med det tekniska hjälpmedlet? 
dataDF <- dataDF %>%
  rename_with(~ sub("VAR26_", "develop_", .x), matches("VAR26_"))

# VAR27_x - I vilken utsträckning har du fått möjlighet att medverka i införandet av det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR27_", "possibility_introduce_", .x), matches("VAR27_"))

# VAR28_x - I vilken utsträckning har du fått möjlighet att medverka i att utveckla hur ni jobbar med det tekniska hjälpmedlet? 
dataDF <- dataDF %>%
  rename_with(~ sub("VAR28_", "possibility_develop_", .x), matches("VAR28_"))

# VAR29_x - I vilken grad upplever du att dina synpunkter har påverkat införandet av det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR29_", "opinion_introduce_", .x), matches("VAR29_"))

# VAR30_x -  I vilken grad upplever du att dina synpunkter har påverkat utvecklingen av hur ni jobbar med det tekniska hjälpmedlet? 
dataDF <- dataDF %>%
  rename_with(~ sub("VAR30_", "opinion_develop_", .x), matches("VAR30_"))

# VAR31 - I vilken grad upplever du att arbetsmiljön varit i fokus vid införandet av det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR31_", "environment_introduce_", .x), matches("VAR31_"))

# VAR32 - I vilken grad genomfördes en riskanalys innan det tekniska hjälpmedlet infördes?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR32_", "risk_analysis_", .x), matches("VAR32_"))

# VAR33 - I vilken utsträckning har dina kollegor i markpersonalen medverkat i utvecklingsarbetet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR33_", "staff_involvement_", .x), matches("VAR33_"))

# VAR34 -  I vilken grad vet du hur du ska gå till väga om du har synpunkter om förbättringar som gäller arbetet med det tekniska hjälpmedlet? 
dataDF <- dataDF %>%
  rename_with(~ sub("VAR34_", "feedback_", .x), matches("VAR34_"))

# VAR35 - I vilken grad upplever du att det har varit tydligt varför det tekniska hjälpmedlet har införts?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR35_", "clearity_", .x), matches("VAR35_"))

# VAR36 - I vilken utsträckning har du medverkat i arbetet att införa det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR36_", "participated_", .x), matches("VAR36_"))

# VAR37 -  I vilken utsträckning har du medverkat i att utveckla hur berörd markpersonal jobbar med det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR37_", "participated_staff_", .x), matches("VAR37_"))

# VAR38 - I vilken utsträckning har berörd markpersonal fått möjlighet att medverka i införandet av det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR38_", "opportunity_introduction_", .x), matches("VAR38_"))

# VAR39 -  I vilken utsträckning har berörd markpersonal fått möjlighet att medverka i att utveckla hur de jobbar med det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR39_", "opportunity_development_", .x), matches("VAR39_"))

# VAR40 - I vilken grad upplever du att dina synpunkter har påverkat införandet av det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR40_", "opinion_introduce2_", .x), matches("VAR40_"))

# VAR41 - I vilken grad upplever du att dina synpunkter har påverkat utvecklingen av hur berörd markpersonal jobbar med det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR41_", "opinion_develop2_", .x), matches("VAR41_"))

# VAR42 - I vilken grad har arbetsmiljö varit ett fokus när det tekniska hjälpmedlet har införts?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR42_", "environment_introduce2_", .x), matches("VAR42_"))

# VAR43 - I vilken grad genomfördes en riskanalys innan det tekniska hjälpmedlet infördes? 
dataDF <- dataDF %>%
  rename_with(~ sub("VAR43_", "risk_analysis2_", .x), matches("VAR43_"))

# VAR44 - I vilken utsträckning har dina chefskollegor medverkat i utvecklingsarbetet gällande det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR44_", "chef_development_", .x), matches("VAR44_"))

# VAR45 - I vilken grad har berörd markpersonal fått möjlighet att framföra synpunkter om förbättringar som gäller arbetet med det tekniska hjälpmedlet?
dataDF <- dataDF %>%
  rename_with(~ sub("VAR45_", "chef_feedback_", .x), matches("VAR45_"))



# ipad data and personal data - VARXX_1
df_subset <- dataDF[, unique(c(1:19, grep("_1\\b", names(dataDF))))]



########### Initial analysis #################

# get people's roles
dataDF$role <- as.factor(dataDF$role)
summary(dataDF$role)

# Compute counts per role
role_counts <- dataDF %>%
  count(role)

# Plot
ggplot(role_counts, aes(x = "", y = n, fill = role)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(title = "managers vs ground staff counts", fill = "roles") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# ground roles
dataDF$mark_role <- as.factor(dataDF$mark_role)
summary(dataDF$mark_role)

# Compute counts per role
role_counts <- dataDF %>%
  count(mark_role)

# Plot
ggplot(role_counts, aes(x = "", y = n, fill = mark_role)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = n),  position = position_stack(vjust = 0.5)) +
  labs(title = "Ground staff roles", fill = "Roles") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# boss roles
dataDF$chef_role <- as.factor(dataDF$chef_role)
summary(dataDF$chef_role)

# Compute counts per role
role_counts <- dataDF %>%
  count(chef_role)

# Plot
ggplot(role_counts, aes(x = "", y = n, fill = chef_role)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = n),  position = position_stack(vjust = 0.5)) +
  labs(title = "Ground staff roles", fill = "Roles") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )



# age
dataDF$age <- as.factor(dataDF$age)
summary(dataDF$age)

ggplot(dataDF, aes(x = age)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "age",
    y = "count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


# gender
dataDF$gender <- as.factor(dataDF$gender)
summary(dataDF$gender)

ggplot(dataDF, aes(x = gender)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "gender",
    y = "count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


# airport size
dataDF$airport_size <- as.factor(dataDF$airport_size)
summary(dataDF$airport_size)

ggplot(dataDF, aes(x = airport_size)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "airport_size",
    y = "count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# time at airport
dataDF$VAR06_worktime <- as.factor(dataDF$VAR06_worktime)
summary(dataDF$VAR06_worktime)

ggplot(dataDF, aes(x = VAR06_worktime)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "VAR06_worktime",
    y = "count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# time at airports generally
dataDF$VAR07_worktime <- as.factor(dataDF$VAR07_worktime)
summary(dataDF$VAR07_worktime)

ggplot(dataDF, aes(x = VAR07_worktime)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "VAR07_worktime",
    y = "count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# available tech
dataDF <- dataDF %>%
  mutate(across(starts_with("VAR08_"), ~ factor(case_when(
    .x == 1 ~ "Ja",
    .x == 2 ~ "Nej",
    TRUE ~ NA_character_
  ))))

# ipad
summary(dataDF$VAR08_1)
# Smartphone
summary(dataDF$VAR08_2)
# BRS
summary(dataDF$VAR08_3)
# De-icing bil
summary(dataDF$VAR08_4)
# - Bagagetransport
summary(dataDF$VAR08_5)
# Räddningsfordon
summary(dataDF$VAR08_6)
# Power Stow
summary(dataDF$VAR08_7)
# Lastband
summary(dataDF$VAR08_8)
# Tankbil
summary(dataDF$VAR08_9)
# Water/Waste
summary(dataDF$VAR08_10)
# Fingerscanner
summary(dataDF$VAR08_11)
# Lyfthjälpmedel
summary(dataDF$VAR08_12)
# Pushback
summary(dataDF$VAR08_13)
# dator
summary(dataDF$VAR08_14)
# Digitalisering
summary(dataDF$VAR08_15)
# Gater
summary(dataDF$VAR08_16)
# Ingen
summary(dataDF$VAR08_17)

data_long <- dataDF %>%
  pivot_longer(
    cols = starts_with("VAR08_"),
    names_to = "Question",
    values_to = "Response"
  )

ggplot(data_long, aes(x = Question, fill = Response)) +
  geom_bar(position = "stack") +
  geom_text(
    stat = "count",               # use counts automatically computed by geom_bar
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),  # center text within each segment
    size = 3.5
  ) +
  labs(
    x = "VAR08_",
    y = "available tech - ground staff answers",
    title = "VAR8_"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# get tech people filled nasa TLX in for
dataDF$job_tech 

dataDF <- dataDF %>%
  mutate(across(starts_with("VAR10_"), ~ factor(case_when(
    .x == 1 ~ "Ja",
    .x == 2 ~ "Nej",
    TRUE ~ NA_character_
  ))))

# ipad
summary(dataDF$VAR10_1)
# Smartphone
summary(dataDF$VAR10_2)
# BRS
summary(dataDF$VAR10_3)
# De-icing bil
summary(dataDF$VAR10_4)
# - Bagagetransport
summary(dataDF$VAR10_5)
# Räddningsfordon
summary(dataDF$VAR10_6)
# Power Stow
summary(dataDF$VAR10_7)
# Lastband
summary(dataDF$VAR10_8)
# Tankbil
summary(dataDF$VAR10_9)
# Water/Waste
summary(dataDF$VAR10_10)
# Fingerscanner
summary(dataDF$VAR10_11)
# Lyfthjälpmedel
summary(dataDF$VAR10_12)
# Pushback
summary(dataDF$VAR10_13)
# dator
summary(dataDF$VAR10_14)
# Digitalisering
summary(dataDF$VAR10_15)
# Gater
summary(dataDF$VAR10_16)
# Inget ovanstående är av betydelse
summary(dataDF$VAR10_17)

data_long <- dataDF %>%
  pivot_longer(
    cols = starts_with("VAR10_"),
    names_to = "Question",
    values_to = "Response"
  )

# Show
data_long %>%
  filter(Response == "Ja") %>%
  count(Question) %>%                           
  ggplot(aes(x = Question, y = n, fill = Question)) +
  geom_col(show.legend = FALSE) +               
  geom_text(aes(label = n),                     
            vjust = -0.3, size = 3.5) +         
  labs(
    x = "VAR10_",
    y = "number of 'Ja' answers",
    title = "The tech ground staff filled nasa TLX in for"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
