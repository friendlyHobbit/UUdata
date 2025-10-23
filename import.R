library(dplyr)
library(purrr)
library(stringr)


# read data
dataDir <- getwd()
dataDF <- read.csv(file.path(dataDir, "data\\testdata.csv"), sep = ",")
metaDataDF <- read.csv(file.path(dataDir, "data\\testdata_meta.csv"), sep = ",")


#------------- Restructucture data -------------------
# new DF
UU_DF <- data.frame()

# role
# if dataDF$VAR00 == 1 -> Markpersonal, else Chef  
dataDF$role <- ifelse(dataDF$VAR00 == 1, "Markpersonal", "Chef")

# markpersonal role - FIX MULTIMPLE ROLES
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

# Chef role - FIX MULTIMPLE ROLES
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

# age
dataDF <- dataDF %>%
  mutate(age = case_when(
    VAR03 == 1 ~ "Under 25 år",
    VAR03 == 2 ~ "25-34 år",
    VAR03 == 3 ~ "35-44 år",
    VAR03 == 4 ~ "45-54 år",
    VAR03 == 5 ~ "55+ år",
    TRUE ~ NA_character_  # default if none match
  ))

# gender
dataDF <- dataDF %>%
  mutate(gender = case_when(
    VAR04 == 1 ~ "Man",
    VAR04 == 2 ~ "Kvinna",
    VAR04 == 3 ~ "Annat",
    VAR04 == 4 ~ "Vill inte uppge",
    TRUE ~ NA_character_  # default if none match
  ))

# Storlek på flygplatsen
dataDF <- dataDF %>%
  mutate(airport_size = case_when(
    VAR05 == 1 ~ "Liten (mindre än 50 anställda)",
    VAR05 == 2 ~ "Medelstor (mellan 50-150 anställda)",
    VAR05 == 3 ~ "Stor (över 150 anställda)",
    TRUE ~ NA_character_  # default if none match
  ))

# Hur länge har du arbetat på den flygplats där du jobbar nu?
dataDF <- dataDF %>%
  mutate(VAR06_worktime = case_when(
    VAR06 == 1 ~ "Mindre än 1 år",
    VAR06 == 2 ~ "1–5 år",
    VAR06 == 3 ~ "6–10 år",
    VAR06 == 4 ~ "11–15 år",
    VAR06 == 5 ~ "Mer än 15 år",
    TRUE ~ NA_character_  # default if none match
  ))

# Hur länge har du totalt arbetat på flygplats(er)?
dataDF <- dataDF %>%
  mutate(VAR07_worktime = case_when(
    VAR07 == 1 ~ "Mindre än 1 år",
    VAR07 == 2 ~ "1–5 år",
    VAR07 == 3 ~ "6–10 år",
    VAR07 == 4 ~ "11–15 år",
    VAR07 == 5 ~ "Mer än 15 år",
    TRUE ~ NA_character_  # default if none match
  ))

# Vilka av följande tekniska hjälpmedel finns tillgängliga i din arbetsroll på den flygplats där du arbetar?
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

# Vilka av följande tekniska hjälpmedel finns tillgängliga för markpersonalen på flygplatsen där du arbetar?
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

# VAR12_1 - Hur ofta använder du de tekniska hjälpmedlen? - iPad
dataDF <- dataDF %>%
  mutate(use_ipad = case_when(
    VAR12_1 == 1 ~ "Aldrig",
    VAR12_1 == 2 ~ "Mindre än varje månad",
    VAR12_1 == 3 ~ "Varje månad",
    VAR12_1 == 4 ~ "Varje vecka",
    VAR12_1 == 5 ~ "Dagligen",
    VAR12_1 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_2 - Hur ofta använder du de tekniska hjälpmedlen? - Smartphone
dataDF <- dataDF %>%
  mutate(use_smartphone = case_when(
    VAR12_2 == 1 ~ "Aldrig",
    VAR12_2 == 2 ~ "Mindre än varje månad",
    VAR12_2 == 3 ~ "Varje månad",
    VAR12_2 == 4 ~ "Varje vecka",
    VAR12_2 == 5 ~ "Dagligen",
    VAR12_2 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_3 - Hur ofta använder du de tekniska hjälpmedlen? - BRS
dataDF <- dataDF %>%
  mutate(use_BRS = case_when(
    VAR12_3 == 1 ~ "Aldrig",
    VAR12_3 == 2 ~ "Mindre än varje månad",
    VAR12_3 == 3 ~ "Varje månad",
    VAR12_3 == 4 ~ "Varje vecka",
    VAR12_3 == 5 ~ "Dagligen",
    VAR12_3 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_4 - Hur ofta använder du de tekniska hjälpmedlen? - De-icing bil
dataDF <- dataDF %>%
  mutate(use_deicing = case_when(
    VAR12_4 == 1 ~ "Aldrig",
    VAR12_4 == 2 ~ "Mindre än varje månad",
    VAR12_4 == 3 ~ "Varje månad",
    VAR12_4 == 4 ~ "Varje vecka",
    VAR12_4 == 5 ~ "Dagligen",
    VAR12_4 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_5 - Hur ofta använder du de tekniska hjälpmedlen? - Bagagetransport
dataDF <- dataDF %>%
  mutate(use_bagagetransport = case_when(
    VAR12_5 == 1 ~ "Aldrig",
    VAR12_5 == 2 ~ "Mindre än varje månad",
    VAR12_5 == 3 ~ "Varje månad",
    VAR12_5 == 4 ~ "Varje vecka",
    VAR12_5 == 5 ~ "Dagligen",
    VAR12_5 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_6 - Hur ofta använder du de tekniska hjälpmedlen? - Räddningsfordon
dataDF <- dataDF %>%
  mutate(use_raddningsfordon = case_when(
    VAR12_6 == 1 ~ "Aldrig",
    VAR12_6 == 2 ~ "Mindre än varje månad",
    VAR12_6 == 3 ~ "Varje månad",
    VAR12_6 == 4 ~ "Varje vecka",
    VAR12_6 == 5 ~ "Dagligen",
    VAR12_6 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_7 - Hur ofta använder du de tekniska hjälpmedlen? - Power Stow
dataDF <- dataDF %>%
  mutate(use_powerstow = case_when(
    VAR12_7 == 1 ~ "Aldrig",
    VAR12_7 == 2 ~ "Mindre än varje månad",
    VAR12_7 == 3 ~ "Varje månad",
    VAR12_7 == 4 ~ "Varje vecka",
    VAR12_7 == 5 ~ "Dagligen",
    VAR12_7 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_8 - Hur ofta använder du de tekniska hjälpmedlen? - Lastband (andra än Power Stow)
dataDF <- dataDF %>%
  mutate(use_lastband = case_when(
    VAR12_8 == 1 ~ "Aldrig",
    VAR12_8 == 2 ~ "Mindre än varje månad",
    VAR12_8 == 3 ~ "Varje månad",
    VAR12_8 == 4 ~ "Varje vecka",
    VAR12_8 == 5 ~ "Dagligen",
    VAR12_8 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_9 - Hur ofta använder du de tekniska hjälpmedlen? - Tankbil
dataDF <- dataDF %>%
  mutate(use_tankbil = case_when(
    VAR12_9 == 1 ~ "Aldrig",
    VAR12_9 == 2 ~ "Mindre än varje månad",
    VAR12_9 == 3 ~ "Varje månad",
    VAR12_9 == 4 ~ "Varje vecka",
    VAR12_9 == 5 ~ "Dagligen",
    VAR12_9 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_10 - Hur ofta använder du de tekniska hjälpmedlen? - Water/Waste
dataDF <- dataDF %>%
  mutate(use_water_waste = case_when(
    VAR12_10 == 1 ~ "Aldrig",
    VAR12_10 == 2 ~ "Mindre än varje månad",
    VAR12_10 == 3 ~ "Varje månad",
    VAR12_10 == 4 ~ "Varje vecka",
    VAR12_10 == 5 ~ "Dagligen",
    VAR12_10 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_11 - Hur ofta använder du de tekniska hjälpmedlen? - Fingerscanner
dataDF <- dataDF %>%
  mutate(use_fingerscanner = case_when(
    VAR12_11 == 1 ~ "Aldrig",
    VAR12_11 == 2 ~ "Mindre än varje månad",
    VAR12_11 == 3 ~ "Varje månad",
    VAR12_11 == 4 ~ "Varje vecka",
    VAR12_11 == 5 ~ "Dagligen",
    VAR12_11 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_12 - Hur ofta använder du de tekniska hjälpmedlen? - Lyfthjälpmedel
dataDF <- dataDF %>%
  mutate(use_lyfthjalpmedel = case_when(
    VAR12_12 == 1 ~ "Aldrig",
    VAR12_12 == 2 ~ "Mindre än varje månad",
    VAR12_12 == 3 ~ "Varje månad",
    VAR12_12 == 4 ~ "Varje vecka",
    VAR12_12 == 5 ~ "Dagligen",
    VAR12_12 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_13 - Hur ofta använder du de tekniska hjälpmedlen? - Pushback
dataDF <- dataDF %>%
  mutate(use_pushback = case_when(
    VAR12_13 == 1 ~ "Aldrig",
    VAR12_13 == 2 ~ "Mindre än varje månad",
    VAR12_13 == 3 ~ "Varje månad",
    VAR12_13 == 4 ~ "Varje vecka",
    VAR12_13 == 5 ~ "Dagligen",
    VAR12_13 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_14 - Hur ofta använder du de tekniska hjälpmedlen? - Stationär eller bärbar dator
dataDF <- dataDF %>%
  mutate(use_dator = case_when(
    VAR12_14 == 1 ~ "Aldrig",
    VAR12_14 == 2 ~ "Mindre än varje månad",
    VAR12_14 == 3 ~ "Varje månad",
    VAR12_14 == 4 ~ "Varje vecka",
    VAR12_14 == 5 ~ "Dagligen",
    VAR12_14 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_15 - Hur ofta använder du de tekniska hjälpmedlen? - Digitalisering
dataDF <- dataDF %>%
  mutate(use_digitalisering = case_when(
    VAR12_15 == 1 ~ "Aldrig",
    VAR12_15 == 2 ~ "Mindre än varje månad",
    VAR12_15 == 3 ~ "Varje månad",
    VAR12_15 == 4 ~ "Varje vecka",
    VAR12_15 == 5 ~ "Dagligen",
    VAR12_15 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# VAR12_16 - Hur ofta använder du de tekniska hjälpmedlen? - Gater
dataDF <- dataDF %>%
  mutate(use_gater = case_when(
    VAR12_16 == 1 ~ "Aldrig",
    VAR12_16 == 2 ~ "Mindre än varje månad",
    VAR12_16 == 3 ~ "Varje månad",
    VAR12_16 == 4 ~ "Varje vecka",
    VAR12_16 == 5 ~ "Dagligen",
    VAR12_16 == 6 ~ "Vid varje möjlighet",
    TRUE ~ NA_character_  # default if none match
  ))

# ipad
# VAR13_1 - NASA-TLX ipad - mental
dataDF$ipad_TLX_MentalDemand <- dataDF$VAR13_1
# VAR14_1 - NASA-TLX ipad - fysical 
dataDF$ipad_TLX_PhysicalDemand <- dataDF$VAR14_1
# VAR15_1 - NASA-TLX ipad -  Temporal Demand 
dataDF$ipad_TLX_TemporalDemand <- dataDF$VAR15_1
# VAR16_1 - NASA-TLX ipad - Performance
dataDF$ipad_TLX_Performance <- dataDF$VAR16_1
# VAR17_1 - NASA-TLX ipad - Effort
dataDF$ipad_TLX_Effort <- dataDF$VAR17_1
# VAR18_1 - NASA-TLX ipad - Frustration
dataDF$ipad_TLX_Frustration <- dataDF$VAR18_1

# Smartphone
# VAR13_2 - NASA-TLX  - mental
dataDF$smartphone_TLX_MentalDemand <- dataDF$VAR13_2
# VAR14_2 - NASA-TLX  - fysical 
dataDF$smartphone_TLX_PhysicalDemand <- dataDF$VAR14_2
# VAR15_2 - NASA-TLX  -  Temporal Demand 
dataDF$smartphone_TLX_TemporalDemand <- dataDF$VAR15_2
# VAR16_2 - NASA-TLX  - Performance
dataDF$smartphone_TLX_Performance <- dataDF$VAR16_2
# VAR17_2 - NASA-TLX  - Effort
dataDF$smartphone_TLX_Effort <- dataDF$VAR17_2
# VAR18_2 - NASA-TLX  - Frustration
dataDF$smartphone_TLX_Frustration <- dataDF$VAR18_2

# BRS
# VAR13_3 - NASA-TLX  - mental
dataDF$BRS_TLX_MentalDemand <- dataDF$VAR13_3
# VAR14_3 - NASA-TLX  - fysical 
dataDF$BRS_TLX_PhysicalDemand <- dataDF$VAR14_3
# VAR15_3 - NASA-TLX  -  Temporal Demand 
dataDF$BRS_TLX_TemporalDemand <- dataDF$VAR15_3
# VAR16_3 - NASA-TLX  - Performance
dataDF$BRS_TLX_Performance <- dataDF$VAR16_3
# VAR17_3 - NASA-TLX  - Effort
dataDF$BRS_TLX_Effort <- dataDF$VAR17_3
# VAR18_3 - NASA-TLX  - Frustration
dataDF$BRS_TLX_Frustration <- dataDF$VAR18_3

# De-icing bil
# VAR13_4 - NASA-TLX  - mental
dataDF$deicing_TLX_MentalDemand <- dataDF$VAR13_4
# VAR14_4 - NASA-TLX  - fysical 
dataDF$deicing_TLX_PhysicalDemand <- dataDF$VAR14_4
# VAR15_4 - NASA-TLX  -  Temporal Demand 
dataDF$deicing_TLX_TemporalDemand <- dataDF$VAR15_4
# VAR16_4 - NASA-TLX  - Performance
dataDF$deicing_TLX_Performance <- dataDF$VAR16_4
# VAR17_4 - NASA-TLX  - Effort
dataDF$deicing_TLX_Effort <- dataDF$VAR17_4
# VAR18_4 - NASA-TLX  - Frustration
dataDF$deicing_TLX_Frustration <- dataDF$VAR18_4

# Bagagetransport
# VAR13_5 - NASA-TLX  - mental
dataDF$bagagetransport_TLX_MentalDemand <- dataDF$VAR13_5
# VAR14_5 - NASA-TLX  - fysical 
dataDF$bagagetransport_TLX_PhysicalDemand <- dataDF$VAR14_5
# VAR15_5 - NASA-TLX  -  Temporal Demand 
dataDF$bagagetransport_TLX_TemporalDemand <- dataDF$VAR15_5
# VAR16_5 - NASA-TLX  - Performance
dataDF$bagagetransport_TLX_Performance <- dataDF$VAR16_5
# VAR17_5 - NASA-TLX  - Effort
dataDF$bagagetransport_TLX_Effort <- dataDF$VAR17_5
# VAR18_5 - NASA-TLX  - Frustration
dataDF$bagagetransport_TLX_Frustration <- dataDF$VAR18_5

# Räddningsfordon
# VAR13_6 - NASA-TLX  - mental
dataDF$raddningsfordon_TLX_MentalDemand <- dataDF$VAR13_6
# VAR14_6 - NASA-TLX  - fysical 
dataDF$raddningsfordon_TLX_PhysicalDemand <- dataDF$VAR14_6
# VAR15_6 - NASA-TLX  -  Temporal Demand 
dataDF$raddningsfordon_TLX_TemporalDemand <- dataDF$VAR15_6
# VAR16_6 - NASA-TLX  - Performance
dataDF$raddningsfordon_TLX_Performance <- dataDF$VAR16_6
# VAR17_6 - NASA-TLX  - Effort
dataDF$raddningsfordon_TLX_Effort <- dataDF$VAR17_6
# VAR18_6 - NASA-TLX  - Frustration
dataDF$raddningsfordon_TLX_Frustration <- dataDF$VAR18_6

# Power stow
# VAR13_7 - NASA-TLX  - mental
dataDF$powerstow_TLX_MentalDemand <- dataDF$VAR13_7
# VAR14_7 - NASA-TLX  - fysical 
dataDF$powerstow_TLX_PhysicalDemand <- dataDF$VAR14_7
# VAR15_7 - NASA-TLX  -  Temporal Demand 
dataDF$powerstow_TLX_TemporalDemand <- dataDF$VAR15_7
# VAR16_7 - NASA-TLX  - Performance
dataDF$powerstow_TLX_Performance <- dataDF$VAR16_7
# VAR17_7 - NASA-TLX  - Effort
dataDF$powerstow_TLX_Effort <- dataDF$VAR17_7
# VAR18_7 - NASA-TLX  - Frustration
dataDF$powerstow_TLX_Frustration <- dataDF$VAR18_7

# Lastband
# VAR13_8 - NASA-TLX  - mental
dataDF$lastband_TLX_MentalDemand <- dataDF$VAR13_8
# VAR14_8 - NASA-TLX  - fysical 
dataDF$lastband_TLX_PhysicalDemand <- dataDF$VAR14_8
# VAR15_8 - NASA-TLX  -  Temporal Demand 
dataDF$lastband_TLX_TemporalDemand <- dataDF$VAR15_8
# VAR16_8 - NASA-TLX  - Performance
dataDF$lastband_TLX_Performance <- dataDF$VAR16_8
# VAR17_8 - NASA-TLX  - Effort
dataDF$lastband_TLX_Effort <- dataDF$VAR17_8
# VAR18_8 - NASA-TLX  - Frustration
dataDF$lastband_TLX_Frustration <- dataDF$VAR18_8

# Tankbil
# VAR13_9 - NASA-TLX  - mental
dataDF$tankbil_TLX_MentalDemand <- dataDF$VAR13_9
# VAR14_9 - NASA-TLX  - fysical 
dataDF$tankbil_TLX_PhysicalDemand <- dataDF$VAR14_9
# VAR15_9 - NASA-TLX  -  Temporal Demand 
dataDF$tankbil_TLX_TemporalDemand <- dataDF$VAR15_9
# VAR16_9 - NASA-TLX  - Performance
dataDF$tankbil_TLX_Performance <- dataDF$VAR16_9
# VAR17_9 - NASA-TLX  - Effort
dataDF$tankbil_TLX_Effort <- dataDF$VAR17_9
# VAR18_9 - NASA-TLX  - Frustration
dataDF$tankbil_TLX_Frustration <- dataDF$VAR18_9

# Water/waste
# VAR13_10 - NASA-TLX  - mental
dataDF$water_waste_TLX_MentalDemand <- dataDF$VAR13_10
# VAR14_10 - NASA-TLX  - fysical 
dataDF$water_waste_TLX_PhysicalDemand <- dataDF$VAR14_10
# VAR15_10 - NASA-TLX  -  Temporal Demand 
dataDF$water_waste_TLX_TemporalDemand <- dataDF$VAR15_10
# VAR16_10 - NASA-TLX  - Performance
dataDF$water_waste_TLX_Performance <- dataDF$VAR16_10
# VAR17_10 - NASA-TLX  - Effort
dataDF$water_waste_TLX_Effort <- dataDF$VAR17_10
# VAR18_10 - NASA-TLX  - Frustration
dataDF$water_waste_TLX_Frustration <- dataDF$VAR18_10

# Fingerscanner
# VAR13_11 - NASA-TLX  - mental
dataDF$fingerscanner_TLX_MentalDemand <- dataDF$VAR13_11
# VAR14_11 - NASA-TLX  - fysical 
dataDF$fingerscanner_TLX_PhysicalDemand <- dataDF$VAR14_11
# VAR15_11 - NASA-TLX  -  Temporal Demand 
dataDF$fingerscanner_TLX_TemporalDemand <- dataDF$VAR15_11
# VAR16_11 - NASA-TLX  - Performance
dataDF$fingerscanner_TLX_Performance <- dataDF$VAR16_11
# VAR17_11 - NASA-TLX  - Effort
dataDF$fingerscanner_TLX_Effort <- dataDF$VAR17_11
# VAR18_11 - NASA-TLX  - Frustration
dataDF$fingerscanner_TLX_Frustration <- dataDF$VAR18_11

# Lyfthjälpmedel
# VAR13_12 - NASA-TLX  - mental
dataDF$lyfthjalpmedel_TLX_MentalDemand <- dataDF$VAR13_12
# VAR14_12 - NASA-TLX  - fysical 
dataDF$lyfthjalpmedel_TLX_PhysicalDemand <- dataDF$VAR14_12
# VAR15_12 - NASA-TLX  -  Temporal Demand 
dataDF$lyfthjalpmedel_TLX_TemporalDemand <- dataDF$VAR15_12
# VAR16_12 - NASA-TLX  - Performance
dataDF$lyfthjalpmedel_TLX_Performance <- dataDF$VAR16_12
# VAR17_12 - NASA-TLX  - Effort
dataDF$lyfthjalpmedel_TLX_Effort <- dataDF$VAR17_12
# VAR18_12 - NASA-TLX  - Frustration
dataDF$lyfthjalpmedel_TLX_Frustration <- dataDF$VAR18_12

# Pushback
# VAR13_13 - NASA-TLX  - mental
dataDF$pushback_TLX_MentalDemand <- dataDF$VAR13_13
# VAR14_13 - NASA-TLX  - fysical 
dataDF$pushback_TLX_PhysicalDemand <- dataDF$VAR14_13
# VAR15_13 - NASA-TLX  -  Temporal Demand 
dataDF$pushback_TLX_TemporalDemand <- dataDF$VAR15_13
# VAR16_13 - NASA-TLX  - Performance
dataDF$pushback_TLX_Performance <- dataDF$VAR16_13
# VAR17_13 - NASA-TLX  - Effort
dataDF$pushback_TLX_Effort <- dataDF$VAR17_13
# VAR18_13 - NASA-TLX  - Frustration
dataDF$pushback_TLX_Frustration <- dataDF$VAR18_13
