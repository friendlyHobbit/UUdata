library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)
library(viridis)


# read data - add data to folder called "data"
dataDir <- getwd()
dataDF <- read.csv(file.path(dataDir, "data\\SurveyData2025-12-08.csv"), sep = ";")



#------------- Restructucture data -------------------

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
  )

# these need fixing, something goes wrong
dataDF$VAR01_6 <- ifelse(dataDF$VAR01_6=="", dataDF$VAR01_6<-NA, dataDF$VAR01_6<-dataDF$VAR01_6)
dataDF$VAR02_7 <- ifelse(dataDF$VAR02_7=="", dataDF$VAR02_7<-NA, dataDF$VAR02_7<-dataDF$VAR02_7)

#subset
RoleDF <- subset(dataDF, select = ID:VAR02_7)

longDF <- RoleDF %>%
  pivot_longer(
    cols = matches("VAR01_|VAR02_"),
    names_to = c(".value", "type"),
    names_pattern = "(VAR\\d{2})_(\\d+)",
    values_drop_na = TRUE
  )
longDF<-subset(longDF, select = -c(type))

longDF$VAR01_VAR02 <- longDF$VAR01 
longDF$VAR01_VAR02 <- ifelse(!is.na(longDF$VAR01_VAR02), longDF$VAR01_VAR02<-longDF$VAR01_VAR02, longDF$VAR01_VAR02<-longDF$VAR02)

# merge longDF back into dataDF
dataDF <- subset(dataDF, select=-c(VAR01_1, VAR01_2,VAR01_3, VAR01_4, VAR01_5, VAR02_1, VAR02_2, VAR02_3, VAR02_4, VAR02_5, VAR02_6))
PH_DF <- merge(dataDF, longDF, by = "ID")
dataDF <- PH_DF


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

# subset Nasa-TLX and other relevant data
df_tlx <- dataDF[grepl("TLX", names(dataDF))]

# remove empty rows
#df_tlx <- df_tlx[rowSums(is.na(df_tlx)) != ncol(df_tlx), ]



# visualize sum data per tech with jitter
df_tlx %>%
  ggplot( aes(y=`NASA-TLX_Mental_1`)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  #theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")






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
