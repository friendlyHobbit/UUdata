
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(viridis)
library(ggplot2)


# read data - add data to folder called "data"
dataDir <- getwd()
dataDF <- read.csv(file.path(dataDir, "data\\Long_SurveyData2025-12-08.csv"), sep = ",")

# turn all colums to factors
dataDF <- dataDF %>% mutate_if(is.character, as.factor)
dataDF$ID <- as.factor(dataDF$ID)

summary(dataDF$ID)

# recode 0 to NA
#dataDF[dataDF == 0] <- NA

########### Initial analysis #################

# get people's roles
summary(dataDF$VAR00)

# Compute counts per role, unique ID
role_counts <- dataDF %>%
  count(VAR00)

# Plot
ggplot(dataDF, aes(x = VAR00, fill = VAR04)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs( title = "managers vs ground staff",
    x = "Roles",
    y = "count",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# age
summary(dataDF$VAR03)

ggplot(dataDF, aes(x = VAR03, fill = VAR03)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "Age",
    y = "count"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))


# gender
summary(dataDF$VAR04)

ggplot(dataDF, aes(x = VAR04)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "gender",
    y = "count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# airport size
summary(dataDF$VAR05)

ggplot(dataDF, aes(x = VAR05)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "",
    y = "count"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)) 



# time at airport
summary(dataDF$VAR06)

ggplot(dataDF, aes(x = VAR06)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "VAR06_worktime",
    y = "count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# time at airports generally
summary(dataDF$VAR07)

ggplot(dataDF, aes(x = VAR07)) +
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


########### Subset roles ###################

RoleDF <- subset(dataDF, select = ID:VAR02_6)
RoleDF<-subset(RoleDF, select = -c(VAR01_6))

RoleDF <- RoleDF %>%
  pivot_longer(
    cols = matches("VAR01_|VAR02_"),
    names_to = c(".value", "type"),
    names_pattern = "(VAR\\d{2})_(\\d+)",
    values_drop_na = TRUE
  )
RoleDF<-subset(RoleDF, select = -c(type))

summary(RoleDF)

# number of roles people have
summary(RoleDF$ID)

# chef roles
ggplot(data=subset(RoleDF, !is.na(VAR02)), aes(x = VAR02)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "Manager roles",
    y = "count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# ground staff roles
ggplot(data=subset(RoleDF, !is.na(VAR01)), aes(x = VAR01)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "Ground staff roles",
    y = "count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


########### general functions for plot labels ####################

# Adds numbers to tool labels in the plots
addToolCount <- function(df){
  name_counts <- df %>%    
    count(name)
  
  x_labels <- setNames(
    paste0(name_counts$name, " (n = ", name_counts$n, ")"),
    name_counts$name
  )
  return(x_labels)
}



########### Tech #############

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
  #long_df <- subset(long_df, select= -c(name))
  return(long_df)
}

VAR08_DF <- toLongDF(dataDF, "VAR08")
VAR09_DF <- toLongDF(dataDF, "VAR09")
VAR10_DF <- toLongDF(dataDF, "VAR10")
VAR11_DF <- toLongDF(dataDF, "VAR11")


# VAR08_DF - Vilka tekniska hjälpmedel i denna lista har ni på flygplatsen där du arbetar och som finns tillgänglig i din arbetsroll?
# add airport size
ggplot(VAR08_DF, aes(x = VAR08)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    #x = "VAR08",
    y = "count",
    title = "Vilka tekniska hjälpmedel i denna lista har ni på flygplatsen där du arbetar och som finns tillgänglig i din arbetsroll?"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# VAR09_DF - Vilka tekniska hjälpmedel i denna lista har ni på flygplatsen?   
ggplot(VAR09_DF, aes(x = VAR09)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    #x = "VAR09",
    y = "count",
    title = "Vilka tekniska hjälpmedel i denna lista har ni på flygplatsen?",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# VAR10_DF - välj minst två tekniska hjälpmedel som du anser ha koppling till arbetsmiljön i ditt arbete. Det kan vara teknik som blir svårare att använda under specifika väderförhållanden, teknik som finns tillgänglig men inte används, ett hjälpmedel som har orsakat stora problem i ditt arbete, eller något annat som du tycker är viktigt.
ggplot(VAR10_DF, aes(x = VAR10)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    #x = "VAR10",
    y = "count",
    title = "Välj minst två tekniska hjälpmedel som du anser ha koppling till arbetsmiljön i ditt arbete."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# VAR11_DF - Du kommer att få svara på frågor om hur du upplevde processen med att införa tekniska hjälpmedel, samt vilken roll du hade i införandet. Välj minst två tekniska hjälpmedel som du anser vara mest relevant kopplat till arbetsmiljön i arbetet för markpersonalen.
ggplot(VAR11_DF, aes(x = VAR11)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    #x = "VAR11",
    y = "count",
    title = " Välj minst två tekniska hjälpmedel som du anser vara mest relevant kopplat till arbetsmiljön i arbetet för markpersonalen."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


############## NASA TLX ##################

labels_tech <- c(
  "iPad", "Smartphone", "BRS", "De-icing bil", "Bagagetransport",
  "Räddningsfordon", "Power Stow", "Lastband", "Tankbil",
  "Water/Waste", "Fingerscanner", "Lyfthjälpmedel", "Pushback",
  "Stationär eller bärbar dator", "Digitalisering (i allmänhet)",
  "Gater", "Inget ovanstående"
)

RecodeTech <- function(df, prefix, col = "name") {
  df %>%
    mutate(
      !!col := {
        idx <- as.numeric(sub(paste0(prefix, "_"), "", .data[[col]]))
        factor(labels_tech[idx], levels = labels_tech)
      }
    )
}


# VAR12 - usage
VAR12_DF <- toLongDF(dataDF, "VAR12")
VAR12_DF <- RecodeTech(VAR12_DF, "VAR12")

# tlx VAR13
VAR13_DF <- toLongDF(dataDF, "VAR13")
VAR13_DF <- RecodeTech(VAR13_DF, "VAR13")

#tlx VAR14
summary(dataDF$VAR14_1)
VAR14_DF <- toLongDF(dataDF, "VAR14")
VAR14_DF <- RecodeTech(VAR14_DF, "VAR14")

# tlx VAR15
summary(dataDF$VAR15_1)
VAR15_DF <- toLongDF(dataDF, "VAR15")
VAR15_DF <- RecodeTech(VAR15_DF, "VAR15")

# tlx VAR16 - reversed
summary(dataDF$VAR16_1)
VAR16_DF <- toLongDF(dataDF, "VAR16")
VAR16_DF <- RecodeTech(VAR16_DF, "VAR16")

# tlx VAR17
summary(dataDF$VAR17_1)
VAR17_DF <- toLongDF(dataDF, "VAR17")
VAR17_DF <- RecodeTech(VAR17_DF, "VAR17")

# tlx VAR18
summary(dataDF$VAR18_1)
VAR18_DF <- toLongDF(dataDF, "VAR18")
VAR18_DF <- RecodeTech(VAR18_DF, "VAR18")

# NASA-TLX
MergeDF1 <- merge(VAR18_DF, VAR17_DF, by = c("ID", "name"))
MergeDF2 <- merge(VAR16_DF, VAR15_DF, by = c("ID", "name"))
MergeDF3 <- merge(VAR14_DF, VAR13_DF, by = c("ID", "name"))
MargeDF4 <- merge(MergeDF1, MergeDF2, by = c("ID", "name"))
NASA_TLX_DF <- merge(MargeDF4, MergeDF3, by = c("ID", "name"))

# recode VAR16
NASA_TLX_DF$VAR16_reverse <- 6 - NASA_TLX_DF$VAR16

# total NASA score
NASA_TLX_DF$total <- rowSums(NASA_TLX_DF[, c("VAR16_reverse", "VAR18", "VAR17", "VAR15", "VAR14", "VAR13")])

summary(NASA_TLX_DF)


# VAR13
ggplot(NASA_TLX_DF, aes(x=VAR13)) + 
  facet_wrap(~name, labeller = labeller(name = addToolCount(NASA_TLX_DF))) +
  geom_histogram(binwidth = 0.5, center=NULL) +
  labs(
    x = "Mental belastning",
    y = "Count",
    title = "Hur mentalt ansträngande är det att använda de tekniska hjälpmedlen i ditt arbete?"
  ) 

# VAR14
ggplot(NASA_TLX_DF, aes(x=VAR14)) + 
  facet_wrap(~name, labeller = labeller(name = addToolCount(NASA_TLX_DF))) +
  geom_histogram(binwidth = 0.5, center=NULL) +
  labs(
    x = "Fysisk belastning",
    y = "Count",
    title = "Hur fysiskt ansträngande är det att använda de tekniska hjälpmedlen i ditt arbete?"
  ) 

# VAR15
ggplot(NASA_TLX_DF, aes(x=VAR15)) + 
  facet_wrap(~name, labeller = labeller(name = addToolCount(NASA_TLX_DF))) +
  geom_histogram(binwidth = 0.5, center=NULL) +
  labs(
    x = "Tidsmässig belastning",
    y = "Count",
    title = "tekniska hjälpmedlen i ditt arbete?"
  ) 

# VAR16
ggplot(NASA_TLX_DF, aes(x=VAR16)) + 
  facet_wrap(~name, labeller = labeller(name = addToolCount(NASA_TLX_DF))) +
  geom_histogram(binwidth = 0.5, center=NULL) +
  labs(
    x = "Prestation",
    y = "Count",
    title = "Hur framgångsrikt kan du genomföra dina arbetsuppgifter när du använder de tekniska hjälpmedlen?"
  ) 

# VAR17
ggplot(NASA_TLX_DF, aes(x=VAR17)) + 
  facet_wrap(~name, labeller = labeller(name = addToolCount(NASA_TLX_DF))) +
  geom_histogram(binwidth = 0.5, center=NULL) +
  labs(
    x = "Ansträngning",
    y = "Count",
    title = "Hur ansträngande är det att använda de tekniska hjälpmedlen effektivt?"
  ) 

# VAR18
ggplot(NASA_TLX_DF, aes(x=VAR17)) + 
  facet_wrap(~name, labeller = labeller(name = addToolCount(NASA_TLX_DF))) +
  geom_histogram(binwidth = 0.5, center=NULL) +
  labs(
    x = "Frustration",
    y = "Count",
    title = "Hur osäker, nedslagen, stressad eller irriterad känner du dig vid användning av de tekniska hjälpmedlen?"
  ) 


# plot total nasa tlx
ggplot(NASA_TLX_DF, aes(name, total)) +
  geom_jitter(width = 0.15) +
  scale_x_discrete(labels = addToolCount(NASA_TLX_DF)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(NASA_TLX_DF, aes(name, total)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = addToolCount(NASA_TLX_DF)) +
  geom_jitter(width = 0.2) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



############# Weather ##########################

# Den valda tekniken blir en större belastning att använda under: Starkt solsken. 
VAR19_DF <- toLongDF(dataDF, "VAR19")
VAR19_DF <- RecodeTech(VAR19_DF, "VAR19")
VAR19_DF$VAR19 <- as.factor(VAR19_DF$VAR19)
summary(VAR19_DF)

ggplot(VAR19_DF, aes(x = VAR19)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Starkt solsken"
  ) +
  theme_minimal() 

# Den valda tekniken blir en större belastning att använda under: Snö.
VAR20_DF <- toLongDF(dataDF, "VAR20")
VAR20_DF <- RecodeTech(VAR20_DF, "VAR20")
VAR20_DF$VAR20 <- as.factor(VAR20_DF$VAR20)

ggplot(VAR20_DF, aes(x = VAR20)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Snö."
  ) +
  theme_minimal() 

# Den valda tekniken blir en större belastning att använda under: Kyla
VAR21_DF <- toLongDF(dataDF, "VAR21")
VAR21_DF <- RecodeTech(VAR21_DF, "VAR21")
VAR21_DF$VAR21 <- as.factor(VAR21_DF$VAR21)

ggplot(VAR21_DF, aes(x = VAR21)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Kyla"
  ) +
  theme_minimal() 

# Den valda tekniken blir en större belastning att använda under: Regn
VAR22_DF <- toLongDF(dataDF, "VAR22")
VAR22_DF <- RecodeTech(VAR22_DF, "VAR22")
VAR22_DF$VAR22 <- as.factor(VAR22_DF$VAR22)

ggplot(VAR22_DF, aes(x = VAR22)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Regn"
  ) +
  theme_minimal() 

# Den valda tekniken blir en större belastning att använda under: Mörker
VAR23_DF <- toLongDF(dataDF, "VAR23")
VAR23_DF <- RecodeTech(VAR23_DF, "VAR23")
VAR23_DF$VAR23 <- as.factor(VAR23_DF$VAR23)

ggplot(VAR23_DF, aes(x = VAR23)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Mörker"
  ) +
  theme_minimal() 

# Den valda tekniken blir en större belastning att använda under: Dimma
VAR24_DF <- toLongDF(dataDF, "VAR24")
VAR24_DF <- RecodeTech(VAR24_DF, "VAR24")
VAR24_DF$VAR24 <- as.factor(VAR24_DF$VAR24)

ggplot(VAR24_DF, aes(x = VAR24)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Dimma"
  ) +
  theme_minimal() 


# merge into 1 DF
MergeDF1 <- merge(VAR24_DF, VAR23_DF, by = c("ID", "name"))
MergeDF2 <- merge(MergeDF1, VAR22_DF, by = c("ID", "name"))
MergeDF3 <- merge(MergeDF2, VAR21_DF, by = c("ID", "name"))
MergeDF4 <- merge(MergeDF3, VAR20_DF, by = c("ID", "name"))
MergeDF5 <- merge(MergeDF4, VAR19_DF, by = c("ID", "name"))

WeatherDF <- pivot_longer(MergeDF5, 
                        cols = starts_with("VAR"), 
                        names_to = "Weather", 
                        values_to = "Belastning")

WeatherDF <- WeatherDF |>
  mutate(
    Weather = recode(Weather, `VAR24` = "dimma", `VAR23` = "mörker", `VAR22` = "regn", `VAR21` = "kyla", `VAR20` = "snö", `VAR19` = "solsken", .default = NULL),
  )

ggplot(WeatherDF, aes(x = Belastning, fill=Weather)) +
  facet_wrap(~name) +
  geom_bar(position = 'dodge') +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under"
  ) +
  theme_minimal() 


########### Involvement #####################

# Har du medverkat i arbetet för att utveckla, eller förbättra arbetssättet där den valda tekniken har införts som ett hjälpmedel?
VAR25_DF <- toLongDF(dataDF, "VAR25")
VAR25_DF <- RecodeTech(VAR25_DF, "VAR25")
VAR25_DF$VAR25 <- as.factor(VAR25_DF$VAR25)

# Har du medverkat i arbetet för att utveckla, införa eller förbättra den valda tekniken?
VAR26_DF <- toLongDF(dataDF, "VAR26")
VAR26_DF <- RecodeTech(VAR26_DF, "VAR26")
VAR26_DF$VAR26 <- as.factor(VAR26_DF$VAR26)

# Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av arbetssättet? 
VAR27_DF <- toLongDF(dataDF, "VAR27")
VAR27_DF <- RecodeTech(VAR27_DF, "VAR27")
VAR27_DF$VAR27 <- as.factor(VAR27_DF$VAR27)

# Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av den valda tekniken?
VAR28_DF <- toLongDF(dataDF, "VAR28")
VAR28_DF <- RecodeTech(VAR28_DF, "VAR28")
VAR28_DF$VAR28 <- as.factor(VAR28_DF$VAR28)

# Upplever du att dina synpunkter har påverkat utvecklingen av arbetssättet?
VAR29_DF <- toLongDF(dataDF, "VAR29")
VAR29_DF <- RecodeTech(VAR29_DF, "VAR29")
VAR29_DF$VAR29 <- as.factor(VAR29_DF$VAR29)

# Upplever du att dina synpunkter har påverkat utvecklingen av den valda tekniken?
VAR30_DF <- toLongDF(dataDF, "VAR30")
VAR30_DF <- RecodeTech(VAR30_DF, "VAR30")
VAR30_DF$VAR30 <- as.factor(VAR30_DF$VAR30)

# Har arbetsmiljö varit ett fokus när den valda tekniken har införts?
VAR31_DF <- toLongDF(dataDF, "VAR31")
VAR31_DF <- RecodeTech(VAR31_DF, "VAR31")
VAR31_DF$VAR31 <- as.factor(VAR31_DF$VAR31)

ggplot(VAR31_DF, aes(x = VAR31)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har arbetsmiljö varit ett fokus när den valda tekniken har införts?"
  ) +
  theme_minimal() 

# Genomfördes en riskanalys innan den valda tekniken infördes? 
VAR32_DF <- toLongDF(dataDF, "VAR32")
VAR32_DF <- RecodeTech(VAR32_DF, "VAR32")
VAR32_DF$VAR32 <- as.factor(VAR32_DF$VAR32)

ggplot(VAR32_DF, aes(x = VAR32)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Genomfördes en riskanalys innan den valda tekniken infördes? "
  ) +
  theme_minimal() 

# Har någon av dina kollegor medverkat i något utvecklingsarbete? 
VAR33_DF <- toLongDF(dataDF, "VAR33")
VAR33_DF <- RecodeTech(VAR33_DF, "VAR33")
#VAR33_DF$VAR33 <- as.factor(VAR33_DF$VAR33)

# Vet du hur du ska gå till väga om du har synpunkter eller önskemål om förbättringar som gäller arbetssättet eller den valda tekniken? 
VAR34_DF <- toLongDF(dataDF, "VAR34")
VAR34_DF <- RecodeTech(VAR34_DF, "VAR34")
VAR34_DF$VAR34 <- as.factor(VAR34_DF$VAR34)

ggplot(VAR34_DF, aes(x = VAR34)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Vet du hur du ska gå till väga om du har synpunkter eller önskemål om förbättringar som gäller arbetssättet eller den valda tekniken? "
  ) +
  theme_minimal() 

# Har du upplevt att det har varit tydligt varför den valda tekniken har införts?
VAR35_DF <- toLongDF(dataDF, "VAR35")
VAR35_DF <- RecodeTech(VAR35_DF, "VAR35")
VAR35_DF$VAR35 <- as.factor(VAR35_DF$VAR35)

ggplot(VAR35_DF, aes(x = VAR35)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du upplevt att det har varit tydligt varför den valda tekniken har införts?"
  ) +
  theme_minimal() 

# Har du medverkat i arbetet för att utveckla, eller förbättra arbetssättet där den valda tekniken har införts som ett hjälpmedel?
VAR36_DF <- toLongDF(dataDF, "VAR36")
VAR36_DF <- RecodeTech(VAR36_DF, "VAR36")
VAR36_DF$VAR36 <- as.factor(VAR36_DF$VAR36)

# Har du medverkat i arbetet för att utveckla, införa eller förbättra den valda tekniken? 
VAR37_DF <- toLongDF(dataDF, "VAR37")
VAR37_DF <- RecodeTech(VAR37_DF, "VAR37")
VAR37_DF$VAR37 <- as.factor(VAR37_DF$VAR37)

# Har du gett stöd, utbildning, information, tid, osv. till den personal som ska använda den valda tekniken för att medverka i utvecklingsarbetet av arbetssättet? 
VAR38_DF <- toLongDF(dataDF, "VAR38")
VAR38_DF <- RecodeTech(VAR38_DF, "VAR38")
VAR38_DF$VAR38 <- as.factor(VAR38_DF$VAR38)

# Har gett stöd, utbildning, information, tid, osv. till den berörda personalen för att medverka i utvecklingsarbetet av den valda tekniken? 
VAR39_DF <- toLongDF(dataDF, "VAR39")
VAR39_DF <- RecodeTech(VAR39_DF, "VAR39")
VAR39_DF$VAR39 <- as.factor(VAR39_DF$VAR39)

# Upplever du att dina synpunkter har påverkat utvecklingen av arbetssättet?
VAR40_DF <- toLongDF(dataDF, "VAR40")
VAR40_DF <- RecodeTech(VAR40_DF, "VAR40")
VAR40_DF$VAR40 <- as.factor(VAR40_DF$VAR40)

# Upplever du att dina synpunkter har påverkat utvecklingen av den valda tekniken? 
VAR41_DF <- toLongDF(dataDF, "VAR41")
VAR41_DF <- RecodeTech(VAR41_DF, "VAR41")
VAR41_DF$VAR41 <- as.factor(VAR41_DF$VAR41)

# Har arbetsmiljö varit ett fokus när den valda tekniken har införts? 
VAR42_DF <- toLongDF(dataDF, "VAR42")
VAR42_DF <- RecodeTech(VAR42_DF, "VAR42")
VAR42_DF$VAR42 <- as.factor(VAR42_DF$VAR42)

ggplot(VAR42_DF, aes(x = VAR42)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har arbetsmiljö varit ett fokus när den valda tekniken har införts? "
  ) +
  theme_minimal() 

# Genomfördes en riskanalys innan den valda tekniken infördes? 
VAR43_DF <- toLongDF(dataDF, "VAR43")
VAR43_DF <- RecodeTech(VAR43_DF, "VAR43")
VAR43_DF$VAR43 <- as.factor(VAR43_DF$VAR43)

ggplot(VAR43_DF, aes(x = VAR43)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Genomfördes en riskanalys innan den valda tekniken infördes?"
  ) +
  theme_minimal() 

# # Har någon av dina kollegor medverkat i något utvecklingsarbete? 
VAR44_DF <- toLongDF(dataDF, "VAR44")
VAR44_DF <- RecodeTech(VAR44_DF, "VAR44")
VAR44_DF$VAR44 <- as.factor(VAR44_DF$VAR44)

# Har berörd personal fått möjlighet att framföra synpunkter eller önskemål om förbättringar som gäller arbetssättet eller den valda tekniken?
VAR45_DF <- toLongDF(dataDF, "VAR45")
VAR45_DF <- RecodeTech(VAR45_DF, "VAR45")
VAR45_DF$VAR45 <- as.factor(VAR45_DF$VAR45)

# Har du upplevt att det har varit tydligt varför den valda tekniken har införts?
VAR46_DF <- toLongDF(dataDF, "VAR46")
VAR46_DF <- RecodeTech(VAR46_DF, "VAR46")
VAR46_DF$VAR46 <- as.factor(VAR46_DF$VAR46)

ggplot(VAR46_DF, aes(x = VAR46)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du upplevt att det har varit tydligt varför den valda tekniken har införts? "
  ) +
  theme_minimal() 

# Look at involvement differences between GS and managers
# differences between airport size and role?


########## Tools available vs Most used tools ###########

# tools available
# VAR08 - Vilka tekniska hjälpmedel i denna lista har ni på flygplatsen där du arbetar och som finns tillgänglig i din arbetsroll?
# VAR09 - Vilka tekniska hjälpmedel i denna lista har ni på flygplatsen?
VAR08_DF
VAR09_DF

ggplot(VAR08_DF, aes(x = VAR08)) +
  geom_bar(fill = "#0072B2") +
  coord_flip() +
  labs(
    y = "count",
    title = "Vilka tekniska hjälpmedel har ni på flygplatsen och finns tillgänglig i din arbetsroll?"
  ) +
  theme_minimal()

# table
table(VAR08_DF$VAR08)

# used tools
# VAR12 - Hur ofta använder du de tekniska hjälpmedlen som du har valt?
VAR12_DF

VAR12_DF$VAR12 <- recode_factor(
  VAR12_DF$VAR12,
  `1` = "Vid varje möjlighet",
  `2` = "Dagligen",
  `3` = "Varje vecka",
  `4` = "Varje månad",
  `5` = "Mindre än varje månad",
  `6` = "Aldrig"
)


ggplot(VAR12_DF, aes(x = name, fill = VAR12)) +
  geom_bar() +
  scale_x_discrete(labels = addToolCount(VAR12_DF)) +
  labs(
    y = "Count",
    fill = "Frequency of use",
    title = "Hur ofta använder du de tekniska hjälpmedlen?",
    x = "Technology"
  ) +
  theme_minimal() +
  coord_flip()

# table
table(VAR12_DF$name, VAR12_DF$VAR12)



############## Participation and involvement (manager/ground staff; airport size; roles) #############

# get vars managers filled in
GS_SS <- subset(dataDF, VAR00=="Chef")
GS_SS <- GS_SS %>%  discard(~all(is.na(.x)))
names(GS_SS)

# ground staff
# VAR25 - Har du medverkat i arbetet för att utveckla, eller förbättra arbetssättet där den valda tekniken har införts som ett hjälpmedel?
summary(VAR25_DF)

ggplot(VAR25_DF, aes(x = VAR25)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du medverkat i arbetet för att utveckla, eller förbättra arbetssättet där den valda tekniken har införts som ett hjälpmedel?"
  ) +
  theme_minimal() 

# VAR26 - Har du medverkat i arbetet för att utveckla, införa eller förbättra den valda tekniken?
summary(VAR26_DF)

ggplot(VAR26_DF, aes(x = VAR26)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du medverkat i arbetet för att utveckla, införa eller förbättra den valda tekniken?"
  ) +
  theme_minimal() 

# VAR27 - Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av arbetssättet?
summary(VAR27_DF)

ggplot(VAR27_DF, aes(x = VAR27)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av arbetssättet? "
  ) +
  theme_minimal() 

# VAR28 - Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av den valda tekniken?
summary(VAR28_DF)

ggplot(VAR28_DF, aes(x = VAR28)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av den valda tekniken?"
  ) +
  theme_minimal() 

# VAR29 - Upplever du att dina synpunkter har påverkat utvecklingen av arbetssättet? 
summary(VAR29_DF)

ggplot(VAR29_DF, aes(x = VAR29)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Upplever du att dina synpunkter har påverkat utvecklingen av arbetssättet?"
  ) +
  theme_minimal() 

# VAR30 - Upplever du att dina synpunkter har påverkat utvecklingen av den valda tekniken?
summary(VAR30_DF)

ggplot(VAR30_DF, aes(x = VAR30)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "# Upplever du att dina synpunkter har påverkat utvecklingen av den valda tekniken?"
  ) +
  theme_minimal() 

# VAR33 - Har någon av dina kollegor medverkat i något utvecklingsarbete? 
summary(VAR33_DF)

ggplot(VAR33_DF, aes(x = VAR33)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har någon av dina kollegor medverkat i något utvecklingsarbete?  "
  ) +
  theme_minimal() 


involveDF1 <- merge(VAR25_DF, VAR26_DF, all = TRUE, by = c("ID", "name"))
involveDF2 <- merge(VAR27_DF, VAR28_DF, all = TRUE, by = c("ID", "name"))
involveDF3 <- merge(VAR29_DF, VAR30_DF, all = TRUE, by = c("ID", "name"))
involveDF4 <- merge(involveDF1, involveDF2, all = TRUE, by = c("ID", "name"))
involveDF5 <- merge(involveDF3, involveDF4, all = TRUE, by = c("ID", "name"))
involveDF_gs <- merge(involveDF5, VAR33_DF, all = TRUE, by = c("ID", "name"))

summary(involveDF_gs)

# managers
# VAR36 - Har du medverkat i arbetet för att utveckla, eller förbättra arbetssättet där den valda tekniken har införts som ett hjälpmedel?
ggplot(VAR36_DF, aes(x = VAR36)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du medverkat i arbetet för att utveckla, eller förbättra arbetssättet där den valda tekniken har införts som ett hjälpmedel?"
  ) +
  theme_minimal() 

# VAR37 - Har du medverkat i arbetet för att utveckla, införa eller förbättra den valda tekniken?
ggplot(VAR37_DF, aes(x = VAR37)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du medverkat i arbetet för att utveckla, införa eller förbättra den valda tekniken? "
  ) +
  theme_minimal() 

# VAR38 - Har du gett stöd, utbildning, information, tid, osv. till den personal som ska använda den valda tekniken för att medverka i utvecklingsarbetet av arbetssättet? 
ggplot(VAR38_DF, aes(x = VAR38)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du gett stöd, utbildning, information, tid, osv. till den personal som ska använda den valda tekniken för att medverka i utvecklingsarbetet av arbetssättet? "
  ) +
  theme_minimal() 

# VAR39 - Har gett stöd, utbildning, information, tid, osv. till den berörda personalen för att medverka i utvecklingsarbetet av den valda tekniken?
ggplot(VAR39_DF, aes(x = VAR39)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har gett stöd, utbildning, information, tid, osv. till den berörda personalen för att medverka i utvecklingsarbetet av den valda tekniken? "
  ) +
  theme_minimal() 

# VAR40 - Upplever du att dina synpunkter har påverkat utvecklingen av arbetssättet?
ggplot(VAR40_DF, aes(x = VAR40)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Upplever du att dina synpunkter har påverkat utvecklingen av arbetssättet? "
  ) +
  theme_minimal() 

# VAR41 - Upplever du att dina synpunkter har påverkat utvecklingen av den valda tekniken?
ggplot(VAR41_DF, aes(x = VAR41)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Upplever du att dina synpunkter har påverkat utvecklingen av den valda tekniken?  "
  ) +
  theme_minimal() 

# VAR44 - Har någon av dina kollegor medverkat i något utvecklingsarbete?
ggplot(VAR44_DF, aes(x = VAR44)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har någon av dina kollegor medverkat i något utvecklingsarbete? "
  ) +
  theme_minimal() 

# VAR45 - Har berörd personal fått möjlighet att framföra synpunkter eller önskemål om förbättringar som gäller arbetssättet eller den valda tekniken?
ggplot(VAR45_DF, aes(x = VAR45)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har berörd personal fått möjlighet att framföra synpunkter eller önskemål om förbättringar som gäller arbetssättet eller den valda tekniken? "
  ) +
  theme_minimal() 

involveDF1 <- merge(VAR36_DF, VAR37_DF, all = TRUE, by = c("ID", "name"))
involveDF2 <- merge(VAR38_DF, VAR39_DF, all = TRUE, by = c("ID", "name"))
involveDF3 <- merge(VAR40_DF, VAR41_DF, all = TRUE, by = c("ID", "name"))
involveDF4 <- merge(VAR44_DF, VAR45_DF, all = TRUE, by = c("ID", "name"))
involveDF5 <- merge(involveDF1, involveDF2, all = TRUE, by = c("ID", "name"))
involveDF6 <- merge(involveDF3, involveDF4, all = TRUE, by = c("ID", "name"))
involveDF_chef <- merge(involveDF5, involveDF6, all = TRUE, by = c("ID", "name"))


############## Involvement - NASA-TLX relationship #############

summary(involveDF_gs)
summary(NASA_TLX_DF)


vars <- c("VAR29", "VAR30", "VAR25", "VAR26", "VAR33", "VAR27", "VAR28")

involveDF_gs$sum_score    <- rowSums(data.frame(lapply(involveDF_gs[, vars], function(x) as.numeric(x))), na.rm = TRUE)
involveDF_gs$mean_score   <- rowMeans(data.frame(lapply(involveDF_gs[, vars], function(x) as.numeric(x))), na.rm = TRUE)
involveDF_gs$median_score <- apply(data.frame(lapply(involveDF_gs[, vars], function(x) as.numeric(as.character(x)))), 1, median, na.rm = TRUE)


# plot total nasa tlx
ggplot(NASA_TLX_DF, aes(name, total)) +
  geom_jitter(width = 0.15) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(NASA_TLX_DF, aes(name, total)) +
  geom_boxplot(outlier.shape = NA) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# involvement
ggplot(involveDF_gs, aes(x = median_score)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "involvement median"
  ) +
  theme_minimal() 

ggplot(involveDF_gs, aes(x = mean_score)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "involvement mean"
  ) +
  theme_minimal() 

# involvement x nasa tlx
nasa_involvementDF <- merge(involveDF_gs, NASA_TLX_DF, by = c("ID", "name"))


ggplot(nasa_involvementDF, aes(mean_score, total)) +
  geom_point() +
  labs(
    y = "NASA TLX Rating",
    x = "Mean involvement"
  ) 
  


############## Usage - NASA relationship (with respect to weather) #############

# usage frequency
summary(VAR12_DF)

ggplot(VAR12_DF, aes(x = VAR12)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR12_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Hur ofta använder du tekniska hjälpmedelet?"
  ) +
  theme_minimal() 

# plot total nasa tlx
ggplot(NASA_TLX_DF, aes(name, total)) +
  geom_jitter(width = 0.15) +
  theme_minimal() +
  scale_x_discrete(labels = addToolCount(NASA_TLX_DF)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Tool"
  )

ggplot(NASA_TLX_DF, aes(name, total)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = addToolCount(NASA_TLX_DF)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# correlation nasa tlx - usage
nasa_usageDF <- merge(NASA_TLX_DF, VAR12_DF, by = c("ID", "name"), all = TRUE)

ggplot(nasa_usageDF, aes(total, VAR12)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(nasa_usageDF))) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    y = "Hur ofta använder du tekniska hjälpmedelet?",
    x = "NASA TLX"
  )
  #geom_point()


# weather
# sun
ggplot(VAR19_DF, aes(x = VAR19)) +
  facet_wrap(~name, , labeller = labeller(name = addToolCount(VAR19_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Starkt solsken"
  ) +
  theme_minimal() 

# sun - usage
weather1_usageDF <- merge(VAR19_DF, VAR12_DF, by = c("ID", "name"), all = TRUE)

ggplot(weather1_usageDF, aes(x=VAR19, fill=VAR12)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather1_usageDF))) +
  geom_bar(na.rm = TRUE) +
  labs(
    fill = "Use frequency",
    x = "Difficulty in use",
    title = "Den valda tekniken blir en större belastning att använda under: Starkt solsken "
  )


# snow
ggplot(VAR20_DF, aes(x = VAR20)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR20_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Snö."
  ) +
  theme_minimal() 

# snow - usage
weather2_usageDF <- merge(weather1_usageDF, VAR20_DF, by = c("ID", "name"), all = TRUE)

ggplot(weather2_usageDF, aes(x=VAR20, fill=VAR12)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather2_usageDF))) +
  geom_bar() +
  labs(
    fill = "Use frequency",
    x = "Difficulty in use",
    title = "Den valda tekniken blir en större belastning att använda under: Snö "
  )


# cold
ggplot(VAR21_DF, aes(x = VAR21)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR21_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Kyla"
  ) +
  theme_minimal() 

# cold - usage
weather3_usageDF <- merge(weather2_usageDF, VAR21_DF, by = c("ID", "name"), all = TRUE)

ggplot(weather3_usageDF, aes(x=VAR21, fill=VAR12)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather3_usageDF))) +
  geom_bar() +
  labs(
    fill = "Use frequency",
    x = "Difficulty in use",
    title = "Den valda tekniken blir en större belastning att använda under: Kyla "
  )


# rain
ggplot(VAR22_DF, aes(x = VAR22)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR22_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Regn"
  ) +
  theme_minimal()

# rain - usage
weather4_usageDF <- merge(weather3_usageDF, VAR22_DF, by = c("ID", "name"), all = TRUE)

ggplot(weather4_usageDF, aes(x=VAR22, fill=VAR12)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather4_usageDF))) +
  geom_bar() +
  labs(
    fill = "Use frequency",
    x = "Difficulty in use",
    title = "Den valda tekniken blir en större belastning att använda under: Regn "
  )


# Darkness
ggplot(VAR23_DF, aes(x = VAR23)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR23_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Mörker"
  ) +
  theme_minimal() 

# darkness - usage
weather5_usageDF <- merge(weather4_usageDF, VAR23_DF, by = c("ID", "name"), all = TRUE)

ggplot(weather5_usageDF, aes(x=VAR23, fill=VAR12)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather5_usageDF))) +
  geom_bar() +
  labs(
    fill = "Use frequency",
    x = "Difficulty in use",
    title = "Den valda tekniken blir en större belastning att använda under: Mörker "
  )


# Dimma
ggplot(VAR24_DF, aes(x = VAR24)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR24_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Dimma"
  ) +
  theme_minimal() 

# dimma - usage
weather6_usageDF <- merge(weather5_usageDF, VAR24_DF, by = c("ID", "name"), all = TRUE)

ggplot(weather6_usageDF, aes(x=VAR24, fill=VAR12)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather6_usageDF))) +
  geom_bar() +
  labs(
    fill = "Use frequency",
    x = "Difficulty in use",
    title = "Den valda tekniken blir en större belastning att använda under: Dimma "
)

# weather - difficulty










