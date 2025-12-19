
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
    plot.title = element_text(hjust = 0.5)
  )




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


# tlx VAR12
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
ggplot(NASA_TLX_DF, aes(x=VAR13, fill=name)) + 
  geom_histogram(position = 'dodge', binwidth = 0.5, center=TRUE) +
  labs(
    x = "Mental belastning",
    y = "Count",
    title = "Hur mentalt ansträngande är det att använda de tekniska hjälpmedlen i ditt arbete?"
  ) 

# VAR14
ggplot(NASA_TLX_DF, aes(x=VAR14)) + 
  facet_wrap(~name) +
  geom_histogram(binwidth = 0.5, center=TRUE) +
  labs(
    x = "Fysisk belastning",
    y = "Count",
    title = "Hur fysiskt ansträngande är det att använda de tekniska hjälpmedlen i ditt arbete?"
  ) 

# VAR15
ggplot(NASA_TLX_DF, aes(x=VAR15)) + 
  geom_histogram() +
  labs(
    x = "Tidsmässig belastning",
    y = "Count",
    title = "Hur fysiskt ansträngande är det att använda de tekniska hjälpmedlen i ditt arbete?"
  ) 

# VAR16
ggplot(NASA_TLX_DF, aes(x=VAR16)) + 
  geom_histogram() +
  labs(
    x = "Prestation",
    y = "Count",
    title = "Hur framgångsrikt kan du genomföra dina arbetsuppgifter när du använder de tekniska hjälpmedlen?"
  ) 

# VAR17
ggplot(NASA_TLX_DF, aes(x=VAR17)) + 
  geom_histogram() +
  labs(
    x = "Ansträngning",
    y = "Count",
    title = "Hur ansträngande är det att använda de tekniska hjälpmedlen effektivt?"
  ) 

# VAR18
ggplot(NASA_TLX_DF, aes(x=VAR18)) + 
  geom_histogram() +
  labs(
    x = "Frustration",
    y = "Count",
    title = "Hur osäker, nedslagen, stressad eller irriterad känner du dig vid användning av de tekniska hjälpmedlen?"
  ) 


# plot total nasa tlx
ggplot(NASA_TLX_DF, aes(name, total)) +
  geom_jitter(width = 0.15) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(NASA_TLX_DF, aes(name, total)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(NASA_TLX_DF, aes(name, total)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
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

ggplot(VAR25_DF, aes(x = VAR25)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du medverkat i arbetet för att utveckla, eller förbättra arbetssättet där den valda tekniken har införts som ett hjälpmedel?"
  ) +
  theme_minimal() 

# Har du medverkat i arbetet för att utveckla, införa eller förbättra den valda tekniken?
VAR26_DF <- toLongDF(dataDF, "VAR26")
VAR26_DF <- RecodeTech(VAR26_DF, "VAR26")
VAR26_DF$VAR26 <- as.factor(VAR26_DF$VAR26)

ggplot(VAR26_DF, aes(x = VAR26)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du medverkat i arbetet för att utveckla, införa eller förbättra den valda tekniken?"
  ) +
  theme_minimal() 

# Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av arbetssättet? 
VAR27_DF <- toLongDF(dataDF, "VAR27")
VAR27_DF <- RecodeTech(VAR27_DF, "VAR27")
VAR27_DF$VAR27 <- as.factor(VAR27_DF$VAR27)

ggplot(VAR27_DF, aes(x = VAR27)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av arbetssättet? "
  ) +
  theme_minimal() 

# Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av den valda tekniken?
VAR28_DF <- toLongDF(dataDF, "VAR28")
VAR28_DF <- RecodeTech(VAR28_DF, "VAR28")
VAR28_DF$VAR28 <- as.factor(VAR28_DF$VAR28)

ggplot(VAR28_DF, aes(x = VAR28)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av den valda tekniken?"
  ) +
  theme_minimal() 

# Upplever du att dina synpunkter har påverkat utvecklingen av arbetssättet?
VAR29_DF <- toLongDF(dataDF, "VAR29")
VAR29_DF <- RecodeTech(VAR29_DF, "VAR29")
VAR29_DF$VAR29 <- as.factor(VAR29_DF$VAR29)

ggplot(VAR29_DF, aes(x = VAR29)) +
  facet_wrap(~name) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Upplever du att dina synpunkter har påverkat utvecklingen av arbetssättet?"
  ) +
  theme_minimal() 


