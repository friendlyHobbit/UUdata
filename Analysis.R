# install non standard packages. Uncomment if these are not yet installed on your pc
#install.packages("psych")
#install.packages("ltm")

library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(viridis)
library(ggplot2)
library(psych)
library(ltm)



# read data - add data to folder called "data"
dataDir <- getwd()
dataDF <- read.csv(file.path(dataDir, "data\\Long_SurveyData.csv"), sep = ",")

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
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Vilken roll har du på din arbetsplats?",
    x = NULL,
    y = "Count",
    fill = "Kön"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# age
summary(dataDF$VAR03)

ggplot(dataDF, aes(x = VAR03, fill = VAR03)) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    title = "Hur gammal är du?",
    x = NULL,
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
    x = "Kön",
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
    x = NULL,
    title = "Storlek på flygplatsen där du arbetar",
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
    x = NULL,
    title = "Erfarenhet på den flygplats du för nuvarande arbetar på",
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
    x = NULL,
    title = "Erfarenhet av arbete på flygplats i allmänhet",
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
    x = NULL,
    title = "Vilken roll som chef har du på din arbetsplats?",
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
    x = NULL,
    title = "Vilken roll som markpersonal har du på din arbetsplats?",
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


# turn to long df
toLongDF <- function(df, var){
  sub_df <- df %>% dplyr::select(dplyr::matches(var), ID, VAR05, VAR00)
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

toLongDF <- function(df, var){
  sub_df <- df %>%
    dplyr::select(dplyr::matches(var), ID, VAR05, VAR00)
  long_df <- sub_df %>%
    pivot_longer(
      cols = matches(var),
      names_to = "name",
      values_to = var,
      values_drop_na = TRUE
    ) %>%
    distinct(ID, name, .keep_all = TRUE)
  return(long_df)
}

########### Tech #############

VAR08_DF <- toLongDF(dataDF, "VAR08")
VAR09_DF <- toLongDF(dataDF, "VAR09")
VAR10_DF <- toLongDF(dataDF, "VAR10")
VAR11_DF <- toLongDF(dataDF, "VAR11")

# VAR08_DF
ggplot(VAR08_DF, aes(x = VAR08, fill = VAR05)) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.2
  ) +
  labs(
    fill = "Storlek flygplatsen",
    x = NULL,
    y = "Count",
    title = "Vilka tekniska hjälpmedel i denna lista har ni på flygplatsen där du arbetar och som finns tillgänglig i din arbetsroll?"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# VAR09_DF  
ggplot(VAR09_DF, aes(x = VAR09, fill = VAR05)) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.2
  ) +
  labs(
    fill = "Storlek flygplatsen",
    y = "Count",
    title = "Vilka tekniska hjälpmedel i denna lista har ni på flygplatsen?",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# VAR10_DF - välj minst två tekniska hjälpmedel som du anser ha koppling till arbetsmiljön i ditt arbete. Det kan vara teknik som blir svårare att använda under specifika väderförhållanden, teknik som finns tillgänglig men inte används, ett hjälpmedel som har orsakat stora problem i ditt arbete, eller något annat som du tycker är viktigt.
ggplot(VAR10_DF, aes(x = VAR10, fill = VAR05)) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.2
  ) +
  labs(
    fill = "Storlek flygplatsen",
    y = "Count",
    title = "Välj minst två tekniska hjälpmedel som du anser ha koppling till arbetsmiljön i ditt arbete.",
    x = NULL
    
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# VAR11_DF - Du kommer att få svara på frågor om hur du upplevde processen med att införa tekniska hjälpmedel, samt vilken roll du hade i införandet. Välj minst två tekniska hjälpmedel som du anser vara mest relevant kopplat till arbetsmiljön i arbetet för markpersonalen.
ggplot(VAR11_DF, aes(x = VAR11, fill = VAR05)) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.2
  ) +
  labs(
    fill = "Storlek flygplatsen",
    y = "Count",
    x = NULL,
    title = "Du kommer att få svara på frågor om hur du upplevde processen med att införa tekniska hjälpmedel, samt vilken roll du hade i införandet. Välj minst två tekniska hjälpmedel som du anser vara mest relevant kopplat till arbetsmiljön i arbetet för markpersonalen."
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
NASA_TLX_DF <- Reduce(function(x, y) merge(x, y, all=TRUE), list(VAR18_DF, VAR17_DF, VAR16_DF, VAR15_DF, VAR14_DF,VAR13_DF))

# recode VAR16
NASA_TLX_DF$VAR16_reverse <- 6 - NASA_TLX_DF$VAR16

# total NASA score
NASA_TLX_DF$nasa_total <- rowSums(NASA_TLX_DF[, c("VAR16_reverse", "VAR18", "VAR17", "VAR15", "VAR14", "VAR13")])
NASA_TLX_DF$nasa_mean <- rowMeans(NASA_TLX_DF[, c("VAR16_reverse", "VAR18", "VAR17", "VAR15", "VAR14", "VAR13")]) 

summary(NASA_TLX_DF)


# VAR13
ggplot(NASA_TLX_DF, aes(x=VAR13)) + 
  facet_wrap(~name, labeller = labeller(name = addToolCount(NASA_TLX_DF))) +
  geom_histogram(binwidth = 0.5,  position = "stack") +
  labs(
    x = "Mental belastning",
    y = "Count",
    title = "Hur mentalt ansträngande är det att använda de tekniska hjälpmedlen i ditt arbete?"
  ) 

# VAR14
ggplot(NASA_TLX_DF, aes(x=VAR14)) + 
  facet_wrap(~name, labeller = labeller(name = addToolCount(NASA_TLX_DF))) +
  geom_histogram(binwidth = 0.5, position = "stack") +
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
ggplot(NASA_TLX_DF, aes(name, nasa_mean)) +
  geom_jitter(width = 0.15) +
  scale_x_discrete(labels = addToolCount(NASA_TLX_DF)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = NULL,
    y = "NASA-TLX mean"
  )

ggplot(NASA_TLX_DF, aes(name, nasa_mean)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = addToolCount(NASA_TLX_DF)) +
  geom_jitter(width = 0.2) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = NULL,
    y = "NASA-TLX mean"
  )


########### Involvement #####################

# Har du medverkat i arbetet för att utveckla, eller förbättra arbetssättet där den valda tekniken har införts som ett hjälpmedel?
VAR25_DF <- toLongDF(dataDF, "VAR25")
VAR25_DF <- RecodeTech(VAR25_DF, "VAR25")
VAR25_DF <- rename(VAR25_DF, "VAR25_36" = "VAR25")
# Har du medverkat i arbetet för att utveckla, eller förbättra arbetssättet där den valda tekniken har införts som ett hjälpmedel?
VAR36_DF <- toLongDF(dataDF, "VAR36")
VAR36_DF <- RecodeTech(VAR36_DF, "VAR36")
VAR36_DF <- rename(VAR36_DF, "VAR25_36" = "VAR36")

VAR25_36_DF <- rbind(VAR36_DF, VAR25_DF)

# Har du medverkat i arbetet för att utveckla, införa eller förbättra den valda tekniken?
VAR26_DF <- toLongDF(dataDF, "VAR26")
VAR26_DF <- RecodeTech(VAR26_DF, "VAR26")
VAR26_DF <- rename(VAR26_DF, "VAR26_37" = "VAR26")
# Har du medverkat i arbetet för att utveckla, införa eller förbättra den valda tekniken? 
VAR37_DF <- toLongDF(dataDF, "VAR37")
VAR37_DF <- RecodeTech(VAR37_DF, "VAR37")
VAR37_DF <- rename(VAR37_DF, "VAR26_37" = "VAR37")

VAR26_37_DF <- rbind(VAR37_DF, VAR26_DF)

# Upplever du att dina synpunkter har påverkat utvecklingen av arbetssättet?
VAR29_DF <- toLongDF(dataDF, "VAR29")
VAR29_DF <- RecodeTech(VAR29_DF, "VAR29")
VAR29_DF <- rename(VAR29_DF, "VAR29_40" = "VAR29")
# Upplever du att dina synpunkter har påverkat utvecklingen av arbetssättet?
VAR40_DF <- toLongDF(dataDF, "VAR40")
VAR40_DF <- RecodeTech(VAR40_DF, "VAR40")
VAR40_DF <- rename(VAR40_DF, "VAR29_40" = "VAR40")

VAR29_40_DF <- rbind(VAR40_DF, VAR29_DF)

# Upplever du att dina synpunkter har påverkat utvecklingen av den valda tekniken? 
VAR41_DF <- toLongDF(dataDF, "VAR41")
VAR41_DF <- RecodeTech(VAR41_DF, "VAR41")
VAR41_DF <- rename(VAR41_DF, "VAR30_41" = "VAR41")
# Upplever du att dina synpunkter har påverkat utvecklingen av den valda tekniken?
VAR30_DF <- toLongDF(dataDF, "VAR30")
VAR30_DF <- RecodeTech(VAR30_DF, "VAR30")
VAR30_DF <- rename(VAR30_DF, "VAR30_41" = "VAR30")

VAR30_41_DF <- rbind(VAR41_DF, VAR30_DF)

# Har arbetsmiljö varit ett fokus när den valda tekniken har införts?
VAR31_DF <- toLongDF(dataDF, "VAR31")
VAR31_DF <- RecodeTech(VAR31_DF, "VAR31")
VAR31_DF <- rename(VAR31_DF, "VAR31_42" = "VAR31")
# Har arbetsmiljö varit ett fokus när den valda tekniken har införts? 
VAR42_DF <- toLongDF(dataDF, "VAR42")
VAR42_DF <- RecodeTech(VAR42_DF, "VAR42")
VAR42_DF <- rename(VAR42_DF, "VAR31_42" = "VAR42")

VAR31_42_DF <- rbind(VAR42_DF, VAR31_DF)

# Genomfördes en riskanalys innan den valda tekniken infördes? 
VAR32_DF <- toLongDF(dataDF, "VAR32")
VAR32_DF <- RecodeTech(VAR32_DF, "VAR32")
VAR32_DF <- rename(VAR32_DF, "VAR43_32" = "VAR32")
# Genomfördes en riskanalys innan den valda tekniken infördes? 
VAR43_DF <- toLongDF(dataDF, "VAR43")
VAR43_DF <- RecodeTech(VAR43_DF, "VAR43")
VAR43_DF <- rename(VAR43_DF, "VAR43_32" = "VAR43")

VAR32_43_DF <- rbind(VAR43_DF, VAR32_DF)

# Har någon av dina kollegor medverkat i något utvecklingsarbete? 
VAR33_DF <- toLongDF(dataDF, "VAR33")
VAR33_DF <- RecodeTech(VAR33_DF, "VAR33")
VAR33_DF <- rename(VAR33_DF, "VAR44_33" = "VAR33")
# Har någon av dina kollegor medverkat i något utvecklingsarbete? 
VAR44_DF <- toLongDF(dataDF, "VAR44")
VAR44_DF <- RecodeTech(VAR44_DF, "VAR44")
VAR44_DF <- rename(VAR44_DF, "VAR44_33" = "VAR44")

VAR33_44_DF <- rbind(VAR44_DF, VAR33_DF)

# Har du upplevt att det har varit tydligt varför den valda tekniken har införts?
VAR35_DF <- toLongDF(dataDF, "VAR35")
VAR35_DF <- RecodeTech(VAR35_DF, "VAR35")
VAR35_DF <- rename(VAR35_DF, "VAR46_35" = "VAR35")
# Har du upplevt att det har varit tydligt varför den valda tekniken har införts?
VAR46_DF <- toLongDF(dataDF, "VAR46")
VAR46_DF <- RecodeTech(VAR46_DF, "VAR46")
VAR46_DF <- rename(VAR46_DF, "VAR46_35" = "VAR46")

VAR35_46_DF <- rbind(VAR46_DF, VAR35_DF)

# Vet du hur du ska gå till väga om du har synpunkter eller önskemål om förbättringar som gäller arbetssättet eller den valda tekniken? 
VAR34_DF <- toLongDF(dataDF, "VAR34")
VAR34_DF <- RecodeTech(VAR34_DF, "VAR34")
VAR34_DF <- rename(VAR34_DF, "VAR45_34" = "VAR34")
# Har berörd personal fått möjlighet att framföra synpunkter eller önskemål om förbättringar som gäller arbetssättet eller den valda tekniken?
VAR45_DF <- toLongDF(dataDF, "VAR45")
VAR45_DF <- RecodeTech(VAR45_DF, "VAR45")
VAR45_DF <- rename(VAR45_DF, "VAR45_34" = "VAR45")

VAR34_45_DF <- rbind(VAR45_DF, VAR34_DF)

# Har du gett stöd, utbildning, information, tid, osv. till den personal som ska använda den valda tekniken för att medverka i utvecklingsarbetet av arbetssättet? 
VAR38_DF <- toLongDF(dataDF, "VAR38")
VAR38_DF <- RecodeTech(VAR38_DF, "VAR38")
VAR38_DF <- rename(VAR38_DF, "VAR27_38" = "VAR38")
# Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av arbetssättet? 
VAR27_DF <- toLongDF(dataDF, "VAR27")
VAR27_DF <- RecodeTech(VAR27_DF, "VAR27")
VAR27_DF <- rename(VAR27_DF, "VAR27_38" = "VAR27")

VAR38_27_DF <- rbind(VAR38_DF, VAR27_DF)

# Har du fått stöd, utbildning, information, tid, osv. för att medverka i utvecklingsarbetet av den valda tekniken?
VAR28_DF <- toLongDF(dataDF, "VAR28")
VAR28_DF <- RecodeTech(VAR28_DF, "VAR28")
VAR28_DF <- rename(VAR28_DF, "VAR28_39" = "VAR28")
# Har gett stöd, utbildning, information, tid, osv. till den berörda personalen för att medverka i utvecklingsarbetet av den valda tekniken? 
VAR39_DF <- toLongDF(dataDF, "VAR39")
VAR39_DF <- RecodeTech(VAR39_DF, "VAR39")
VAR39_DF <- rename(VAR39_DF, "VAR28_39" = "VAR39")

VAR28_39_DF <- rbind(VAR28_DF, VAR39_DF)

# combine dfs and scores
involvementDF <- list(VAR28_39_DF, VAR38_27_DF, VAR34_45_DF, VAR25_36_DF, VAR26_37_DF,
                      VAR29_40_DF, VAR30_41_DF, VAR31_42_DF, VAR32_43_DF, VAR33_44_DF, 
                      VAR35_46_DF) %>%
  reduce(dplyr::full_join, by = c("ID","name","VAR00","VAR05"))



head(involvementDF)

# check internal consistency of items - Cronbach’s alpha
involvementDF_check <- involvementDF %>%
  dplyr::select(VAR28_39:VAR46_35)

cronbach.alpha(involvementDF_check)
# Items: 11
# Sample units: 721
# alpha: 0.917

# add mean and median
involvementDF$mean <- rowMeans(involvementDF[ , 5:15], na.rm=TRUE)
involvementDF <- involvementDF %>%
  rowwise() %>%
  mutate(rMedian = median(c_across(5:15), na.rm = TRUE)) %>%
  ungroup()

# to factor
involvementDF$rMedian <- as.factor(involvementDF$rMedian)
VAR45_DF$VAR45 <- as.factor(VAR45_DF$VAR45)
VAR44_DF$VAR44 <- as.factor(VAR44_DF$VAR44)
VAR43_DF$VAR43 <- as.factor(VAR43_DF$VAR43)
VAR42_DF$VAR42 <- as.factor(VAR42_DF$VAR42)
VAR41_DF$VAR41 <- as.factor(VAR41_DF$VAR41)
VAR40_DF$VAR40 <- as.factor(VAR40_DF$VAR40)
VAR39_DF$VAR39 <- as.factor(VAR39_DF$VAR39)
VAR38_DF$VAR38 <- as.factor(VAR38_DF$VAR38)
VAR37_DF$VAR37 <- as.factor(VAR37_DF$VAR37)
VAR36_DF$VAR36 <- as.factor(VAR36_DF$VAR36)
VAR35_DF$VAR35 <- as.factor(VAR35_DF$VAR35)
VAR34_DF$VAR34 <- as.factor(VAR34_DF$VAR34)
VAR33_DF$VAR33 <- as.factor(VAR33_DF$VAR33)
VAR32_DF$VAR32 <- as.factor(VAR32_DF$VAR32)
VAR31_DF$VAR31 <- as.factor(VAR31_DF$VAR31)
VAR30_DF$VAR30 <- as.factor(VAR30_DF$VAR30)
VAR29_DF$VAR29 <- as.factor(VAR29_DF$VAR29)
VAR28_DF$VAR28 <- as.factor(VAR28_DF$VAR28)
VAR27_DF$VAR27 <- as.factor(VAR27_DF$VAR27)
VAR26_DF$VAR26 <- as.factor(VAR26_DF$VAR26)
VAR25_DF$VAR25 <- as.factor(VAR25_DF$VAR25)


# plots - median for airport size
ggplot(involvementDF, aes(x = rMedian, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    y = "count",
    x = "Median engagemang",
    fill = "Storlek flygplatsen"
  ) +
  theme_minimal() 

ggplot(involvementDF, aes(x = factor(rMedian), fill = VAR05)) +
  geom_bar(position = "fill") +
  labs(
    x = "Median engagemang",
    y = "Proportion",
    fill = "Roll"
  ) +
  theme_minimal()


# median staff vs manager
ggplot(involvementDF, aes(x = rMedian, fill = VAR00)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    y = "count",
    x = "Median engagemang",
    fill = "Roll"
  ) +
  theme_minimal() 

ggplot(involvementDF, aes(x = factor(rMedian), fill = VAR00)) +
  geom_bar(position = "fill") +
  labs(
    x = "Median engagemang",
    y = "Proportion",
    fill = "Roll"
  ) +
  theme_minimal()

# VAR25_36
ggplot(involvementDF, aes(x = VAR25_36, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    y = "count",
    x = NULL,
    fill = "Storlek flygplatsen",
    title = "VAR25_36"
  ) +
  theme_minimal() 

# VAR26_37 
ggplot(involvementDF, aes(x = VAR26_37, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    fill = "Storlek flygplatsen",
    y = "count",
    x = NULL,
    title = "VAR26_37"
  ) +
  theme_minimal() 

# VAR27_38
ggplot(involvementDF, aes(x = VAR27_38, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    fill = "Storlek flygplatsen",
    y = "count",
    title = "VAR27_38",
    x = NULL
  ) +
  theme_minimal() 

# VAR28_39
ggplot(involvementDF, aes(x = VAR28_39, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    fill = "Storlek flygplatsen",
    y = "count",
    x = NULL,
    title = "VAR28_39"
  ) +
  theme_minimal() 

# VAR29_40
ggplot(involvementDF, aes(x = VAR29_40, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    fill = "Storlek flygplatsen",
    x = NULL,
    y = "count",
    title = "VAR29_40"
  ) +
  theme_minimal() 

# VAR30_41 
ggplot(involvementDF, aes(x = VAR30_41, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    fill = "Storlek flygplatsen",
    x = NULL,
    y = "count",
    title = "VAR30_41"
  ) +
  theme_minimal() 

ggplot(involvementDF, aes(x = VAR31_42, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    fill = "Storlek flygplatsen",
    x = NULL,
    y = "count",
    title = "VAR31_42"
  ) +
  theme_minimal() 

# VAR43_32
ggplot(involvementDF, aes(x = VAR43_32, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    fill = "Storlek flygplatsen",
    x = NULL,
    y = "count",
    title = "VAR43_32 "
  ) +
  theme_minimal() 

# VAR33 - Har någon av dina kollegor medverkat i något utvecklingsarbete? 
ggplot(involvementDF, aes(x = VAR44_33, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    fill = "Storlek flygplatsen",
    x = NULL,
    y = "count",
    title = "VAR33_44 "
  ) +
  theme_minimal() 

# VAR45_34
ggplot(involvementDF, aes(x = VAR45_34, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    fill = "Storlek flygplatsen",
    x = NULL,
    y = "count",
    title = "VAR45_34"
  ) +
  theme_minimal() 

# VAR46_35
ggplot(involvementDF, aes(x = VAR46_35, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    fill = "Storlek flygplatsen",
    x = NULL,
    y = "count",
    title = "VAR46_35"
  ) +
  theme_minimal() 




# differences between roles?




############## Involvement - NASA-TLX relationship #############

summary(NASA_TLX_DF)
summary(involvementDF)

# plots - median for airport size
ggplot(involvementDF, aes(x = rMedian, fill = VAR05)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    y = "count",
    x = "median",
    fill = "Airport size"
  ) +
  theme_minimal() 

# median staff vs manager
ggplot(involvementDF, aes(x = rMedian, fill = VAR00)) +
  facet_wrap(~name) +
  geom_bar(position = "stack") +
  labs(
    y = "count",
    x = "median",
    fill = "role"
  ) +
  theme_minimal() 


# plot total nasa tlx
ggplot(NASA_TLX_DF, aes(name, nasa_mean)) +
  geom_jitter(width = 0.15) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(NASA_TLX_DF, aes(name, nasa_mean)) +
  geom_boxplot(outlier.shape = NA) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# involvement x nasa tlx
nasa_involvementDF <- merge(involvementDF, NASA_TLX_DF, by = c("ID", "name", "VAR00", "VAR05"))

ggplot(nasa_involvementDF, aes(mean, nasa_mean)) +
  geom_point() +
  labs(
    y = "NASA TLX",
    x = "Mean engagemang"
  ) 




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
VAR12_DF <- toLongDF(dataDF, "VAR12")
VAR12_DF <- RecodeTech(VAR12_DF, "VAR12")
summary(VAR12_DF)

# inverse
VAR12_DF$VAR12_rev <- 7 - VAR12_DF$VAR12

VAR12_DF$VAR12 <- recode_factor(
  VAR12_DF$VAR12,
  `1` = "Vid varje möjlighet",
  `2` = "Dagligen",
  `3` = "Varje vecka",
  `4` = "Varje månad",
  `5` = "Mindre än varje månad",
  `6` = "Aldrig"
)


VAR12_DF$VAR12_rev <- recode_factor(
  VAR12_DF$VAR12_rev,
  `6` = "Vid varje möjlighet",
  `5` = "Dagligen",
  `4` = "Varje vecka",
  `3` = "Varje månad",
  `2` = "Mindre än varje månad",
  `1` = "Aldrig"
)

ggplot(VAR12_DF, aes(x = name, fill = VAR12_rev)) +
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
table(VAR12_DF$name, VAR12_DF$VAR12_rev)







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
MergeDF1 <- merge(VAR24_DF, VAR23_DF, by = c("ID", "name", "VAR00", "VAR05"))
MergeDF2 <- merge(MergeDF1, VAR22_DF, by = c("ID", "name", "VAR00", "VAR05"))
MergeDF3 <- merge(MergeDF2, VAR21_DF, by = c("ID", "name", "VAR00", "VAR05"))
MergeDF4 <- merge(MergeDF3, VAR20_DF, by = c("ID", "name", "VAR00", "VAR05"))
MergeDF5 <- merge(MergeDF4, VAR19_DF, by = c("ID", "name", "VAR00", "VAR05"))
MergeDF5 <- subset(MergeDF5, select = -c(VAR00,VAR05) )

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

############## Usage - NASA relationship (with respect to weather) #############

# usage frequency
summary(VAR12_DF)

ggplot(VAR12_DF, aes(x = VAR12_rev)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR12_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Hur ofta använder du tekniska hjälpmedelet?",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# plot  nasa tlx
ggplot(NASA_TLX_DF, aes(name, nasa_mean)) +
  geom_jitter(width = 0.15) +
  theme_minimal() +
  scale_x_discrete(labels = addToolCount(NASA_TLX_DF)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Tool"
  )

ggplot(NASA_TLX_DF, aes(name, nasa_mean)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = addToolCount(NASA_TLX_DF)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# correlation nasa tlx - usage
nasa_usageDF <- merge(NASA_TLX_DF, VAR12_DF, by = c("ID", "name", "VAR00", "VAR05"), all = TRUE)

ggplot(nasa_usageDF, aes(x = factor(VAR12_rev), y = nasa_mean)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(nasa_usageDF))) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Hur ofta använder du tekniska hjälpmedelet?",
    y = "NASA TLX"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(nasa_usageDF, aes(x = factor(VAR12_rev), y = nasa_mean)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(nasa_usageDF))) +
  geom_point() +
  labs(
    x = "Hur ofta använder du tekniska hjälpmedelet?",
    y = "NASA TLX"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# weather
# sun
ggplot(VAR19_DF, aes(x = VAR19)) +
  facet_wrap(~name, , labeller = labeller(name = addToolCount(VAR19_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Starkt solsken",
    y = NULL
  ) +
  theme_minimal() 

# sun - usage
weather1_usageDF <- merge(VAR19_DF, VAR12_DF, by = c("ID", "name", "VAR00", "VAR05"), all = TRUE)

ggplot(weather1_usageDF, aes(x=VAR19, fill=factor(VAR12_rev))) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather1_usageDF))) +
  geom_bar(na.rm = TRUE) +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Den valda tekniken blir en större belastning att använda under: Starkt solsken "
  )


# snow
ggplot(VAR20_DF, aes(x = VAR20)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR20_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Snö.",
    x = NULL
  ) +
  theme_minimal() 

# snow - usage
weather2_usageDF <- merge(weather1_usageDF, VAR20_DF, by = c("ID", "name", "VAR00", "VAR05"), all = TRUE)

ggplot(weather2_usageDF, aes(x=VAR20, fill=factor(VAR12_rev))) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather2_usageDF))) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Den valda tekniken blir en större belastning att använda under: Snö "
  )


# cold
ggplot(VAR21_DF, aes(x = VAR21)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR21_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Kyla",
    x = NULL
  ) +
  theme_minimal() 

# cold - usage
weather3_usageDF <- merge(weather2_usageDF, VAR21_DF, by = c("ID", "name", "VAR00", "VAR05"), all = TRUE)

ggplot(weather3_usageDF, aes(x=VAR21, fill=factor(VAR12_rev))) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather3_usageDF))) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Den valda tekniken blir en större belastning att använda under: Kyla "
  )


# rain
ggplot(VAR22_DF, aes(x = VAR22)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR22_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Regn",
    x = NULL
  ) +
  theme_minimal()

# rain - usage
weather4_usageDF <- merge(weather3_usageDF, VAR22_DF, by = c("ID", "name", "VAR00", "VAR05"), all = TRUE)

ggplot(weather4_usageDF, aes(x=VAR22, fill=VAR12_rev)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather4_usageDF))) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Den valda tekniken blir en större belastning att använda under: Regn "
  )


# Darkness
ggplot(VAR23_DF, aes(x = VAR23)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR23_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Mörker",
    x = NULL
  ) +
  theme_minimal() 

# darkness - usage
weather5_usageDF <- merge(weather4_usageDF, VAR23_DF, by = c("ID", "name", "VAR00", "VAR05"), all = TRUE)

ggplot(weather5_usageDF, aes(x=VAR23, fill=VAR12_rev)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather5_usageDF))) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Den valda tekniken blir en större belastning att använda under: Mörker "
  )


# Dimma
ggplot(VAR24_DF, aes(x = VAR24)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(VAR24_DF))) +
  geom_bar(fill = "#0072B2") +
  labs(
    y = "count",
    title = "Den valda tekniken blir en större belastning att använda under: Dimma",
    x =  NULL 
  ) +
  theme_minimal() 

# dimma - usage
weather6_usageDF <- merge(weather5_usageDF, VAR24_DF, by = c("ID", "name", "VAR00", "VAR05"), all = TRUE)

ggplot(weather6_usageDF, aes(x=VAR24, fill=VAR12_rev)) +
  facet_wrap(~name, labeller = labeller(name = addToolCount(weather6_usageDF))) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Den valda tekniken blir en större belastning att använda under: Dimma "
)


# merge usage x weather dfs
long_weather_usage <- pivot_longer(weather6_usageDF, 
                        cols = c(VAR19, VAR20, VAR21, VAR22, VAR23, VAR24), 
                        names_to = "weather", 
                        values_to = "weather_difficulty")

long_weather_usage <- long_weather_usage %>%
  mutate(
    weather = recode(weather,
                     "VAR19" = "sun",
                     "VAR20" = "snow",
                     "VAR21" = "cold",
                     "VAR22" = "rain",
                     "VAR23" = "darkness",
                     "VAR24" = "fog")
  )

#ipad
ipad_weatherDF <- subset(long_weather_usage, name == "iPad")

ggplot(ipad_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "iPad blir en större belastning att använda under:"
  )

#fingerscanner
fingerscanner_weatherDF <- subset(long_weather_usage, name == "Fingerscanner")

ggplot(fingerscanner_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Fingerscanner blir en större belastning att använda under:"
  )

#smartphone
smartphone_weatherDF <- subset(long_weather_usage, name == "Smartphone")

ggplot(smartphone_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Smartphone blir en större belastning att använda under:"
  )

# Stationär eller bärbar dator
dator_weatherDF <- subset(long_weather_usage, name == "Stationär eller bärbar dator")

ggplot(dator_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Stationär eller bärbar dator blir en större belastning att använda under:"
  )

# BRS
brs_weatherDF <- subset(long_weather_usage, name == "BRS")

ggplot(brs_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "BRS blir en större belastning att använda under:"
  )

# Tankbil
tankbil_weatherDF <- subset(long_weather_usage, name == "Tankbil")

ggplot(tankbil_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Tankbil blir en större belastning att använda under:"
  )

# 	Power Stow
powerstow_weatherDF <- subset(long_weather_usage, name == "Power Stow")

ggplot(powerstow_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "Power Stow blir en större belastning att använda under:"
  )

# De-icing bil
deicing_weatherDF <- subset(long_weather_usage, name == "De-icing bil")

ggplot(deicing_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "Frekvens",
    x = NULL,
    title = "De-icing bil blir en större belastning att använda under:"
  )

# Digitalisering (i allmänhet)
Digitalisering_weatherDF <- subset(long_weather_usage, name == "Digitalisering (i allmänhet)")

ggplot(Digitalisering_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "use frequency",
    x = NULL,
    title = "Digitalisering (i allmänhet) blir en större belastning att använda under:"
  )

# Lyfthjälpmedel
lyft_weatherDF <- subset(long_weather_usage, name == "Lyfthjälpmedel")

ggplot(lyft_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "use frequency",
    x = NULL,
    title = "Lyfthjälpmedel blir en större belastning att använda under:"
  )

# Bagagetransport
bagage_weatherDF <- subset(long_weather_usage, name == "Bagagetransport")

ggplot(bagage_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "use frequency",
    x = NULL,
    title = "Bagagetransport blir en större belastning att använda under:"
  )

# Pushback
pushback_weatherDF <- subset(long_weather_usage, name == "Pushback")

ggplot(pushback_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "use frequency",
    x = NULL,
    title = "Pushback blir en större belastning att använda under:"
  )

# Räddningsfordon
radding_weatherDF <- subset(long_weather_usage, name == "Räddningsfordon")

ggplot(radding_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "use frequency",
    x = NULL,
    title = "Räddningsfordon blir en större belastning att använda under:"
  )

# Water/Waste
waste_weatherDF <- subset(long_weather_usage, name == "Water/Waste")

ggplot(waste_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "use frequency",
    x = NULL,
    title = "Water/Waste blir en större belastning att använda under:"
  )

# lastband
lastband_weatherDF <- subset(long_weather_usage, name == "Lastband")

ggplot(lastband_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "use frequency",
    x = NULL,
    title = "Lastband blir en större belastning att använda under:"
  )

# gater
gater_weatherDF <- subset(long_weather_usage, name == "Gater")

ggplot(gater_weatherDF, aes(x=weather_difficulty, fill=VAR12_rev)) +
  facet_wrap(~weather) +
  geom_bar() +
  labs(
    fill = "use frequency",
    x = NULL,
    title = "Gater blir en större belastning att använda under:"
  )



