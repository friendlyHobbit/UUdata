
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
ggplot(role_counts, aes(x="", y=n, fill=VAR00)) +
  geom_bar(stat="identity", width=1, color="white", linewidth=.5) +
  coord_polar(theta="y", start=1) +
  labs(title = "managers vs ground staff", fill = "Roles") +
  theme_void()  +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette="Set1") +
  theme( plot.title = element_text(hjust = 0.5))

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
  long_df <- subset(long_df, select= -c(name))
  return(long_df)
}

VAR08_DF <- toLongDF(dataDF, "VAR08")
VAR09_DF <- toLongDF(dataDF, "VAR09")
VAR10_DF <- toLongDF(dataDF, "VAR10")
VAR11_DF <- toLongDF(dataDF, "VAR11")


