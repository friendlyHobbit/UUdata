
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




########### Initial analysis #################

data_unique <- dataDF %>%
  distinct(ID, .keep_all = TRUE)


# get people's roles
summary(data_unique$VAR00.x)

# Compute counts per role, unique ID
role_counts <- data_unique %>%
  count(VAR00.x)

# Plot
ggplot(role_counts, aes(x="", y=n, fill=VAR00.x)) +
  geom_bar(stat="identity", width=1, color="white", linewidth=.5) +
  coord_polar(theta="y", start=1) +
  labs(title = "managers vs ground staff", fill = "Roles") +
  theme_void()  +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette="Set1") +
  theme( plot.title = element_text(hjust = 0.5))


# ground roles
summary(data_unique$VAR01)

# Plot
data_unique %>% 
  filter(!is.na(VAR01)) %>% 
  ggplot(aes(x = VAR01, fill = VAR01)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.3, size = 4) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Ground staff roles", x = "", y = "Count")



# boss roles
summary(data_unique$VAR02)

# Plot
data_unique %>% 
  filter(!is.na(VAR02)) %>% 
  ggplot(aes(x = VAR02, fill = VAR02)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.3, size = 4) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Manager roles", x = "", y = "Count")



# age
summary(data_unique$VAR03)

ggplot(data_unique, aes(x = VAR03, fill = VAR03)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "Age",
    y = "count"
  ) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))


# gender
summary(data_unique$VAR04)

ggplot(data_unique, aes(x = VAR04)) +
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
summary(data_unique$VAR05)

ggplot(data_unique, aes(x = VAR05)) +
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
summary(data_unique$VAR06)

ggplot(data_unique, aes(x = VAR06)) +
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
summary(data_unique$VAR07)

ggplot(data_unique, aes(x = VAR07)) +
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
