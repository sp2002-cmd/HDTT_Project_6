```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Children Migration Numbers

```{r}
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

ChildrenMigration <- undesa_pd_2020_ims_stock_by_age_sex_and_destination[-c(1, 2, 3, 4, 5, 6, 7, 8), ]
selected_columns <- ChildrenMigration[, c("...7", "...8", "...9")]
subset_df <- selected_columns[35:45, ] #37:43

df <- data.frame(
  Year = c(1990, 1995, 2000, 2005, 2010, 2015, 2020),
  `Age 0-4` = c(320974, 260698, 294397, 225245, 493534, 423540, 917986),
  `Age 5-9` = c(395219, 311982, 349716, 363867, 545322, 565166, 1115877),
  `Age 10-14` = c(422440, 364030, 324750, 403733, 470486, 571970, 1021689),
  `Age 15-19` = c(451445, 414705, 360146, 436710, 498951, 574297, 931351)
)

# Reshape the dataframe to a long format
df_long <- df %>%
  pivot_longer(cols = starts_with("Age"), names_to = "AgeGroup", values_to = "Population")

# Create the line plot
migration <- ggplot(df_long, aes(x = Year, y = Population, color = AgeGroup)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c("Age.0.4" = "blue", "Age.5.9" = "green3", "Age.10.14" = "red", "Age.15.19" = "lightblue3"),
    labels = c("0-4", "5-9", "10-14", "15-19"))+
  labs(x = "Year",
    y = "Population",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),     # X-axis label size and style
    axis.title.y = element_text(size = 20),     # Y-axis label size and style
    axis.text.x = element_text(size = 17),                     # X-axis numbers size
    axis.text.y = element_text(size = 17),                     # Y-axis numbers size
    legend.title = element_text(size = 20),     # Legend title size and style
    legend.text = element_text(size = 17)                      # Legend text size
  )

print(migration)

ggsave("migration.png", migration, width = 15, height = 10)

```

# Common Climate Change Disasters

```{r}
latin_america_data <- public_emdat_custom_request_2024_12_04_0f25786b_2121_4d2b_8157_fb535fe4fd50 %>%
  select(`Start Year`, `Disaster Type`, Country, Subregion) %>%
  filter(Subregion == "Latin America and the Caribbean")

# Group by Year and Disaster Type, then count occurrences
disaster_trends <- latin_america_data %>%
  group_by(`Start Year`, `Disaster Type`) %>%
  summarise(count = n()) %>%
  arrange(`Start Year`, `Disaster Type`)

filtered_disaster_trends <- disaster_trends %>%
  filter(!`Disaster Type` %in% c("Mass movement (wet)", "Mass movement (dry)", "Volcanic activity"))

# Plot the trends
disasters <- ggplot(filtered_disaster_trends, aes(x = `Start Year`, y = count, color = `Disaster Type`)) +
  geom_line(size = 1.5) +
  labs(x = "Year",
       y = "Count",
       color = "Disaster Type") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 20),     # X-axis label size and style
    axis.title.y = element_text(size = 20),     # Y-axis label size and style
    axis.text.x = element_text(size = 17),                     # X-axis numbers size
    axis.text.y = element_text(size = 17),                     # Y-axis numbers size
    legend.title = element_text(size = 20),     # Legend title size and style
    legend.text = element_text(size = 17), 
    strip.text = element_text(size = 20),
    anel.spacing = unit(2, "lines")
  ) +
  facet_wrap(~`Disaster Type`, scales = "free_y")

print(disasters)

ggsave("disasters.png", disasters, width = 22, height = 12)
```


# Mental Health 

```{r}

CHIS_Filtered <- AskCHISResults202407092335[-c(6:9, 1), ] %>%
  select("Citizenship and immigration status (3 levels)","...4", "...7", "...10" ) %>%
  rename("Yes" = "...4", "No" = "...7", "Total" = "...10")

CHIS_Percent <- CHIS_Filtered %>%
  mutate(
    Yes = as.numeric(Yes),
    Total = as.numeric(Total),
    Yes_Percentage = Yes / Total * 100
  )

CHIS_Percent <- CHIS_Percent[-c(4),]

# Create the bar graph
CHIS_plot <- ggplot(CHIS_Percent, aes(x = `Citizenship and immigration status (3 levels) compared by Climate change makes you nervous/depressed/stressed`, y = Yes_Percentage, fill = `Citizenship and immigration status (3 levels) compared by Climate change makes you nervous/depressed/stressed`)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Latino/Hispanic Children Who Feel Nervous/Stressed/Depressed About Climate Change",
       x = "Citizenship and Immigration Status",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 40) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),       # Title size and style
    axis.title.x = element_text(size = 20, face = "bold"),     # X-axis label size and style
    axis.title.y = element_text(size = 20, face = "bold"),     # Y-axis label size and style
    axis.text.x = element_text(size = 17),                     # X-axis numbers size
    axis.text.y = element_text(size = 17),                     # Y-axis numbers size
    legend.title = element_text(size = 20, face = "bold"),     # Legend title size and style
    legend.text = element_text(size = 17)                      # Legend text size
  )

print(CHIS_plot)

ggsave("CHIS.png", CHIS_plot, width = 10, height = 5, dpi = 700)

```

# Delayed/Didn't Recieve Care
```{r}

CHIS_Filtered2 <- AskCHISResults202407100033[-c(5:9, 1), ] %>%
  select("Citizenship and immigration status (3 levels)","...4", "...7", "...10" ) %>%
  rename("Yes" = "...4", "No" = "...7", "Total" = "...10")

CHIS_Percent2 <- CHIS_Filtered2 %>%
  mutate(
    Yes = as.numeric(Yes),
    Total = as.numeric(Total),
    Yes_Percentage = Yes / Total * 100
  )

# Create the bar graph
CHIS2_plot <- ggplot(CHIS_Percent2, aes(x = `Citizenship and immigration status (3 levels)`, y = Yes_Percentage, fill = `Citizenship and immigration status (3 levels)`)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Latino/Hispanic Children Who Recieved Delayed/No Care at All",
       x = "Citizenship and Immigration Status",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 15)

ggsave("CHIS2.png", CHIS2_plot, width = 10, height = 5, dpi = 700)

```

# Usual Source of Care
```{r}

CHIS_Filtered3 <- AskCHISResults202407100041[-c(5:9, 1), ] %>%
  select("Citizenship and immigration status (3 levels)","...4", "...7", "...10" ) %>%
  rename("Yes" = "...4", "No" = "...7", "Total" = "...10")

CHIS_Percent3 <- CHIS_Filtered3 %>%
  mutate(
    Yes = as.numeric(No),
    Total = as.numeric(Total),
    Yes_Percentage = Yes / Total * 100
  )

CHIS_plot3 <- ggplot(CHIS_Percent3, aes(x = `Citizenship and immigration status (3 levels)`, y = No_Percentage, fill = `Citizenship and immigration status (3 levels)`)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Latino/Hispanic Children Who Don't Recieve Usual Care",
       x = "Citizenship and Immigration Status",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 20)

ggsave("CHIS3.png", CHIS_plot3, width = 10, height = 5, dpi = 700)


```


# Multi-Bar Plot
```{r}
CHIS_Percent$Source <- "Climate change makes me stressed/nervous/depressed"
CHIS_Percent2$Source <- "Delayed or didn't recieve care"
CHIS_Percent3$Source <- "Has a usual source of care"

# Combine the datasets
CHIS_multi <- combinedCHIS_data %>%
  filter(`Citizenship and immigration status (3 levels)` != "Total") %>%
  mutate(`Citizenship and immigration status (3 levels)` = 
           recode(`Citizenship and immigration status (3 levels)`, 
                  "U.S. born citizen" = "US-born citizen")) %>%
  ggplot(aes(
    y = Source,  # Questions are now on the y-axis
    x = Yes_Percentage, 
    fill = `Citizenship and immigration status (3 levels)`  # Citizenship status as fill
  )) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  geom_text(aes(label = `Citizenship and immigration status (3 levels)`),  # Citizenship status as labels
            position = position_dodge(width = 0.8), 
            hjust = -0.2, size = 6, color = "black") +  # Adjust label position
  scale_y_discrete(labels = c(
    "Climate change makes me stressed/nervous/depressed" = "Climate-change-related stress or depression",
    "Delayed or didn't recieve care" = "Delay or lack of health care",
    "Has a usual source of care" = "Has usual source of health care"
  )) +  
  scale_y_discrete(labels = c(
    "Climate change makes me stressed/nervous/depressed" = "Climate-change-related\nstress or depression",
    "Delayed or didn't receive care" = "Delay or lack\nof health care",
    "Has a usual source of care" = "Has usual source\nof health care"
  )) + # Rename y-axis labels
  labs(y = "Question Type", x = "Percentage", fill = "Citizenship Status") +  # Adjust labels
  theme_light() +  # Cleaner theme
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
        panel.grid.minor = element_blank(),
        legend.position = "none") +  # Remove legend if labels are enough
  xlim(0, 40)  # Adjust axis limits if needed

print(CHIS_multi)

ggsave("CHIS_multi.png", CHIS_multi, width = 18, height = 10, dpi = 700)

```
