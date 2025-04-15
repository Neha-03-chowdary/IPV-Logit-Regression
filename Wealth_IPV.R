library(readxl)
library(dplyr)
library(ggplot2)

Wealth_IPV=read_xlsx("C:/Users/Neha Chowdary/OneDrive/Desktop/EndTerms/Semester 6/R Programming/Wealth_IPV.xlsx")


logit = glm(SevereViolence~WealthIndex, family=binomial(link="logit"), data=Wealth_IPV)

summary(logit)

cor(Wealth_IPV$SevereViolence, Wealth_IPV$WealthIndex)


summary_data <- Wealth_IPV %>%
  group_by(d107, v190) %>%
  summarise(count = n())

summary_data$v190 <- factor(summary_data$v190, levels = c("poorest", "poorer", "middle", "richer", "richest"))

ggplot(summary_data, aes(x = v190, y = count, fill = d107)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Intimate Partner Violence (IPV) Across Wealth Categories",
       x = "Wealth Category",
       y = "Count", caption = "Source: NFHS-5 Data") +
  scale_fill_manual(values = c("no" = "blue", "yes" = "red")) +
  theme_minimal() 

Residence_IPV=read_xlsx("C:/Users/Neha Chowdary/OneDrive/Desktop/EndTerms/Semester 6/R Programming/Aniket_Residence.xlsx")

summary_residence= Residence_IPV %>%
  group_by(States, Placeofresidence, Experiencedviolence) %>%
  summarise(count = n())

area_colors <- c("rural" = "blue", "urban" = "red")
area_graph <- ggplot(summary_residence, aes(x = States, y = count, fill = Placeofresidence)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Rural and Urban Cases Reported within Each State",
       x = "State",
       y = "Count",
       caption = "Source: NFHS-5 data") +
  scale_fill_manual(values = area_colors, name = "Place of Residence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(area_graph)





