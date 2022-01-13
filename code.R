library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)

IDMC <- read_xlsx("IDMC_Internal_Displacement_Conflict-Violence_Disasters_2008_2020.xlsx")

unique(IDMC$Year)


IDMC_total <- IDMC %>%
  group_by(Name, Year) %>% 
  mutate(total = sum(ConflictStockDisplacementRaw, DisasterStockDisplacementRaw, na.rm = T))

sum_year<- IDMC_total %>%
  group_by(Year) %>% 
  summarise(sum(total)) %>% 
  arrange(desc(`sum(total)`))

sum_country <- IDMC_total %>% 
  group_by(Name) %>% 
  summarise(sum(total)) %>% 
  arrange(desc(`sum(total)`))

IDMC_total %>% 
  filter(Year == 2020) %>%
  group_by(Name) %>% 
  summarise(sum(total)) %>% 
  arrange(desc(`sum(total)`))

IDMC_total %>% 
  filter(Year == 2020) %>%
  group_by(Name) %>% 
  summarise(sum(ConflictStockDisplacementRaw, na.rm = T)) %>% 
  arrange(desc(`sum(ConflictStockDisplacementRaw, na.rm = T)`))


IDMC_total %>% 
  filter(Year == 2020) %>%
  group_by(Name) %>% 
  summarise(sum(DisasterStockDisplacementRaw, na.rm = T)) %>% 
  arrange(desc(`sum(DisasterStockDisplacementRaw, na.rm = T)`))


##################################################################



ACNUR <- read.csv("population.csv", header = FALSE, sep = ",")

ACNUR = ACNUR[-1,]

ACNUR_final <- ACNUR %>%
  select(V1, V2, V3, V6) %>% 
  rename(Year = V1,
         Country_origin = V2,
         ISO = V3,
         idp_concern = V6)

ACNUR_final$idp_concern = as.numeric(ACNUR_final$idp_concern)
ACNUR_final$Year = as.double(ACNUR_final$Year)

write_xlsx(ACNUR_final, "acnur_final.xlsx")

ACNUR_final <- read_xlsx("acnur_final.xlsx")

ACNUR_final$idp_concern = as.numeric(ACNUR_final$idp_concern)
ACNUR_final$Year = as.character(ACNUR_final$Year)

sum_year2 <- ACNUR_final %>% 
  group_by(Year) %>% 
  summarise(sum(idp_concern)) %>% 
  arrange(desc(`sum(idp_concern)`))

sum_country2 <- ACNUR_final %>% 
  group_by(Country_origin) %>% 
  summarise(sum(idp_concern)) %>% 
  arrange(desc(`sum(idp_concern)`))

ACNUR_final %>% 
  filter(Year == 2020) %>% 
  group_by(Country_origin) %>%
  arrange(desc(idp_concern))


############################### GRÁFICOS #################################

# gráfico geral

ACNUR_final <- ACNUR_final %>% 
  filter(Year != 2021)

ggplot(ACNUR_final, aes(Year, idp_concern)) +
  geom_col() +
  coord_cartesian(ylim = c(4000000, 50000000)) +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")
  
# gráfico por país

# COLOMBIA #

ACNUR_final %>% 
  filter(Country_origin == "Colombia") %>% 
  group_by(Year) %>% 
  arrange(desc(idp_concern))

ACNUR_final %>% 
  filter(Country_origin == "Colombia") %>% 
  ggplot(aes(Year, idp_concern)) +
    geom_col() +
    coord_cartesian(ylim = c(500000, 10000000)) +
    scale_y_continuous(labels = scales::comma, breaks = seq(500000, 10000000, by = 1200000)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(x = "",
         y = "")

# SIRIA #

ACNUR_final %>% 
  filter(Country_origin == "Syrian Arab Rep.") %>% 
  group_by(Year) %>% 
  arrange(desc(idp_concern))

ACNUR_final %>% 
  filter(Country_origin == "Syrian Arab Rep.") %>% 
  ggplot(aes(Year, idp_concern)) +
  geom_col() +
  coord_cartesian(ylim = c(500000, 8000000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(500000, 8000000, by = 1500000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")

# RDC #

ACNUR_final %>% 
  filter(Country_origin == "Dem. Rep. of the Congo") %>% 
  group_by(Year) %>% 
  arrange(desc(idp_concern))

ACNUR_final %>% 
  filter(Country_origin == "Dem. Rep. of the Congo") %>% 
  ggplot(aes(Year, idp_concern)) +
  geom_col() +
  coord_cartesian(ylim = c(1000, 5000000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(1000, 5000000, by = 700000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")

# IÊMEN #

ACNUR_final %>% 
  filter(Country_origin == "Yemen") %>% 
  group_by(Year) %>% 
  arrange(desc(idp_concern))

ACNUR_final %>% 
  filter(Country_origin == "Yemen") %>% 
  ggplot(aes(Year, idp_concern)) +
  geom_col() +
  coord_cartesian(ylim = c(100000, 4000000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(100000, 4000000, by = 550000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")

# SOMALIA #

ACNUR_final %>% 
  filter(Country_origin == "Somalia") %>% 
  group_by(Year) %>% 
  arrange(desc(idp_concern))

ACNUR_final %>% 
  filter(Country_origin == "Somalia") %>% 
  ggplot(aes(Year, idp_concern)) +
  geom_col() +
  coord_cartesian(ylim = c(100000, 3000000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(100000, 3000000, by = 400000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")

# AFEGANISTAO #

ACNUR_final %>% 
  filter(Country_origin == "Afghanistan") %>% 
  group_by(Year) %>% 
  arrange(desc(idp_concern))

ACNUR_final %>% 
  filter(Country_origin == "Afghanistan") %>% 
  ggplot(aes(Year, idp_concern)) +
  geom_col() +
  coord_cartesian(ylim = c(100000, 3000000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(100000, 3000000, by = 400000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")

# ETIOPIA #

ACNUR_final %>% 
  filter(Country_origin == "Ethiopia") %>% 
  group_by(Year) %>% 
  arrange(desc(idp_concern))

ACNUR_final %>% 
  filter(Country_origin == "Ethiopia") %>% 
  ggplot(aes(Year, idp_concern)) +
  geom_col() +
  coord_cartesian(ylim = c(1000000, 3000000)) +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")

# NIGERIA #

ACNUR_final %>% 
  filter(Country_origin == "Nigeria") %>% 
  group_by(Year) %>% 
  arrange(desc(idp_concern))

ACNUR_final %>% 
  filter(Country_origin == "Nigeria") %>% 
  ggplot(aes(Year, idp_concern)) +
  geom_col() +
  coord_cartesian(ylim = c(1000000, 2500000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(100000, 2500000, by = 200000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")

# SUDAO #

ACNUR_final %>% 
  filter(Country_origin == "Sudan") %>% 
  group_by(Year) %>% 
  arrange(desc(idp_concern))

ACNUR_final %>% 
  filter(Country_origin == "Sudan") %>% 
  ggplot(aes(Year, idp_concern)) +
  geom_col() +
  coord_cartesian(ylim = c(600000, 3000000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(600000, 3000000, by = 400000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")

# SUDAO DO SUL #

ACNUR_final %>% 
  filter(Country_origin == "South Sudan") %>% 
  group_by(Year) %>% 
  arrange(desc(idp_concern))

ACNUR_final %>% 
  filter(Country_origin == "South Sudan") %>% 
  ggplot(aes(Year, idp_concern)) +
  geom_col() +
  coord_cartesian(ylim = c(300000, 2000000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(300000, 2000000, by = 200000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")

# TOTAL #

ACNUR_final %>% 
  filter(Country_origin == c("Colombia", "Syrian Arab Rep.", "Dem. Rep. of the Congo", "Yemen", "Somalia", "Afghanistan", "Ethiopia", "Nigeria", "Sudan", "South Sudan")) %>% 
  ggplot(aes(Year, idp_concern, fill = Country_origin)) +
  geom_col() +
  coord_cartesian(ylim = c(1000, 10000000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(1000, 10000000, by = 500000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       y = "")
