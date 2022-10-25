library(tidyverse)
library(readxl)
library(foreign)

# Leitura da base de UIFS
uif <- read_excel("Modelo das UIF's.xlsx")
uif$`Model Adopted`[uif$`Model Adopted`=='?'] <- NA

# Leitura do painel de dados
pannel <- read_excel("PAINEL_COMPLETO_formatado.xlsx", skip=1)
pannel$gdpcapita <- as.numeric(pannel$`GDP (current US$)`) / pannel$`Population, total`

# Junção das bases de uif com o painel de dados
uif.pannel <- left_join(pannel, select(uif,`Country Name`, `Model Adopted`))
uif.pannel$`Administrative` <- ifelse(uif.pannel$`Model Adopted` == 'Administrative', 1, 0)
uif.pannel$`Hybrid` <- ifelse(uif.pannel$`Model Adopted` == 'Hybrid', 1, 0)
uif.pannel$`Judicial` <- ifelse(uif.pannel$`Model Adopted` == 'Judicial', 1, 0)
uif.pannel$`Law Enforcement` <- ifelse(uif.pannel$`Model Adopted` == 'Law Enforcement', 1, 0)
uif.pannel[uif.pannel=='..'] <- NA
uif.pannel[uif.pannel=='NA'] <- NA


tratamento1 <- uif.pannel %>% filter(`Year` == 2020) %>% select(-`Model Adopted`, -Year, -`Income Level`) %>% type_convert()

years <- c(2020, 2019, 2018, 2017, 2016)

tratamento2 <- uif.pannel %>% type_convert() %>% filter(`Year` %in% years) %>% group_by(`Country Name`) %>% summarise_all(funs(mean(., na.rm = TRUE))) %>% ungroup() %>% select(-Year, -`Model Adopted`, -`Income Level`) %>% type_convert()

tratamento2[tratamento2=='NaN'] <- NA

colnames <- c(
  "name",
  "gdp",
  "inflation",
  "gini",
  "homicides",
  "childmort",
  "literacy",
  "extdebt",
  "democracy",
  "cpi",
  "hdi",
  "ecofreedom",
  "pop",
  "gdppcapita",
  "administrative",
  "hybrid",
  "judicial",
  "lawenforcement"
)

colnames(tratamento1) <- colnames
colnames(tratamento2) <- colnames

write.dta(tratamento1, 'tratamento1.dta')
write.dta(tratamento2, 'tratamento2.dta')



