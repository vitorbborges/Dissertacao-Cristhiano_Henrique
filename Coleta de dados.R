# Imports
library(tidyverse)
library(readxl)

# Transformar os dados de PIB em painel

v.name <- 'GDP (current US$)'

PIB <- read_csv('dados/GDP_Data.csv') %>% 
  pivot_longer(cols='2005 [YR2005]':'2020 [YR2020]', names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

# Transformar os dados de Variação dos preços Deflator em painel

v.name <- 'Inflation, GDP deflator (annual %)'

Deflator <- read_csv('dados/GDP_Deflator_Data.csv') %>% 
  pivot_longer(cols='2005 [YR2005]':'2020 [YR2020]', names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

#Atualizando a DF

DF <- full_join(PIB,Deflator)
DF$Year <- DF$Year %>% lapply(function(x) substr(x,1,4)) %>% as.character()

# Transformar os dados de Indice de GINI em painel

v.name <- 'Gini index'

GINI <- read_csv('dados/API_SI.POV.GINI_DS2_en_csv_v2_4333947.csv', skip = 4) %>% 
  pivot_longer(cols='1960':'2021', names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

DF <- full_join(DF,GINI)

# Transformar os dados de Indice de homicidios em painel

v.name <- 'Intentional homicides (per 100,000 people)'

homicides <- read_csv('dados/API_VC.IHR.PSRC.P5_DS2_en_csv_v2_4355130.csv', skip = 4) %>% 
  pivot_longer(cols='1960':'2021', names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

DF <- full_join(DF,homicides)

# Transformar os dados de Indice de mortalidade infantil em painel

v.name <- 'Mortality rate, under-5 (per 1,000 live births)'

child.mort <- read_csv('dados/API_SH.DYN.MORT_DS2_en_csv_v2_4354417.csv', skip = 4) %>% 
  pivot_longer(cols='1960':'2021', names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

DF <- full_join(DF,child.mort)

# Transformar os dados de Indice de alfabetizacao em painel

v.name <- 'Literacy rate, adult total (% of people ages 15 and above)'

lit <- read_csv('dados/API_SE.ADT.LITR.ZS_DS2_en_csv_v2_4413588.csv', skip = 4) %>% 
  pivot_longer(cols='1960':'2021', names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

DF <- full_join(DF,lit)

# Transformar os dados de divida externa em painel

v.name <- 'External debt stocks (% of GNI)'

lit <- read_csv('dados/API_DT.DOD.DECT.GN.ZS_DS2_en_csv_v2_4368705.csv', skip = 4) %>% 
  pivot_longer(cols='1960':'2021', names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

DF <- full_join(DF,lit)

# Transformar os dados de Indice de democracia em painel

v.name <- 'Democracy Index'

dem <- read_excel('dados/Democracy Index.xlsx', skip = 0) %>% 
  pivot_longer(cols='2006':'2021', names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

DF <- full_join(DF,dem)

# Transformar os dados de Indice de percepcao de corrupcao em painel

v.name <- 'Corruption Perception Index'

cpi <- read_excel('dados/Corruption Perception Index.xlsx', skip = 0) %>% 
  pivot_longer(cols=`2012`:`2020`, names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

DF <- full_join(DF,cpi)


# Transformar os dados de Indice de desenvolvimento humano em painel

v.name <- 'Human Development Index'

idh <- read_excel('dados/IDH.xlsx', skip = 0)%>% mutate(`2017`, `2017`=as.character(`2017`)) %>% mutate(`2018`, `2018`=as.character(`2018`))%>% mutate(`2019`, `2019`=as.character(`2019`)) %>% 
  pivot_longer(cols=-`Country Name`, names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

DF <- full_join(DF,idh)

# Nivel de desenvolvimento designado pelo banco mundial

dev <- read_excel('dados/Development Level.xlsx')

DF <- full_join(DF,dev)

# Nível de liberdade econômica

freedom <- read_excel('dados/efotw-2021-master-index-data-for-researchers.xlsx', skip=4) %>% select(`Country Name`, `Year`, `Economic Freedom Summary Index`)%>% mutate(`Year`, `Year`=as.character(`Year`))

DF <- full_join(DF,freedom)

# População dos países

v.name <- 'Population, total'

pop <- read_csv('dados/API_SP.POP.TOTL_DS2_en_csv_v2_4473555.csv', skip = 4) %>% 
  pivot_longer(cols='1960':'2021', names_to='Year', values_to=v.name) %>% 
  select(`Country Name`, Year, `v.name`)

DF <- full_join(DF,pop)

# Finalizando

DF <- DF[order(DF$`Country Name`, DF$Year),]

DF <- DF %>% group_by(`Country Name`) %>% fill(names(.))

drop_rows <- c("Africa Eastern and Southern", 
               "Africa Western and Central",
               "Arab World",
               "Caribbean small states",
               "Central Europe and the Baltics",
               "Channel Islands",
               "Data from database: World Development Indicators",
               "Early-demographic dividend",
               "East Asia & Pacific",
               "East Asia & Pacific (excluding high income)",
               "East Asia & Pacific (IDA & IBRD countries)",
               "Euro area",
               "Europe & Central Asia",
               "Europe & Central Asia (excluding high income)",
               "Europe & Central Asia (IDA & IBRD countries)",
               "European Union",
               "Fragile and conflict affected situations",
               "Heavily indebted poor countries (HIPC)",
               "High income",
               "IBRD only",
               "IDA & IBRD total",
               "IDA blend",
               "IDA only",
               "IDA total",
               "Last Updated: 07/20/2022",
               "Late-demographic dividend",
               "Latin America & Caribbean",
               "Latin America & Caribbean (excluding high income)",
               "Latin America & the Caribbean (IDA & IBRD countries)",
               "Least developed countries: UN classification",
               "Low & middle income",
               "Low income",
               "Lower middle income",
               "Middle East & North Africa",
               "Middle East & North Africa (excluding high income)",
               "Middle East & North Africa (IDA & IBRD countries)",
               "Middle income",
               "North America" ,
               "Northern Mariana Islands",
               "Not classified",
               "OECD members",
               "Other small states",
               "Pacific island small states",
               "Post-demographic dividend",
               "Pre-demographic dividend",
               "Sint Maarten (Dutch part)",
               "St. Martin (French part)",
               "Small states",
               "South Asia",
               "South Asia (IDA & IBRD)",
               "Sub-Saharan Africa",
               "Sub-Saharan Africa (excluding high income)",
               "Sub-Saharan Africa (IDA & IBRD countries)",
               "Upper middle income",
               "World",
               NA)

DF <- DF[!(DF$`Country Name` %in% drop_rows),]#dados irrelevantes que precisam ser dropados

DF <- DF[(DF$`Year` %in% '2005':'2020'),] #15 anos de dados

write_csv(DF, 'PAINEL_COMPLETO.csv')


