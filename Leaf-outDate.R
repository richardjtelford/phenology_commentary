### Leaf-out date

# stations
stationSF <- read.csv(file = "data/Phenology Scandinavia/PEP725_FI_106_020/PEP725_FI_stations.csv", sep = ";", stringsAsFactors = FALSE)

stationSF <- stationSF %>% 
  filter(LAT > 68)

stationNO <- read.csv(file = "data/Phenology Scandinavia/PEP725_NO_106_021/PEP725_NO_stations.csv", sep = ";", stringsAsFactors = FALSE)

stationNO <- stationNO %>% 
  filter(LAT < 63)


# leaf-out date Northern Finnland
BetulaSF <- read.csv(file = "data/Phenology Scandinavia/PEP725_FI_106_020/PEP725_FI_Betula(Betula_pendula_(B._verrucosa|_B._alba)).csv", sep = ";", stringsAsFactors = FALSE)
Picea_abiesSF <- read.csv(file = "data/Phenology Scandinavia/PEP725_FI_110_000/PEP725_FI_Picea_abies_(P.excelsa).csv", sep = ";", stringsAsFactors = FALSE)
Pinus_sylvestrisSF <- read.csv(file = "data/Phenology Scandinavia/PEP725_FI_140_082/PEP725_FI_Pinus(Pinus_sylvestris).csv", sep = ";", stringsAsFactors = FALSE)

Picea_abiesSF <- Picea_abiesSF %>% 
  mutate(species = "Picea_abies")

Pinus_sylvestrisSF <- Pinus_sylvestrisSF %>% 
  mutate(species = "Pinus_sylvestris")

Betula_pendulaSF %>% 
  mutate(species = "Betula") %>% 
  bind_rows(Picea_abiesSF, Pinus_sylvestrisSF) %>% 
  inner_join(stationSF, by = "PEP_ID") %>% 
  group_by(species, YEAR) %>% 
  summarise(mean = mean(DAY)) %>% print(n = Inf)

  
# leaf-out date Southern Norway
BetulaNO <- read.csv(file = "data/Phenology Scandinavia/PEP725_NO_106_021/PEP725_NO_Betula(Betula_pubescens).csv", sep = ";", stringsAsFactors = FALSE)
Fagus_sylvaticaNO <- read.csv(file = "data/Phenology Scandinavia/PEP725_NO_108_010/PEP725_NO_Fagus(Fagus_sylvatica).csv", sep = ";", stringsAsFactors = FALSE)
PiceaNO <- read.csv(file = "data/Phenology Scandinavia/PEP725_NO_110_000/PEP725_NO_Picea_abies_(P.excelsa).csv", sep = ";", stringsAsFactors = FALSE)
Larix_deciduaNO <- read.csv(file = "data/Phenology Scandinavia/PEP725_NO_122_000/PEP725_NO_Larix_decidua.csv", sep = ";", stringsAsFactors = FALSE)
  
BetulaNO <- BetulaNO %>% 
  mutate(species = "Betula")
  
Fagus_sylvaticaNO <- Fagus_sylvaticaNO %>% 
  mutate(species = "Fagus_sylvatica")

PiceaNO <- PiceaNO %>% 
  mutate(species = "Picea")

Larix_deciduaNO %>% 
    mutate(species = "Larix_decidua")
  bind_rows(BetulaNO, Fagus_sylvaticaNO, PiceaNO) %>% 
    inner_join(stationNO, by = "PEP_ID") %>% 
    group_by(species, YEAR) %>% 
    summarise(mean = mean(DAY)) %>% print(n = Inf)
  