## R version of geospatial-data-analysis
library(tidyverse)
library(maps)
library(janitor)
library("cancensus")
library("sf")
library("geojsonsf")

### Data wrangling 
us_states = map_data("state")
us_drug = read_csv("VSRR_Provisional_Drug_Overdose_Death_Counts.csv")
us_population = read_csv("PopulationReport.csv",show_col_types = FALSE)
us_population = us_population[1:56,1:6] %>%
  row_to_names(row_number = 3)

###  Data clearing
us_drug = us_drug %>% 
          select(State,Year,Month,`State Name`,`Predicted Value`,Indicator) %>% 
          filter(Indicator == "Number of Drug Overdose Deaths", Year != 2021, `State Name` != "United States")
us_drug = us_drug %>% 
          group_by(Year,`State Name`) %>%
          summarise(drug_death = sum(`Predicted Value`))
us_drug2 = us_drug %>% 
          filter(Year == 2020) %>%
          mutate(region = tolower(`State Name`))

us_population = us_population %>%
  select(Name,`Pop. 2020`) %>%
  mutate(`Pop. 2020` = as.numeric(str_remove_all(`Pop. 2020`,",")),
         region = tolower(Name))

us_drug3 = us_drug2 %>% left_join(us_population,by="region") %>%
  mutate(death_per_10000 = 10000*(drug_death / `Pop. 2020`))

drug_map  = us_states %>% left_join(us_drug3,by="region")

p0 = ggplot(data = drug_map, mapping=aes(x = long, y = lat,
                                         group = group, fill = death_per_10000))

### plot 
p1 = p0 + geom_polygon(color="grey90",size = 0.1) +
  coord_map(projection = "albers",lat0 = 39, lat1=45)

p2 = p1 + scale_fill_gradient2(low = "white",high = "red") +
  labs(title = "USA State level Drug Overdose Deaths per 10000 population in 2020",fill="Drug Overdose Deaths per 10000 population") + theme(legend.position ="bottom") 

p2


### second data analysis 

## setting API
options(cancensus.api_key = "CensusMapper_b7c75dff4cc52fd36a7f8ec54d1d8780")
options(cancensus.cache_path = "custom cache path")
readRenviron("~/.Renviron")

### select Montreal census data and some columns.
list_census_datasets()
list_census_regions('CA16') %>%
  filter(level == "CMA", name == "Montréal")
census_data_2016 <- get_census(dataset='CA16', regions=list(CMA="24462"),
                               vectors=c("v_CA16_1371","v_CA16_1368","v_CA16_1365"),
                               level='CSD', use_cache = FALSE, geo_format = 'sf')  %>%   
                    select(GeoUID,geometry,`Region Name`,Population,`v_CA16_1368: French`, 
                           `v_CA16_1371: Non-official languages`,`v_CA16_1365: English`)

census_2016  = census_data_2016 %>%
               mutate(fre_ratio =  (`v_CA16_1365: English` +  `v_CA16_1371: Non-official languages`) / `v_CA16_1368: French`)


#### plot 
census_2016 %>% 
  ggplot(aes(geometry=geometry)) + 
  geom_sf(aes(fill = fre_ratio), colour = "gray") +
  scale_fill_viridis_c(option = "viridis", "Number of resident who speaks non-French language 
  at home per each French speaking resident at home") + 
  theme_minimal() +
  theme(legend.position ="bottom",legend.key.width = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(datum=NA) +
  labs(title = "The number of residents who do not speak French at home per 
       each resident who speaks French at home in the each Metropolitan Montreal area")

