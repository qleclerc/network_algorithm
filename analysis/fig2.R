
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)
library(RColorBrewer)

options(dplyr.summarise.inform = FALSE)
pal = brewer.pal(6, "Set2")

adm_data = read.csv(here::here("data","toy_admission.csv"), sep=";") %>%
  select(id, hospitalization, cat, ward)
adm_data$cat[adm_data$cat == ""] = adm_data$hospitalization[adm_data$cat == ""]
adm_data = adm_data[,c(1,3,4)] %>%
  distinct()
eq_table = openxlsx::read.xlsx(here::here("data", "cat_groupings.xlsx")) %>%
  select(cat, cat_ag)
adm_data = adm_data %>%
  left_join(eq_table, by = "cat") %>%
  mutate(staff = grepl("PE-", id))

all_files = list.files(here::here("data", "contact"))

simu_files = grep("Simulated", all_files, value = T)
simu_record_files = grep("Record", all_files, value = T)
random_files = grep("Random", all_files, value = T)

data = read.csv2(here::here("data", "contact", "toy_mat_ctc.csv"))

graph_data = data %>%
  mutate(date_posix = as_date(date_posix)) %>%
  mutate(date_posix = floor_date(date_posix, "day")) %>%
  filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
  group_by(from, to, date_posix) %>%
  summarise(length = sum(length)) %>%
  ungroup


## SIMULATED #####

all_degrees_simu = rep(0,length(unique(graph_data$date_posix))*length(simu_files))
all_densities_simu = rep(0,length(unique(graph_data$date_posix))*length(simu_files))
all_transitivities_simu = rep(0,length(unique(graph_data$date_posix))*length(simu_files))
all_assortativities_simu = rep(0,length(unique(graph_data$date_posix))*length(simu_files))

index=1

for(f in simu_files){
  
  data = read.csv2(here::here("data", "contact", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct

  for(d in unique(graph_data$date_posix)){
    
    # full network
    data_d = graph_data %>%
      filter(date_posix == d)
    
    graph_d = graph_from_data_frame(data_d, directed = F)
    graph_d = simplify(graph_d)
    
    all_degrees_simu[index] = mean(degree(graph_d))
    all_densities_simu[index] = edge_density(graph_d)
    all_transitivities_simu[index] = transitivity(graph_d)
    all_assortativities_simu[index] = assortativity.degree(graph_d, directed = F)
    
    index=1
  }
  
}


## SIMULATED WITH RECORD #####

all_degrees_simu_record = c()
all_densities_simu_record = c()
all_transitivities_simu_record = c()
all_assortativities_simu_record = c()

index=1

for(f in simu_record_files){
  
  data = read.csv2(here::here("data", "contact", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct
  
  for(d in unique(graph_data$date_posix)){
    
    # full network
    data_d = graph_data %>%
      filter(date_posix == d)
    
    graph_d = graph_from_data_frame(data_d, directed = F)
    graph_d = simplify(graph_d)
    
    all_degrees_simu_record[index] = mean(degree(graph_d))
    all_densities_simu_record[index] = edge_density(graph_d)
    all_transitivities_simu_record[index] = transitivity(graph_d)
    all_assortativities_simu_record[index] = assortativity.degree(graph_d, directed = F)
    index=index+1
  }
  
}



## RANDOM #####

all_degrees_random = c()
all_densities_random = c()
all_transitivities_random = c()
all_assortativities_random = c()

index=1

for(f in random_files){
  
  data = read.csv2(here::here("data", "contact", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct
  
  for(d in unique(graph_data$date_posix)){
    
    # full network
    data_d = graph_data %>%
      filter(date_posix == d)
    
    graph_d = graph_from_data_frame(data_d, directed = F)
    graph_d = simplify(graph_d)
    
    all_degrees_random[index] = mean(all_degrees_random, degree(graph_d))
    all_densities_random[index] = edge_density(graph_d)
    all_transitivities_random[index] = transitivity(graph_d)
    all_assortativities_random[index] = assortativity.degree(graph_d, directed = F)
    
  }
  
}

simu_data = data.frame(degrees = all_degrees_simu,
                       densities = all_densities_simu,
                       transitivities = all_transitivities_simu,
                       assortativities = all_assortativities_simu,
                       network = "Simulated")
simu_record_data = data.frame(degrees = all_degrees_simu_record,
                       densities = all_densities_simu_record,
                       transitivities = all_transitivities_simu_record,
                       assortativities = all_assortativities_simu_record,
                       network = "Simulated with record")
random_data = data.frame(degrees = all_degrees_random,
                       densities = all_densities_random,
                       transitivities = all_transitivities_random,
                       assortativities = all_assortativities_random,
                       network = "Random")

summary_data = rbind(simu_data, simu_record_data, random_data)

ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = degrees)) +
  theme_bw()

