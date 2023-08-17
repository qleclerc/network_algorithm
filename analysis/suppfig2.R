
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)
library(RColorBrewer)

options(dplyr.summarise.inform = FALSE)
pal = brewer.pal(5, "Set1")

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

data = read.csv2(here::here("data", "toy_mat_ctc.csv"))

graph_data = data %>%
  mutate(date_posix = as_date(date_posix)) %>%
  mutate(date_posix = floor_date(date_posix, "day")) %>%
  filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
  group_by(from, to, date_posix) %>%
  summarise(length = sum(length)) %>%
  ungroup

all_files = list.files(here::here("data", "contact"))

random_files = grep("RandomGraphN", all_files, value = T)
random_record_files = grep("RandomGraph2", all_files, value = T)


## RANDOM #####

all_degrees_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
all_densities_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
all_transitivities_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
all_assortativities_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
all_assortativities_ward_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
all_efficiencies_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
all_iters_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))

index=1
iter=1

for(f in random_files){
  
  data = read.csv2(here::here("data", "contact", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  for(d in unique(graph_data$date_posix)){
    
    # full network
    data_d = graph_data %>%
      filter(date_posix == d)
    
    graph_d = graph_from_data_frame(data_d, directed = F)
    graph_d = simplify(graph_d)
    vertex_atts = data.frame(id = get.vertex.attribute(graph_d, "name")) %>%
      left_join(adm_data, "id") %>%
      mutate(ward = replace(ward, ward == "Menard 1", "Neurologic (1)"),
             ward = replace(ward, ward == "Menard 2", "Neurologic (2)"),
             ward = replace(ward, ward == "Sorrel 0", "Nutrition"),
             ward = replace(ward, ward == "Sorrel 1", "Neurologic (3)"),
             ward = replace(ward, ward == "Sorrel 2", "Geriatric"),
             ward = replace(ward, ward == "Other", "Mobile"))
    graph_d = graph_d %>%
      set_vertex_attr("cat", value = vertex_atts$cat) %>%
      set_vertex_attr("cat_ag", value = vertex_atts$cat_ag) %>%
      set_vertex_attr("staff", value = vertex_atts$staff) %>%
      set_vertex_attr("ward", value = vertex_atts$ward)
    
    all_degrees_random[index] = mean(degree(graph_d))
    all_densities_random[index] = edge_density(graph_d)
    all_transitivities_random[index] = transitivity(graph_d)
    all_assortativities_random[index] = assortativity.degree(graph_d, directed = F)
    all_assortativities_ward_random[index] = assortativity.nominal(graph_d, as.factor(V(graph_d)$ward), directed = F)
    all_efficiencies_random[index] = global_efficiency(graph_d, directed = F)
    all_iters_random[index] = iter
    index=index+1
  }
  iter=iter+1
}



##RANDOM RECORD #####

all_degrees_random_record = rep(0,length(unique(graph_data$date_posix))*length(random_record_files))
all_densities_random_record = rep(0,length(unique(graph_data$date_posix))*length(random_record_files))
all_transitivities_random_record = rep(0,length(unique(graph_data$date_posix))*length(random_record_files))
all_assortativities_random_record = rep(0,length(unique(graph_data$date_posix))*length(random_record_files))
all_assortativities_ward_random_record = rep(0,length(unique(graph_data$date_posix))*length(random_record_files))
all_efficiencies_random_record = rep(0,length(unique(graph_data$date_posix))*length(random_record_files))
all_iters_random_record = rep(0,length(unique(graph_data$date_posix))*length(random_record_files))

index=1
iter=1

for(f in random_record_files){
  
  data = read.csv2(here::here("data", "contact", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  for(d in unique(graph_data$date_posix)){
    
    # full network
    data_d = graph_data %>%
      filter(date_posix == d)
    
    graph_d = graph_from_data_frame(data_d, directed = F)
    graph_d = simplify(graph_d)
    vertex_atts = data.frame(id = get.vertex.attribute(graph_d, "name")) %>%
      left_join(adm_data, "id") %>%
      mutate(ward = replace(ward, ward == "Menard 1", "Neurologic (1)"),
             ward = replace(ward, ward == "Menard 2", "Neurologic (2)"),
             ward = replace(ward, ward == "Sorrel 0", "Nutrition"),
             ward = replace(ward, ward == "Sorrel 1", "Neurologic (3)"),
             ward = replace(ward, ward == "Sorrel 2", "Geriatric"),
             ward = replace(ward, ward == "Other", "Mobile"))
    graph_d = graph_d %>%
      set_vertex_attr("cat", value = vertex_atts$cat) %>%
      set_vertex_attr("cat_ag", value = vertex_atts$cat_ag) %>%
      set_vertex_attr("staff", value = vertex_atts$staff) %>%
      set_vertex_attr("ward", value = vertex_atts$ward)
    
    all_degrees_random_record[index] = mean(degree(graph_d))
    all_densities_random_record[index] = edge_density(graph_d)
    all_transitivities_random_record[index] = transitivity(graph_d)
    all_assortativities_random_record[index] = assortativity.degree(graph_d, directed = F)
    all_assortativities_ward_random_record[index] = assortativity.nominal(graph_d, as.factor(V(graph_d)$ward), directed = F)
    all_efficiencies_random_record[index] = global_efficiency(graph_d, directed = F)
    all_iters_random_record[index] = iter
    index=index+1
  }
  iter=iter+1
}



# GROUPED #####

random_data = data.frame(degrees = all_degrees_random,
                         densities = all_densities_random,
                         transitivities = all_transitivities_random,
                         assortativities = all_assortativities_random,
                         assortativities_ward = all_assortativities_ward_random,
                         efficiencies = all_efficiencies_random,
                         iter = all_iters_random,
                         network = "Random")
random_record_data = data.frame(degrees = all_degrees_random_record,
                         densities = all_densities_random_record,
                         transitivities = all_transitivities_random_record,
                         assortativities = all_assortativities_random_record,
                         assortativities_ward = all_assortativities_ward_random_record,
                         efficiencies = all_efficiencies_random_record,
                         iter = all_iters_random_record,
                         network = "Random (bias)")

summary_data = rbind(random_data, random_record_data)
summary_data$day = rep(c(1:28), max(summary_data$iter)*length(unique(summary_data$network)))


## COMBINED #####

summary_data = summary_data %>%
  group_by(day, network) %>%
  summarise(across(everything(), median)) %>%
  select(-iter)

pa = ggplot() +
  geom_boxplot(data=summary_data,
               aes(x = network, y = degrees, colour = network)) +
  scale_colour_discrete(type = c(pal[1], "darkred")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Degree (daily)")

pb = ggplot() +
  geom_boxplot(data=summary_data, aes(x = network, y = efficiencies, colour = network)) +
  scale_colour_discrete(type = c(pal[1], "darkred")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Global efficiency")

pc = ggplot() +
  geom_boxplot(data=summary_data, aes(x = network, y = densities, colour = network)) +
  scale_colour_discrete(type = c(pal[1], "darkred")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Density")

pd = ggplot() +
  geom_boxplot(data=summary_data, aes(x = network, y = transitivities, colour = network)) +
  scale_colour_discrete(type = c(pal[1], "darkred")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Transitivity")

pe = ggplot() +
  geom_boxplot(data=summary_data, aes(x = network, y = assortativities, colour = network)) +
  scale_colour_discrete(type = c(pal[1], "darkred")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Assortativity (degree)")

pf = ggplot() +
  geom_boxplot(data=summary_data, aes(x = network, y = assortativities_ward, colour = network)) +
  scale_colour_discrete(type = c(pal[1], "darkred")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Assortativity (ward)")

plot_grid(pa,pb,pc,pd,pe,pf, ncol=2, labels=c("a)", "b)", "c)", "d)", "e)","f)"), hjust = 0)

ggsave(here::here("figures", "suppfig2.png"), width = 10, height = 7.1)

