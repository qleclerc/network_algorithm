
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

data = read.csv2(here::here("data", "contact", "toy_mat_ctc.csv"))

graph_data = data %>%
  mutate(date_posix = as_date(date_posix)) %>%
  mutate(date_posix = floor_date(date_posix, "day")) %>%
  filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
  group_by(from, to, date_posix) %>%
  summarise(length = sum(length)) %>%
  ungroup

all_files = list.files(here::here("data", "contact"))

simu_files = grep("BuiltSimulated", all_files, value = T)
simu_record_files = grep("Record", all_files, value = T)
resimu_files = grep("ReSimulated", all_files, value = T)
random_files = grep("Random", all_files, value = T)


## SIMULATED #####

all_degrees_simu = rep(0,length(unique(graph_data$date_posix))*length(simu_files))
all_densities_simu = rep(0,length(unique(graph_data$date_posix))*length(simu_files))
all_transitivities_simu = rep(0,length(unique(graph_data$date_posix))*length(simu_files))
all_assortativities_simu = rep(0,length(unique(graph_data$date_posix))*length(simu_files))
all_assortativities_ward_simu = rep(0,length(unique(graph_data$date_posix))*length(simu_files))
all_iters_simu = rep(0,length(unique(graph_data$date_posix))*length(simu_files))

index=1
iter=1

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
    
    all_degrees_simu[index] = mean(degree(graph_d))
    all_densities_simu[index] = edge_density(graph_d)
    all_transitivities_simu[index] = transitivity(graph_d)
    all_assortativities_simu[index] = assortativity.degree(graph_d, directed = F)
    all_assortativities_ward_simu[index] = assortativity.nominal(graph_d, as.factor(V(graph_d)$ward), directed = F)
    all_iters_simu[index] = iter
    index=index+1
  }
  iter=iter+1
}


## SIMULATED WITH RECORD #####

all_degrees_simu_record = rep(0,length(unique(graph_data$date_posix))*length(simu_record_files))
all_densities_simu_record = rep(0,length(unique(graph_data$date_posix))*length(simu_record_files))
all_transitivities_simu_record = rep(0,length(unique(graph_data$date_posix))*length(simu_record_files))
all_assortativities_simu_record = rep(0,length(unique(graph_data$date_posix))*length(simu_record_files))
all_assortativities_ward_simu_record = rep(0,length(unique(graph_data$date_posix))*length(simu_record_files))
all_iters_simu_record = rep(0,length(unique(graph_data$date_posix))*length(simu_record_files))

index=1
iter=1

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
    
    all_degrees_simu_record[index] = mean(degree(graph_d))
    all_densities_simu_record[index] = edge_density(graph_d)
    all_transitivities_simu_record[index] = transitivity(graph_d)
    all_assortativities_simu_record[index] = assortativity.degree(graph_d, directed = F)
    all_assortativities_ward_simu_record[index] = assortativity.nominal(graph_d, as.factor(V(graph_d)$ward), directed = F)
    all_iters_simu_record[index] = iter
    index=index+1
  }
  iter=iter+1
}


## RESIMULATED #####

all_degrees_resimu = rep(0,length(unique(graph_data$date_posix))*length(resimu_files))
all_densities_resimu = rep(0,length(unique(graph_data$date_posix))*length(resimu_files))
all_transitivities_resimu = rep(0,length(unique(graph_data$date_posix))*length(resimu_files))
all_assortativities_resimu = rep(0,length(unique(graph_data$date_posix))*length(resimu_files))
all_assortativities_ward_resimu = rep(0,length(unique(graph_data$date_posix))*length(resimu_files))
all_iters_simu = rep(0,length(unique(graph_data$date_posix))*length(resimu_files))

index=1
iter=1

for(f in resimu_files){
  
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
    
    all_degrees_resimu[index] = mean(degree(graph_d))
    all_densities_resimu[index] = edge_density(graph_d)
    all_transitivities_resimu[index] = transitivity(graph_d)
    all_assortativities_resimu[index] = assortativity.degree(graph_d, directed = F)
    all_assortativities_ward_resimu[index] = assortativity.nominal(graph_d, as.factor(V(graph_d)$ward), directed = F)
    all_iters_simu[index] = iter
    index=index+1
  }
  iter=iter+1
}




## RANDOM #####

all_degrees_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
all_densities_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
all_transitivities_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
all_assortativities_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
all_assortativities_ward_random = rep(0,length(unique(graph_data$date_posix))*length(random_files))
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
    distinct
  
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
    all_iters_random[index] = iter
    index=index+1
  }
  iter=iter+1
}

simu_data = data.frame(degrees = all_degrees_simu,
                       densities = all_densities_simu,
                       transitivities = all_transitivities_simu,
                       assortativities = all_assortativities_simu,
                       assortativities_ward = all_assortativities_ward_simu,
                       iter = all_iters_simu,
                       network = "Simulated")
simu_record_data = data.frame(degrees = all_degrees_simu_record,
                              densities = all_densities_simu_record,
                              transitivities = all_transitivities_simu_record,
                              assortativities = all_assortativities_simu_record,
                              assortativities_ward = all_assortativities_ward_simu_record,
                              iter = all_iters_simu_record,
                              network = "Simulated (bias)")
resimu_data = data.frame(degrees = all_degrees_resimu,
                       densities = all_densities_resimu,
                       transitivities = all_transitivities_resimu,
                       assortativities = all_assortativities_resimu,
                       assortativities_ward = all_assortativities_ward_resimu,
                       iter = all_iters_resimu,
                       network = "Re-simulated")
random_data = data.frame(degrees = all_degrees_random,
                         densities = all_densities_random,
                         transitivities = all_transitivities_random,
                         assortativities = all_assortativities_random,
                         assortativities_ward = all_assortativities_ward_random,
                         iter = all_iters_random,
                         network = "Random")

summary_data = rbind(simu_data, simu_record_data, resimu_data, random_data)

write.csv(summary_data, "summary_data.csv", row.names = F)


summary_data = read.csv("summary_data.csv")

## REAL #####

data = read.csv2(here::here("data", "contact", "toy_mat_ctc.csv"))

graph_data = data %>%
  mutate(date_posix = as_date(date_posix)) %>%
  mutate(date_posix = floor_date(date_posix, "day")) %>%
  filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
  group_by(from, to, date_posix) %>%
  summarise(length = sum(length)) %>%
  ungroup

all_degrees = c()
all_densities = c()
all_transitivities = c()
all_assortativities = c()
all_assortativities_ward = c()

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
  
  all_degrees = c(all_degrees, degree(graph_d))
  all_densities = c(all_densities, edge_density(graph_d))
  all_transitivities = c(all_transitivities, transitivity(graph_d))
  all_assortativities = c(all_assortativities,assortativity.degree(graph_d, directed = F))
  all_assortativities_ward = c(all_assortativities_ward,assortativity.nominal(graph_d, as.factor(V(graph_d)$ward), directed = F))
  
}


pa = ggplot() +
  geom_boxplot(data=summary_data%>%filter(iter==1), aes(x = network, y = degrees, colour = network)) +
  geom_boxplot(aes(x = "Real", y = all_degrees, colour = "Real")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Degree (daily)")

pb = ggplot() +
  geom_boxplot(data=summary_data%>%filter(iter==1), aes(x = network, y = densities, colour = network)) +
  geom_boxplot(aes(x = "Real", y = all_densities, colour = "Real")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Density")

pc = ggplot() +
  geom_boxplot(data=summary_data%>%filter(iter==1), aes(x = network, y = transitivities, colour = network)) +
  geom_boxplot(aes(x = "Real", y = all_transitivities, colour = "Real")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Transitivity")

pd = ggplot() +
  geom_boxplot(data=summary_data%>%filter(iter==1), aes(x = network, y = assortativities, colour = network)) +
  geom_boxplot(aes(x = "Real", y = all_assortativities, colour = "Real")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Assortativity (degree)")

pe = ggplot() +
  geom_boxplot(data=summary_data%>%filter(iter==1), aes(x = network, y = assortativities_ward, colour = network)) +
  geom_boxplot(aes(x = "Real", y = all_assortativities_ward, colour = "Real")) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Assortativity (ward)")

plot_grid(pa,pb,pc,pd,pe, ncol=2, labels=c("a)", "b)", "c)", "d)", "e)"), hjust = 0)

ggsave(here::here("figures", "fig2.png"), width = 8, height = 7.1)

wilcox.test(summary_data %>% filter(network == "Random" & iter == 1) %>% pull(degrees), all_degrees,
            conf.int = T)
wilcox.test(summary_data %>% filter(network == "Simulated" & iter == 1) %>% pull(degrees), all_degrees,
            conf.int = T)
wilcox.test(summary_data %>% filter(network == "Simulated (record)" & iter == 1) %>% pull(degrees), all_degrees,
            conf.int = T)

wilcox.test(summary_data %>% filter(network == "Random" & iter == 1) %>% pull(densities), all_densities,
            conf.int = T)
wilcox.test(summary_data %>% filter(network == "Simulated" & iter == 1) %>% pull(densities), all_densities,
            conf.int = T)
wilcox.test(summary_data %>% filter(network == "Simulated (record)" & iter == 1) %>% pull(densities), all_densities,
            conf.int = T)

wilcox.test(summary_data %>% filter(network == "Random" & iter == 1) %>% pull(transitivities), all_transitivities,
            conf.int = T)
wilcox.test(summary_data %>% filter(network == "Simulated" & iter == 1) %>% pull(transitivities), all_transitivities,
            conf.int = T)
wilcox.test(summary_data %>% filter(network == "Simulated (record)" & iter == 1) %>% pull(transitivities), all_transitivities,
            conf.int = T)

wilcox.test(summary_data %>% filter(network == "Random" & iter == 1) %>% pull(assortativities), all_assortativities,
            conf.int = T)
wilcox.test(summary_data %>% filter(network == "Simulated" & iter == 1) %>% pull(assortativities), all_assortativities,
            conf.int = T)
wilcox.test(summary_data %>% filter(network == "Simulated (record)" & iter == 1) %>% pull(assortativities), all_assortativities,
            conf.int = T)

wilcox.test(summary_data %>% filter(network == "Random" & iter == 1) %>% pull(assortativities_ward), all_assortativities_ward,
            conf.int = T)
wilcox.test(summary_data %>% filter(network == "Simulated" & iter == 1) %>% pull(assortativities_ward), all_assortativities_ward,
            conf.int = T)
wilcox.test(summary_data %>% filter(network == "Simulated (record)" & iter == 1) %>% pull(assortativities_ward), all_assortativities_ward,
            conf.int = T)
