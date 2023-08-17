
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)
library(RColorBrewer)

source("helper_functions.r")

options(dplyr.summarise.inform = FALSE)
pal = c(brewer.pal(5, "Set1")[1], brewer.pal(3, "Blues")[-1], brewer.pal(5, "Greens")[-1])

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
simu_files = grep("BuiltSimulated", all_files, value = T)

all_files = list.files(here::here("data", "truncated"))
all_files = grep("BuiltSimulated", all_files, value = T)
first_half_files = grep("first_half", all_files, value = T)
second_half_files = grep("second_half", all_files, value = T)
first_quarter_files = grep("first_quarter", all_files, value = T)
second_quarter_files = grep("second_quarter", all_files, value = T)
third_quarter_files = grep("third_quarter", all_files, value = T)
fourth_quarter_files = grep("fourth_quarter", all_files, value = T)


## SIMULATED #####

iter=1

simu_data = data.frame()

for(f in simu_files){
  
  data = read.csv2(here::here("data", "contact", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  simu_data = rbind(simu_data,
                    get_net_metrics(graph_data, iter, "Complete"))
  
  iter=iter+1
}


## FIRST HALF #####

iter=1

first_half_data = data.frame()

for(f in first_half_files){
  
  data = read.csv2(here::here("data", "truncated", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  first_half_data = rbind(first_half_data,
                    get_net_metrics(graph_data, iter, "1st half"))
  
  iter=iter+1
}


## SECOND HALF #####

iter=1

second_half_data = data.frame()

for(f in second_half_files){
  
  data = read.csv2(here::here("data", "truncated", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  second_half_data = rbind(second_half_data,
                          get_net_metrics(graph_data, iter, "2nd half"))
  
  iter=iter+1
}


## FIRST QUARTER #####

iter=1

first_quarter_data = data.frame()

for(f in first_quarter_files){
  
  data = read.csv2(here::here("data", "truncated", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  first_quarter_data = rbind(first_quarter_data,
                           get_net_metrics(graph_data, iter, "1st quarter"))
  
  iter=iter+1
}




## SECOND QUARTER #####

iter=1

second_quarter_data = data.frame()

for(f in second_quarter_files){
  
  data = read.csv2(here::here("data", "truncated", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  second_quarter_data = rbind(second_quarter_data,
                             get_net_metrics(graph_data, iter, "2nd quarter"))
  
  iter=iter+1
}





## third QUARTER #####

iter=1

third_quarter_data = data.frame()

for(f in third_quarter_files){
  
  data = read.csv2(here::here("data", "truncated", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  third_quarter_data = rbind(third_quarter_data,
                              get_net_metrics(graph_data, iter, "3rd quarter"))
  
  iter=iter+1
}



## fourth QUARTER #####

iter=1

fourth_quarter_data = data.frame()

for(f in fourth_quarter_files){
  
  data = read.csv2(here::here("data", "truncated", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  fourth_quarter_data = rbind(fourth_quarter_data,
                             get_net_metrics(graph_data, iter, "4th quarter"))
  
  iter=iter+1
}




# GROUPED #####

summary_data = rbind(simu_data, first_half_data, second_half_data,
                     first_quarter_data, second_quarter_data, third_quarter_data, fourth_quarter_data)

write.csv(summary_data, "truncated_data.csv", row.names = F)

summary_data = read.csv("truncated_data.csv")

summary_data = summary_data %>%
  mutate(network = factor(network,
                          levels = c("Complete",
                                     "1st half",
                                     "2nd half",
                                     "1st quarter",
                                     "2nd quarter",
                                     "3rd quarter",
                                     "4th quarter")))

summary_data = summary_data %>%
  group_by(day, network) %>%
  summarise(across(everything(), median)) %>%
  select(-iter)

pa = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = degrees, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Period used", y = "Degree (daily)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pb = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = efficiencies, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Period used", y = "Global efficiency") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pc = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = densities, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Period used", y = "Density") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pd = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = transitivities, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Period used", y = "Transitivity") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pe = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = assortativities, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Period used", y = "Assortativity (degree)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pf = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = assortativities_ward, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Period used", y = "Assortativity (ward)")

pg = ggplot(summary_data%>%filter(temp_corr>0)) +
  geom_boxplot(aes(x = network, y = temp_corr, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Period used", y = "Temporal correlation")

plot_grid(plot_grid(pa,pc,pe,pg, ncol=1, rel_heights = c(1,1,1,1.2),
                    labels = c("a)", "c)", "e)", "g)"), hjust = 0, vjust=1, align = "v"),
          plot_grid(pb,pd,pf,NULL, ncol=1, rel_heights = c(1,1,1.2,1), 
                    labels = c("b)", "d)", "f)", ""), hjust = 0, vjust=1, align = "v"))

ggsave(here::here("figures", "suppfig4.png"), width = 11, height = 8)

