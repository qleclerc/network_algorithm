
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)
library(RColorBrewer)

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

all_files = list.files(here::here("data", "meetProb"))
all_files = grep("BuiltSimulated", all_files, value = T)
prob_0.1_files = grep("0.1", all_files, value = T)
prob_0.5_files = grep("0.5", all_files, value = T)
prob_0.9_files = grep("0.9", all_files, value = T)


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
                             get_net_metrics(graph_data, iter, "Full"))
  
  iter=iter+1
}

## MEET PROBA 0.1 #####

iter=1

prob_0.1_data = data.frame()

for(f in prob_0.1_files){
  
  data = read.csv2(here::here("data", "meetProb", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  prob_0.1_data = rbind(prob_0.1_data,
                    get_net_metrics(graph_data, iter, "0.1"))
  
  iter=iter+1
}


## MEET PROBA 0.5 #####

iter=1

prob_0.5_data = data.frame()

for(f in prob_0.5_files){
  
  data = read.csv2(here::here("data", "meetProb", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  prob_0.5_data = rbind(prob_0.5_data,
                        get_net_metrics(graph_data, iter, "0.5"))
  
  iter=iter+1
}


## MEET PROBA 0.9 #####

iter=1

prob_0.9_data = data.frame()

for(f in prob_0.9_files){
  
  data = read.csv2(here::here("data", "meetProb", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  prob_0.9_data = rbind(prob_0.9_data,
                        get_net_metrics(graph_data, iter, "0.9"))
  
  iter=iter+1
}

# GROUPED #####


summary_data = rbind(simu_data, prob_0.1_data, prob_0.5_data, prob_0.9_data)

write.csv(summary_data, "meetprob_data.csv", row.names = F)

summary_data = read.csv("meetprob_data.csv")

summary_data = summary_data %>%
  mutate(network = factor(network,
                          levels = c("Full","0.1","0.5","0.9")))

summary_data = summary_data %>%
  group_by(day, network) %>%
  summarise(across(everything(), median)) %>%
  select(-iter)

pa = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = degrees, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Recurring contact probability", y = "Degree (daily)")

pb = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = efficiencies, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Recurring contact probability", y = "Global efficiency")

pc = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = densities, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Recurring contact probability", y = "Density")

pd = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = transitivities, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Recurring contact probability", y = "Transitivity")

pe = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = assortativities, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Recurring contact probability", y = "Assortativity (degree)")

pf = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = assortativities_ward, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Recurring contact probability", y = "Assortativity (ward)")

pg = ggplot(summary_data%>%filter(temp_corr>0)) +
  geom_boxplot(aes(x = network, y = temp_corr, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Recurring contact probability", y = "Temporal correlation")

plot_grid(pa,pb,pc,pd,pe,pf,pg, ncol=2, labels=c("a)", "b)", "c)", "d)", "e)","f)","g)"), hjust = 0)

ggsave(here::here("figures", "suppfig5.png"), width = 10, height = 7.1)

