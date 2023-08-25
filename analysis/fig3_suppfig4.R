
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)

source(here::here("Analysis", "helper_functions.R"))

options(dplyr.summarise.inform = FALSE)
pal = c("grey60", "grey20", "#E41A1C", "#8128c9", "#ba91db")

adm_data = read.csv(here::here("Data", "Observed", "toy_admission.csv"), sep=";") %>%
  select(id, hospitalization, cat, ward)
adm_data$cat[adm_data$cat == ""] = adm_data$hospitalization[adm_data$cat == ""]
adm_data = adm_data[,c(1,3,4)] %>%
  distinct()
eq_table = openxlsx::read.xlsx(here::here("Data", "Observed", "cat_groupings.xlsx")) %>%
  select(cat, cat_ag)
adm_data = adm_data %>%
  left_join(eq_table, by = "cat") %>%
  mutate(staff = grepl("PE-", id))

data = read.csv2(here::here("Data", "Observed", "toy_mat_ctc.csv"))

graph_data = data %>%
  mutate(date_posix = as_date(date_posix)) %>%
  mutate(date_posix = floor_date(date_posix, "day")) %>%
  filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
  select(from, to, date_posix) %>%
  distinct %>%
  arrange(date_posix)

all_files = list.files(here::here("Data", "Synthetic"))

simu_files = grep("BuiltSimulated", all_files, value = T)
simu_record_files = grep("Record", all_files, value = T)
resimu_files = grep("ReSimulated", all_files, value = T)
random_files = grep("RandomGraphN", all_files, value = T)
random_record_files = grep("RandomGraph2", all_files, value = T)


## Reconstructed full #####

iter=1

simu_data = data.frame()

for(f in simu_files){
  
  data = read.csv2(here::here("Data", "Synthetic", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  simu_data = rbind(simu_data,
                    get_net_metrics(graph_data, iter, "Reconstructed"))
  
  iter=iter+1
}


## Reconstructed bias #####

iter=1

simu_record_data = data.frame()

for(f in simu_record_files){
  
  data = read.csv2(here::here("Data", "Synthetic", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  simu_record_data = rbind(simu_record_data,
                           get_net_metrics(graph_data, iter, "Reconstructed (bias)"))
  
  iter=iter+1
}

## Random #####

iter=1

random_data = data.frame()

for(f in random_files){
  
  data = read.csv2(here::here("Data", "Synthetic", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  random_data = rbind(random_data,
                      get_net_metrics(graph_data, iter, "Random"))
  
  iter=iter+1
}

## Random bias #####

iter=1

random_record_data = data.frame()

for(f in random_record_files){
  
  data = read.csv2(here::here("Data", "Synthetic", f))
  
  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)
  
  random_record_data = rbind(random_record_data,
                             get_net_metrics(graph_data, iter, "Random (bias)"))
  
  iter=iter+1
}



## Observed #####

data = read.csv2(here::here("Data", "Observed", "toy_mat_ctc.csv"))

graph_data = data %>%
  mutate(date_posix = as_date(date_posix)) %>%
  mutate(date_posix = floor_date(date_posix, "day")) %>%
  filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
  select(from, to, date_posix) %>%
  distinct() %>%
  arrange(date_posix)

observed_data = get_net_metrics(graph_data, 1, "Observed")



# GROUPED #####

summary_data = rbind(simu_data, simu_record_data, random_data,
                     random_record_data, observed_data)

# for convenience, save the summary here and read it in later
# write.csv(summary_data, here::here("Data", "summary_data.csv"), row.names = F)
# summary_data = read.csv(here::here("Data", "summary_data.csv"))

summary_data = summary_data %>%
  mutate(network = factor(network,
                          levels = c("Random", "Random (bias)", "Observed",
                                     "Reconstructed (bias)", "Reconstructed")))

summary_data = summary_data %>%
  group_by(day, network) %>%
  summarise(across(everything(), median)) %>%
  select(-iter) %>%
  ungroup

summary_data %>%
  select(-day) %>%
  group_by(network) %>%
  summarise(across(everything(), \(x) quantile(x, probs=0.75))) %>%
  View

pa = ggplot() +
  geom_boxplot(data=summary_data,
               aes(x = network, y = degrees, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Degree (daily)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))

pb = ggplot() +
  geom_boxplot(data=summary_data,
               aes(x = network, y = efficiencies, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Global efficiency") +
  scale_y_continuous(breaks = seq(0,0.8,0.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))

pc = ggplot() +
  geom_boxplot(data=summary_data,
               aes(x = network, y = transitivities, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Transitivity") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))

pd = ggplot() +
  geom_boxplot(data=summary_data,
               aes(x = network, y = assortativities, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Assortativity (degree)") +
  theme(axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12))

pe = ggplot() +
  geom_boxplot(data=summary_data,
               aes(x = network, y = assortativities_ward, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Assortativity (ward)") +
  theme(axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12))

pf = ggplot() +
  geom_boxplot(data=summary_data%>%filter(temp_corr>0),
               aes(x = network, y = temp_corr, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Temporal correlation") +
  theme(axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12))

plot_grid(pa,pb,pc,pd,pe,pf, nrow=2,
                    labels = c("a)", "b)", "c)", "d)", "e)", "f)"),
          hjust = 0, vjust=1, align = "v", rel_heights = c(0.77, 1))

ggsave(here::here("Figures", "fig3.png"), width = 12, height = 8)


ggplot() +
  geom_boxplot(data=summary_data,
               aes(x = network, y = densities, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  scale_y_continuous(breaks = seq(0,0.3,0.05)) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Density") +
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12))

ggsave(here::here("Figures", "suppfig4.png"))
