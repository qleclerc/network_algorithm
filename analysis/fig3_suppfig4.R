
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)
library(ggsignif)

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

p_to_sig = function(pval){
  
  pval = pval*6
  
  if(pval <= 0.001) sig = "***"
  else if(pval <= 0.01) sig = "**"
  else if(pval <= 0.05) sig = "*"
  else sig = "ns"
   
  return(sig)     
      
}


pa = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = degrees, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  geom_signif(aes(x = network, y = degrees),
              comparisons = list(c("Random", "Random (bias)"),
                                 c("Random (bias)", "Observed"),
                                 c("Observed", "Reconstructed (bias)"),
                                 c("Reconstructed (bias)", "Reconstructed"),
                                 c("Random (bias)", "Reconstructed (bias)"),
                                 c("Random", "Reconstructed")),
              map_signif_level = p_to_sig, tip_length = 0.01, y_position = c(41, 42, 22, 23, 45, 48)) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Degree (daily)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))

pb = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = efficiencies, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  geom_signif(aes(x = network, y = efficiencies),
              comparisons = list(c("Random", "Random (bias)"),
                                 c("Random (bias)", "Observed"),
                                 c("Observed", "Reconstructed (bias)"),
                                 c("Reconstructed (bias)", "Reconstructed"),
                                 c("Random (bias)", "Reconstructed (bias)"),
                                 c("Random", "Reconstructed")),
              map_signif_level = p_to_sig, tip_length = 0.01, y_position = c(0.63, 0.65, 0.51, 0.53, 0.68, 0.71)) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Global efficiency") +
  scale_y_continuous(breaks = seq(0,0.8,0.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))

pc = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = transitivities, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  geom_signif(aes(x = network, y = transitivities),
              comparisons = list(c("Random", "Random (bias)"),
                                 c("Random (bias)", "Observed"),
                                 c("Observed", "Reconstructed (bias)"),
                                 c("Reconstructed (bias)", "Reconstructed"),
                                 c("Random (bias)", "Reconstructed (bias)"),
                                 c("Random", "Reconstructed")),
              map_signif_level = p_to_sig, tip_length = 0.01, y_position = c(0.37, 0.40, 0.42, 0.40, 0.44, 0.46)) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Transitivity") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))

pd = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = assortativities, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  geom_signif(aes(x = network, y = assortativities),
              comparisons = list(c("Random", "Random (bias)"),
                                 c("Random (bias)", "Observed"),
                                 c("Observed", "Reconstructed (bias)"),
                                 c("Reconstructed (bias)", "Reconstructed"),
                                 c("Random (bias)", "Reconstructed (bias)"),
                                 c("Random", "Reconstructed")),
              map_signif_level = p_to_sig, tip_length = 0.01, y_position = c(0.13, 0.09, 0.11, 0.02, 0.15, 0.19)) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Assortativity (degree)") +
  theme(axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12))

pe = ggplot(summary_data) +
  geom_boxplot(aes(x = network, y = assortativities_ward, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  geom_signif(aes(x = network, y = assortativities_ward),
              comparisons = list(c("Random", "Random (bias)"),
                                 c("Random (bias)", "Observed"),
                                 c("Observed", "Reconstructed (bias)"),
                                 c("Reconstructed (bias)", "Reconstructed"),
                                 c("Random (bias)", "Reconstructed (bias)"),
                                 c("Random", "Reconstructed")),
              map_signif_level = p_to_sig, tip_length = 0.01, y_position = c(0.02, 0.72, 0.74, 0.7, 0.78, 0.825)) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Assortativity (ward)") +
  theme(axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12))

pf = ggplot(summary_data%>%filter(temp_corr>0)) +
  geom_boxplot(aes(x = network, y = temp_corr, colour = network), linewidth = 0.8) +
  scale_colour_discrete(type = pal) +
  geom_signif(aes(x = network, y = temp_corr),
              comparisons = list(c("Random", "Random (bias)"),
                                 c("Random (bias)", "Observed"),
                                 c("Observed", "Reconstructed (bias)"),
                                 c("Reconstructed (bias)", "Reconstructed"),
                                 c("Random (bias)", "Reconstructed (bias)"),
                                 c("Random", "Reconstructed")),
              map_signif_level = p_to_sig, tip_length = 0.01, y_position = c(0.3, 0.74, 0.76, 0.62, 0.79, 0.83)) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Temporal correlation") +
  theme(axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12))

plot_grid(pa,pb,pc,pd,pe,pf, nrow=2,
          labels = c("a)", "b)", "c)", "d)", "e)", "f)"),
          hjust = 0, vjust=1, align = "v", rel_heights = c(0.77, 1))

ggsave(here::here("Figures", "fig3.png"), width = 12, height = 9)


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
