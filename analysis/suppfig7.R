

library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)

source(here::here("Analysis", "helper_functions.R"))

options(dplyr.summarise.inform = FALSE)
pal = c("#984EA3", "grey40")

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
resimu_files = grep("ReSimulated", all_files, value = T)


## Reconstructed #####

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


## Re-simulated #####

iter=1

resimu_data = data.frame()

for(f in resimu_files){

  data = read.csv2(here::here("Data", "Synthetic", f))

  graph_data = data %>%
    mutate(date_posix = as_date(date_posix)) %>%
    mutate(date_posix = floor_date(date_posix, "day")) %>%
    filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
    select(from, to, date_posix) %>%
    distinct %>%
    arrange(date_posix)

  resimu_data = rbind(resimu_data,
                      get_net_metrics(graph_data, iter, "Re-simulated"))

  iter=iter+1
}


## PLOT #####

summary_data = rbind(simu_data, resimu_data)

summary_data = summary_data %>%
  mutate(network = factor(network,
                          levels = c("Reconstructed", "Re-simulated")))

summary_data = summary_data %>%
  group_by(day, network) %>%
  summarise(across(everything(), median)) %>%
  select(-iter) %>%
  ungroup

pa = ggplot() +
  geom_boxplot(data=summary_data,
               aes(x = network, y = degrees, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Degree (daily)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pb = ggplot() +
  geom_boxplot(data=summary_data, aes(x = network, y = efficiencies, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Global efficiency") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pc = ggplot() +
  geom_boxplot(data=summary_data, aes(x = network, y = densities, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Density") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pd = ggplot() +
  geom_boxplot(data=summary_data, aes(x = network, y = transitivities, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Transitivity") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pe = ggplot() +
  geom_boxplot(data=summary_data, aes(x = network, y = assortativities, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Assortativity (degree)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pf = ggplot() +
  geom_boxplot(data=summary_data, aes(x = network, y = assortativities_ward, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Assortativity (ward)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pg = ggplot() +
  geom_boxplot(data=summary_data%>%filter(temp_corr>0), aes(x = network, y = temp_corr, colour = network)) +
  scale_colour_discrete(type = pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(x = "Network", y = "Temporal correlation") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())


## CONTACT NUMBER AND DURATION #####

# SIMU ######

data = read.csv2(here::here("Data", "Synthetic", "matContactBuiltSimulatedCtcNetworks1.csv"))

dur_simu = data %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix < as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  group_by(from, to, date_posix) %>%
  summarise(length = sum(length)) %>%
  pull(length)

distrib_PAPA = data %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PA-PA") %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix < as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  select(-length) %>%
  distinct() %>%
  count(date_posix) %>%
  ungroup() %>%
  mutate(day = wday(date_posix, week_start = 1)) %>%
  mutate(day = as.character(day)) %>%
  mutate(day = replace(day, day %in% c("6", "7"), "Weekend")) %>%
  mutate(day = replace(day, day != "Weekend", "Weekday")) %>%
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix, day) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Patient-patient")

distrib_PEPE = data %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PE") %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix < as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  select(-length) %>%
  distinct() %>%
  count(date_posix) %>%
  ungroup() %>%
  mutate(day = wday(date_posix, week_start = 1)) %>%
  mutate(day = as.character(day)) %>%
  mutate(day = replace(day, day %in% c("6", "7"), "Weekend")) %>%
  mutate(day = replace(day, day != "Weekend", "Weekday")) %>%
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix, day) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-staff")

distrib_PAPE = data %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PA-PE" | type == "PE-PA") %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix < as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  select(-length) %>%
  distinct() %>%
  count(date_posix) %>%
  ungroup() %>%
  mutate(day = wday(date_posix, week_start = 1)) %>%
  mutate(day = as.character(day)) %>%
  mutate(day = replace(day, day %in% c("6", "7"), "Weekend")) %>%
  mutate(day = replace(day, day != "Weekend", "Weekday")) %>%
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix, day) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Patient-staff")

distrib_simu = rbind(distrib_PAPA, distrib_PEPE, distrib_PAPE) %>%
  mutate(date_posix = paste0(date_posix, ":00")) %>%
  mutate(date_posix = factor(date_posix, levels = unique(date_posix))) %>%
  mutate(network = "Reconstructed")


# RESIMU ######

data = read.csv2(here::here("Data", "Synthetic", "matContactBuiltReSimulatedCtcNetworks1.csv"))

dur_resimu = data %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix < as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  group_by(from, to, date_posix) %>%
  summarise(length = sum(length)) %>%
  pull(length)

distrib_PAPA = data %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PA-PA") %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix < as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  select(-length) %>%
  distinct() %>%
  count(date_posix) %>%
  ungroup() %>%
  mutate(day = wday(date_posix, week_start = 1)) %>%
  mutate(day = as.character(day)) %>%
  mutate(day = replace(day, day %in% c("6", "7"), "Weekend")) %>%
  mutate(day = replace(day, day != "Weekend", "Weekday")) %>%
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix, day) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Patient-patient")

distrib_PEPE = data %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PE") %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix < as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  select(-length) %>%
  distinct() %>%
  count(date_posix) %>%
  ungroup() %>%
  mutate(day = wday(date_posix, week_start = 1)) %>%
  mutate(day = as.character(day)) %>%
  mutate(day = replace(day, day %in% c("6", "7"), "Weekend")) %>%
  mutate(day = replace(day, day != "Weekend", "Weekday")) %>%
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix, day) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-staff")

distrib_PAPE = data %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PA-PE" | type == "PE-PA") %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix < as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  select(-length) %>%
  distinct() %>%
  count(date_posix) %>%
  ungroup() %>%
  mutate(day = wday(date_posix, week_start = 1)) %>%
  mutate(day = as.character(day)) %>%
  mutate(day = replace(day, day %in% c("6", "7"), "Weekend")) %>%
  mutate(day = replace(day, day != "Weekend", "Weekday")) %>%
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix, day) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Patient-staff")

distrib_resimu = rbind(distrib_PAPA, distrib_PEPE, distrib_PAPE) %>%
  mutate(date_posix = paste0(date_posix, ":00")) %>%
  mutate(date_posix = factor(date_posix, levels = unique(date_posix))) %>%
  mutate(network = "Re-simulated")


distrib_all = rbind(distrib_simu, distrib_resimu) %>%
  mutate(network = factor(network, levels = unique(network))) %>%
  mutate(type = factor(type, levels = unique(type))) %>%
  mutate(date_posix = factor(date_posix, levels = c("0:00","1:00","2:00","3:00","4:00",
                                                    "5:00","6:00","7:00","8:00","9:00",
                                                    "10:00","11:00","12:00","13:00","14:00",
                                                    "15:00","16:00","17:00","18:00","19:00",
                                                    "20:00","21:00","22:00","23:00")))

pi = ggplot(distrib_all) +
  geom_ribbon(aes(date_posix, ymin = q25, ymax = q75, fill = network, group = interaction(type, network, day)),
              alpha = 0.3) +
  geom_point(aes(date_posix, med, colour = network)) +
  geom_line(aes(date_posix, med, colour = network, group = interaction(type, network, day))) +
  scale_colour_discrete(type = pal) +
  scale_fill_discrete(type = pal) +
  scale_y_continuous(breaks = seq(0,300,75)) +
  scale_x_discrete(breaks = c("0:00", "4:00", "8:00", "12:00",
                              "16:00","20:00")) +
  facet_grid(cols = vars(type), rows = vars(day)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))  +
  labs(x = "Hour", y = "Number of unique contacts", colour = "Network:", fill = "Network:")

ph = ggplot() +
  geom_boxplot(aes(y=dur_simu/60, x = "1", colour = "Reconstructed"), outlier.shape = NA) +
  geom_boxplot(aes(y=dur_simu/60, x = "2", colour = "Re-simulated"), outlier.shape = NA) +
  scale_colour_manual(values = pal, breaks = c("Reconstructed",
                                               "Re-simulated")) +
  scale_x_discrete(labels = c("Reconstructed","Re-simulated")) +
  coord_cartesian(ylim = c(0,35)) +
  guides(colour="none") +
  theme_bw() +
  labs(y = "Contact duration (minutes)", x = "Network") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())


plot_grid(plot_grid(pa,pb,pc,pd,pe,pf,pg,ph, nrow=2,
                    labels = c("a)", "b)", "c)", "d)",
                               "e)", "f)", "g)", "h)"),
                    hjust = 0, vjust=1, align = "v"),
          pi, ncol = 1, rel_heights = c(1,0.7))

ggsave(here::here("Figures", "suppfig7.png"), width = 8, height = 8)
