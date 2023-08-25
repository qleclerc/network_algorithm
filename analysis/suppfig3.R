
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)
library(RColorBrewer)

source(here::here("Analysis", "helper_functions.r"))

pal = brewer.pal(6, "Set2")

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
  mutate(date_posix = as_datetime(date_posix)) %>%
  mutate(date_posix = floor_date(date_posix, "day")) %>%
  filter(date_posix >= as_date("2009-07-27") & date_posix < as_date("2009-08-24")) %>%
  group_by(from, to, date_posix) %>%
  summarise(length = sum(length)) %>%
  ungroup %>%
  arrange(date_posix)

graph_data_PA_PA = graph_data %>%
  filter(grepl("PA-", from) & grepl("PA-", to))

graph_data_PE_PE = graph_data %>%
  filter(grepl("PE-", from) & grepl("PE-", to))

graph_data_PA_PE = graph_data %>%
  filter((grepl("PE-", from) & grepl("PA-", to)) | (grepl("PA-", from) & grepl("PE-", to)))

all_metrics = get_net_metrics(graph_data, network = "Full")
PA_PA_metrics = get_net_metrics(graph_data_PA_PA, network = "Patient-patient")
PE_PE_metrics = get_net_metrics(graph_data_PE_PE, network = "Staff-staff")
PA_PE_metrics = get_net_metrics(graph_data_PA_PE, network = "Patient-staff")

cols = colnames(all_metrics)[1:7]
summary_tab = rbind(all_metrics, PA_PA_metrics, PE_PE_metrics, PA_PE_metrics) %>%
  mutate(temp_corr = replace(temp_corr, temp_corr==0, NA)) %>%
  mutate(day = wday(day, week_start = 1)) %>%
  mutate(day = as.character(day)) %>%
  mutate(day = replace(day, day %in% c("6", "7"), "weekend")) %>%
  mutate(day = replace(day, day != "weekend", "weekday")) %>%
  group_by(network, day) %>%
  summarise(across(all_of(cols), list(mean=mean, sd=sd), na.rm=T))

summary_tab[,-c(1,2)] = round(summary_tab[,-c(1,2)], 2)

View(summary_tab)

rbind(all_metrics, PA_PA_metrics, PE_PE_metrics, PA_PE_metrics) %>%
  mutate(temp_corr = replace(temp_corr, temp_corr==0, NA)) %>%
  mutate(day_lab = wday(day, week_start = 1)) %>%
  group_by(network, day_lab) %>%
  summarise(mean = mean(temp_corr, na.rm=T), sd = sd(temp_corr, na.rm = T)) %>%
  mutate(day_lab = as.character(day_lab)) %>%
  mutate(day_lab = replace(day_lab, day_lab == "1", "Monday")) %>%
  mutate(day_lab = replace(day_lab, day_lab == "2", "Tuesday")) %>%
  mutate(day_lab = replace(day_lab, day_lab == "3", "Wednesday")) %>%
  mutate(day_lab = replace(day_lab, day_lab == "4", "Thursday")) %>%
  mutate(day_lab = replace(day_lab, day_lab == "5", "Friday")) %>%
  mutate(day_lab = replace(day_lab, day_lab == "6", "Saturday")) %>%
  mutate(day_lab = replace(day_lab, day_lab == "7", "Sunday")) %>%
  mutate(day_lab = factor(day_lab,
                          levels = c("Monday", "Tuesday", "Wednesday",
                                     "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  ggplot() +
  geom_pointrange(aes(x = day_lab, y = mean, ymin = mean-1.96*sd, ymax = mean+1.96*sd,
                      colour = day_lab)) +
  facet_grid(rows = vars(network), scales = "free_y") +
  theme_bw() +
  scale_colour_discrete(type = c(rep("blue3", 5), rep("red3", 2))) +
  labs(x = "Day of the week", y = "Temporal correlation") +
  guides(colour = "none") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(breaks = seq(0,1,0.15))

ggsave(here::here("Figures", "suppfig3.png"))
