
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)
library(RColorBrewer)
library(ggsankey)

staff_pal = c(brewer.pal(7, "Dark2"))
pat_pal = brewer.pal(6, "Set2")

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
  group_by(from, to, date_posix) %>%
  summarise(length = sum(length)) %>%
  ungroup


pa = graph_data %>%
  select(from, to) %>%
  distinct %>%
  mutate(fromto = paste0(from, to),
         tofrom = paste0(to, from)) %>%
  filter(!(fromto %in% tofrom)) %>%
  select(from, to) %>%
  left_join(adm_data %>% select(id, cat_ag, staff, ward) %>% rename(from = id)) %>%
  rename(from_cat = cat_ag, from_staff = staff, from_ward = ward) %>%
  left_join(adm_data %>% select(id, cat_ag, staff, ward) %>% rename(to = id)) %>%
  rename(to_cat = cat_ag, to_staff = staff, to_ward = ward) %>%
  filter(from_staff != to_staff) %>%
  filter(!is.na(from_cat)) %>%
  filter(!is.na(to_cat)) %>%
  mutate(Staff = replace(from_cat, from_staff==F, to_cat[from_staff==F]),
         Patients = replace(to_ward, from_staff==F, from_ward[from_staff==F])) %>%
  mutate(Patients = replace(Patients, Patients == "Menard 1", "Neurologic (1)"),
         Patients = replace(Patients, Patients == "Menard 2", "Neurologic (2)"),
         Patients = replace(Patients, Patients == "Sorrel 0", "Nutrition"),
         Patients = replace(Patients, Patients == "Sorrel 1", "Neurologic (3)"),
         Patients = replace(Patients, Patients == "Sorrel 2", "Geriatric"),
         Patients = replace(Patients, Patients == "Other", "Mobile")) %>%
  make_long(Staff, Patients) %>%
  ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node,
             fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 0.6, node.colour = "grey30") +
  geom_sankey_label(hjust = c(1.2,1.4,1.4,1.45,1.4,1.25,
                              -0.4,-0.3,-0.3,-0.25,-0.5)) +
  scale_fill_manual(breaks = c("Care assistants", "Reeducation", "Doctors", "Other", "Porters", "Nurses",
                               "Geriatric", "Mobile", "Neurologic (1)", "Neurologic (2)", "Neurologic (3)",
                               "Nutrition"),
                    values = c(staff_pal[-1],pat_pal)) +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.margin = margin(t = 0, b = 0, r = 10, l = 0))



PE_contacts = graph_data %>%
  select(from, to) %>%
  distinct %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type %in% c("PE-PA", "PA-PE")) %>%
  select(from, to)

summary_PE_contacts = data.frame()

for(PE in adm_data$id[adm_data$staff]){
  
  PE_contacts_i = PE_contacts %>%
    filter(from == PE | to == PE) %>%
    unlist %>% unname %>% unique
  
  if(length(PE_contacts_i)==0) next
  
  PE_contacts_i = data.frame(id = PE_contacts_i[PE_contacts_i != PE]) %>%
    left_join(adm_data, by = "id") %>%
    select(ward) %>% pull %>% unique %>% length
  
  PE_contacts_i = data.frame(id = PE,
                             cat = adm_data$cat_ag[adm_data$id==PE],
                             wards = PE_contacts_i)
  
  summary_PE_contacts = rbind(summary_PE_contacts,
                              PE_contacts_i)
  
}

pb = ggplot(summary_PE_contacts) +
  geom_boxplot(aes(cat, wards, colour = cat), linewidth = 0.8, outlier.size = 2) + 
  scale_colour_manual(breaks = c("Care assistants", "Reeducation", "Doctors", "Other", "Porters", "Nurses"),
                      values = c(staff_pal[-1])) +
  
  theme_bw() +
  guides(colour = "none") +
  labs(x = "", y = "Number of wards\ncontacted at least once") +
  theme(axis.title = element_text(size=12),
        axis.text = element_markdown(size=12))



graph_data_sorted = graph_data %>%
  arrange(date_posix)

probs_PA = c()
probs_PE = c()

for(pp in unique(adm_data$id)){
  
  pp_contacts = graph_data_sorted %>%
    filter(from == pp | to == pp)
  
  unique_cc = c()
  proba=c()
  
  tot_cc = c(pp_contacts$from, pp_contacts$to)
  tot_cc = tot_cc[which(tot_cc != pp)]
  
  for(dd in unique(pp_contacts$date_posix)){
    pp_contacts_dd = pp_contacts %>%
      filter(date_posix == dd)
    if(is.null(unique_cc)){
      unique_cc = unique(c(pp_contacts_dd$from, pp_contacts_dd$to))
      unique_cc = unique_cc[which(unique_cc != pp)]
    } else {
      pp_contacts_dd = unique(c(pp_contacts_dd$from, pp_contacts_dd$to))
      pp_contacts_dd = pp_contacts_dd[which(pp_contacts_dd != pp)]
      prev_cc = intersect(pp_contacts_dd, unique_cc)
      proba = c(proba, length(prev_cc)/length(pp_contacts_dd))
      unique_cc = c(unique_cc, setdiff(pp_contacts_dd, unique_cc))
    }
  }
  
  if(grepl("PA-", pp)) probs_PA = c(probs_PA, mean(proba, na.rm = T))
  else probs_PE = c(probs_PE, mean(proba, na.rm = T))
  
}

#note some values are NA because these are people with no contacts recorded over the period
mean(probs_PA, na.rm = T)
mean(probs_PE, na.rm = T)

pc = ggplot() +
  geom_boxplot(aes(x = "Staff", probs_PE, group = "Staff", colour = "Staff")) +
  geom_boxplot(aes(x = "Patients", probs_PA, group = "Patients", colour = "Patients")) +
  geom_point(aes(x = "Staff", y = mean(probs_PE, na.rm = T)), size = 5, pch = 18, col = "grey10") +
  geom_point(aes(x = "Patients", y = mean(probs_PA, na.rm = T)), size = 5, pch = 18, col = "grey60") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  scale_x_discrete(limits = c("Staff", "Patients")) +
  labs(x = "", y = "Probability of contact recurrence") +
  guides(colour = "none") +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size=12),
        axis.title.y = element_text(size = 12)) +
  scale_colour_discrete(type = c("grey60", "grey10"))

plot_grid(plot_grid(pa, pc, nrow = 1, rel_widths = c(1,0.5), labels = c("a)", "c)"),
                    hjust = 0, align = "h"),
          pb, nrow = 2, rel_heights = c(1, 0.6), labels = c("", "b)"), hjust = 0)

ggsave(here::here("Figures", "fig2.png"))

