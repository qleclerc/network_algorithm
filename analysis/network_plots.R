
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)

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
  filter(date_posix == as_date("2009-08-20")) %>%
  select(from, to) %>%
  distinct()

dur_data = data %>%
  mutate(date_posix = as_date(date_posix)) %>%
  mutate(date_posix = floor_date(date_posix, "day")) %>%
  filter(date_posix == as_date("2009-08-20")) %>%
  select(from, to, length)
dur_data = rbind(dur_data, dur_data) %>%
  select(from, length) %>%
  group_by(from) %>%
  summarise(length = sum(length)) %>%
  rename(id = from)

freq_data = rbind(graph_data, graph_data) %>%
  select(from) %>%
  group_by(from) %>%
  summarise(n = n()) %>%
  rename(id = from)

graph = graph_from_data_frame(graph_data, directed = F)

vertex_atts = data.frame(id = get.vertex.attribute(graph, "name")) %>%
  left_join(adm_data, "id") %>%
  left_join(dur_data, "id") %>%
  left_join(freq_data, "id") %>%
  mutate(ward = replace(ward, ward == "Menard 1", "Neurologic (1)"),
         ward = replace(ward, ward == "Menard 2", "Neurologic (2)"),
         ward = replace(ward, ward == "Sorrel 0", "Nutrition"),
         ward = replace(ward, ward == "Sorrel 1", "Neurologic (3)"),
         ward = replace(ward, ward == "Sorrel 2", "Geriatric"),
         ward = replace(ward, ward == "Other", "Mobile"))

graph = graph %>%
  set_vertex_attr("cat", value = vertex_atts$cat) %>%
  set_vertex_attr("cat_ag", value = vertex_atts$cat_ag) %>%
  set_vertex_attr("staff", value = vertex_atts$staff) %>%
  set_vertex_attr("ward", value = vertex_atts$ward) %>%
  set_vertex_attr("duration", value = vertex_atts$length/60/60) %>%
  set_vertex_attr("freq", value = vertex_atts$n)

ggplot(ggnetwork(graph, layout = igraph::layout.kamada.kawai(graph)),
       aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.5) +
  geom_nodes(aes(colour = ward, shape = staff), size = 4) +
  theme_blank() +
  scale_shape_manual(breaks = c("TRUE", "FALSE"), labels = c("Staff", "Patients"),
                     values = c(15,16)) +
  labs(colour = "Ward:", shape = "")

ggsave(here::here("figures", "example_network_plot.png"))

ggplot(ggnetwork(graph, layout = igraph::layout.kamada.kawai(graph)),
       aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.5) +
  geom_nodes(aes(colour = ward, size = duration, shape = staff)) +
  theme_blank() +
  scale_shape_manual(breaks = c("TRUE", "FALSE"), labels = c("Staff", "Patients"),
                     values = c(15,16)) +
  labs(colour = "Ward:", size = "Time in\ncontact (mins):") +
  guides(shape = "none")

ggsave(here::here("figures", "example_network_plot_dur.png"))

ggplot(ggnetwork(graph, layout = igraph::layout.kamada.kawai(graph)),
       aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.5) +
  geom_nodes(aes(colour = ward, size = freq, shape = staff)) +
  theme_blank() +
  scale_shape_manual(breaks = c("TRUE", "FALSE"), labels = c("Staff", "Patients"),
                     values = c(15,16)) +
  labs(colour = "Ward:", size = "Number of\ncontacts:") +
  guides(shape = "none")

ggsave(here::here("figures", "network_plot_freq.png"))
