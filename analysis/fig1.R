
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)
library(RColorBrewer)

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

graph_data_PA_PA = graph_data %>%
  filter(grepl("PA-", from) & grepl("PA-", to))

graph_data_PE_PE = graph_data %>%
  filter(grepl("PE-", from) & grepl("PE-", to))

graph_data_PA_PE = graph_data %>%
  filter((grepl("PE-", from) & grepl("PA-", to)) | (grepl("PA-", from) & grepl("PE-", to)))


all_degrees = c()
all_densities = c()
all_transitivities = c()
all_assortativities = c()

all_degrees_PA_PA = c()
all_densities_PA_PA = c()
all_transitivities_PA_PA = c()
all_assortativities_PA_PA = c()

all_degrees_PE_PE = c()
all_densities_PE_PE = c()
all_transitivities_PE_PE = c()
all_assortativities_PE_PE = c()

all_degrees_PA_PE = c()
all_densities_PA_PE = c()
all_transitivities_PA_PE = c()
all_assortativities_PA_PE = c()

for(d in unique(graph_data$date_posix)){
  
  # full network
  data_d = graph_data %>%
    filter(date_posix == d)
  
  graph_d = graph_from_data_frame(data_d, directed = F)
  graph_d = simplify(graph_d)
  
  all_degrees = c(all_degrees, degree(graph_d))
  all_densities = c(all_densities, edge_density(graph_d))
  all_transitivities = c(all_transitivities, transitivity(graph_d))
  all_assortativities = c(all_assortativities,assortativity.degree(graph_d, directed = F))
  
  # patient-patient
  data_d = graph_data_PA_PA %>%
    filter(date_posix == d)
  
  graph_d = graph_from_data_frame(data_d, directed = F)
  graph_d = simplify(graph_d)
  
  all_degrees_PA_PA = c(all_degrees_PA_PA, degree(graph_d))
  all_densities_PA_PA = c(all_densities_PA_PA, edge_density(graph_d))
  all_transitivities_PA_PA = c(all_transitivities_PA_PA, transitivity(graph_d))
  all_assortativities_PA_PA = c(all_assortativities_PA_PA,assortativity.degree(graph_d, directed = F))
  
  # staff-staff
  data_d = graph_data_PE_PE %>%
    filter(date_posix == d)
  
  graph_d = graph_from_data_frame(data_d, directed = F)
  graph_d = simplify(graph_d)
  
  all_degrees_PE_PE = c(all_degrees_PE_PE, degree(graph_d))
  all_densities_PE_PE = c(all_densities_PE_PE, edge_density(graph_d))
  all_transitivities_PE_PE = c(all_transitivities_PE_PE, transitivity(graph_d))
  all_assortativities_PE_PE = c(all_assortativities_PE_PE,assortativity.degree(graph_d, directed = F))
  
  # patient-staff
  data_d = graph_data_PA_PE %>%
    filter(date_posix == d)
  
  graph_d = graph_from_data_frame(data_d, directed = F)
  graph_d = simplify(graph_d)
  
  all_degrees_PA_PE = c(all_degrees_PA_PE, degree(graph_d))
  all_densities_PA_PE = c(all_densities_PA_PE, edge_density(graph_d))
  all_transitivities_PA_PE = c(all_transitivities_PA_PE, transitivity(graph_d))
  all_assortativities_PA_PE = c(all_assortativities_PA_PE,assortativity.degree(graph_d, directed = F))
  
}

all_degrees = data.frame(id = names(all_degrees), degree = unname(all_degrees))
all_degrees_PA_PA = data.frame(id = names(all_degrees_PA_PA), degree = unname(all_degrees_PA_PA))
all_degrees_PE_PE = data.frame(id = names(all_degrees_PE_PE), degree = unname(all_degrees_PE_PE))
all_degrees_PA_PE = data.frame(id = names(all_degrees_PA_PE), degree = unname(all_degrees_PA_PE))

summary_tab = data.frame(full = c(mean(all_degrees$degree),
                                  mean(all_densities),
                                  mean(all_transitivities),
                                  mean(all_assortativities)),
                         full_sd = c(sd(all_degrees$degree),
                                     sd(all_densities),
                                     sd(all_transitivities),
                                     sd(all_assortativities)),
                         PA_PA = c(mean(all_degrees_PA_PA$degree),
                                   mean(all_densities_PA_PA),
                                   mean(all_transitivities_PA_PA),
                                   mean(all_assortativities_PA_PA)),
                         PA_PA_sd = c(sd(all_degrees_PA_PA$degree),
                                      sd(all_densities_PA_PA),
                                      sd(all_transitivities_PA_PA),
                                      sd(all_assortativities_PA_PA)),
                         PE_PE = c(mean(all_degrees_PE_PE$degree),
                                   mean(all_densities_PE_PE),
                                   mean(all_transitivities_PE_PE),
                                   mean(all_assortativities_PE_PE)),
                         PE_PE_sd = c(sd(all_degrees_PE_PE$degree),
                                      sd(all_densities_PE_PE),
                                      sd(all_transitivities_PE_PE),
                                      sd(all_assortativities_PE_PE)),
                         PA_PE = c(mean(all_degrees_PA_PE$degree),
                                   mean(all_densities_PA_PE),
                                   mean(all_transitivities_PA_PE),
                                   mean(all_assortativities_PA_PE)),
                         PA_PE_sd = c(sd(all_degrees_PA_PE$degree),
                                      sd(all_densities_PA_PE),
                                      sd(all_transitivities_PA_PE),
                                      sd(all_assortativities_PA_PE)))

summary_tab = round(summary_tab, 2)

summary_tab

pb = ggplot(all_degrees) +
  geom_histogram(aes(degree, after_stat(density)), binwidth = 1, colour = "grey") +
  geom_vline(xintercept = mean(all_degrees$degree), linetype = "dashed", linewidth = 1, colour = "red3") +
  theme_bw() +
  labs(x = "Node degree", y = "Frequency") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  scale_x_continuous(breaks = seq(0,70,10))


example_date = as_date("2009-08-11")
exampe_data = data %>% filter(date_posix == example_date)
graph_example = graph_from_data_frame(exampe_data, directed = F)
#E(graph_example)$weight = exampe_data$length
graph_example = simplify(graph_example)

vertex_atts = data.frame(id = get.vertex.attribute(graph_example, "name")) %>%
  left_join(adm_data, "id") %>%
  mutate(ward = replace(ward, ward == "Menard 1", "Neurologic (1)"),
         ward = replace(ward, ward == "Menard 2", "Neurologic (2)"),
         ward = replace(ward, ward == "Sorrel 0", "Nutrition"),
         ward = replace(ward, ward == "Sorrel 1", "Neurologic (3)"),
         ward = replace(ward, ward == "Sorrel 2", "Geriatric"),
         ward = replace(ward, ward == "Other", "Mobile"))

graph_example = graph_example %>%
  set_vertex_attr("cat", value = vertex_atts$cat) %>%
  set_vertex_attr("cat_ag", value = vertex_atts$cat_ag) %>%
  set_vertex_attr("staff", value = vertex_atts$staff) %>%
  set_vertex_attr("ward", value = vertex_atts$ward)

pa = ggplot(ggnetwork(graph_example, layout = igraph::layout_with_kk(graph_example)),
            aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.5) +
  geom_nodes(aes(colour = ward, shape = staff), size = 4) +
  theme_blank() +
  scale_shape_manual(breaks = c("TRUE", "FALSE"), labels = c("Staff", "Patients"),
                     values = c(15,17)) +
  scale_colour_discrete(type = pal) +
  labs(colour = "Ward:", shape = "Category:") +
  theme(legend.text = element_text(size=12))

plot_grid(pa, pb, rel_heights = c(1,0.4), ncol = 1, labels = c("a)", "b)"), hjust = 0)

ggsave(here::here("figures", "fig1.png"))
