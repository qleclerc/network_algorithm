
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)

adm_data = read.csv(here::here("toy_admission.csv"), sep=";") %>%
  select(id, hospitalization, cat)
adm_data$cat[adm_data$cat == ""] = adm_data$hospitalization[adm_data$cat == ""]
adm_data = adm_data[,c(1,3)] %>%
  distinct()
eq_table = openxlsx::read.xlsx(here::here("cat_groupings.xlsx")) %>%
  select(cat, cat_ag)
adm_data = adm_data %>%
  left_join(eq_table, by = "cat") %>%
  mutate(staff = grepl("PE-", id))

data = read.csv2(here::here("contact", "toy_mat_ctc.csv"))

graph_data = data %>%
  mutate(date_posix = as_date(date_posix)) %>%
  mutate(date_posix = floor_date(date_posix, "day")) %>%
  filter(date_posix == as_date("2009-08-20")) %>%
  select(from, to) %>%
  distinct()

#https://r.igraph.org/articles/igraph.html 
graph = graph_from_data_frame(graph_data, directed = F)

vertex_atts = data.frame(id = get.vertex.attribute(graph, "name")) %>%
  left_join(adm_data, "id")

graph = graph %>%
  set_vertex_attr("cat", value = vertex_atts$cat) %>%
  set_vertex_attr("cat_ag", value = vertex_atts$cat_ag) %>%
  set_vertex_attr("staff", value = vertex_atts$staff)
  
ggplot(ggnetwork(graph, layout = igraph::layout.fruchterman.reingold(graph)),
       aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes(aes(colour = cat_ag), size = 3) +
  theme_blank() 

graphPA = subgraph(graph=graph, vids=which(V(graph)$staff==FALSE))

ggplot(ggnetwork(graphPA, layout = igraph::layout.fruchterman.reingold(graphPA)),
       aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes(aes(colour = cat_ag), size = 3) +
  theme_blank() 

graphPE = subgraph(graph=graph, vids=which(V(graph)$staff==T))

ggplot(ggnetwork(graphPE, layout = igraph::layout.fruchterman.reingold(graphPE)),
       aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes(aes(colour = cat_ag), size = 3) +
  theme_blank() 
