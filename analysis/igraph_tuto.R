
library(igraph)
library(ggnetwork)
library(dplyr)

# tuto pour igraph:
# https://r.igraph.org/articles/igraph.html 
# tuto pour ggnetwork:
# https://www.r-bloggers.com/2016/03/ggnetwork-network-geometries-for-ggplot2/
# https://briatte.github.io/ggnetwork/articles/ggnetwork.html#main-building-blocks

# il y a plusieurs méthodes pour créer des objets igraph

# ici, je le fais à partir d'une matrice
test_net = graph_from_edgelist(matrix(c("Alice", "Bob",
                                 "Alice", "Claire",
                                 "Alice", "Frank",
                                 "Claire", "Alice",
                                 "Claire", "Dennis",
                                 "Claire", "Frank",
                                 "Claire", "Esther",
                                 "George", "Dennis",
                                 "George", "Frank",
                                 "Dennis", "Esther"), nc = 2, byrow = TRUE))

# ici, je le fais à partir d'un dataframe, et c'est pareil
test_net = graph_from_data_frame(as.data.frame(matrix(c("Alice", "Bob",
                                                 "Alice", "Claire",
                                                 "Alice", "Frank",
                                                 "Claire", "Alice",
                                                 "Claire", "Dennis",
                                                 "Claire", "Frank",
                                                 "Claire", "Esther",
                                                 "George", "Dennis",
                                                 "George", "Frank",
                                                 "Dennis", "Esther"), nc = 2, byrow = TRUE)))

# méthode de base pour plot le network
plot(test_net)

# méthode avec ggnetwork
# ça marche comme ggplot avec deux nouveaux geom pour le network (geom_edges et geom_nodes)
# la fonction ggnetwork() va just convertir le réseau en un format qui marche bien avec ggplot
ggnetwork(test_net) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes() +
  theme_blank() 

#avec ggnetwork et plein d'options fancy
# pour choisir l'algo à utiliser pour représenter le réseau, ça se passe dans ggnetwork()
# ici layout = igraph::layout.fruchterman.reingold(g) utilise un algo de igraph pour modifier le layout
# ça marche avec toutes les autres fonctions layout de igraph
ggnetwork(test_net,
          layout = igraph::layout.fruchterman.reingold(g),
          arrow.gap = 0.05) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(0.5, "lines"), type = "closed"),
             curvature = 0.1,
             size = 1) +
  geom_nodes() +
  geom_nodelabel(aes(label = name)) +
  theme_blank() 

# l'objet igraph test_net contient les infos sur les edges (vertices) et nodes (edges)
# on peut recup les infos:
V(test_net)
E(test_net)

# et on peut directement rajouter des attributs, par exemple age et genre des participants
V(test_net)$age = c(25, 31, 18, 23, 47, 22, 50) 
V(test_net)$gender = c("f", "m", "f", "m", "m", "f", "m")

# ou "type" de contact
E(test_net)$is_formal = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)

summary(test_net)

# aussi possible d'utiliser des fonctions set_vertex_attr et set_edge_attr qui marchent bien avec dplyr
test_net = graph_from_edgelist(matrix(c("Alice", "Bob",
                                        "Alice", "Claire",
                                        "Alice", "Frank",
                                        "Claire", "Alice",
                                        "Claire", "Dennis",
                                        "Claire", "Frank",
                                        "Claire", "Esther",
                                        "George", "Dennis",
                                        "George", "Frank",
                                        "Dennis", "Esther"), nc = 2, byrow = TRUE)) %>%
  set_vertex_attr("age", value = c(25, 31, 18, 23, 47, 22, 50)) %>%
  set_vertex_attr("gender", value = c("f", "m", "f", "m", "m", "f", "m")) %>%
  set_edge_attr("is_formal", value = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE))

summary(test_net)
plot(test_net)

# l'astuce plus "facile" pour créer les attribus c'est de créer un dataframe qui liste les infos
# puis d'utiliser set_vertex_attr
# pratique si jamais on centralise des attributs un peu partout et on veut que tout soit bien aligné
# avec qqchose comme left_join() ça permet de facilement relier les attribus avec un autre dataset que t'as
# par exemple:
test_net = graph_from_edgelist(matrix(c("Alice", "Bob",
                                        "Alice", "Claire",
                                        "Alice", "Frank",
                                        "Claire", "Alice",
                                        "Claire", "Dennis",
                                        "Claire", "Frank",
                                        "Claire", "Esther",
                                        "George", "Dennis",
                                        "George", "Frank",
                                        "Dennis", "Esther"), nc = 2, byrow = TRUE))

#data avec les noms dans le mauvais ordre par rapport au network
test_attributes = data.frame(name = c("George", "Frank", "Alice", "Claire", "Bob", "Dennis", "Esther"),
                             age = c(25, 31, 18, 23, 47, 22, 50),
                             gender = c("m", "m", "f", "f", "m", "m", "f"))

#get.vertex.attribute(test_net, "name") comme ça les attribus sont dans le bon ordre, puis left_join
test_attributes_joined = data.frame(name = get.vertex.attribute(test_net, "name")) %>%
  left_join(test_attributes, by = "name")

test_net = test_net %>%
  set_vertex_attr("age", value = test_attributes_joined$age) %>%
  set_vertex_attr("gender", value = test_attributes_joined$gender)
  
summary(test_net)

# on peut aussi attribuer une date explicite au réseau
test_net$date = c("2022-02-11")
graph_attr(test_net, "date")

# et ensuite on peut faire plein d'analyses, youpi!
# (je sais pas encore faire)
degree(g)

