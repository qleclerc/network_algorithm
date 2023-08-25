
library(dplyr)
library(ggplot2)
library(cowplot)

# to avoid re-running lengthy code, I'm assuming the summary_data file exists
# this files can be generated at the end of the fig3_suppfig4.R script
summary_data = read.csv(here::here("Data", "summary_data.csv"))

pvals = data.frame(network = unique(summary_data$network),
                   degrees=0, densities=0, transitivities=0,
                   assortativities=0, assortativities_ward=0, efficiencies=0, temp_corr=0)

for(net in unique(pvals$network)){
  if(net=="Observed") next
  pvals$degrees[pvals$network==net] = kruskal.test(degrees~iter,
               data = summary_data %>% filter(network == net))$p.value
  pvals$densities[pvals$network==net] = kruskal.test(densities~iter,
                                                   data = summary_data %>% filter(network == net))$p.value
  pvals$transitivities[pvals$network==net] = kruskal.test(transitivities~iter,
                                                     data = summary_data %>% filter(network == net))$p.value
  pvals$assortativities[pvals$network==net] = kruskal.test(assortativities~iter,
                                                     data = summary_data %>% filter(network == net))$p.value
  pvals$assortativities_ward[pvals$network==net] = kruskal.test(assortativities_ward~iter,
                                                     data = summary_data %>% filter(network == net))$p.value
  pvals$efficiencies[pvals$network==net] = kruskal.test(efficiencies~iter,
                                                     data = summary_data %>% filter(network == net))$p.value
  pvals$temp_corr[pvals$network==net] = kruskal.test(temp_corr~iter,
                                                        data = summary_data %>% filter(network == net))$p.value
  
}

pvals[,-1] = round(pvals[,-1],10)
pvals

simu_data = summary_data %>%
  filter(network == "Reconstructed")

simu_data = simu_data %>%
  mutate(day = wday(day, week_start=1),
         weekend="") %>%
  mutate(weekend = replace(weekend, ((day+1)%%7==0 | day%%7==0), "weekend")) %>%
  mutate(weekend = replace(weekend, weekend =="", "weekday"))

pa = ggplot(simu_data) +
  geom_point(aes(x = iter, colour = weekend, y = degrees), alpha = 0.3) +
  geom_hline(aes(yintercept = median(degrees)), colour = "green3", linewidth = 1) +
  theme_bw() +
  labs(y = "Degrees") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_cartesian(xlim = c(3,97)) +
  guides(colour = "none")
  

pb = ggplot(simu_data) +
  geom_point(aes(x = iter, colour = weekend, y = efficiencies), alpha = 0.3) +
  geom_hline(aes(yintercept = median(efficiencies)), colour = "green3", linewidth = 1) +
  theme_bw() +
  labs(y = "Global efficiency") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_cartesian(xlim = c(3,97)) +
  guides(colour = "none")

pc = ggplot(simu_data) +
  geom_point(aes(x = iter, colour = weekend, y = densities), alpha = 0.3) +
  geom_hline(aes(yintercept = median(densities)), colour = "green3", linewidth = 1) +
  theme_bw() +
  labs(y = "Density") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_cartesian(xlim = c(3,97)) +
  guides(colour = "none")

pd = ggplot(simu_data) +
  geom_point(aes(x = iter, colour = weekend, y = transitivities), alpha = 0.3) +
  geom_hline(aes(yintercept = median(transitivities)), colour = "green3", linewidth = 1) +
  theme_bw() +
  labs(y = "Transitivity") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_cartesian(xlim = c(3,97)) +
  guides(colour = "none")

pe = ggplot(simu_data) +
  geom_point(aes(x = iter, colour = weekend, y = assortativities), alpha = 0.3) +
  geom_hline(aes(yintercept = median(assortativities)), colour = "green3", linewidth = 1) +
  theme_bw() +
  labs(y = "Assortativity (degree)") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_cartesian(xlim = c(3,97)) +
  guides(colour = "none")

pf = ggplot(simu_data) +
  geom_point(aes(x = iter, colour = weekend, y = assortativities_ward), alpha = 0.3) +
  geom_hline(aes(yintercept = median(assortativities_ward)), colour = "green3", linewidth = 1) +
  theme_bw() +
  labs(y = "Assortativity (ward)", x = "Iteration") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_cartesian(xlim = c(3,97)) +
  guides(colour = "none")

pg = ggplot(simu_data%>%filter(temp_corr>0)) +
  geom_point(aes(x = iter, colour = weekend, y = temp_corr), alpha = 0.3) +
  geom_hline(aes(yintercept = median(temp_corr)), colour = "green3", linewidth = 1) +
  theme_bw() +
  labs(y = "Temporal correlation", x = "Iteration") +
  scale_x_continuous(breaks = seq(10,100,10)) +
  coord_cartesian(xlim = c(3,97)) +
  guides(colour = "none")

plot_grid(pa,pb,pc,pd,pe,pf,pg, ncol = 1, align = "v",
          rel_heights = c(1,1,1,1,1,1.3), labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)"),
          hjust=0, vjust=1)

ggsave(here::here("Figures", "suppfig6.png"), height = 12, width = 10)
