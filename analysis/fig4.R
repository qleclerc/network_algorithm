
library(dplyr)
library(lubridate)
library(igraph)
library(ggnetwork)
library(ggtext)
library(cowplot)
library(RColorBrewer)

pal = brewer.pal(5, "Set1")[c(2,3,4,1)]

# OBSERVED ######

data = read.csv2(here::here("data", "toy_mat_ctc.csv"))

dur_observed = data %>%
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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Patient-Patient")

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Staff")

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Patient")

distrib_real = rbind(distrib_PAPA, distrib_PEPE, distrib_PAPE) %>%
  mutate(date_posix = paste0(date_posix, ":00")) %>%
  mutate(date_posix = factor(date_posix, levels = unique(date_posix))) %>%
  mutate(network = "Observed")


# SIMU (RECORD BIAS) ######

data = read.csv2(here::here("data", "contact", "matContactBuilttestRecordNetworks1.csv"))

dur_simu_bias = data %>%
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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Patient-Patient")

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Staff")

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Patient")

distrib_simu_bias = rbind(distrib_PAPA, distrib_PEPE, distrib_PAPE) %>%
  mutate(date_posix = paste0(date_posix, ":00")) %>%
  mutate(date_posix = factor(date_posix, levels = unique(date_posix))) %>%
  mutate(network = "Simulated (bias)")


# SIMU ######

data = read.csv2(here::here("data", "contact", "matContactBuiltSimulatedCtcNetworks1.csv"))

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Patient-Patient")

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Staff")

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Patient")

distrib_simu = rbind(distrib_PAPA, distrib_PEPE, distrib_PAPE) %>%
  mutate(date_posix = paste0(date_posix, ":00")) %>%
  mutate(date_posix = factor(date_posix, levels = unique(date_posix))) %>%
  mutate(network = "Simulated")


# RESIMU ######

data = read.csv2(here::here("data", "contact", "matContactBuiltReSimulatedCtcNetworks1.csv"))

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Patient-Patient")

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Staff")

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Patient")

distrib_resimu = rbind(distrib_PAPA, distrib_PEPE, distrib_PAPE) %>%
  mutate(date_posix = paste0(date_posix, ":00")) %>%
  mutate(date_posix = factor(date_posix, levels = unique(date_posix))) %>%
  mutate(network = "Re-simulated")

# RANDOM ######

data = read.csv2(here::here("data", "contact", "matContactBuiltRandomGraphNetworks1.csv"))

dur_random = data %>%
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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Patient-Patient")

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Staff")

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
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Patient")

distrib_random = rbind(distrib_PAPA, distrib_PEPE, distrib_PAPE) %>%
  mutate(date_posix = paste0(date_posix, ":00")) %>%
  mutate(date_posix = factor(date_posix, levels = unique(date_posix))) %>%
  mutate(network = "Random")


# COMBINED ######

distrib_all = rbind(distrib_real, distrib_simu_bias, distrib_simu, distrib_random) %>%
  mutate(network = factor(network, levels = unique(network))) %>%
  mutate(type = factor(type, levels = unique(type)))

pa= ggplot(distrib_all) +
  geom_ribbon(aes(date_posix, ymin = q25, ymax = q75, fill = network, group = interaction(type, network)),
              alpha = 0.3) +
  geom_point(aes(date_posix, med, colour = network)) +
  geom_line(aes(date_posix, med, colour = network, group = interaction(type, network))) +
  scale_colour_discrete(type = pal) +
  scale_fill_discrete(type = pal) +
  scale_y_continuous(breaks = seq(0,300,50)) +
  scale_x_discrete(breaks = c("0:00", "4:00", "8:00", "12:00",
                              "16:00","20:00")) +
  facet_wrap(~type) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12))  +
  labs(x = "Hour", y = "Number of unique contacts", colour = "Network:", fill = "Network:")

pb = ggplot() +
  geom_boxplot(aes(y=dur_observed/60, x = "Observed", colour = "Observed"), outlier.shape = NA) +
  geom_boxplot(aes(y=dur_simu/60, x = "Simulated", colour = "Simulated"), outlier.shape = NA) +
  geom_boxplot(aes(y=dur_simu_bias/60, x = "Simulated (bias)", colour = "Simulated (bias)"), outlier.shape = NA) +
  geom_boxplot(aes(y=dur_random/60, x = "Random", colour = "Random"), outlier.shape = NA) +
  scale_colour_manual(values = pal, breaks = c("Observed", "Simulated (bias)", "Simulated", "Random")) +
  coord_cartesian(ylim = c(0,40)) +
  guides(colour="none") +
  labs(y = "Contact duration (minutes)", x = "Network") +
  theme_bw()

plot_grid(pa, pb, nrow = 2, labels = c("a)", "b)"), rel_heights = c(1,0.65), hjust=0, vjust=c(1,0))

ggsave(here::here("figures", "fig4.png"), width = 10, height = 6)


ggplot() +
  geom_histogram(aes(dur_observed/60, after_stat(density), fill = "Observed"), alpha = 0.3, binwidth = 1) +
  geom_histogram(aes(rlnorm(100000, log10(mean(dur_observed/60)), log10(sd(dur_observed/60))),
                     after_stat(density), fill = "Lognormal"), alpha = 0.3, binwidth = 1) +
  scale_fill_discrete(type = c("grey55", pal[1])) +
  coord_cartesian(xlim = c(0,60)) +
  labs(x = "Contact duration (minutes)", y = "Frequency", fill = "Distribution:") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

ggsave(here::here("figures", "suppfig3.png"))
