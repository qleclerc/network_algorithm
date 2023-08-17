
library(here)
library(dplyr)
library(lubridate)
library(ggplot2)

# min_date = "2009-07-02"
# max_date = "2009-09-30"
min_date = "2009-07-27"
max_date = "2009-09-23"

eq_table = openxlsx::read.xlsx(here::here("data", "cat_groupings.xlsx"))

adm = read.csv2(here("data", "toy_admission.csv"))

adm = adm %>%
  filter(!grepl("PE-", id)) %>%
  mutate(lastDate = replace(lastDate, lastDate=="", "2009-10-26")) %>%
  mutate(firstDate = as_date(firstDate),
         lastDate = as_date(lastDate)) %>%
  filter(lastDate >= as_date(min_date)) %>%
  filter(firstDate <= as_date(max_date)) %>%
  mutate(firstDate = replace(firstDate, firstDate < as_date(min_date), as_date(min_date)),
         lastDate = replace(lastDate, lastDate > as_date(max_date), as_date(max_date))) %>%
  mutate(ward = replace(ward, ward == "Menard 1", "Neurologic (1)"),
         ward = replace(ward, ward == "Menard 2", "Neurologic (2)"),
         ward = replace(ward, ward == "Sorrel 0", "Nutrition"),
         ward = replace(ward, ward == "Sorrel 1", "Neurologic (3)"),
         ward = replace(ward, ward == "Sorrel 2", "Geriatric"),
         ward = replace(ward, ward == "Other", "Mobile")) %>%
  select(-c(cat, sex, hospitalization)) %>%
  rename(cat = ward)

agenda = read.csv2(here("data", "toy_agenda.csv"))
agenda = agenda %>%
  mutate(firstDate = as_datetime(firstDate), lastDate = as_datetime(lastDate)) %>%
  select(id, status, firstDate, lastDate, ward, cat) %>%
  filter(!(firstDate %within% interval(as_date("2009-07-20"), as_date("2009-07-26")))) %>%
  filter(!(firstDate %within% interval(as_date("2009-08-24"), as_date("2009-08-30")))) %>%
  filter(!(firstDate %within% interval(as_date("2009-09-14"), as_date("2009-09-20")))) %>%
  filter(lastDate >= as_datetime(min_date)) %>%
  filter(firstDate <= as_datetime(max_date))


contacts = read.csv2(here("data", "toy_mat_ctc.csv")) %>%
  select(-length) %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_date(min_date)) %>%
  filter(date_posix <= as_date(max_date))

summary_data = data.frame(id = c(unique(adm$id), unique(agenda$id)),
                          tot_slots = 0, contacts_slots = 0, average_contacts = 0, cat = "")


#for PE: just look at each time slot, then check if a contact was recorded or not in that slot
#for PA: look at each day between admission and discharge, then check if a contact was recorded or not in that day

for(id in summary_data$id){
  
  if(grepl("PA-", id)){
    
    adm_id = adm[adm$id == id,]
    contacts_id = contacts %>%
      filter(from == id | to == id) %>%
      select(date_posix) %>%
      mutate(date_posix = floor_date(date_posix, "day"))
    
    tot_days = 0
    contacts_d = 0
    number_contacts = 0
    for(i in 1:nrow(adm_id)){
      
      days_id = seq.Date(adm_id$firstDate[i], adm_id$lastDate[i], by = "day")
      days_id = days_id[!(days_id %within% interval(as_date("2009-07-20"), as_date("2009-07-26")) | 
                           days_id %within% interval(as_date("2009-08-24"), as_date("2009-08-30")) |
                            days_id %within% interval(as_date("2009-09-14"), as_date("2009-09-20")))]
      tot_days = tot_days + length(days_id)
      for(d in seq_along(days_id)){
        contacts_d = contacts_d + (any(contacts_id$date_posix == days_id[d]))
        number_contacts = number_contacts + sum(contacts_id$date_posix == days_id[d])
      }
    }
    
    summary_data$average_contacts[summary_data$id==id] = number_contacts/contacts_d
    summary_data$tot_slots[summary_data$id==id] = tot_days
    summary_data$contacts_slots[summary_data$id==id] = contacts_d
    summary_data$cat[summary_data$id==id] = unique(adm_id$cat)
    
  } else {
    
    agenda_id = agenda[agenda$id == id,]
    contacts_id = contacts %>%
      filter(from == id | to == id) %>%
      select(date_posix)
    
    summary_data$tot_slots[summary_data$id==id] = nrow(agenda_id)
    contacts_d = 0
    number_contacts = 0
    for(i in 1:nrow(agenda_id)){
      interval_i = interval(agenda_id$firstDate[i], agenda_id$lastDate[i])
      contacts_d = contacts_d + (any(contacts_id$date_posix %within% interval_i))
      number_contacts = number_contacts + sum(contacts_id$date_posix %within% interval_i)
    }
    summary_data$average_contacts[summary_data$id==id] = number_contacts/contacts_d
    summary_data$contacts_slots[summary_data$id==id] = contacts_d
    summary_data$cat[summary_data$id==id] = unique(agenda_id$cat)
    
  }
  
}

summary_data = summary_data %>%
  mutate(prop = contacts_slots/tot_slots) %>%
  mutate(staff = grepl("PE-", id)) %>%
  mutate(staff = replace(staff, staff=="TRUE", "Staff")) %>%
  mutate(staff = replace(staff, staff!="Staff", "Patients")) %>%
  left_join(eq_table, by = "cat")

summary_data$tr[summary_data$staff=="Patients"] = summary_data$cat[summary_data$staff=="Patients"]
summary_data = summary_data %>%
  filter(!is.na(tr))

summary_data %>%
  group_by(tr) %>%
  summarise(mean = mean(prop), sd = sd(prop)) %>%
  arrange(mean)

summary_data %>%
  group_by(staff) %>%
  summarise(mean = mean(prop), sd = sd(prop)) %>%
  arrange(mean)

summary_data = summary_data %>%
  mutate(tr = factor(tr, levels = summary_data %>%
                       group_by(tr) %>%
                       summarise(median = median(prop), sd = sd(prop)) %>%
                       arrange(median) %>%
                       select(tr) %>% pull))
  

ggplot(summary_data) +
  geom_violin(aes(tr, prop), alpha = 0.5, adjust = 0.5, fill = "grey70") +
  geom_dotplot(aes(tr, prop), binaxis='y', stackdir='center', binwidth = 0.005,
               method = "histodot", dotsize = 2) +
  #geom_boxplot(aes(cat, prop), width = 0.2) +
  facet_grid(cols = vars(staff), scales = "free_x", space = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  labs(x = "", y = "Proportion of presence days for which contact data was recorded") +
  guides(fill = "none")

ggsave(here::here("figures", "sensor.png"), width = 12, height = 7)

ggplot(summary_data, aes(prop, average_contacts)) +
  geom_point() +
  facet_wrap(~staff) +
  labs(y = "Average number of contacts per day",
       x = "Proportion of presence days for which contact data was recorded") +
  theme_bw() +
  theme(text = element_text(size=12))

ggsave(here::here("figures", "sensor2.png"), width = 8, height = 6)

tt = glm(average_contacts~prop*tr, data = summary_data)
summary(tt)
