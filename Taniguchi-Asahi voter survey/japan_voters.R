library(tidyverse)
library(ggrepel)
library(scales)

# Load data
df <- read_csv('2017UTASV20200326.csv',locale = locale(encoding = 'cp932'))

# Clean up
df %>%
  filter(Q1 == 2) %>%
  mutate(vote = recode(Q2,
                        `1` = 'LDP',
                        `2` = 'Party of Hope',
                        `3` = 'Komeito',
                        `4` = 'Communist Party',
                        `5` = 'JRP',
                        `6` = 'Constitutional Democratic Party',
                        `7` = 'SDP',
                        `8` = 'Other',
                        `9` = 'Other',
                       .default = NA_character_)) %>%
  mutate(left_right = na_if(Q21,99)) %>%
  mutate_at(vars(c(starts_with('Q23_'))), ~na_if(.x,99)) %>%
  mutate(free_educ = Q23_9, prog_tax = Q23_10, fiscal_stimulus = Q23_8, public_works = Q23_7, small_gov = Q23_6, cons_tax = na_if(Q22_1,99)) %>%
  mutate(defence = Q23_1, preemtive_strike = Q23_2, nk_pressure = Q23_3, non_prolif = Q23_4, yasukuni = Q23_5, safety_privacy = Q23_11) %>%
  mutate(migrant_worker = Q23_12, restart_nuclear = Q23_13, maiden_name = Q23_14, gay_marriage = Q23_15, pol_age = Q23_16, unicam = Q23_17) -> df


# Economic issues
df %>%
  filter(!is.na(left_right)) %>%
  group_by(left_right) %>%
  summarise_at(vars(free_educ,prog_tax,fiscal_stimulus,public_works,small_gov,cons_tax), ~mean(6 - .x,na.rm=T)) %>%
  rename(`Free education through university` = free_educ, `Progressive taxation` = prog_tax, `Fiscal stimulus over austerity` = fiscal_stimulus, `Public works to protect employment` = public_works, `Prefer small government` = small_gov, `Raise consumption tax` = cons_tax) %>%
  gather(key = 'Policy', value = 'Average support',-left_right) %>%
  ggplot(aes(x=left_right,y=`Average support`)) +
  geom_bar(stat = 'identity', fill = '#1f77b4') +
  facet_wrap(~Policy) +
  coord_flip() +
  theme_bw() +
  labs(x = 'Subjective left-right position',
       y = 'Average support (1 = Strongly disagree, 5 = Strongly agree)',
       title = 'Where Japanese voters stand on economic policy',
       subtitle = 'Voters in the 2017 general election',
       caption = 'UTokyo-Asahi Survey; John Handley') +
  ggsave('econ_policy.png', width = 8*1.2, height = 5*1.2, type = 'cairo')

# National security issues
df %>%
  filter(!is.na(left_right)) %>%
  group_by(left_right) %>%
  summarise_at(vars(defence, preemtive_strike, nk_pressure, non_prolif, yasukuni, safety_privacy), ~mean(6 - .x,na.rm=T)) %>%
  rename(`Strengthen national defence` = defence, `Pre-emptive strike` = preemtive_strike, `Put preasure on North Korea` = nk_pressure, `Nuclear non-proliferation` = non_prolif, `PM at Yasukuni shrine` = yasukuni, `Safety over privacy` = safety_privacy) %>%
  gather(key = 'Policy', value = 'Average support',-left_right) %>%
  ggplot(aes(x=left_right,y=`Average support`)) +
  geom_bar(stat = 'identity', fill = '#1f77b4') +
  facet_wrap(~Policy) +
  coord_flip() +
  theme_bw() +
  labs(x = 'Subjective left-right position',
       y = 'Average support (1 = Strongly disagree, 5 = Strongly agree)',
       title = 'Where Japanese voters stand on foreign/security policy',
       subtitle = 'Voters in the 2017 general election',
       caption = 'UTokyo-Asahi Survey; John Handley') +
  ggsave('foreign_policy.png', width = 8*1.2, height = 5*1.2, type = 'cairo')

# Social issues
df %>%
  filter(!is.na(left_right)) %>%
  group_by(left_right) %>%
  summarise_at(vars(migrant_worker, restart_nuclear, maiden_name, gay_marriage, pol_age, unicam), ~mean(6 - .x,na.rm=T)) %>%
  rename(`Allow more migrant workers` = migrant_worker,
         `Restart nuclear power plants` = restart_nuclear,
         `Allow wives to keep their maiden names` = maiden_name,
         `Legalise gay marriage` = gay_marriage,
         `Lower age to run for office` = pol_age,
         `Unicameral legislature` = unicam) %>%
  gather(key = 'Policy', value = 'Average support',-left_right) %>%
  ggplot(aes(x=left_right,y=`Average support`)) +
  geom_bar(stat = 'identity', fill = '#1f77b4') +
  facet_wrap(~Policy) +
  coord_flip() +
  theme_bw() +
  labs(x = 'Subjective left-right position',
       y = 'Average support (1 = Strongly disagree, 5 = Strongly agree)',
       title = 'Where Japanese voters stand on domestic policy and social issues',
       subtitle = 'Voters in the 2017 general election',
       caption = 'UTokyo-Asahi Survey; John Handley') +
  ggsave('domestic_policy.png', width = 8*1.2, height = 5*1.2, type = 'cairo')

# Political compass 
df %>%
  filter(!is.na(left_right)) %>%
  filter_at(vars(starts_with('Q23_')),~!is.na(.x)) %>%
  mutate(`Nationalism-Internationalism` = defence + preemtive_strike + nk_pressure - non_prolif + yasukuni + safety_privacy) %>%
  mutate(`Progressivism-Conservatism` = -migrant_worker + restart_nuclear - maiden_name - gay_marriage - pol_age) %>%
  mutate_at(vars(`Nationalism-Internationalism`,`Progressivism-Conservatism`), ~scale(.x)[,1]) %>%
  select(left_right,`Nationalism-Internationalism`,`Progressivism-Conservatism`)%>%
  gather('Axis','Position',2:3) %>%
  ggplot(aes(x=left_right,y=Position,color=Axis)) +
  geom_smooth()



