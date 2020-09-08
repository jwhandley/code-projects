library(tidyverse)
library(ggrepel)
library(scales)

# Load data
df <- read_csv('2017UTASP20180628.csv',locale = locale(encoding = 'cp932'))

# Clean up
df %>%
  mutate(party = recode(PARTY,
                        `1` = 'LDP',
                        `2` = 'Komeito',
                        `3` = 'Communist Party',
                        `4` = 'JIP',
                        `5` = 'SDP',
                        `6` = 'Other',
                        `7` = 'Party of Hope',
                        `8` = 'Constitutional Democratic Party',
                        `9` = 'Other',
                        `10` = 'Other',
                        `11` = 'Other')) %>%
  mutate(party = factor(party,levels=c('LDP','Komeito','Party of Hope','Constitutional Democratic Party','SDP','Communist Party','JIP','Other'))) %>%
  mutate(likeAbe = na_if(Q2_10,999), likeDPJ = na_if(Q2_2,999)) %>%
  mutate_at(vars(c(starts_with('Q4_'))), ~na_if(.x,99)) %>%
  mutate(free_educ = Q4_9, prog_tax = Q4_10, fiscal_stimulus = Q4_8, public_works = Q4_7, small_gov = Q4_6, cons_tax = na_if(Q3,99)) %>%
  mutate(defence = Q4_1, preemtive_strike = Q4_2, nk_pressure = Q4_3, non_prolif = Q4_4, yasukuni = Q4_5, safety_privacy = Q4_11) %>%
  mutate(migrant_worker = Q4_12, restart_nuclear = Q4_13, maiden_name = Q4_14, gay_marriage = Q4_15, pol_age = Q4_16, unicam = Q4_17) -> df

# Compute scores for Abe and DPJ by party
df %>%
  group_by(party) %>%
  summarise(Abe = mean(likeAbe,na.rm=T),DPJ = mean(likeDPJ,na.rm=T)) %>%
  gather(key = 'Person/Group', value = 'Warmth', c(Abe,DPJ)) %>%
  ggplot(aes(x=party,y=Warmth,fill=`Person/Group`)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = 'Party',
       y = 'Warmth (0-100 degrees)',
       title = 'Favourability of Abe and the former Democratic Party of Japan',
       subtitle = 'Candidates in the 2017 general election by party',
       caption = 'UTokyo-Asahi survey; @jwhandley17') +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  ggsave('likeAbe_DPJ.png',width=8,height=8,type='cairo')
  
# Economic issues
df %>%
  filter(RESULT>1) %>%
  group_by(party) %>%
  summarise_at(vars(free_educ,prog_tax,fiscal_stimulus,public_works,small_gov,cons_tax), ~mean(6 - .x,na.rm=T)) %>%
  rename(`Free education through university` = free_educ, `Progressive taxation` = prog_tax, `Fiscal stimulus over austerity` = fiscal_stimulus, `Public works to protect employment` = public_works, `Prefer small government` = small_gov, `Raise consumption tax` = cons_tax) %>%
  gather(key = 'Policy', value = 'Average support',-party) %>%
  ggplot(aes(x=party,y=`Average support`)) +
  geom_bar(stat = 'identity', fill = '#1f77b4') +
  facet_wrap(~Policy) +
  coord_flip() +
  theme_bw() +
  labs(x = 'Party',
       y = 'Average support (1 = Strongly disagree, 5 = Strongly agree)',
       title = 'Where Japanese legislators stand on economic policy',
       subtitle = 'MPs elected in the 2017 general election by party',
       caption = 'UTokyo-Asahi Survey; John Handley') +
  ggsave('econ_policy.png', width = 8*1.2, height = 5*1.2, type = 'cairo')

# National security issues
df %>%
  filter(RESULT>1) %>%
  group_by(party) %>%
  summarise_at(vars(defence, preemtive_strike, nk_pressure, non_prolif, yasukuni, safety_privacy), ~mean(6 - .x,na.rm=T)) %>%
  rename(`Strengthen national defence` = defence, `Pre-emptive strike` = preemtive_strike, `Put preasure on North Korea` = nk_pressure, `Nuclear non-proliferation` = non_prolif, `PM at Yasukuni shrine` = yasukuni, `Safety over privacy` = safety_privacy) %>%
  gather(key = 'Policy', value = 'Average support',-party) %>%
  ggplot(aes(x=party,y=`Average support`)) +
  geom_bar(stat = 'identity', fill = '#1f77b4') +
  facet_wrap(~Policy) +
  coord_flip() +
  theme_bw() +
  labs(x = 'Party',
       y = 'Average support (1 = Strongly disagree, 5 = Strongly agree)',
       title = 'Where Japanese legislators stand on national security',
       subtitle = 'MPs elected in the 2017 general election by party',
       caption = 'UTokyo-Asahi Survey; John Handley') +
  ggsave('foreign_policy.png', width = 8*1.2, height = 5*1.2, type = 'cairo')

# Social issues
df %>%
  filter(RESULT>1) %>%
  group_by(party) %>%
  summarise_at(vars(migrant_worker, restart_nuclear, maiden_name, gay_marriage, pol_age, unicam), ~mean(6 - .x,na.rm=T)) %>%
  rename(`Allow more migrant workers` = migrant_worker,
         `Restart nuclear power plants` = restart_nuclear,
         `Allow wives to keep their maiden names` = maiden_name,
         `Legalise gay marriage` = gay_marriage,
         `Lower age to run for office` = pol_age,
         `Unicameral legislature` = unicam) %>%
  gather(key = 'Policy', value = 'Average support',-party) %>%
  ggplot(aes(x=party,y=`Average support`)) +
  geom_bar(stat = 'identity', fill = '#1f77b4') +
  facet_wrap(~Policy) +
  coord_flip() +
  theme_bw() +
  labs(x = 'Party',
       y = 'Average support (1 = Strongly disagree, 5 = Strongly agree)',
       title = 'Where Japanese legislators stand on domestic policy and social issues',
       subtitle = 'MPs elected in the 2017 general election by party',
       caption = 'UTokyo-Asahi Survey; John Handley') +
  ggsave('domestic_policy.png', width = 8*1.2, height = 5*1.2, type = 'cairo')

# Political compass 
df %>%
  filter_at(vars(starts_with('Q4_')),~!is.na(.x)) %>%
  mutate(left_right = free_educ + prog_tax + fiscal_stimulus + public_works - small_gov) %>%
  mutate(nationalist_internationalist = defence + preemtive_strike + nk_pressure - non_prolif + yasukuni + safety_privacy) %>%
  mutate(progressive_conservative = -migrant_worker + restart_nuclear - maiden_name - gay_marriage - pol_age) %>%
  mutate_at(vars(left_right,nationalist_internationalist,progressive_conservative), ~scale(.x)) %>%
  group_by(party) %>%
  summarise_at(vars(left_right,nationalist_internationalist,progressive_conservative), ~mean(.x)) %>%
  ggplot(aes(x=left_right,y=nationalist_internationalist,color=progressive_conservative,label=party)) +
  geom_point() +
  geom_text_repel() +
  scale_color_gradient(low = 'blue', high = 'red') +
  theme_bw() +
  labs(x = 'Left-Right',
       y = 'Nationalist-Internationalist',
       color = 'Progressive-Conservative',
       title = 'Ideological positions of Japanese parties',
       subtitle = 'Average position of MPs elected in the 2017 general election by party',
       caption = 'UTokyo-Asahi Survey; John Handley') +
  coord_fixed() +
  ggsave('pol_com_3d.png', width = 8, height = 6, type = 'cairo')
