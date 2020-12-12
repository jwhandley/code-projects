library(tidyverse)

df <- read_csv('anes_data.csv')

df %>%
  filter(age != 0,
         age_group != 0,
         race4cat > 0, race4cat < 9,
         race3cat > 0, race3cat < 9,
         educ > 0,
         south > 0,
         faminc > 0,
         relig > 0,
         party_id7 > 0,
         party_id3 > 0,
         turnout > 0,
         pres > 0) %>%
  mutate(age_group = recode(age_group,
                            `1` = '17-24',
                            `2` = '25-34',
                            `3` = '35-44',
                            `4` = '45-54',
                            `5` = '55-64',
                            `6` = '65-74',
                            `7` = '75-99'),
         race = recode(race4cat,
                            `1` = 'White non-hispanic',
                            `2` = 'Black non-hispanic',
                            `3` = 'Hispanic',
                            `4` = 'Other'),
         south = as.numeric(south==1),
         faminc = recode(faminc,
                            `1` = '0-16p',
                            `2` = '17-33p',
                            `3` = '34-67p',
                            `4` = '68-95p',
                            `5` = '95-100p'),
         relig = recode(relig,`1` = 'Protestant',
                              `2` = 'Catholic',
                              `3` = 'Other',
                              `4` = 'Other'),
         educ = case_when(educ == 1 ~ 'High school or less',
                          educ == 2 ~ 'High school or less',
                          educ == 3 ~ 'Some college',
                          educ == 4 ~ 'Degree'),
         party_id = case_when(party_id3 == 1 ~ 'Democrat',
                              party_id3 == 2 ~ 'Independent',
                              party_id3 == 3 ~ 'Republican'),
         pres = case_when(pres == 1 ~ 'Democrat',
                          pres == 2 ~ 'Republican',
                          pres == 3 ~ 'Other',
                          pres == 4 ~ 'Other',
                          pres == 7 ~ "Didn't vote")) %>%
  select(year,wt0,wt1,wt2,age_group,race,educ,faminc,south,relig,pres) -> data

data %>%
  write_csv('cleaned_data.csv')
