library(tidyverse)
library(readr)

df <- read_csv('mpd_dataset.csv')

df %>%
  mutate(partyfamily=recode(parfam,`30`='Social Democratic',`50`='Christian Democratic',`40`='Liberal',`60`='Conservative'),usa=recode(as.numeric(countryname=='United States'),`1`='United States',`0`='Other countries')) -> data

g1 <- ggplot(filter(data,year>=1945,!is.na(partyfamily)),aes(x=year,y=rile,colour=partyfamily)) + geom_smooth() + scale_color_manual(values=c('black','blue','orange','red')) + scale_y_continuous(limits=c(-30,30))
g2 <- ggplot(filter(df,year>1945,countryname=='United States',!is.na(partyabbrev),!is.na(rile),!is.na(year)),aes(x=year,y=rile,colour=partyabbrev)) + geom_line(size=1) + geom_point() + scale_color_manual(values=c('blue','red')) + scale_y_continuous(limits=c(-30,30))

library(cowplot)

plot_grid(g1, g2, labels = c('A', 'B'), label_size = 12)


ggplot(filter(data,year>=1945,!is.na(partyfamily)),aes(x=year,y=rile)) + geom_smooth(aes(color=partyfamily)) + facet_grid(~usa) + scale_color_manual(values=c('black','blue','orange','red')) + ylab('Overall left-right position') + ggtitle('Left and Right in the US and Europe')#ggsave('us_eu_lr.png',width=8,height=5,type='cairo')
