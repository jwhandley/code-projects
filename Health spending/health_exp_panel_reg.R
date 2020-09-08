library(tidyverse)
library(plm)
library(readr)
library(stargazer)

df <- read_csv('health_exp_gdp_oecd.csv')
View(df)

df %>%
  filter(!is.na(gdppc),!is.na(health_exp)) -> df

clse = function(reg) { 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg,"id")))
  N = length(index(reg,"id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

reg1 <- plm('health_exp ~ gdppc',data=df,index=c('iso','year'),model='within',effect='individual')
reg2 <- plm('health_exp ~ gdppc',data=df,index=c('iso','year'),model='between')
reg3 <- plm('health_exp ~ gdppc',data=df,index=c('iso','year'),model='pooling')
reg4 <- plm('health_exp ~ cons',data=df,index=c('iso','year'),model='within',effect='individual')
reg5 <- plm('health_exp ~ cons',data=df,index=c('iso','year'),model='between')
reg6 <- plm('health_exp ~ cons',data=df,index=c('iso','year'),model='pooling')


stargazer(reg1, reg2, reg3, reg4, reg5, reg6, se=list(clse(reg1),summary(reg2)$coefficients[,'Std. Error'],clse(reg3),clse(reg4),summary(reg5)$coefficients[,'Std. Error'],clse(reg6)), title="Health spending and GDP per capita, TSCS data", out='reg_results.html', column.labels=c("Country FE", "Between OLS", "Pooled OLS","Country FE", "Between OLS", "Pooled OLS"),dep.var.labels = 'Log Health Spending per capita at 2010 prices and PPPs',covariate.labels = c('Log real GDP per capita at PPP','Log real Consumption per capita at PPP'))

ggplot(df,aes(x=cons,y=health_exp,colour=iso)) + geom_line(size=1) + ggsave('health_exp_cons.png',width=8,height=5,type='cairo')


df %>%
  filter(iso=='USA') %>%
  summarise(mean=mean(health_exp)) -> mean

df$predict4 <- predict(reg4) + as.numeric(mean)
df$predict6 <- predict(reg6)

ggplot(filter(df,iso=='USA'),aes(x=year)) + geom_line(aes(y=health_exp),color='black',size=1) + geom_line(aes(y=predict4),color='red',size=1) + geom_line(aes(y=predict6),color='blue',size=1) + ylab('Log Health spending per capita') + ggtitle('Predicted vs. Actual US Health Expenditure') + ggsave('mod4_pred.png',type='cairo',width=8,height=5)
