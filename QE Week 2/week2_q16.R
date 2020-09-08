library(tidyverse)

df <- read_csv('tsadata.csv')

# plot and save histogram of the TSA score

png(file='hist_tsa.png')
hist(df$TSA,xlab='TSA Score',main='Histogram of TSA Score')
dev.off()

# Estimate mean and standard deviation of the distribution of scores

# Mean
mean <- mean(df$TSA)

# Standard deviation

sd <- sd(df$TSA)


# Plot and save fitted normal distribution of TSA scores
png(file='density_tsa.png')
plot(seq(20,100,1),dnorm(x=seq(20,100,1),mean=mean,sd=sd),type='l',xlab='TSA scores',ylab='Fitted density',main='Fitted Density of TSA Scores')
dev.off()

# Compare histogram with fitted normal distribution

df %>%
  mutate(n=n(),mean=mean(TSA),sd=sd(TSA),binwidth=5) %>%
  ggplot(aes(x=TSA,mean=mean,sd=sd,n=n,binwidth=binwidth)) + geom_histogram(aes(y=..density..)) + stat_function(fun=dnorm,args=c(mean=mean,sd=sd),color='blue',size=1) + ggtitle('Comparison of histogram and fitted normal for TSA Scores') + ggsave('hist_density_compare.png',width=8,height=5,type='cairo')

# Plot fitted normals for the following on a single set of axes and comment briefly on the differences

x <- seq(20,100,1)

# Gender

# Calculate means and standard deviations for each gender
mean_f <- mean(filter(df,Gender=='F')$TSA)
sd_f <- sd(filter(df,Gender=='F')$TSA)
mean_m <- mean(filter(df,Gender=='M')$TSA)
sd_m <- sd(filter(df,Gender=='M')$TSA)


# Estimate densities using a normal distribution
Male <- dnorm(x=x,mean=mean_m,sd=sd_m)
Female <- dnorm(x=x,mean=mean_f,sd=sd_f)

# Combine and plot data
data.frame(x,Male,Female) %>%
  gather(key='Gender',value='Density',c(Male,Female)) %>%
  ggplot(aes(x=x,color=Gender,y=Density)) + geom_line() + ggtitle('Fitted distribution of TSA scores by gender') + xlab('TSA score') + ggsave('gender_plot.png',width=8,height=5,type='cairo')

# School types

df %>%
  group_by(SchoolType) %>%
  summarise(mean=mean(TSA),sd=sd(TSA)) -> school

State <- dnorm(x,mean=filter(school,SchoolType=='S')$mean,filter(school,SchoolType=='S')$sd)  
Independent <- dnorm(x,mean=filter(school,SchoolType=='I')$mean,filter(school,SchoolType=='I')$sd)  
Overseas <- dnorm(x,mean=filter(school,SchoolType=='O')$mean,filter(school,SchoolType=='O')$sd)  

data.frame(x,State,Independent,Overseas) %>%
  gather(key='School Type',value='Density',c(State,Independent,Overseas)) %>%
  ggplot(aes(x=x,color=`School Type`,y=Density)) + geom_line() + ggtitle('Fitted distribution of TSA scores by school type') + xlab('TSA score') + ggsave('school_plot.png',width=8,height=5,type='cairo')

# Admission status

df %>%
  group_by(Admit) %>%
  summarise(mean=mean(TSA),sd=sd(TSA)) -> admit

admitted <- dnorm(x,mean=filter(admit,Admit==1)$mean,sd=filter(admit,Admit==1)$sd)
non_admit <- dnorm(x,mean=filter(admit,Admit==0)$mean,sd=filter(admit,Admit==0)$sd)

data.frame(x,admitted,non_admit) %>%
  gather(key='Admission',value='Density',c(admitted,non_admit)) %>%
  ggplot(aes(x=x,color=Admission,y=Density)) + geom_line() + scale_color_manual(values=c('red','blue'),labels=c('Admitted','Not Admitted')) + ggtitle('Fitted distribution of TSA scores by admission status') + xlab('TSA score') + ggsave('admit_plot.png',width=8,height=5,type='cairo')

# Test the hypothesis that

# females did worse than males

t.test(TSA ~ Gender,data=df,alternative='less')
# Can reject the null

# independent schools did better than state schools
t.test(TSA ~ SchoolType,data=filter(df,SchoolType != 'O'),alternative='greater')
# Cannot reject the null

# admitted and non-admitted has the same scores
t.test(TSA ~ Admit,data=df)
# Can reject the null
