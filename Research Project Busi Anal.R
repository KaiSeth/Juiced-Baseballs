# Frequency for avreage exit velocity for 2015 and 2021
gf_dhistogram(~avg_hit_speed, data = exit_velocity2015, color = 'black',
              fill = 'forestgreen', xlab = 'Average Exit Velocity (MPH)',
              ylab = 'Frequency', title = 'Frequency of Average \nExit Velocity in 2015')
gf_dhistogram(~avg_hit_speed, data = exit_velocity2022, color = 'black', 
              fill = 'slateblue4', xlab = 'Average Exit Velocity (MPH)',
              ylab = 'Frequency', title = 'Frequency of Average \nExit Velocity in 2020')

# Frequency for exit velocity above 95 MPH
gf_histogram(~ev95plus, data = exit_velocity2015, color = 'black',
              fill = 'forestgreen', xlab = '95+ Exit Velocity', 
             ylab = 'Frequency', title = '95+ Frequency 2015')
gf_histogram(~ev95plus, data = exit_velocity2022, color = 'black',
              fill = 'slateblue4', xlab = '95+ Exit Velocity', 
             ylab = 'Frequency', title = '95+ Frequency 2015')

# Frequency for plus contact percent
gf_histogram(~ev95percent, data = exit_velocity2015, color = 'black',
             fill = 'forestgreen', xlab = '95+ Exit Velocity Percent', 
             ylab = 'Frequency', title = '95+ Frequency 2015 Percent')
gf_histogram(~ev95percent, data = exit_velocity2022, color = 'black',
             fill = 'slateblue4', xlab = '95+ Exit Velocity Percent', 
             ylab = 'Frequency', title = '95+ Frequency 2015 Percent')


# Viewing the datasets
View(MLBo2015)
View(MLBo2022)


# Calculating the percent increasae in home runs
ttl_2015_hr <- sum(MLBo2015$HR)
ttl_2022_hr <- sum(MLBo2022$HR)
((ttl_2022_hr - ttl_2015_hr) / ttl_2015_hr) * 100


# Finding percent of the tto for the 2015 season
ttl_k_2015 <- sum(MLBo2015$SO)
ttl_bb_2015 <- sum(MLBo2015$BB)
ttl_ab_2015 <- sum(MLBo2015$AB)
tto2015 <- ((ttl_2015_hr + ttl_k_2015 + ttl_bb_2015) / ttl_ab_2015)
tto2015
  

# finding the percent of the tto for the 2022 season
ttl_k_2022 <- sum(MLBo2022$SO)
ttl_bb_2022 <- sum(MLBo2022$BB)
ttl_ab_2022 <- sum(MLBo2022$AB)
tto2022 <- ((ttl_2022_hr + ttl_k_2022 + ttl_bb_2022) / ttl_ab_2022)
tto2022


# create a pairs pamnel to see how strong the variables are that I chose
pairs.panels(MLBo2015[c('R', 'HR', 'Barrel %', 'Hard Hit %', 'Exit Velocity')])
pairs.panels(MLBo2022[c('R', 'HR', 'Barrel %', 'Hard Hit %', 'Exit Velocity')])


# renaming certain variables
names(MLBo2015)[5]='R'
names(MLBo2015)[9]='HR'
names(MLBo2015)[20]='BarrelPercent'
names(MLBo2015)[21]='HardHitPercent'
names(MLBo2015)[22]='EV'

names(MLBo2022)[5]='R'
names(MLBo2022)[9]='HR'
names(MLBo2022)[20]='BarrelPercent'
names(MLBo2022)[21]='HardHitPercent'
names(MLBo2022)[22]='EV'


# creating model for 2015 season
model2015 <- lm (R ~ HR + BarrelPercent + HardHitPercent + EV, data = MLBo2015)
hist(residuals(model2015), col = 'forestgreen')
plot(fitted(model2015), residuals(model2015))%>%
  abline(h = 0, lty = 2)
summary(model2015)



# creating model for 2022 season
model2022 <- lm(R ~ HR + BarrelPercent + HardHitPercent + EV, data = MLBo2022)
hist(residuals(model2022), col = 'slateblue')
plot(fitted(model2022), residuals(model2022))%>%
  abline(h = 0, lty = 2)
summary(model2022)



# Run models using training and test sets 2015
set.seed(1)
sample2015 <- sample(c(TRUE, FALSE), nrow(MLBo2015), replace = TRUE, prob = c(0.7, 0.3))
train2015 <- MLBo2015[sample2015, ]
test2015 <- MLBo2015[!sample2015, ]
dim(train2015)
dim(test2015)
lm (R ~ HR + BarrelPercent + HardHitPercent + EV, data = train2015)
lm (R ~ HR + BarrelPercent + HardHitPercent + EV, data = test2015)



# Run models using training and test sets 2022
set.seed(1)
sample2022 <- sample(c(TRUE, FALSE), nrow(MLBo2022), replace = TRUE, prob = c(0.7, 0.3))
train2022 <- MLBo2022[sample2022, ]
test2022 <- MLBo2022[!sample2022, ]
dim(train2022)
dim(test2022)
lm (R ~ HR + BarrelPercent + HardHitPercent + EV, data = train2022)
lm (R ~ HR + BarrelPercent + HardHitPercent + EV, data = test2022)

