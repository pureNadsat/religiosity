
# Senior thesis: Religiosity and Wage Differentials: Does Religion Play a Role in Gender Income Inequality?

setwd('C:/Users/craig/Documents/R/R Directory/religiosity')

library('plyr')
library('foreign')
library('calibrate')

# IPUMS data; includes demographic, occupational, and wage variables
data = read.csv('cps_00012.csv')

# Hispanic data 
data1 = read.csv('cps_00013.csv')

#religiosity data obtained from Pew Research Center
relig = read.csv('relig.csv')

data = cbind(data, data1)
rm(data1)

colnames(data) = c('stfp', 'cityStatus', 'nchild', 'youngestChild', 'age', 'gender', 'race', 'marst',
                   'education', 'occupation', 'workHours', 'income', 'hispanic')

# match state religiosity data with IPUMS state ids
data = merge(data, relig, by.wtf = 'sftp')
rm(relig)

# include only full-time workers
data = data[(data$workHours >= 40 & data$workHours < 997 & data$income > 0),]

# hourly wage
data$wage = data$income/(52*data$workHours)

# include workers making over federal minimum wage
data = data[data$wage > 7.25,]

############################## clean ipum codes ###################################

# replace 'not in universe' IPUM codes with median age of a family unit's youngest child
data$youngestChild[data$youngestChild == 99] = median(data$youngestChild[data$youngestChild != 99])

# gender 
data$male = ifelse(data$gender == 1, 1, 0)
data$female = ifelse(data$gender == 2, 1, 0)

# interaction variable combining female observations with their state's religiosity expressed in percentages
data$religiosity = data$religiosity*100
data$femaleRelig = data$female*data$religiosity

#race
data$race = ifelse(data$race == 100 & data$hispanic == 0, 'white',
            ifelse(data$race == 200 & data$hispanic == 0, 'black', 
            ifelse(data$hispanic != 0 & data$hispanic > 412, 'latino', 
            ifelse(data$race %in% c(650:651) & data$hispanic == 0, 'asian', 
            ifelse(data$race %in% c(652:700, 999) & data$hispanic == 0, 'other race', 
            'multiracial')))))

# urbanization
data$cityStatus = ifelse(data$cityStatus %in% c(1:2), 'urban',
                    ifelse(data$cityStatus == 3, 'outside urban', 'rural'))

#education
data$education = ifelse(data$education <= 72 | data$education == 999, 'hs dropout', 
                 ifelse(data$education == 73, 'highschool', 
                 ifelse(data$education > 73 & data$education <= 110, 'some college', 
                 ifelse(data$education == 111, 'bachelors',
                 ifelse(data$education %in% c(120:122), 'post graduate',
                 ifelse(data$education == 123, 'masters', 'doctorate'))))))

#marital status
data$marst = ifelse(data$marst %in% c(6, 9),'single',
             ifelse(data$marst %in% c(1:2), 'married',
             ifelse(data$marst == 3, 'separated',
             ifelse(data$marst == 4, 'divorced', 'widowed'))))

# occupation according to 1990 census classification; 
# MPSO: Managerial and Professional Specialty Occupations
# TSAS: Technical, Sales, and Administrativ Support Occupations
# FFF: Farming, Forestry, and Fishing
# OFL: Operators, Fabricators, and Laborers
data$occupation = ifelse(data$occupation %in% c(1:200), 'MPSO',
                       ifelse(data$occupation %in% c(203:469), 'TSAS',
                              ifelse(data$occupation %in% c(473:699), 'FFF',
                                     ifelse(data$occupation %in% c(700:890), 'OFL', 'Military'))))

# relevel factors within data frame to specifiy reference variables
data[, c('occupation', 'education', 'marst', 'race', 'cityStatus')] = lapply(
data[, c('occupation', 'education', 'marst', 'race', 'cityStatus')], factor) 

data = within(data, marst <- relevel(marst, ref = 4)) 
data = within(data, education <- relevel(education, ref = 3))
data = within(data, race <- relevel(race, ref = 6))
data = within(data, occupation <- relevel(occupation, ref = 3))
data = within(data, cityStatus <- relevel(cityStatus, ref = 2))

################################# Regression #################################

reg = lm(log(wage) ~ femaleRelig + female + religiosity + age + nchild + youngestChild +
               race + cityStatus + education + occupation + workHours, data = data)

summary(reg)

######################### Scatter plot with  residuals #######################
plot(data$femaleRelig[data$femaleRelig != 0], data[data$femaleRelig != 0,]$income, xlim = c(min(data$femaleRelig[data$femaleRelig != 0]),
     max(data$femaleRelig[data$femaleRelig != 0])), ylim = c(0, 150000))

plot(data[data$female == 1,]$religiosity, data[data$female == 1,]$wage, xlim = c(min(data$religiosity),
     max(data$religiosity)), ylim = c(0, 600))
     










