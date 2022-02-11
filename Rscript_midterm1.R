library(pacman)
library(tidyverse)
library(plyr)
library(magrittr) 
library(knitr)
library(systemfonts)
library(vctrs)
library(DT)
install.packages("pivottabler")
library(pivottabler)
rm()

rmarkdown::render("~/Downloads/XXXXXXXX")

#Quesrtion set 1
lung_surv <- readRDS(url("XXXXX"))
str(lung_surv)

#Calculate all the age- and sex-specific mortality rates for five-month "age" groups, 
#with 25 months or more as the open-ended interval. Output the results in a table below with the following structure, 
#and with mortality rates rounded to four decimal places


#need to count number of people who survive to a particular age

#separate the data for easier analysis
lungfem <- lung_surv %>% filter(str_detect(sex, "female"))
lungmale <- lung_surv %>% filter(sex == "male")

###FEMALE###
#create person years lived for each 5-month interval
lungfem <- lungfem %>% mutate(PMage0 = case_when( #for age group 0-5
  person_months < 5 ~ person_months,
  person_months >=5 ~ 5
)) %>% mutate(PMage5 = case_when(    #for age group 5-10
  person_months < 5 ~ 0,
  person_months > 10 ~ 5,
  person_months >= 5 ~ person_months-5
)) %>% mutate(PMage10 = case_when( #for age group 10-15
  person_months < 10 ~ 0,
  person_months > 15 ~ 5,
  person_months >= 10 ~ person_months-10
)) %>% mutate(PMage15 = case_when( #for age group 15-20
  person_months < 15 ~ 0,
  person_months > 20 ~ 5,
  person_months >= 15 ~ person_months-15
)) %>% mutate(PMage20 = case_when( #for age group 20-25
  person_months < 20 ~ 0,
  person_months > 25  ~ 5,
  person_months >= 20 ~ person_months-20
)) %>% mutate(PMage25 = case_when( #for age group 25 and above
  person_months < 25 ~ 0,
  person_months >=  25 ~ person_months-25
))

#create a life-table-like data frame for FEMALE
lungfem1 <- data.frame(
  x = c(0, 5, 10, 15, 20, 25),
  lx = c(
    (nrow(lungfem %>% filter(person_months >= 0))), #first age group
    (nrow(lungfem %>% filter(person_months >= 5))),
    (nrow(lungfem %>% filter(person_months >= 10))),
    (nrow(lungfem %>% filter(person_months >= 15))),
    (nrow(lungfem %>% filter(person_months >= 20))),
    (nrow(lungfem %>% filter(person_months >= 25)))),
  death_counts = c(
    (nrow(lungfem %>% filter(person_months >= 0, person_months < 5) %>% filter(status == "dead"))),
    (nrow(lungfem %>% filter(person_months >= 5, person_months < 10) %>% filter(status == "dead"))),
    (nrow(lungfem %>% filter(person_months >= 10, person_months < 15) %>% filter(status == "dead"))),
    (nrow(lungfem %>% filter(person_months >= 15, person_months < 20) %>% filter(status == "dead"))),
    (nrow(lungfem %>% filter(person_months >= 20, person_months < 25) %>% filter(status == "dead"))),
    (nrow(lungfem %>% filter(person_months >= 25) %>% filter(status == "dead")))),
  PM_lived = c(
    (sum(lungfem$PMage0)),
    (sum(lungfem$PMage5)),
    (sum(lungfem$PMage10)),
    (sum(lungfem$PMage15)),
    (sum(lungfem$PMage20)),
    (sum(lungfem$PMage25))
  )
)


#mort rate
lungfem1$mx = lungfem1$death_counts/lungfem1$PM_lived

####MALE####
#create person years lived for each 5-month interval
lungmale <- lungmale %>% mutate(PMage0 = case_when( #for age group 0-5
  person_months < 5 ~ person_months,
  person_months >=5 ~ 5
)) %>% mutate(PMage5 = case_when(    #for age group 5-10
  person_months < 5 ~ 0,
  person_months > 10 ~ 5,
  person_months >= 5 ~ person_months-5
)) %>% mutate(PMage10 = case_when( #for age group 10-15
  person_months < 10 ~ 0,
  person_months > 15 ~ 5,
  person_months >= 10 ~ person_months-10
)) %>% mutate(PMage15 = case_when( #for age group 15-20
  person_months < 15 ~ 0,
  person_months > 20 ~ 5,
  person_months >= 15 ~ person_months-15
)) %>% mutate(PMage20 = case_when( #for age group 20-25
  person_months < 20 ~ 0,
  person_months > 25  ~ 5,
  person_months >= 20 ~ person_months-20
)) %>% mutate(PMage25 = case_when( #for age group 25 and above
  person_months < 25 ~ 0,
  person_months >=  25 ~ person_months-25
))

#create a life-table-like data frame for MALE
lungmale1 <- data.frame(
  x = c(0, 5, 10, 15, 20, 25),
  lx = c(
    (nrow(lungmale %>% filter(person_months >= 0))), #first age group
    (nrow(lungmale %>% filter(person_months >= 5))),
    (nrow(lungmale %>% filter(person_months >= 10))),
    (nrow(lungmale %>% filter(person_months >= 15))),
    (nrow(lungmale %>% filter(person_months >= 20))),
    (nrow(lungmale %>% filter(person_months >= 25)))),
  death_counts = c(
    (nrow(lungmale %>% filter(person_months >= 0, person_months < 5) %>% filter(status == "dead"))),
    (nrow(lungmale %>% filter(person_months >= 5, person_months < 10) %>% filter(status == "dead"))),
    (nrow(lungmale %>% filter(person_months >= 10, person_months < 15) %>% filter(status == "dead"))),
    (nrow(lungmale %>% filter(person_months >= 15, person_months < 20) %>% filter(status == "dead"))),
    (nrow(lungmale %>% filter(person_months >= 20, person_months < 25) %>% filter(status == "dead"))),
    (nrow(lungmale %>% filter(person_months >= 25) %>% filter(status == "dead")))),
  PM_lived = c(
    (sum(lungmale$PMage0)),
    (sum(lungmale$PMage5)),
    (sum(lungmale$PMage10)),
    (sum(lungmale$PMage15)),
    (sum(lungmale$PMage20)),
    (sum(lungmale$PMage25))
))
#mort rate
lungmale1$mx = lungmale1$death_counts/lungmale1$PM_lived



#conclude  in a new table for the homework
tableq1 <- data.frame(
  x = c(0, 5, 10, 15, 20, 25),
  mx_female = lungfem1$mx,
  mx_male = lungmale1$mx
)

tableq1 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE)) %>% formatRound(columns = c("mx_female", "mx_male"), digits = 4)

##---###

#Calculate all the "age-" and sex-specific" probabilities of death for five-month "age" groups,
#with 25 months or more as the open-ended interval. Output the results in a table below with the following structure, 
#and with probabilities rounded to four decimal places


#count the numbers of persons in the data who survived to age x
#denominator has to be everyone that is not censored
#create a new variable called qxdem (denominator for calculating qx)
#this should be equal to lx just to double check because it uses the same calculations


#NOT THIS
#lungfem1$qxdem <- c(
 # (nrow(lungfem %>% filter(PMage0 > 0) %>% filter(status == "dead"))), #for age group 0-5, everyone who has PM lived during that period more than 0, and was not censored (because we never know the death fact about the censored people)
  #(nrow(lungfem %>% filter(PMage0 == 5) %>% filter(status == "dead"))), #for age group 5-10, everyone who has PM lived during 0-5 that is more than 5
  #(nrow(lungfem %>% filter(PMage5 == 5) %>% filter(status == "dead"))),
 # (nrow(lungfem %>% filter(PMage10 == 5) %>% filter(status == "dead"))),
 # (nrow(lungfem %>% filter(PMage15 == 5) %>% filter(status == "dead"))),
 # (nrow(lungfem %>% filter(PMage20 >= 5) %>% filter(status == "dead")))
#)

lungfem1$qxdem <- c(
  (nrow(lungfem %>% filter(PMage0 > 0))), #for age group 5-10, we don't count those censored between age 0 to 5 because we have no way to tell if they were alive  or dead
  (nrow(lungfem %>% filter(PMage0 == 5))),
  (nrow(lungfem %>% filter(PMage5 == 5))),
  (nrow(lungfem %>% filter(PMage10 == 5))),
  (nrow(lungfem %>% filter(PMage15 == 5))),
  (nrow(lungfem %>% filter(PMage20 >= 5)))
)


lungfem1$qx <- lungfem1$death_counts/lungfem1$qxdem

#NOT THIS
#lungmale1$qxdem <- c(
#  (nrow(lungmale %>% filter(PMage0 > 0) %>% filter(status == "dead"))), #for age group 0-5, everyone who has PM lived during that period more than 0, and was not censored (because we never know the death fact about the censored people)
#  (nrow(lungmale %>% filter(PMage0 == 5) %>% filter(status == "dead"))), #for age group 5-10, everyone who has PM lived during 0-5 that is more than 5
 # (nrow(lungmale %>% filter(PMage5 == 5) %>% filter(status == "dead"))),
 # (nrow(lungmale %>% filter(PMage10 == 5) %>% filter(status == "dead"))),
 # (nrow(lungmale %>% filter(PMage15 == 5) %>% filter(status == "dead"))),
 # (nrow(lungmale %>% filter(PMage20 >= 5) %>% filter(status == "dead")))
#)

lungmale1$qxdem <- c(
  (nrow(lungmale %>% filter(PMage0 > 0) )), 
  (nrow(lungmale %>% filter(PMage0 == 5) )), 
  (nrow(lungmale %>% filter(PMage5 == 5))),
  (nrow(lungmale %>% filter(PMage10 == 5))),
  (nrow(lungmale %>% filter(PMage15 == 5))),
  (nrow(lungmale %>% filter(PMage20 >= 5)))
)

lungmale1$qx <- lungmale1$death_counts/lungmale1$qxdem



tableq1_1 <- data.frame(
  x = c(0, 5, 10, 15, 20, 25),
  qx_female = lungfem1$qx,
  qx_male = lungmale1$qx
)

tableq1_1 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE)) %>% formatRound(columns = c("qx_female", "qx_male"), digits = 4)
  
######Question 2####
q2 <- readRDS(url("XXXXX"))
str(q2)

#demonstrate two ways to use this data to calculate the CDR by period and sex. 
#Do so using both equations (written in $\LaTeX$; you only need demonstrate how it would be 
#done for one period-sex pair) and code (code should do the calculations for all period-sex pairs)

#CDR method1 = sum((age-specific mort rate)*(Proportion in age group))
#proportion = agegroup_PY/total_PY

#separate data into periods

q2ww1f <- q2 %>% filter(World_War == "WWI") %>% filter(Sex == "Female")
q2ww1f$cx <- q2ww1f$Exposures_5x1/(sum(q2ww1f$Exposures_5x1))
q2ww1m <- q2 %>% filter(World_War == "WWI") %>% filter(Sex == "Male")
q2ww1m$cx <- q2ww1m$Exposures_5x1/(sum(q2ww1m$Exposures_5x1))
q2ww2f <- q2 %>% filter(World_War == "WWII") %>% filter(Sex == "Female")
q2ww2f$cx <- q2ww2f$Exposures_5x1/(sum(q2ww2f$Exposures_5x1))
q2ww2m <- q2 %>% filter(World_War == "WWII") %>% filter(Sex == "Male")
q2ww2m$cx <- q2ww2m$Exposures_5x1/(sum(q2ww2m$Exposures_5x1))


cdr_method1 <- c((sum(q2ww1f$Mx_5x1*q2ww1f$cx)),
                 (sum(q2ww1m$Mx_5x1*q2ww1m$cx)),
                 (sum(q2ww2f$Mx_5x1*q2ww2f$cx)),
                 (sum(q2ww2m$Mx_5x1*q2ww2m$cx)))

#CDR method 2 = death counts/total person-years lived
cdr_method2 <- c(
  (sum(q2ww1f$Deaths_5x1/q2ww1f$Exposures_5x1)),
  (sum(q2ww1m$Deaths_5x1/q2ww1m$Exposures_5x1)),
  (sum(q2ww2f$Deaths_5x1/q2ww2f$Exposures_5x1)),
  (sum(q2ww2m$Deaths_5x1/q2ww2m$Exposures_5x1)))

cdr_method2 <- c(
  (sum(q2ww1f$Deaths_5x1)/sum(q2ww1f$Exposures_5x1)),
  (sum(q2ww1m$Deaths_5x1)/sum(q2ww1m$Exposures_5x1)),
  (sum(q2ww2f$Deaths_5x1)/sum(q2ww2f$Exposures_5x1)),
  (sum(q2ww2m$Deaths_5x1)/sum(q2ww2m$Exposures_5x1)))



tableq2 <- data.frame(
  World_War = c("WWI", "WWI", "WWII", "WWII"),
  Sex = c("Female", "Male", "Female", "Male"),
  cdr_method1 = cdr_method1,
  cdr_method2 = cdr_method2
)

tableq2 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE)) %>% formatRound(columns = c("cdr_method1", "cdr_method2"), digits = 4)

#Q2.2
#Calculate the age-specific contributions to the CDR differences between males and females during WWI. 
#Output the result in a table below
cdrdiff <- abs((sum(q2ww1f$cx*q2ww1f$Mx_5x1)) - (sum(q2ww1m$cx*q2ww1m$Mx_5x1)))

tableq2_1 <- data.frame(
  Age = q2ww1f$Age,
  Mx_contrib = 0.5 * (q2ww1m$Mx_5x1 - q2ww1f$Mx_5x1) *(q2ww1m$cx + q2ww1f$cx),
  Cx_contrib = 0.5 * (q2ww1m$cx - q2ww1f$cx) * (q2ww1m$Mx_5x1 + q2ww1f$Mx_5x1)
)

tableq2_1 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE)) %>% formatRound(columns = c("Mx_contrib", "Cx_contrib"), digits = 4)

#Q2.3
#Calculate the mortality rate schedule and age composition contributions to the CDR difference between males and females during WWI.
tableq2_2 <- data.frame(
  Age_structure = sum(tableq2_1$Cx_contrib),
  Rate_schedule = sum(tableq2_1$Mx_contrib)
)
tableq2_2 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE)) %>% formatRound(columns = c("Age_structure", "Rate_schedule"), digits = 4)

#Q2.4
#Calculate the age-specific contributions to the CDR differences between males and females during WWII. 
tableq2_3 <- data.frame(
  Age = q2ww2f$Age,
  Mx_contrib = 0.5 * (q2ww2m$Mx_5x1 - q2ww2f$Mx_5x1) *(q2ww2m$cx + q2ww2f$cx),
  Cx_contrib = 0.5 * (q2ww2m$cx - q2ww2f$cx) * (q2ww2m$Mx_5x1 + q2ww2f$Mx_5x1)
)

tableq2_3 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE)) %>% formatRound(columns = c("Mx_contrib", "Cx_contrib"), digits = 4)

#Q2.3
#Calculate the mortality rate schedule and age composition contributions to the CDR difference between males and females during WWI.
tableq2_4 <- data.frame(
  Age_structure = sum(tableq2_3$Cx_contrib),
  Rate_schedule = sum(tableq2_3$Mx_contrib)
)
tableq2_4 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE)) %>% formatRound(columns = c("Age_structure", "Rate_schedule"), digits = 4)


######Question 3####
q3 <- readRDS(url("XXXXXX"))
str(q3)

#separate the data
q3_high <- q3 %>% filter(income_level == "High-income countries")
q3_low <- q3 %>% filter(income_level == "Low-income countries")
#q3.1 Build a period life table for each income level. For simplicity, 
#assume a constant force of mortality for all age groups, even the very young ages, but excluding the open age interval.

#A period life table shows what would happen to a cohort if it were subjected for all of its life to the mortality 
#conditions of that period

#start with the high-income countries
#assume nax = n/2
q3_high <- q3_high %>% mutate(nx = lead(x)-x, nax = nx/2)

q3_high <- q3_high %>% mutate(
  nqx = ((nx*mx)/(1+((nx-nax) * mx))))
q3_high$nqx %<>% replace_na(1)#fill the missing interval at the end with 1, because everyone dies at the end

#get npx which is kinda like a flip  of nqx
q3_high <- q3_high %>% mutate(npx = 1- nqx)

#get lx using typically-used arbitrary number
l0 <- 100000
lx <- l0
x <- q3_high$x
npx <- q3_high$npx
####compose a for loop
for (a in x[-length(x)]) {
  l <- lx[which(x == a)] * npx[which(x == a)] # Calculate current survivors
  lx <- c(lx, l) # Append current value to end of existing lx
}
q3_high$lx <- lx 

#get ndx (which is death counts per age group - we can do lx+n - lx OR lx*qx)
q3_high <- q3_high %>% mutate(ndx = lx*nqx)

#get nLx (PY lived between ages x and x+n. this is dffierent from small lx because lx is headcount at that age)
q3_high <- q3_high %>% mutate(Lx = case_when( #when age is not going up by 1
  nqx != 1 ~ (nx*(lead(lx)))+(nax*ndx), #when age difference is 1 (equal interval)
  nqx == 1 ~ lx/mx
))

#now get the Tx (total PY lived ABOVE that age group - we need to reverse the row in that column too)
q3_high <- q3_high %>% mutate(
  Tx = rev(Lx) %>% coalesce(0) %>% cumsum() %>% rev()
)

#get life expectancy ex
q3_high <- q3_high %>% mutate(ex = Tx/lx)
?datatable

#drop the first column to avoid redundancy and output a table
q3_high %>% select(-c(1)) %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("mx", "nax", "nqx", "npx", "lx", "ndx", "Lx", "Tx", "ex"), digits = 4)



###Low income
q3_low <- q3_low %>% mutate(nx = lead(x)-x, nax = nx/2)

q3_low <- q3_low %>% mutate(
  nqx = ((nx*mx)/(1+((nx-nax) * mx))))
q3_low$nqx %<>% replace_na(1)#fill the missing interval at the end with 1, because everyone dies at the end

#get npx which is kinda like a flip  of nqx
q3_low <- q3_low %>% mutate(npx = 1- nqx)

#get lx using typically-used arbitrary number
l0 <- 100000
lx <- l0
x <- q3_low$x
npx <- q3_low$npx
####compose a for loop
for (a in x[-length(x)]) {
  l <- lx[which(x == a)] * npx[which(x == a)] # Calculate current survivors
  lx <- c(lx, l) # Append current value to end of existing lx
}
q3_low$lx <- lx 

#get ndx (which is death counts per age group - we can do lx+n - lx OR lx*qx)
q3_low <- q3_low %>% mutate(ndx = lx*nqx)

#get nLx (PY lived between ages x and x+n. this is dffierent from small lx because lx is headcount at that age)
q3_low <- q3_low %>% mutate(Lx = case_when( #when age is not going up by 1
  nqx != 1 ~ (nx*(lead(lx)))+(nax*ndx), #when age difference is 1 (equal interval)
  nqx == 1 ~ lx/mx
))

#now get the Tx (total PY lived ABOVE that age group - we need to reverse the row in that column too)
q3_low <- q3_low %>% mutate(
  Tx = rev(Lx) %>% coalesce(0) %>% cumsum() %>% rev()
)

#get life expectancy ex
q3_low <- q3_low %>% mutate(ex = Tx/lx)

#drop the first column to avoid redundancy and output a table
q3_low %>% select(-c(1)) %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("mx", "nax", "nqx", "npx", "lx", "ndx", "Lx", "Tx", "ex"), digits = 4)

#Q3.2
#Compute xp0 for each age group x and for each life table. 
#Display the results in a table below 

#lx = l0 * xP0
#xp0 =lx/l0

#l0 = 99770.37

table3 <- data.frame(
  x = q3_high$x,
  xp0_high = (q3_high$lx)/100000,
  xp0_low = (q3_low$lx)/100000
)

table3 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("xp0_high", "xp0_low"), digits = 4)
#Q3.4
#Compute the age structure that represents the stationary population described by these two life table.

#cx in stationary population is equal to nLx/T0
T0h <- (q3_high %>% filter(x == 0))$Tx
T0l <- (q3_low %>% filter(x == 0))$Tx

table3_1 <- data.frame(
  x = q3_high$x,
  cx_actual_highincome = q3_high$cx,
  cx_stationary_highincome = q3_high$Lx/T0h,
  cx_actual_lowincome = q3_low$cx,
  cx_stationary_lowincome = q3_low$Lx/T0l
)

table3_1 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% 
  formatRound(columns = c("cx_actual_highincome", "cx_stationary_highincome", "cx_actual_lowincome", "cx_stationary_lowincome"), digits = 4)

#Q3.5
#CBR is the same as CDR in stationary population according to the assumption of having a balanced birth and death rate
#CDR = sum(mx) * (sumcx)
CBRhigh <- sum((q3_high$mx)*(q3_high$Lx/T0h))
CBRlow <- sum((q3_low$mx)*(q3_low$Lx/T0h))


