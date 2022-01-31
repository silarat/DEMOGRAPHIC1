getwd()
currentwd <-"//destination////" 
setwd(currentwd)
rmarkdown::render("///destination////")

q01dat <- readRDS("q01_data.rds")
q02dat <- readRDS("q02_data.rds")
q05dat <- readRDS("q05_data.rds")
q06dat <- readRDS("q06_data.rds")

library(pacman)
library(tidyverse)
library(stringr) 
library(magrittr)
library(plyr) 



## Calculate the crude death rate (CDR) by income level
#CDR = sum over age groups of (age-specific mortality rate)*(proportion in age group)
highincome <- q01dat %>% filter(str_detect(income_level, "High"))
lowincome <- q01dat %>% filter(str_detect(income_level, "Low"))
CDRhigh <- (sum(highincome$mx*highincome$cx))*1000
CDRlow <- (sum(lowincome$mx*lowincome$cx))*1000

mydf1_2 <- data.frame(
  Income_Level = c("High Income", "Low Income"),
  CDR = c(CDRhigh, CDRlow)
)
mydf1_2 %<>% mutate_if(is.numeric, round, digits = 1)
mydf1_2

## Calculate the age-standardized CDR by income level
#standardpop s and focal pop j
ascdr_highstd <- (sum(lowincome$mx*highincome$cx))*1000 #estimated death rate of low income but with age distribution of high income
CDRhigh <- (sum(highincome$mx*highincome$cx))*1000 #estimated death rate of high income, with age distribution of high income
ascdr_lowstd <- (sum(highincome$mx*lowincome$cx))*1000 #estimated death rate of high income, but with age distribution of low income
CDRlow <- (sum(lowincome$mx*lowincome$cx))*1000 #estimated death rate of low income with age distribution of low income
ascdr_avg_high <- (sum((highincome$mx)*((highincome$cx + lowincome$cx)/2)))*1000 #estimated death rate of high income, but with average age distribution
ascdr_avg_low <- (sum((lowincome$mx)*((highincome$cx + lowincome$cx)/2)))*1000 #estimated death rate of low income, but with average age distribution


mydf1_3 <- data.frame(
  Income_Level = c("High Income", "Low Income"),
  CDR_highincomeage_as_standard = c(CDRhigh, ascdr_highstd),
  CDR_lowincomeage_as_standard = c(ascdr_lowstd, CDRlow),
  CDR_averageage_as_standard = c(ascdr_avg_high, ascdr_avg_low)
)
mydf1_3 %<>% mutate_if(is.numeric, round, digits = 1)

ggplot(data = mydf1_3, aes(x = Income_level, y = CDR_average_standard, fill = CDR_lowincome_as_standard)) +
  geom_bar(stat = "identity") #later

#-----

##calculate the crude customer conversion rate for each channel
str(q02dat)
main <- q02dat %>% filter(str_detect(website, "main"))
partner <- q02dat %>% filter(str_detect(website, "partner"))
ccr_main <- sum(main$rate*main$cx)
ccr_partner <- sum(partner$rate*partner$cx)

tv <- q02dat %>% filter(str_detect(channel, "TV"))
radio <- q02dat %>% filter(str_detect(channel, "radio"))
podcast <- q02dat %>% filter(str_detect(channel, "podcast"))
paidsearch <- q02dat %>% filter(str_detect(channel, "paid"))
organicsearch <- q02dat %>% filter(str_detect(channel, "organic"))

tvccr <- sum(tv$rate*tv$cx)
radioccr <- sum(radio$rate*radio$cx)
podcastccr <- sum(podcast$rate*podcast$cx)
paidsearchccr <- sum(paidsearch$rate*paidsearch$cx)
organicsearchccr <- sum(organicsearch$rate*organicsearch$cx)
ccr <- c(tvccr, radioccr, podcastccr, paidsearchccr, organicsearchccr)

mydf2_3 <- data.frame(
  Channel = main$channel,
  CCR = ccr
)
mydf2_3 %<>% mutate_if(is.numeric, round, digits = 2)
mydf2_3
 # we can see that paid search has low CCR

#calculate the channel-standardized CCR using main website as a standard (cdr adjusted)
ccr_main <- sum(main$rate*main$cx) #ccr for main using main as standard
ccr_mainstd_partner <- sum(partner$rate*main$cx) #ccr for partner using main as standard

tvmain <- tv %>% filter(str_detect(website, "main"))
radiomain <- radio %>% filter(str_detect(website, "main"))
podcastmain <- podcast %>% filter(str_detect(website, "main"))
paidsearchmain <- paidsearch %>% filter(str_detect(website, "main"))
organicsearchmain <- organicsearch %>% filter(str_detect(website, "main"))

ccrtvmain <- sum(tv$rate*tvmain$cx)
ccrradiomain <- sum(radio$rate*radiomain$cx)
ccrpodcastmain <- sum(podcast$rate*podcastmain$cx)
ccrpaidsearchmain <- sum(paidsearch$rate*paidsearchmain$cx)
ccrorganicsearchmain <- sum(organicsearch$rate*organicsearchmain$cx)
ccradj <- c(ccrtvmain, ccrradiomain, ccrpodcastmain, ccrpaidsearchmain, ccrorganicsearchmain)


mydf2_4 <- data.frame(
  Channel = main$channel,
  CCR = ccr,
  CCR_ADJ = ccradj
)
mydf2_4 %<>% mutate_if(is.numeric, round, digits = 2)
mydf2_4

#------
##Age and structure contributions
##report the top three age-specific contributions of mortality rate-
#schedule in terms of absolute value of contribution, sorted in order of decreasing- 
#absolute value. 
View(highincome)
cdrdiff <- (sum(lowincome$cx*lowincome$mx)) - (sum(highincome$cx*highincome$mx))

Age = (q01dat %>% filter(str_detect(income_level, "High")))$x
Highcx = (q01dat %>% filter(str_detect(income_level, "High")))$cx
Lowcx = (q01dat %>% filter(str_detect(income_level, "Low")))$cx
Highmx = (q01dat %>% filter(str_detect(income_level, "High")))$mx
Lowmx = (q01dat %>% filter(str_detect(income_level, "Low")))$mx
Agecont = 0.5 * (Highcx - Lowcx) * (Highmx + Lowmx)
Ratecont = 0.5 * (Highmx - Lowmx) * (Highcx + Lowcx)

mydf3_1 <- data.frame(
  Age = Age,
  Rate_contrib = Ratecont,
  Abs_rate_contrib = abs(Ratecont)
)
mydf3_1 %<>% arrange(desc(Abs_rate_contrib)) %<>% top_n(3)
mydf3_1

##report the top three age-specific contributions of age structure in terms of 
#absolute value of contribution, sorted in order of decreasing absolute value.
mydf3_2 <- data.frame(
  Age = Age,
  struct_contrib = Agecont,
  abs_struct_contrib = abs(Agecont)
)
mydf3_2 %<>% arrange(desc(abs_struct_contrib)) %<>% top_n(3)
mydf3_2

### Total rate schedule and age structure contributions
Total_rate <- sum(Ratecont)
Total_age <- sum(Agecont)
Total_rate
Total_age
cdrdff <- Total_rate + Total_age
#cdrdff*1000 ~ around 1 which is the original difference between the CDR between high and low income countries

newq1dat <- data.frame(
  Age = Age,
  Highcx = Highcx,
  Lowcx = Lowcx,
  Highmx = Highmx,
  Lowmx = Lowmx,
  Age_contribution = 0.5 * (Highcx - Lowcx) * (Highmx + Lowmx)
)


## Channel-specific difference contributions

channel <- (q02dat %>% filter(str_detect(website, "main")))$channel
chanmaincx <- (q02dat %>% filter(str_detect(website, "main")))$cx
chanpartcx <- (q02dat %>% filter(str_detect(website, "partner")))$cx
chanmainrate <- (q02dat %>% filter(str_detect(website, "main")))$rate
chanpartrate <- (q02dat %>% filter(str_detect(website, "partner")))$rate

ratecont <- 0.5 * (chanmainrate - chanpartrate) * (chanmaincx - chanpartcx)
channelcont <- 0.5 * (chanmaincx - chanpartcx) * (chanmainrate + chanpartrate)

mydf4_1 <- data.frame(
  Channel = channel,
  rate_contrib = ratecont,
  channel_contrib = channelcont
)
mydf4_1
## Total rate schedule and channel structure contributions

total_chan_cont <- sum(channelcont)
total_rate_cont <- sum(ratecont)
total_chan_cont + total_rate_cont

#Age-Specific termination probability
#or the oldest hire-year cohort, calculate 
#the probability of termination within 30 days following 360 days of continuous employment
str(q05dat)
#build the oldest hire year cohort
oldest_dat <- q05dat %>% filter(str_detect(hire_date, "1988"))
termdate <- as.POSIXct(oldest_dat$termination_date, format = "%Y-%m-%d")
termdate <- as.numeric(termdate)
oldest_dat$termdateseconds <- termdate
oldest_dat$termdatedays <- termdate/86400

hiredate <- as.POSIXct(oldest_dat$hire_date, format = "%Y-%m-%d")
hiredate <- as.numeric(hiredate)
oldest_dat$hiredateseconds <- hiredate
oldest_dat$hiredatedays <- hiredate/86400

oldest_dat$emplength <- oldest_dat$termdatedays - oldest_dat$hiredatedays

#build a dataset containing only those who were employed for at least 360 days
oldest_dat_360 <- oldest_dat %>% filter(emplength >= 360)
oldest_dat_30 <- oldest_dat_360 %>% filter(emplength <= 390)
#count(oldest_dat_30)
#count(oldest_dat_360)
prob <- 7/130
prob

#promotion probability
###For the oldest hire-year cohort, calculate- 
#the probability of being promoted within 60 days of your 360th day of employment
#keeping oldest_dat_360 from last exercise
promodate1 <- as.POSIXct(oldest_dat_360$promo1_date, format = "%Y-%m-%d")
promodate1 <- as.numeric(promodate1)
oldest_dat_360$promodate1sec <- promodate1
oldest_dat_360$promodate1day <- promodate1/86400

promodate2 <- as.POSIXct(oldest_dat_360$promo2_date, format = "%Y-%m-%d")
promodate2 <- as.numeric(promodate2)
oldest_dat_360$promodate2sec <- promodate2
oldest_dat_360$promodate2day <- promodate2/86400

oldest_dat_360$promlength1 <- oldest_dat_360$promodate1day - oldest_dat_360$hiredatedays
oldest_dat_360$promlength2 <- oldest_dat_360$promodate2day - oldest_dat_360$hiredatedays

promo60 <- (nrow(oldest_dat_360 %>% filter(promlength1 <= 420)))+(nrow(oldest_dat_360 %>% filter(promlength2 <= 420)))    
prob = promo60/(nrow(oldest_dat_360))
prob

