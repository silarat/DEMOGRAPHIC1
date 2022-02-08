setwd("XXXXXX")
library(pacman)
library(tidyverse)
library(plyr)
library(magrittr) #for piping <- you need this especially for %<>% assignment operator
library(knitr)
#install.packages("systemfonts")
library(systemfonts)
library(vctrs)
#install.packages("DT")
library(DT)
dat1 <- readRDS("q01_data.rds")
dat2 <- readRDS("q02_data.rds")
dattest <- readRDS("q01_data.rds")
dat2
str(dat1)


rmarkdown::render("XXXXXX")

#Question1.3
#Px = age-specific probability of survival
dat1
dat1 %<>% mutate(px = (1-nqx))
lifetable <- data.frame(
  x = dat1$x,
  nax = dat1$nax,
  nMx = dat1$nMx,
  nqx = dat1$nqx,
  npx = dat1$px,
  lx = dat1$lx,
  ndx = dat1$ndx,
  nLx = dat1$nLx,
  Tx = dat1$Tx,
  ex = dat1$ex
)
lifetable

knitr::kable(dat1) %>%
  kableExtra::column_spec(2:10, border_left = TRUE, width_min = "5em") 
#better way of building tables!
dat1 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
  # %>%
  #DT::formatRound(columns = c("mx", "qx", "px"), digits = 6) %>%
  #DT::formatRound(columns = c("lx", "dx", "Lx", "Tx", "ex"), digits = 1)
)
#1. start with the older age group (age = 5 and above)
################
life_old <- data.frame(
  x = dat1$x,
  nMx = dat1$nMx)
#get nax
#use dplyr and make sure n comes from the lead(n)-n because it will be more accurate
life_old %<>% filter(x > 1) %>% mutate(nx = lead(x)-x, nax = nx/2)

#2. Then make a separate dataframe for younger age groups
#################

#young age (under 5)
life_young <- data.frame(
  x = dat1$x,
  nMx = dat1$nMx
)

life_young %<>% filter(x<5) %>% mutate(nx = lead(x)-x)
life_young$nx %<>% replace_na(4)
life_young

ax_young <- function(m0, # mortality rate from ages 0 to 1
                     a, b, c) { # parameters of the model
  ifelse(m0 >= 0.107, a, b + c * m0)
}
# For youngest age group 1a0
a0_male <- function(m0) ax_young(m0, 0.330, 0.045, 2.684)
a0_female <- function(m0) ax_young(m0, 0.350, 0.053, 2.800)
# For a four-year age interval between 1 and 5 (i.e., 4a1)
a1_male <- function(m0) ax_young(m0, 1.352, 1.651, -2.816)
a1_female <- function(m0) ax_young(m0, 1.361, 1.522, -1.518)
#age 0-1 mortality rate from the data
m0dat1mort <-  0.1363
#average female and male?
((a0_male(m0dat1mort))+(a0_female(m0dat1mort)))/2
naxm0 <- 0.34

#age 1-5 mortality rate from the data
m1datmort <- 0.0349
a1_male(m1datmort)
a1_female(m1datmort)
#average?
((a1_male(m1datmort))+(a1_female(m1datmort)))/2
naxm1 <- 1.510872

life_young$nax <- c(naxm0, naxm1)
###################
#simply stack two datasets on top of each other (must have the same column numbers)
life <- rbind(life_young, life_old)

###start constructing other columns
#convert nmx to nqx. 
life %<>% mutate(
  nqx = ((nx*nMx)/(1+((nx-nax) * nMx))))
life$nqx %<>% replace_na(1)#fill the missing interval at the end with 1, because everyone dies at the end

life %<>% mutate(npx = 1- nqx)
#get lx: typically the arbitrary number would be 100000, but I will do 1 to match the original life table
l0 <- 1
lx <- l0
x <- life$x
npx <- life$npx
for (a in x[-length(x)]) {
  l <- lx[which(x == a)] * npx[which(x == a)] # Calculate current survivors
  lx <- c(lx, l) # Append current value to end of existing lx
}
life$lx <- lx 

#get ndx (which is death counts per age group - we can do lx+n - lx OR lx*qx)
life %<>% mutate(ndx = lx*nqx)

#get nLx (PY lived between ages x and x+n. this is dffierent from small lx because lx is headcount at that age)
life %<>% mutate(Lx = case_when( #when age is not going up by 1
  nqx != 1 ~ (nx*(lead(lx)))+(nax*ndx), #when age difference is 1 (equal interval)
  nqx == 1 ~ lx/nMx
))

#now get the Tx (total PY lived ABOVE that age group - we need to reverse the row in that column too)
life %<>% mutate(
  Tx = rev(Lx) %>% coalesce(0) %>% cumsum() %>% rev()
)

#get life expectancy ex
life %<>% mutate(ex = Tx/lx)

life %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("nMx", "nax", "nqx", "npx", "lx", "ndx", "Lx", "Tx", "ex"), digits = 3)


###question1.5, compare nax values
compare <- data.frame(
  x = dat1$x,
  nax_original = dat1$nax,
  nax_new = life$nax,
  nMx = dat1$nMx
)
compare %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))


#q1.7 - Expected age at death = x + ex
dat1 %<>% mutate(e_a_d = x + ex)
dat1[19,11] <- NA
q1_7 <- data.frame(
  ageX = dat1$x,
  ex = dat1$ex,
  Expected_age_at_death = dat1$e_a_d
)

#q1.8
PYmiddle <- 27.19 - (sum((dat1 %>%  filter(x>=60))$Tx))
PYmiddle

PYmiddle_b <-(sum((dat1 %>% filter(x>=15, x<=65))$nLx))
PYmiddle_b



#q1.9 - total  PY lived among those who died for each age interaval = nax*ndx
dat1 %<>% mutate(nAx = nax*ndx)
q1_9 <- data.frame(
  ageX = dat1$x,
  lx = dat1$lx,
  nAx = dat1$nAx
)

dat2
####Q2########------
life2 <- dat2
#life2$nMx <- life2$mx
#life2$mx <- NULL



life2
life2 %<>% filter(x > 1) %>% mutate(nx = lead(x)-x, nax = nx/2)

life2 %<>% mutate(
  nqx = ((nx*nMx)/(1+((nx-nax) * nMx))))
life2$nqx %<>% replace_na(1)#fill the missing interval at the end with 1, because everyone dies at the end

life2 %<>% mutate(npx = 1- nqx)
#get lx using typically-used arbitrary number
l0 <- 100000
lx <- l0
x <- life2$x
npx <- life2$npx
for (a in x[-length(x)]) {
  l <- lx[which(x == a)] * npx[which(x == a)] # Calculate current survivors
  lx <- c(lx, l) # Append current value to end of existing lx
}
life2$lx <- lx 

#get ndx (which is death counts per age group - we can do lx+n - lx OR lx*qx)
life2 %<>% mutate(ndx = lx*nqx)

#get nLx (PY lived between ages x and x+n. this is dffierent from small lx because lx is headcount at that age)
life2 %<>% mutate(Lx = case_when( #when age is not going up by 1
  nqx != 1 ~ (nx*(lead(lx)))+(nax*ndx), #when age difference is 1 (equal interval)
  nqx == 1 ~ lx/nMx
))

#now get the Tx (total PY lived ABOVE that age group - we need to reverse the row in that column too)
life2 %<>% mutate(
  Tx = rev(Lx) %>% coalesce(0) %>% cumsum() %>% rev()
)

#get life expectancy ex
life2 %<>% mutate(ex = Tx/lx)

life2 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("nMx", "nax", "nqx", "npx", "lx", "ndx", "Lx", "Tx", "ex"), digits = 3)





