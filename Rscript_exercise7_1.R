library(pacman)
library(tidyverse)
library(plyr)
library(magrittr) 
library(knitr)
library(systemfonts)
library(vctrs)
library(DT)
library(pivottabler)
rm(list = ls())
CHLasfrRRbo <- readRDS(url("XXXXXX"))
CHLbirthsRRbo <- readRDS(url("XXXXXXXX"))

rmarkdown::render("~/Downloads/XXXXXX")
ch1 <- CHLasfrRRbo
ch2 <- CHLbirthsRRbo

str(ch1)
str(ch2)


## Compute the Total Fertility Rate (TFR) from the data provided
#Total Fertility Rate = age interval * sum of the the fertility rate of eaech of the reproductive ages between time 0 and T
#n = age interval = 1
TFR <- 1*sum(ch1$ASFR)
TFR

## Compute the General Fertility Rate (GFR) from the data provided
#GFR = number of births between time 0 and T/PY lived in the period 0 and T
#although we do not have PY information directly in the data, we can calculate it from the ASFR

#ASFR = Births in the period 0 to T to women aged x to x+n/Person-years lived in the period 0 to T by women aged x to x + n
#PYlived = no. of births/ASFR
#combine data
ch3 <- merge.data.frame(ch1, ch2) #note - merge.data.frame will automatically delete replications of columns between two dataframes that are exactly the same
ch3 <- ch3 %>% mutate(PY_lived =
                        Total/ASFR)
#mutate so that the NaN resulted from ASFR being 0 in age group 43, I assume that PY would remain the same and people survive one more year
ch3 <- ch3 %>% mutate_all(~replace(., is.nan(.), 52500.00))


GFR <- sum(ch3$Total)/sum(ch3$PY_lived)
GFR

#period first birth table (NOT a cohort life table), so we are creating a life table for a synthetic cohort
#we assume constant birth rate so nax will be n/2 = 0.5
lt <- data.frame(
  x = ch3$Age,
  n = 1,
  asfr1 = ch3$ASFR1,
  nax = 0.5
)

#qx = age-specific probability of first birth
#px = age-specific probability of no first birth 
lt <- lt %>% mutate(qx = ((n*asfr1)/(1+(n - nax) * asfr1)),
                    px = 1-qx
)

l0 <- 100000
lx <- l0
x <- lt$x
npx <- lt$px
for (a in x[-length(x)]) {
  l <- lx[which(x == a)] * npx[which(x == a)]
  lx <- c(lx, l)
}
lt$lx <- lx #lx = people who "survive" until first birth - those who hasnt had first birth yet

lt <- lt %>%
  mutate(
    lx = lx,
    dx = lx * qx,
    Lx = case_when(is.na(lead(lx)) ~ lx / asfr1,
                   TRUE ~ n * lead(lx) + nax * dx),
    Tx = rev(Lx) %>% cumsum() %>% rev(),
    ex = Tx / lx
  )

lt %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("asfr1", "nax", "qx", "px", "lx", "dx", "Lx", "Tx", "ex"), digits = 3)




#Next question
parity_data <- readRDS(url("XXXXX"))

parity <- parity_data
parity$. <- NULL

#parity measured from child's perspective = parity measured from parents' perspective + variance/Parity measured from parents perspective

#variance = variance in parity among individual people with female sex organs.
var <- max(parity$parity) - min(parity$parity)
mean_p <- mean(parity$parity)

mean_c <- mean_p+(var/mean_p)
mean_c
