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

rmarkdown::render("~/Downloads/XXXXXXXXX")

#{r data-collection}
repo_url <- "XXXXXXXXX"
rus_Mxi_Dxi <-
  readRDS(url(paste0(repo_url, "/blob/main/data/rus_Mxi_Dxi.rds?raw=true")))
jpn_Mxi_Dxi <-
  readRDS(url(paste0(repo_url, "/blob/main/data/jpn_Mxi_Dxi.rds?raw=true")))
rus <- rus_Mxi_Dxi
jpn <- jpn_Mxi_Dxi
str(rus)

#ggplot for proportional hazard
rus_Mxi_Dxi %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = x) +
  ggplot2::geom_line(aes(y = Mx_heart + Mx_other), color = "black") +
  ggplot2::geom_point(aes(y = Mx_heart + Mx_other), color = "black") +
  ggplot2::geom_line(aes(y = Mx_heart), color = "darkorange") +
  ggplot2::geom_point(aes(y = Mx_heart), color = "darkorange") + 
  ggplot2::annotate(
    geom = "text",
    x = 50, y = 0.025,
    label = "All causes",
    color = "black"
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 60, y = 0,
    label = "Heart disease",
    color = "darkorange"
  ) +
  ggplot2::scale_x_continuous(breaks = rus_Mxi_Dxi$x, labels = rus_Mxi_Dxi$x) +
  ggplot2::labs(
    title = "Cause- and age-specific mortality rates in 1997 Russia",
    x = "Exact age",
    y = "Age-specific\nmortality rate"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid = element_blank(),
                 axis.title.y = element_text(angle = 0))

#Construct the "parent life table" - which is a normal single decrement life table
#assume nax = n/2

###RUSSIA###
#find mx by adding up all causes
rus1 <- rus %>% mutate(mx = Mx_heart + Mx_other)
rus1 <- rus1 %>% mutate(nx = lead(x)-x, 
                    nax = nx/2,
                    nqx = ((nx * mx)/(1+(nx - nax)*mx)) %>% coalesce(1),
                    npx = 1- nqx
)
#ignore Dx and find lx first
l0 <- 100000
lx <- l0
x <- rus1$x
npx <- rus1$npx
for (a in x[-length(x)]) {
  l <- lx[which(x == a)] * npx[which(x == a)] # Calculate current survivors
  lx <- c(lx, l) # Append current value to end of existing lx
}
rus1$lx <- lx

#get ndx
rus1 <- rus1 %>% mutate(ndx = lx * nqx)


rus1 <- rus1 %>% mutate(
  nLx = case_when(
    nqx != 1 ~ nx * lead(lx) +nax * ndx,
    nqx == 1 ~ lx/mx
  ),
  Tx = rev(nLx) %>% coalesce(0) %>% cumsum() %>% rev(),
  ex = Tx/lx
)

rus1 %>% select(-c(2:5)) %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("mx", "nax", "nqx", "npx", "lx", "ndx", "nLx", "Tx", "ex"), digits = 4)

#calculate the constant of proportionality of interest (Ri or Rx) for the cause-deleted life table for each age group

#Rx = nDxi/nDx (NOT the ratio of decrements computed during the life table procedure)

rus1_1 <- data.frame(
  x = rus1$x,
  Rx = rus1$Dx_other/(rus1$Dx_other + rus1$Dx_heart)
)
rus1_1 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("Rx"), digits = 4)


#calculating px* (Within-interval probability of survival in the cause-deleted life table)
#px* = npx^Ri

rus1_2 <- data.frame(
  x = rus1$x,
  px = rus1$npx,
  px_star = (rus1$npx)^(rus1_1$Rx)
)
rus1_2 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("px", "px_star"), digits = 4)

#create a cause-deleted life table 
rus_cd <- data.frame(
  x = rus1$x,
  nx = rus1$nx,
  pxstar = rus1_2$px_star
  #we dont have mx or dx
)

rus_cd <- rus_cd %>% mutate(nqxstar = 1 - pxstar) #now we got qxstar
#start lx
l0 <- 100000
lx <- l0
x <- rus_cd$x
npx <- rus_cd$pxstar
for (a in x[-length(x)]) {
  l <- lx[which(x == a)] * npx[which(x == a)] # Calculate current survivors
  lx <- c(lx, l) # Append current value to end of existing lx
}
rus_cd$lxstar <- lx

#get ndx
rus_cd <- rus_cd %>% mutate(dxstar = lxstar * nqxstar)

rus_cd %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("nqxstar", "dxstar", "lxstar", "pxstar"), digits = 4)

#we cannot calculate nLx, Tx, abd ex just yet, because we do not know nax
#create a new dataset so that we can split the age group into young, middle, and old

#start with the middle group

#rus_cd_complete <- rus_cd %>% mutate(naxstar = case_when(
#  x == 80 ~ 
#))
rus_cd_middle <- rus_cd %>% filter(x >= 5, x <= 80)

rus_cd_middle <- rus_cd_middle %>% mutate(naxstar =
                                            (
                                            (((-(nx/24)*lag(nqxstar)))+
                                              ((nx/2)*nqxstar)+
                                              ((nx/24)*lead(nqxstar)))
                                            /nqxstar
                                            )
)
#remove the top and bottom rows
rus_cd_middle <- rus_cd_middle %>% drop_na()


#young group and old group but closed interval
rus_cd_youngold <- rus_cd
rus_cd_youngold$nax <- rus1$nax #add the nax value
rus_cd_youngold$ri <- rus1_1$Rx #add the Ri value
rus_cd_youngold$nqx <- rus1$nqx #add the original nqx value
rus_cd_youngold <- rus_cd_youngold %>% mutate(
  naxstar = nx + ((ri*(nqx/nqxstar))*(nax - nx)) #calculate nax star
) 
#only keep the young age and older age group
rus_cd_youngold <- rus_cd_youngold[-c((4:17),19),]
rus_cd_youngold <- rus_cd_youngold %>% select(x, nx, pxstar, dxstar, nqxstar, lxstar, naxstar)

#join the young and middle group by stacking the dataframe on top of each other
rus_cd_draft <- rbind(rus_cd_youngold[c(1,2,3),], rus_cd_middle)
rus_cd_draft1 <- rbind(rus_cd_draft, rus_cd_youngold[4,])

#take care of the oldest group that is open-interval
rus_cd_old <- rus_cd
rus_cd_old$ex <- rus1$ex #add the ex value from parent life table
rus_cd_old$ri <- rus1_1$Rx #add the Ri value
rus_cd_old <- rus_cd_old %>% mutate(naxstar = (ex/ri)) %>% filter(x >= 85)
rus_cd_old <- rus_cd_old %>% select(x, nx, pxstar, dxstar, nqxstar, lxstar, naxstar)

#finally, stack the dataframe on top of this little column of open-age interval
rus_cd_complete <- rbind(rus_cd_draft1, rus_cd_old)


#complete the cause-deleted life table
#because we have chosen the nax values based on the assumption of proportional hazards,
#we can now complete the cause-deleted life table, but we need to calculate mxstar because mx != mxstar
#mxstar = dxstar/nLxstar

rus_cd_complete <- rus_cd_complete %>% mutate(
  nLxstar = nx * lead(lxstar) + naxstar * dxstar,
  Txstar = rev(nLxstar) %>% coalesce(0) %>% cumsum() %>% rev(),
  exstar = Txstar/lxstar,
  mxstar = dxstar/nLxstar
)

rus_cd_complete %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("nqxstar", "dxstar", "lxstar", "pxstar", "mxstar", "nLxstar", "Txstar", "naxstar", "exstar" ), digits = 4)

#Using the "parent" life table and the cause-deleted life table, calculate the effect of heart disease mortality on 
#life expectancy at each age.

rus1_3 <- data.frame(
  x = rus1$x,
  ex_diff = rus_cd_complete$exstar - rus1$ex
)
ex_pdiff <- 100*(rus1_3$ex_diff)/(rus1$ex)
rus1_3$ex_pdiff <- ex_pdiff

rus1_3 %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE)) %>% formatRound(columns = c("ex_diff", "ex_pdiff"), digits = 4)


###JAPAN LIFE TABLE (ALL CAUSES)
#find mx by adding up all causes
jpn1 <- jpn %>% mutate(mx = Mx_heart + Mx_other)
jpn1 <- jpn1 %>% mutate(nx = lead(x)-x, 
                        nax = nx/2,
                        nqx = ((nx * mx)/(1+(nx - nax)*mx)) %>% coalesce(1),
                        npx = 1- nqx
)
#ignore Dx and find lx first
l0 <- 100000
lx <- l0
x <- jpn1$x
npx <- jpn1$npx
for (a in x[-length(x)]) {
  l <- lx[which(x == a)] * npx[which(x == a)] # Calculate current survivors
  lx <- c(lx, l) # Append current value to end of existing lx
}
jpn1$lx <- lx

#get ndx
jpn1 <- jpn1 %>% mutate(ndx = lx * nqx)


jpn1 <- jpn1 %>% mutate(
  nLx = case_when(
    nqx != 1 ~ nx * lead(lx) +nax * ndx,
    nqx == 1 ~ lx/mx
  ),
  Tx = rev(nLx) %>% coalesce(0) %>% cumsum() %>% rev(),
  ex = Tx/lx
)

jpn1 %>% select(-c(2:5)) %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)) %>% formatRound(columns = c("mx", "nax", "nqx", "npx", "lx", "ndx", "nLx", "Tx", "ex"), digits = 4)

#create another combined data frame for both Russia and Japan so we can work on both data combined
#in this case, Russia = 1, Japan = 2
newdat <- data.frame(
  x = rus1$x,
  nx = rus1$nx,
  lx1 = rus1$lx,
  nLx1 = rus1$nLx,
  Tx1 = rus1$Tx,
  lx2 = jpn1$lx,
  nLx2 = jpn1$nLx,
  Tx2 = jpn1$Tx
)

newdat <- newdat %>% mutate(deltax = case_when(
  x < 85 ~ ((lx1/100000)*((nLx2/lx2)-(nLx1/lx1))) +
                              (((lead(Tx2))/100000)*((lx1/lx2)-(lead(lx1)/lead(lx2)))),
  x == 85 ~ (lx2/100000)*((Tx2/lx2) - (Tx1/lx1))
))
newdat %>% select(-c(2:8)) %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE)) %>% formatRound(columns = c("deltax"), digits = 4)

### Calculate the cause-specific contributions for each age group

#let's start with getting Ri for each cause, and for each country
Ri_R_H <- rus1$Dx_heart/(rus1$Dx_other + rus1$Dx_heart)
Ri_R_O <- rus1$Dx_other/(rus1$Dx_other + rus1$Dx_heart)
Ri_J_H <- jpn1$Dx_heart/(jpn1$Dx_other + jpn1$Dx_heart)
Ri_J_O <- jpn1$Dx_other/(jpn1$Dx_other + jpn1$Dx_heart)

newdat$Ri_R_H <- Ri_R_H
newdat$Ri_R_O <- Ri_R_O
newdat$Ri_J_H <- Ri_J_H
newdat$Ri_J_O <- Ri_J_O

newdat$nmx_jpn <- jpn1$mx
newdat$nmx_rus <- rus1$mx
newdat$nmxh_jpn <- jpn1$Mx_heart
newdat$nmxo_jpn <- jpn1$Mx_other
newdat$nmxh_rus <- rus1$Mx_heart
newdat$nmxo_rus <- rus1$Mx_other

#now let's calculate each component

newdat <- newdat %>% mutate(d_Mx_h = nmxh_jpn - nmxh_rus,
                            d_Mx_o = nmxo_jpn - nmxo_rus,
                            d_Mx = nmx_jpn - nmx_rus,  
                            Deltax_Mx_h = 
                              deltax * ((Ri_J_H * nmx_jpn)-(Ri_R_H * nmx_rus))/(nmx_jpn - nmx_rus),
                            Deltax_Mx_o =
                              deltax * ((Ri_J_O * nmx_jpn)-(Ri_R_O * nmx_rus))/(nmx_jpn - nmx_rus),
                            )

newdat %>% select(c(1,9,20:24)) %>% datatable(
  rownames = FALSE,
  filter = "top",
  options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE)) %>% formatRound(columns = c("d_Mx_h", "d_Mx_o", "d_Mx", "Deltax_Mx_h", "Deltax_Mx_o"), digits = 4)

#calculate the TOTAL cause-specific contributions from the age- and cause-specific contributions

newdat2 <- data.frame(
  Delta_heart = sum(as.numeric(newdat$Deltax_Mx_h)),
  Delta_other = sum(as.numeric(newdat$Deltax_Mx_o))
)
newdat2

#the total difference in life expectancy is the sum across all age- and cause- specific components, so it is essential just

totaldiff <- newdat2$Delta_heart + newdat2$Delta_other
totaldiff