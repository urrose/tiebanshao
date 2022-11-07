myName <- "Yingnan Lyu"

library(tidyverse)
library(magrittr)
library(readxl)

strawb <- read_xlsx("/Users/roselv/Downloads/strawberries-2022oct30-a.xlsx", col_names = T)

cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]
unique(strawb[1])
unique(strawb[2])
unique(strawb[3])

T <- NULL
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}
drop_cols <- cnames[which(T == 1)]
strawb %<>% select(!all_of(drop_cols))
strawb %<>% arrange(Year, State)
temp1 <- strawb %>% select(`Data Item`) %>% 
  distinct()

strawb2 <- strawb %>% separate(col=`Data Item`,
                               into = c("Strawberries", "items", "units"),
                               sep = ",",
                               fill = "right")
strawb3 <- strawb %>% separate(col=`Data Item`,
                               into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")
rm(strawb2, strawb3)
strawb %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")


##2
CA_orgstrb_2016 <- filter(strawb, State == 'CALIFORNIA' & 
                               Year == 2016 & 
                               Domain == 'ORGANIC STATUS')
margin1 <- 231304956*1.96*0.137
upper1 <- 231304956+62110007
lower1 <- 231304956-62110007

##3
CA_non_orgstrb_2016 <- filter(strawb, State == 'CALIFORNIA' & 
                                  Year == 2016 & 
                                  Domain != 'ORGANIC STATUS')
new_non <- filter(CA_non_orgstrb_2016, Value != "(NA)" & 
                    Value != "(D)" & 
                    Domain != "TOTAL")
library(gmodels)
library(Rmisc)
CI(as.numeric(new_non$Value))

##4
unique(strawb[10])
No.chemical <- filter(strawb, Domain != 'ORGANIC STATUS' & 
                     Domain != 'TOTAL')
grep("TOTAL",
     No.chemical$`Domain Category`,
     ignore.case = T)
unique(No.chemical[11])
175 - 36

##5
chem_FL <- filter(strawb, State == 'FLORIDA' & 
                        Domain != 'ORGANIC STATUS' & 
                        Domain != 'TOTAL')
chem_CA <- filter(strawb, State == 'CALIFORNIA' & 
                        Domain != 'ORGANIC STATUS' & 
                        Domain != 'TOTAL')
grep("TOTAL",
     chem_FL$`Domain Category`,
     ignore.case = T)
unique(chem_FL[11])


grep("TOTAL",
     chem_CA$`Domain Category`,
     ignore.case = T)

unique(chem_CA[11])
#ans=(142-16)-(119-16)=23