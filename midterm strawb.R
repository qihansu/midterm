
myName <- "Qihan Su"

library(tidyverse)
library(magrittr)
library(readxl)


strawb <- read_xlsx("/Users/suqihan/Desktop/strawberries-2022oct30-a (1).xlsx")


cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

## Explore data by viewing it in R.  
## Double click the strawb data frame to lauch the view() function.
view(strawb)


## Start by examining the content of the columns

## Column 1 contains two unique values.  
## Retain column 1 -- those values might be needed.
unique(strawb[1])

## Column 2 -- contains the years included in this dataset.
## Keep column 2, of course.
unique(strawb[2])

## Column 3 -- contains the time periods covered by in the dataset.
## There's only one -- years.  No info here.  Drop it
unique(strawb[3])



## Set T as an indicator
T <- NULL

## Collect number of unique rows in each column
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}

## Use T to select columns to drop -- 
drop_cols <- cnames[which(T == 1)]

## Now, drop the columns with only one unique value.
strawb %<>% select(!all_of(drop_cols))

## Let's arrange the data frame by year and state.
strawb %<>% arrange(Year, State)

view(strawb)

## Look at the strawb data frame again. You can see that the 
## columns need work. The State ANSI column contains a unique
## code for each state. If you need to access US Census data for
## the states, this code will come in handy.

colnames(strawb)

## now look at the `Data Item` column

temp1 <- strawb %>% select(`Data Item`) %>% 
         distinct()



strawb2 <- strawb %>% separate(col=`Data Item`,
                into = c("Strawberries", "items", "units"),
                sep = ",",
                fill = "right")

## try 4 columns

strawb3 <- strawb %>% separate(col=`Data Item`,
            into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")

## That worked. Clean up the dat.

rm(strawb2, strawb3)

strawb %<>% separate(col=`Data Item`,
                    into = c("Strawberries", "type", "items", "units"),
                    sep = ",",
                    fill = "right")

view(strawb)
## now explore the new columns

## we know that "THIRAM" is a chemical in the data, so
## test for it to check out the way code
r_thiram <- grep("THIRAM", strawb$`Domain Category`)
r_thiram_1 <- grep("Thiram", 
                   strawb$`Domain Category`, 
                   ignore.case = T)

## Chemicals mentioned in 
## the "Shoppers Guide to Pesticides in Produce"
## Carbendazim, Bifenthrin, methyl bromide, 1,3-dichloropropene,
## chloropicrin, Telone

df_carbendazim <- grep("carbendazim", 
                       strawb$`Domain Category`, ignore.case = T)

## Bifenthrin found 27
df_Bifenthrin <- grep("Bifenthrin", 
                       strawb$`Domain Category`, ignore.case = T)

## methyl bromide found 3
df_methyl_bromide <- grep("methyl bromide", 
                      strawb$`Domain Category`, ignore.case = T)

## 1,3-dichloropropene empty
df_1_3_dichloropropene <- grep("1,3-dichloropropene", 
                          strawb$`Domain Category`, 
                          ignore.case = T)

## chloropicrin found 18
df_chloropicrin <- grep("chloropicrin", 
                               strawb$`Domain Category`, 
                               ignore.case = T)

## Telone empty
df_Telone <- grep("Telone", 
                        strawb$`Domain Category`, 
                        ignore.case = T)



##1
285*100=28500LB




##2
strawb_2016_cal_organ <- filter(strawb, Year == "2016" & State == "CALIFORNIA" & Domain == "ORGANIC STATUS")
view(strawb_2016_cal_organ)
library(gmodels)
library(Rmisc)
CI(as.numeric(strawb_2016_cal_organ$Value))

##3

strawb_2016_cal_norgan <- filter(strawb, Year == "2016" & State == "CALIFORNIA" & Domain != "ORGANIC STATUS" )
view(strawb_2016_cal_norgan)
new <- filter(strawb_2016_cal_norgan, Value != "(NA)" & Value != "(D)" & Domain != "TOTAL")
view(new)
margin <- 231304956*1.96*0.137
margin
highervalue <- 231304956+62110007
highervalue
lowervalue <- 231304956-62110007
lowervalue

##4
chemical <- filter(strawb, Domain != 'ORGANIC STATUS' & 
                     Domain != 'TOTAL' & 
                     Domain != 'FERTILIZER')
chemical
unique(chemical[11])

##5
chemical_fl <- filter(strawb, State == 'FLORIDA' & 
                        Domain != 'ORGANIC STATUS' & 
                        Domain != 'TOTAL' & 
                        Domain != 'FERTILIZER')
chemical_cal <- filter(strawb, State == 'CALIFORNIA' & 
                        Domain != 'ORGANIC STATUS' & 
                        Domain != 'TOTAL' & 
                        Domain != 'FERTILIZER')
unique(chemical_fl[22])
unique(chemical_cal[22])
ans_5 = 138 - 115



