library(tidyverse)
library(readxl) #reads excels sheets into tibbles
library(stringr) # for str_match
 readData <- function () {
  #Read in the data
  health <- read_csv('datajam-data-add-health-workshop.csv')
  dict <- read_excel('DataJam_practice_data_dictionary-170928.xlsx', skip=1)
  
  health2 <- health;
  
  #add HASDAD and HASMOM fields before adding the NAs
  health <- health %>% 
    mutate(HASDAD=DUSBORN!=7,
           HASMOM=MUSBORN!=7)
  
  health$PARENTS<-character(nrow(health))
  health$PARENTS[health$HASDAD&health$HASMOM]="Both parents"
  health$PARENTS[!health$HASDAD&health$HASMOM]="Mom only"
  health$PARENTS[health$HASDAD&!health$HASMOM]="Dad only"
  health$PARENTS[!health$HASDAD&!health$HASMOM]="Neither"
  
  #Change the missing value of 7 in USCITZEN (Born in the U.S.) to 1 (Yes)
  #These two factor levels mean the same thing, yet 7 is a missing value?
  health <- health %>%
    mutate(USCITZEN=ifelse(USCITZEN==7,1,USCITZEN))
  
  #pull out the NA values for nominal levels
  factor_nas <- dict %>%
    filter(!is.na(`Missing Values`)) %>%
    select(Variable,`Missing Values`) %>%
    mutate(HASTHROUGH=grepl(" through ", `Missing Values`)) %>%
    mutate(NAVALS=str_split(`Missing Values`,",| through "))

  #Some lists are ranges, but so far the list will just have two numbers. We
  #need to convert these to lists with the entire range
  #e.g. '96 through 99' -> c(96,99) -> seq(96,99)
  factor_nas$FROMS=lapply(factor_nas$NAVALS,first)
  factor_nas$FROMS=lapply(factor_nas$FROMS,str_trim)

  factor_nas$LASTS=lapply(factor_nas$NAVALS,last)
  factor_nas$LASTS=lapply(factor_nas$LASTS,str_trim)

  factor_nas$NAVALS=mapply(seq,factor_nas$FROMS,factor_nas$LASTS)
  
  factor_nas <- factor_nas %>%
    select(Variable, NAVALS)
  
  for(i in 1:dim(factor_nas)[1]){
    var=factor_nas[[i,1]]
    na_vals=factor_nas[[i,2]]
    health[var]=ifelse(health[[var]] %in% na_vals, NA, health[[var]])
  }
  
  
  #This grabs all the categorical data (described as nominal in the spreadsheet)
  #which have defined descriptions for each available number
  factors <- dict %>%
    select(Variable, `Measurement Level`, Label__1) %>%
    fill(Variable, `Measurement Level`) %>%
    filter(`Measurement Level` == "Nominal" & !is.na(Label__1)) %>%
    select(Variable, Label__1)
  
  #Separate out the number and the description
  #e.g. (1) Yes becomes c("1", "Yes")s
  factor_mat <- str_match(factors$Label__1, "\\((\\d+)\\) (.*)")
  
  factors$ordinal <- strtoi(factor_mat[,2])
  factors$description <- factor_mat[,3]
  
  factors_nested <- factors %>%
    select(-Label__1) %>%
    group_by(Variable) %>%
    nest()
  
  #replace the integers with the labelled factors
  for (i in 1:dim(factors_nested)[1]){
    var=factors_nested[[i,1]];
    factor_mapping=unnest(factors_nested[i,2]);
    fact_levels=factor_mapping$ordinal;
    fact_labels=factor_mapping$description;
    health[var]=factor(health[[var]], fact_levels, labels = fact_labels)
  }
  #Change large values to NA
  health <- health %>%
    mutate(HEALTHED=ifelse(HEALTHED>20, NA, HEALTHED),
           CGPATOT=ifelse(CGPATOT>5, NA, CGPATOT),
           CGPASTEM=ifelse(CGPASTEM>5, NA, CGPASTEM),
           CRPWB=ifelse(CRPWB>5, NA, CRPWB),
           CSCLBELNG=ifelse(CSCLBELNG>5, NA, CSCLBELNG),
           SOCSPPT=ifelse(SOCSPPT>5, NA, SOCSPPT),
           CGRADE=ifelse(CGRADE>12, NA, CGRADE),
           CAGE=ifelse(CAGE>20, NA, CAGE))
  
 
  return(health)
}
health=readData()