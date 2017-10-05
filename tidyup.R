library(tidyverse)
library(readxl) #reads excels sheets into tibbles
library(stringr) # for str_match
 readData <- function () {
  #Read in the data
  health <- read_csv('datajam-data-add-health-workshop.csv')
  dict <- read_excel('DataJam_practice_data_dictionary-170928.xlsx', skip=1)
  
  #This grabs all the categorical data (described as nominal in the spreadsheet)
  #which have defined descriptions for each available number
  factors <- dict %>%
    select(Variable, `Measurement Level`, Label__1) %>%
    fill(Variable, `Measurement Level`) %>%
    filter(`Measurement Level` == "Nominal" & !is.na(Label__1)) %>%
    select(Variable, Label__1)
  
  #Separate out the number and the description
  #e.g. (1) Yes becomes c("1", "Yes")
  factor_mat <- str_match(factors$Label__1, "\\((\\d+)\\) (.*)")
  
  factors$ordinal <- strtoi(factor_mat[,2])
  factors$description <- factor_mat[,3]
  
  factors_nested <- factors %>%
    select(-Label__1) %>%
    group_by(Variable) %>%
    nest()
  
  #replace the integers with the labelled factors
  health_labeled=health;
  for (i in 1:dim(factors_nested)[1]){
    var=factors_nested[[i,1]];
    factor_mapping=unnest(factors_nested[i,2]);
    fact_levels=factor_mapping$ordinal;
    fact_labels=factor_mapping$description;
    health_labeled[var]=factor(health_labeled[[var]], fact_levels, labels = fact_labels)
  }
  #Change large values to NA
  health_labeled %>%
    mutate(HEALTHED=ifelse(HEALTHED>20, NA, HEALTHED),
           CGPATOT=ifelse(CGPATOT>5, NA, CGPATOT),
           CGPASTEM=ifelse(CGPASTEM>5, NA, CGPASTEM),
           CRPWB=ifelse(CRPWB>5, NA, CRPWB),
           CSCLBELNG=ifelse(CSCLBELNG>5, NA, CSCLBELNG),
           SOCSPPT=ifelse(SOCSPPT>5, NA, SOCSPPT),
           CGRADE=ifelse(CGRADE>12, NA, CGRADE),
           CAGE=ifelse(CAGE>20, NA, CAGE))
}
health=readData()