
original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review"
setwd(original.data.dir)
setwd("Cooney 2016")

library(readxl)
dat = read_xlsx("cleanImpactStudyData.ForAnalysis.xlsx")


library(dplyr)
dat %>% group_by(group) %>%
  summarise( total.mean = mean(totalAnimalProductConsumption),
             total.sd = sd(totalAnimalProductConsumption),
             zero.mean = mean(zeroServingsOfMeat) )

dat$totalMeatConsumption
dat$zeroServingsOfMeat