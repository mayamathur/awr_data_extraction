
original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review"
setwd(original.data.dir)
setwd("Reese 2015")

library(readxl)

# study 3 data
dat3 = read_xlsx("Animal_Advocacy_Messaging_Data.xlsx", sheet = 3)

# study 4 data
dat4 = read_xlsx("Animal_Advocacy_Messaging_Data.xlsx", sheet = 4)

table(dat3$condition)
table(dat4$condition)


# look at software miscodings
# there is no missing data
table(dat3$consumption, useNA = "ifany")
table(dat4$consumption, useNA = "ifany")

# dichotomize the outcome at reduce vs. stay the same or increase
dat3$Y.reduce = dat3$consumption <= 3
dat4$Y.reduce = dat4$consumption <= 3

library(dplyr)
dat3 %>% group_by(condition) %>%
  summarise( Preduce = mean(Y.reduce),
             N = n() )

# ~~~ stopped here: need to check with author about miscodings