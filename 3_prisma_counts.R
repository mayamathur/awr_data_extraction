
#################################### PRISMA COUNTS ####################################

data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction"
code.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction/awr_data_extraction_git"
# location of original datasets and code for reproducible studies
original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review/*INCLUDED STUDIES"
setwd(code.dir); source("helper_extraction.R")

# first few counts in PRISMA:
# **81 comes from "Studies added to search from elsewhere.xlsx" (number of rows minus
#  three red ones added mistakenly)

# **6,559 comes from "Paul's comments on search update"

# **4,076 excluded in title/abstract screen comes from "2020-2-7 Covidence article status screenshot"

# **145 entering full text screen comes from "2020-2-7 Covidence article status screenshot"
#   but we added 2 for the two for which we couldn't determine eligibility and hence allowed to sit
#   in full-text screening purgatory without voting

# **4140 that entered tiab screen after duplicates removed = (145-81) + 4076
#  i.e., (# that entered full-text screen FROM database searches, not grey lit search) + (# excluded in tiab screen)

# qualitative data entered into Excel
setwd(data.dir)
# NOTE: this step breaks if cell values are hyphenated!
d2 = read_xlsx("Extracted qualitative data.xlsx", na = "NR")
# remove missing rows, used for human-readability
d2 = d2 %>% filter(!is.na(`First author last name`))

# dataset of unique ARTICLES (as understood by Covidence)
d2$article = paste( d2$`First author last name`, d2$Year)
d.arts = d2[ !duplicated(d2$article),]
nrow(d.arts)
# **49 eligible articles in PRISMA diagram

# get unique refids (to match Covidence)
library(stringr)
x = d.arts$`Ref #`
x = as.list(x)
# remove commas to get full list of unique Covidence numbers
temp2 = lapply(x, FUN = function(.x) strsplit(.x, split=", ") )
x2 = unlist(temp2)
x2 = unique(x2)
length(x2)
# 60 Covidence numbers, which matches the screenshot of articles to extract
#  ("2020-2-7 Covidence article status screenshot")

# **therefore, inexact duplicates = 60-49 = 11
#  (this matches a hand count of the "extra" refids beyond 1 per article: #3790, #3783, #3866, #3867, #3835, #3855, #3848, #3849, #3850, #3851, #3852, #3861)

# **42-1 = 41 eligible articles with usable statistics:
# the -1 is because Mensink article contributed two separate SSWS challenges
sum(d.arts$`Stats source (public data, data from author, paper, hopeless)` != "Hopeless")

# **8-1 = 7 SSWS:
# the -1 is because Mensink article contributed two separate SSWS challenges
sum(d.arts$`Excluded challenge` & d.arts$`Stats source (public data, data from author, paper, hopeless)` != "Hopeless")

# **34 articles in main analysis:
sum(d.arts$`Excluded challenge` == 0 & 
      d.arts$`Stats source (public data, data from author, paper, hopeless)` != "Hopeless")


##### Compare to Covidence
setwd("~/Desktop")
cov = read.csv("Included.csv")

cov$Covidence..[ !cov$Covidence.. %in% x2 ]
x2[ !x2 %in% cov$Covidence.. ]

