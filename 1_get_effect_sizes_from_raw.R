
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                           PRELIMINARIES                                             #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review/*INCLUDED STUDIES"
code.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Data extraction/awr_data_extraction_git"

# helper code
setwd(code.dir)
source("helper_extraction.R")

library(dplyr)
library(mediation)
library(foreign)
library(sandwich)
library(metafor)
library(data.table)
library(readxl)
library(tidyverse)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   MAIN-ANALYSIS STUDIES                                             #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################# AMIOT 2018 #################################

setwd(original.data.dir)
setwd("Amiot 2018, #243")

dat = read.spss("data.sav", to.data.frame=TRUE)

# confirm which variables are total meat consumption at each time point
attr(dat, "variable.labels")[names(dat)=="CE"]
attr(dat, "variable.labels")[names(dat)=="size"]
attr(dat, "variable.labels")[names(dat)=="xsize"]
attr(dat, "variable.labels")[names(dat)=="ysize"]
# also corroborated by their code (codefile2, line 224)

# sanity check vs. Table 2 stats
# agrees! :)
dat %>% group_by(CE) %>%
  summarise(size.mean = mean(size),
            size.sd = sd(size),
            n = n())
dat %>% group_by(CE) %>%
  summarise(size.mean = mean(size),
            size.sd = sd(size),
            n = n())

##### Calculate Main RR #####
# whether they were above or below (baseline) median
#  for total meat consumption at time 3, controlling for
# (continuous) meat consumption at baseline
# dichtomize at baseline med
bl.med = median(dat$size)
dat$Y = dat$ysize < bl.med

get_rr_adj( condition.var.name = "CE",
            control.name = "Control",
            baseline.var.name = "size",
            .dat = dat )


##### Mediation by Positive Emotions #####

# control for baseline meat consumption
model.m = lm( emopos ~ CE + size, data = dat )
model.y = lm( ysize ~ emopos + CE + size, data = dat )
res = mediate(model.m,
              model.y,
              treat = "CE",
              mediator = "emopos",
              covariates = "size")
# 13% mediated


################################# ANDERSON 2016 (PLOS) #################################

setwd(original.data.dir)
setwd("Anderson 2016, #3742")

# only Study 3 is eligible
dat = read.spss("Study3_data.sav", to.data.frame=TRUE)

# remove subjects who didn't follow instructions, as in article
dat = dat[ dat$ReadDescFilter == "read desc", ]
expect_equal( nrow(dat), 114 )

# missing data: 1/114
table(is.na(dat$CTeaten) | is.na(dat$FFeaten))
dat = dat[ !is.na(dat$CTeaten) & !is.na(dat$FFeaten), ]

# sex
prop.table(table(dat$Gender))

# sanity check (compare visually to Fig 5)
mean(dat$CTeaten)
mean(dat$FFeaten)

cntrl.med = median( dat$CTeaten )
dat$Y = dat$FFeaten < cntrl.med


##### Calculate Main RR #####
# controlling for subject's own consumption in control condition
# ~~~ does this make sense??
# ~~~ need to use same approach as for Norris 2016
mod = glm( Y ~ CTeaten,
           data = dat,
           family = "poisson" )
summary(mod)

library(sandwich)
diag( vcovHC(mod, type="HC0") )

# # sanity check: expect effect size to be much smaller, as in original paper,
# #  when ignoring within-subject correlation
# escalc( measure = "RR",
#
#         ai = 61,  # these are from my code
#         bi = 52,
#         ci = 55,
#         di = 58 )

es = escalc( "MPRR",
             ai = tab[1,1],
             bi = tab[1,2], 
             ci = tab[2,1],
             di = tab[2,2] )



################################# ANDERSON 2017 #################################


# they fit a cumulative logit mixed model
setwd(original.data.dir)
setwd("Anderson 2017 (iAnimal), #3797")

dat = data.table( read.csv("dtfu_prepped_data.csv") ) # I made this by running the initial data-prep section of their code (cast as data table after reading in to avoid errors)

# verbatim from their code (cumulative logit mixed-model)
#model2dcontrol <- clmm(factor(diet.f2) ~ cond.f + (1|campus), data=dtfu[cond.f != 2])
#model360control <- clmm(factor(diet.f2) ~ cond.f + (1|campus), data=dtfu[cond.f != 1])

# diet.f1 is an ordinal (6-level) variable for consumption frequency
bl.med = median(dat$diet.f1)
dat$low.t2 = dat$diet.f2 < bl.med

# raw probabilites by group
dat %>% group_by(cond.f) %>%
  summarise( Plow = mean(low.t2),
             n = n())

##### Point Estimate for 2D Video #####
# robust SEs below take care of campus clustering
#  so RE is omitted
mod.2d = glm( low.t2 ~ cond.f + diet.f1,
                data = dat[cond.f != 2],
                family = "poisson")
summary(mod.2d)
diag( vcovHC(mod.2d, type="HC0", cluster = "campus") )

##### Point Estimate for 3D Video #####
mod.3d = glm( low.t2 ~ cond.f + diet.f1,
                data = dat[cond.f != 1],
                family = "poisson" )
summary(mod.3d)
diag( vcovHC(mod.3d, type="HC0", cluster = "campus") )


################################# MACDONALD 2016 #################################

# note this also has outcome at a shorter time lag; using the longer time lag only
setwd(original.data.dir)
setwd("MacDonald 2016, #3800")

dat = read.csv("all_waves_cleaned.csv")

names(dat)[ grepl("FFQ", names(dat))]

# confirm coding scheme
table( dat$FFQtotalSumMeat_chg == dat$FFQtotalSumMeat.3 - dat$FFQtotalSumMeat.1,
       useNA = "no")

# reproduce Table 12 means
agg.sanity = dat %>% group_by(treatment) %>%
  summarise( mean.chg = mean(FFQtotalSumMeat_chg, na.rm = TRUE),
             sd.chg = sd(FFQtotalSumMeat_chg, na.rm = TRUE),
             n = length(!is.na(FFQtotalSumMeat_chg)) )
diff(agg.sanity$mean.chg)



##### Calculate Main RR #####
# whether they were above or below (baseline) median
#  for total meat consumption (servings consumed over past week) at time 3, controlling for
# (continuous) meat consumption at baseline
# dichtomize at baseline med
dat = dat[ !is.na(dat$FFQtotalSumMeat.3) & !is.na(dat$FFQtotalSumMeat.1), ]

# sample size
nrow(dat)

# % male
1 - mean(dat$female, na.rm = TRUE)

bl.med = median(dat$FFQtotalSumMeat.1)
dat$Y = dat$FFQtotalSumMeat.3 < bl.med
table(dat$Y, dat$treatment)

### "reduce" intervention
get_rr_adj( condition.var.name = "treatment",
            control.name = "control",
            baseline.var.name = "FFQtotalSumMeat.1",
            .dat = dat[ dat$treatment != "veg",] )


### "eliminate" intervention
get_rr_adj( condition.var.name = "treatment",
            control.name = "control",
            baseline.var.name = "FFQtotalSumMeat.1",
            .dat = dat[ dat$treatment != "reduce",] )


################################# CALDWELL 2016 #################################

# their summary spreadsheet already had needed stats, so now just getting the % male
setwd(original.data.dir)
setwd("Caldwell 2016, #3794")

dat = read.csv("CleanWelfareReformsData.csv")

# sex
dat %>% filter( experimentGroup %in% c("porkLegislation",
                                       "controlPorkLegislation",
                                       "porkPolicy",
                                       "controlPorkPolicy" ) ) %>%
  summarise( prop.male = mean( gender[ !gender %in% c("", "Prefer not to answer") ] == "Male" ) )

mean(dat$gender[ !dat$gender %in% c("", "Prefer not to answer") ] == "Male")

# missing data situation unclear given lack of codebook

##### Get RRs ######
# outcome variable
dat$Y = dat$pork.decrease

# pork legislation article
get_rr_unadj(condition = "porkLegislation",
             condition.var.name = "experimentGroup",
             control.name = "controlPorkLegislation",
             dat)

# pork policy article
get_rr_unadj(condition = "porkPolicy",
             condition.var.name = "experimentGroup",
             control.name = "controlPorkPolicy",
             dat)



################################# CALDWELL 2017a #################################

# their summary spreadsheet already had needed stats, so now just getting the % male
#  and confirming number in control group

setwd(original.data.dir)
setwd("Caldwell 2017a, #3795")

dat = read_csv( "cleanVideoDataForAnalysis_forPublic.csv" )

# percent male: 51.7
1 - mean(dat$female, na.rm = TRUE)

# sample sizes by condition
dat %>% group_by(videoTreatment) %>% summarise(n())

# total N for paper, not including lifestyle videos since they're not animal welfare
dat %>% filter(videoTreatment != "lifestyle") %>% summarise(n())


################################# ROUK 2017 #################################

setwd(original.data.dir)
setwd("Rouk 2017, #3832")

dat = read_csv("Fish Welfare Issues Study - Cleaned Data.csv")

# recode treatment for forest plot prettiness
dat$X = dat$treatmentClean
dat$X[ dat$treatmentClean == "diseaseAndWater" ] = "disease and water quality"
dat$X[ dat$treatmentClean == "stockingDensity" ] = "crowding"

# make outcome variable: reduce vs. don't
dat$Y = dat$fishConsumpIntent90Days %in% c("Completely eliminate",
                                                "Significantly less",
                                                "Somewhat less")

( all.conditions = unique( dat$X[ dat$X != "control" ] ) )


# effect sizes from raw data
draw = as.data.frame( matrix( ncol = 10, nrow = 0 ) )
names(draw) = c( "authoryear",
                 "substudy",
                 "desired.direction",
                 "effect.measure",
                 "interpretation",
                 "use.rr.analysis",
                 "use.grams.analysis",
                 "use.veg.analysis",
                 "yi",
                 "vi")

for (j in 1:length( all.conditions ) ) {

  temp = dat %>% filter( X %in% c( all.conditions[j], "control") )

  # print percent male
  print( paste( all.conditions[j], "percent male:",
                round( 100* mean( temp$gender == "Male", na.rm = TRUE ), 1 ) ) )

  es = get_rr_unadj( condition = all.conditions[j],
                     condition.var.name = "X",
                     control.name = "control",
                     dat = temp )

  draw <<- add_row(draw,
                   authoryear = "Rouk 2017",
                   substudy = all.conditions[j],
                   desired.direction = es$yi > 0,
                   effect.measure = "log-rr",
                   interpretation = "Reduce vs. don't",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = as.numeric(es$yi),
                   vi = as.numeric(es$vi)
  )
}

write.csv(draw, "rouk_prepped_effect_sizes.csv")


################################# PALOMO-VELEZ #################################


##### Study 1, Calculate Main RR #####
setwd(original.data.dir)
setwd("Palomo-Velez, #107")
library(foreign)
dat1 = read.spss("Study1.sav", to.data.frame=TRUE)

# sanity check (compare visually to Fig 1)
library(dplyr)
dat1 %>% group_by(CONDITION) %>%
  summarise(mean = mean(MEAT_BUYINGLIKEHOOD),
            sd = sd(MEAT_BUYINGLIKEHOOD))


# sample size for relevant comparison for us
dat1 = dat1[ dat1$CONDITION %in% c("Neutral essay", "Moral essay"), ]
dat1 = droplevels(dat1)
nrow(dat1)

# percent male
prop.table(table(dat1$Gender))

# no GLM needed; there are no other covariates
cntrl.med = median( dat1$MEAT_BUYINGLIKEHOOD[dat1$CONDITION == "Neutral essay"] )
dat1$Y = dat1$MEAT_BUYINGLIKEHOOD < cntrl.med

get_rr_unadj(condition = "Moral essay",
             condition.var.name = "CONDITION",
             control.name = "Neutral essay",
             dat = dat1)


##### Study 2, Calculate Main RR #####
setwd("From author")
dat2 = read.spss("Study 2 with ratings created.sav", to.data.frame=TRUE)

# filter out vegetarians per author's email (saved)
dat2 = dat2[ dat2$filter_. == "Selected",]

# sample size
nrow(dat2)
prop.table(table(dat2$SEX))

# remove non-animal conditions
dat2 = dat2[ dat2$CONDITION %in% c("CONTROL", "ANIMAL WELFARE"), ]
dat2 = droplevels(dat2)

cntrl.med = median( dat2$MEAT_BUYINGLIKEHOOD[dat2$CONDITION == "CONTROL"] )
dat2$Y = dat2$MEAT_BUYINGLIKEHOOD < cntrl.med

# no GLM needed; there are no other covariates
get_rr_unadj(condition = "ANIMAL WELFARE",
             condition.var.name = "CONDITION",
             control.name = "CONTROL",
             dat = dat2)


##### Study 3, Calculate Main RR #####
dat3 = read.spss("Study 3. with ratings created.sav", to.data.frame=TRUE)

# filter out vegetarians per author's email (saved)
dat3 = dat3[ dat3$filter_. == "Selected",]

# sample size
nrow(dat3)
prop.table(table(dat3$SEX))

# remove non-animal conditions
dat3 = dat3[ dat3$CONDITION %in% c("CONTROL", "ANIMAL WELFARE"), ]
dat3 = droplevels(dat3)

cntrl.med = median( dat3$MEAT_BUYINGLIKEHOOD[dat3$CONDITION == "CONTROL"] )
dat3$Y = dat3$MEAT_BUYINGLIKEHOOD < cntrl.med

# no GLM needed; there are no other covariates
get_rr_unadj(condition = "ANIMAL WELFARE",
             condition.var.name = "CONDITION",
             control.name = "CONTROL",
             dat = dat3)



################################# REESE 2015 #################################

# this one has a ton of effect sizes, so will make its dataset automatically

setwd(original.data.dir)
setwd("Reese 2015, #3787")

# read in Studies 3 and 4 data
dats = list( read_xlsx("Animal_Advocacy_Messaging_Data.xlsx", sheet = 3),
             read_xlsx("Animal_Advocacy_Messaging_Data.xlsx", sheet = 4) )

# look at conditions
table(dats[[1]]$condition)
table(dats[[2]]$condition)

# remove non-animal-welfare conditions
dats[[1]] = dats[[1]] %>% filter( !condition %in% c("environment",
                                                    "health",
                                                    "news"))

# rename conditions for forest plot prettiness
dats[[1]]$condition[ dats[[1]]$condition == "activist" ] = "activist passage"
dats[[1]]$condition[ dats[[1]]$condition == "consciousness" ] = "consciousness passage"
dats[[1]]$condition[ dats[[1]]$condition == "dxestory" ] = "undercover passage"
dats[[1]]$condition[ dats[[1]]$condition == "ff" ] = "factory farming passage"
dats[[1]]$condition[ dats[[1]]$condition == "generalchicken" ] = "generic chicken passage"
dats[[1]]$condition[ dats[[1]]$condition == "hrcongrat" ] = "congratulatory corporate passage"
dats[[1]]$condition[ dats[[1]]$condition == "hrhope" ] = "hopeful corporate passage"
# "mixed" can stay the same
dats[[1]]$condition[ dats[[1]]$condition == "personalchicken" ] = "individual chicken"
dats[[1]]$condition[ dats[[1]]$condition == "personalcow" ] = "individual cow"
dats[[1]]$condition[ dats[[1]]$condition == "personalpig" ] = "individual pig"
dats[[1]]$condition[ dats[[1]]$condition == "reduce" ] = "reducetarian passage"
dats[[1]]$condition[ dats[[1]]$condition == "reduce" ] = "reducetarian passage"
dats[[1]]$condition[ dats[[1]]$condition == "social" ] = "social norms passage"
dats[[1]]$condition[ dats[[1]]$condition == "vegan" ] = "vegan passage"
dats[[1]]$condition[ dats[[1]]$condition == "vegetarian" ] = "vegetarian passage"

dats[[2]]$condition[ dats[[2]]$condition == "reducetarian" ] = "reducetarian passage"
dats[[2]]$condition[ dats[[2]]$condition == "vegetarian" ] = "vegetarian passage"

# effect sizes from raw data
draw = as.data.frame( matrix( ncol = 10, nrow = 0 ) )
names(draw) = c( "authoryear",
                 "substudy",
                 "desired.direction",
                 "effect.measure",
                 "interpretation",
                 "use.rr.analysis",
                 "use.grams.analysis",
                 "use.veg.analysis",
                 "yi",
                 "vi")

# dichotomize the outcome at reduce vs. stay the same or increase
for ( i in 1:length(dats) ) {
  dats[[i]] = mutate( dats[[i]],
                      Y = consumption <= 3 )

  # calculate proportion male
  # coding scheme from author email
  print( paste("Prop. male = ", mean( dats[[i]]$gender == 1, na.rm = TRUE ) ) )

  agg = dats[[i]] %>% group_by(condition) %>%
    summarise( Preduce = mean(Y),
               N.tot = n(),
               N.reduce = sum(Y == 1) )

  print(agg)

  # get ES for each condition
  all.conditions = unique( dats[[i]]$condition[ !dats[[i]]$condition == "control" ] )
  for (j in 1:length( all.conditions ) ) {

    es = get_rr_unadj( condition = all.conditions[j],
                 condition.var.name = "condition",
                 control.name = "control",
            dat = dats[[i]] )

    draw <<- add_row(draw,
                   authoryear = "Reese 2015",
                   substudy = paste( "Study ", i+2, ", ", all.conditions[j], sep=""),
                   desired.direction = es$yi > 0,
                   effect.measure = "log-rr",
                   interpretation = "Reduce vs. don't",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = as.numeric(es$yi),
                   vi = as.numeric(es$vi)
                   )
  }
}

write.csv(draw, "reese_prepped_effect_sizes.csv")

# # sanity check: reproduce one by hand
# temp = dats[[1]][ dats[[1]]$condition %in% c("hopeful corporate passage", "control" ), ]
# temp %>% group_by(condition, Y) %>%
#   summarise(n())
# (7/67) / (5/46); exp(draw$yi[draw$substudy == "Study 3, hopeful corporate passage"])


################################# COONEY 2014 #################################

# ~~~ not double-checked

setwd(original.data.dir)
setwd("Cooney 2014, #3856")

dat = read_xlsx("data.xlsx", sheet = 2)

# clean up coding
dat$condition = tolower(dat$`4- Booklet Version`)
dat$sex = tolower(dat$Gender)

# each food variable is the number of weekly weels containing that food
# lower totals are better
dat$pre.total = dat$`3-Red Meat` + dat$`3-Poultry` + dat$`3-Fish` + dat$`3-Eggs`
dat$post.total = dat$`8 - Red Meat` + dat$`8 -Poultry` + dat$`8 - Fish` + dat$`8 - Eggs`

# make outcome
bl.med = median( dat$pre.total, na.rm = TRUE )
dat$Y = dat$post.total < bl.med

# rename conditions
dat$condition[ dat$condition == "g" ] = '"why", all animals, cruelty'
dat$condition[ dat$condition == "h" ] = '"why", chickens, cruelty'
dat$condition[ dat$condition == "i" ] = '"how", all animals, cruelty'
dat$condition[ dat$condition == "j" ] = '"how", chickens, cruelty'
dat$condition[ dat$condition == "k" ] = '"why", all animals, mixed'
dat$condition[ dat$condition == "l" ] = '"why", chickens, mixed'
dat$condition[ dat$condition == "m" ] = '"how", all animals, mixed'
dat$condition[ dat$condition == "n" ] = '"how", chickens, mixed'

# total N for paper
nrow( dat %>% filter( !is.na(post.total) & !is.na(condition) & !is.na(pre.total) ) )

##### Calculate Main RR #####
# effect sizes from raw data
draw = as.data.frame( matrix( ncol = 10, nrow = 0 ) )
names(draw) = c( "authoryear",
                 "substudy",
                 "desired.direction",
                 "effect.measure",
                 "interpretation",
                 "use.rr.analysis",
                 "use.grams.analysis",
                 "use.veg.analysis",
                 "yi",
                 "vi")

# get ES for each condition
all.conditions = unique( dat$condition[ !dat$condition == "control" ] )

for (j in 1:length( all.conditions ) ) {

  # keep only the desired condition and control
  dat2 = dat %>% filter( condition %in% c( all.conditions[j], "control" ) )

  es = get_rr_adj( condition.var.name = "condition",
                   control.name = "control",
                   baseline.var.name = "pre.total",
                   .dat = dat2 )

  # print percent male
  prop.male = mean( dat2$sex[ !is.na(dat2$post.total) ] == "male" )
  print( paste( all.conditions[j], "percent male:",
                round( 100* prop.male, 2 ) ) )
  cat("\n\n")


  draw <<- add_row(draw,
                   authoryear = "Cooney 2014",
                   substudy = all.conditions[j],
                   desired.direction = es$yi > 0,  # log(RR) > 0 for being below baseline median is good
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high weekly consumption",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = as.numeric(es$yi),
                   vi = as.numeric(es$vi)
  )
}

# # sanity check: reproduce one manually
# dat2 = dat %>% filter( condition %in% c( "control", '"how", all animals, cruelty' ) )
# dat2$condition = factor( dat2$condition, levels = c( "control", '"less meat" leaflet' ) )
# ( mod = glm( Y ~ (condition == '"how", all animals, cruelty') + pre.total,
#            data = dat2,
#            family = "poisson" ) )
# library(sandwich)
# diag( vcovHC(mod, type="HC0") )[2]

write.csv(draw, "cooney_2014_prepped_effect_sizes.csv")




################################# DOEBEL 2015 #################################

# outcome is number of weekly meals containing animal products
setwd(original.data.dir)
setwd("Doebel 2015, #3866")
dat = read.csv("raw_data.csv")

# group the booklets with same message type, as in JP's analysis
dat$condition = gsub('[0-9]+', '', dat$BookletDescrp)


# remove subjects without a booklet or no F/U data
dat = droplevels( dat[ dat$condition != "", ] )
dat = dat[ !is.na(dat$Total_Chg), ]


# confirm that total FU and current variables
#  are equal to sum of individual components - yes :)
FU.cols = names(dat)[grepl("Followup", names(dat))][1:5]
table( apply( dat[,FU.cols],
              1,
              sum ) ==
       dat$Total_FollowupYN )

# sanity check for calculation of Total_Current variable
current.cols = names(dat)[grepl("Current", names(dat))][1:5]
table( apply( dat[,current.cols],
              1,
              sum ) ==
         dat$Total_Current )

# # sanity check for Total_Chg variable
# table( dat$Total_Chg == dat$Total_FollowupYN - dat$Total_Current )
# # total_chg is not always equal to the relevant difference
# # often off by a sign
# # fix the Total_Chg variable accordingly
# dat$Total_Chg = dat$Total_FollowupYN - dat$Total_Current

# outcome: being below baseline median
bl.med = median( dat$Total_Current, na.rm = TRUE )
dat$Y = dat$Total_FollowupYN < bl.med

# # probability of reducing consumption by group
# agg = dat %>% group_by(condition) %>%
#   summarise( Preduce = mean(Y),
#              N.tot = n(),
#              N.reduce = sum(Y == 1) )
# print(agg)

# rename conditions for forest plot prettiness
dat$condition[ dat$condition == "comb" ] = '"cut out or cut back" leaflet'
dat$condition[ dat$condition == "lessmeat" ] = '"less meat" leaflet'
dat$condition[ dat$condition == "veget" ] = 'vegetarian leaflet'
dat$condition[ dat$condition == "vegan" ] = 'vegan leaflet'

# percent male
# this variable is super messy
dat %>% filter( Gender %in% c("female", "Female", "male", "Male", "other") ) %>%
  summarise( prop.male = mean( Gender %in% c("male", "Male") ) )


##### Calculate Main RR #####
# effect sizes from raw data
draw = as.data.frame( matrix( ncol = 10, nrow = 0 ) )
names(draw) = c( "authoryear",
                 "substudy",
                 "desired.direction",
                 "effect.measure",
                 "interpretation",
                 "use.rr.analysis",
                 "use.grams.analysis",
                 "use.veg.analysis",
                 "yi",
                 "vi")

# get ES for each condition
all.conditions = unique( dat$condition[ !dat$condition == "control" ] )

for (j in 1:length( all.conditions ) ) {

  # keep only the desired condition and control
  dat2 = dat %>% filter( condition %in% c( all.conditions[j], "control" ) )

  es = get_rr_adj(
                   condition.var.name = "condition",
                   control.name = "control",
                   baseline.var.name = "Total_Current",
                   .dat = dat2 )

  draw <<- add_row(draw,
                   authoryear = "Doebel 2015",
                   substudy = all.conditions[j],
                   desired.direction = es$yi > 0,  # log(RR) > 0 for being below baseline median is good
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high weekly consumption",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = as.numeric(es$yi),
                   vi = as.numeric(es$vi)
  )
}

# # sanity check: reproduce one manually
# dat2 = dat %>% filter( condition %in% c( "control", '"less meat" leaflet' ) )
# dat2$condition = factor( dat2$condition, levels = c( "control", '"less meat" leaflet' ) )
# ( mod = glm( Y ~ condition + Total_Current,
#            data = dat2,
#            family = "poisson" ) )
# library(sandwich)
# diag( vcovHC(mod, type="HC0") )[2]


write.csv(draw, "doebel_2015_prepped_effect_sizes.csv")


################################# COONEY 2016 #################################

setwd(original.data.dir)
setwd("Cooney 2016, #3796")

# don't worry about warnings
library(readxl)
dat = read_xlsx("cleanImpactStudyData.ForAnalysis.xlsx")

#sanity checks
( agg = dat %>% group_by(group) %>%
  summarise( total.mean = mean(totalAnimalProductConsumption),
             total.sd = sd(totalAnimalProductConsumption),
             zero.mean = mean(zeroServingsOfMeat) ) )



##### Calculate Main RR #####
# use animal product consumption per our hierarchy of outcomes
cntrl.med = median( dat$totalAnimalProductConsumption[ dat$group == "Control"] )
dat$lo = dat$totalAnimalProductConsumption < cntrl.med

# no GLM needed; there are no other covariates
tab = table( dat$group, dat$lo )
prop.table(tab, margin = 1)
escalc( measure = "RR",
        ai = tab[1,1],
        bi = tab[1,2],
        ci = tab[2,1],
        di = tab[2,2] )

# sanity check for RR
#(296/(296+362)) / (283/(338+283))


################################# KUNST 2016 #################################

setwd(original.data.dir)
setwd("Kunst 2016, #1453/Data from author/kunst_hohle_2016")


##### Study 2A, Calculate Main RR #####
dat = read.spss("study2a.sav", to.data.frame=TRUE)

# missing data
mean( is.na(dat$eat_1) )

# percent male
prop.table(table(dat$gender))

# sample size: 168
nrow(dat)

# look at variable codings
# View(attr(dat, "variable.labels"))
# here's the outcome we want
attr(dat, "variable.labels")[names(dat)=="eat_1"]

# reproduce Fig 4 bar plot - looks good
dat %>% group_by(con) %>%
  summarise( mean = mean(eat_1, na.rm = TRUE),
             n = sum( !is.na(con) & !is.na(eat_1) ) )

# outcome: below vs. above control group's median willingness to eat
cntrl.med = median( dat$eat_1[ dat$con == "no-head"] )
dat$Y = dat$eat_1 < cntrl.med

get_rr_unadj(condition = "head",
       condition.var.name = "con",
       control.name = "no-head",
       dat = dat)

# sanity check
dat %>% group_by(con, Y) %>% summarise(n())
(61/(24+61)) / (41/(41+42))


# own mediation analysis
# control for baseline meat consumption
model.m = lm( EMPATHY ~ con, data = dat )
model.y = lm( Y ~ EMPATHY + con, data = dat )
res = mediate(model.m,
              model.y,
              treat = "con",
              control.value = "no-head",
              treat.value = "head",
              mediator = "EMPATHY",
              covariates = "Y")
# point estimate: 113% mediated

model.m = lm( DISSOCIATION ~ con, data = dat )
model.y = lm( Y ~ DISSOCIATION + con, data = dat )
res = mediate(model.m,
              model.y,
              treat = "con",
              control.value = "no-head",
              treat.value = "head",
              mediator = "DISSOCIATION",
              covariates = "Y")
# point estimate: 102% mediated

##### Study 2B, Calculate Main RR #####
dat = read.spss("study_2b.sav", to.data.frame=TRUE)

# look at variable codings
# View(attr(dat, "variable.labels"))
# here's the outcome we want
attr(dat, "variable.labels")[names(dat)=="veg_1"]

# missing data
mean( is.na(dat$eat_1) )

# percent male
prop.table(table(dat$gender))

# sample size: 101
nrow(dat)

# reproduce bottom of pg. 764 - looks good
dat %>% group_by(con) %>%
  summarise( mean = mean(veg_1, na.rm = TRUE),
             n = sum( !is.na(con) & !is.na(veg_1) ) )

# outcome: above vs. below control group's median willingness to choose veg alternative
cntrl.med = median( dat$veg_1[ dat$con == "no-head"] )
dat$Y = dat$veg_1 > cntrl.med

get_rr_unadj(condition = "head",
       condition.var.name = "con",
       control.name = "no-head",
       dat = dat)


##### Study 3, Calculate Main RR #####
dat = read.spss("study3.sav", to.data.frame=TRUE)

# look at variable codings
# View(attr(dat, "variable.labels"))
# here's the outcome we want
attr(dat, "variable.labels")[names(dat)=="eat_1"]

# missing data
mean( is.na(dat$eat_1) )

# percent male
prop.table(table(dat$gender))

# sample size: 187
nrow(dat)

dat %>% group_by(con) %>%
  summarise( mean = mean(eat_1, na.rm = TRUE),
             n = sum( !is.na(con) & !is.na(eat_1) ) )

# outcome: below vs. above control group's median willingness to eat
cntrl.med = median( dat$eat_1[ dat$con == "control"] )
dat$Y = dat$eat_1 < cntrl.med

get_rr_unadj(condition = "animal shown",
       condition.var.name = "con",
       control.name = "control",
       dat = dat)


##### Study 5, Calculate Main RR #####

dat = read.spss("study5.sav", to.data.frame=TRUE)

# look at variable codings
# View(attr(dat, "variable.labels"))
# here's the outcome we want
attr(dat, "variable.labels")[names(dat)=="veg_1"]

dat %>% group_by(con) %>%
  summarise( mean = mean(veg_1, na.rm = TRUE),
             n = sum( !is.na(con) & !is.na(veg_1) ) )

# missing data
mean( is.na(dat$eat_1) )

# percent male
prop.table(table(dat$gender))

# sample size: 190
nrow(dat)

# outcome: above vs. below control group's median willingness to choose veg alternative
cntrl.med = median( dat$veg_1[ dat$con == "euphemisms"] )
dat$Y = dat$veg_1 > cntrl.med

get_rr_unadj(condition = "animal names",
       condition.var.name = "con",
       control.name = "euphemisms",
       dat = dat)



################################# KUNST 2018 #################################

setwd(original.data.dir)
setwd("Kunst 2016/Data from author/kunst_haugestad_2018")

dat = read.spss("omnivores.sav", to.data.frame=TRUE)

# look at variable codings
# View(attr(dat, "variable.labels"))
# here's the outcome we want
attr(dat, "variable.labels")[names(dat)=="veg_1"]

# NAs in these variables seem to distinguish participants' countries
table( !is.na(dat$ethnic_ecuador), !is.na(dat$ethnic_us))


##### American sample #####
dat1 = dat[ !is.na(dat$ethnic_us), ]

# missing data
mean( is.na(dat1$veg_1) )

# percent male
prop.table(table(dat1$gender))

# sample size: 178
nrow(dat1)

# outcome: above vs. below control group's median willingness to choose veg alternative
cntrl.med = median( dat1$veg_1[ dat1$con == "no head"] )
dat1$Y = dat1$veg_1 > cntrl.med

get_rr_unadj(condition = "head",
       condition.var.name = "con",
       control.name = "no head",
       dat = dat1)

##### Ecuadorian sample #####
dat2 = dat[ !is.na(dat$ethnic_ecuador), ]

# missing data
mean( is.na(dat2$veg_1) )

# percent male
prop.table(table(dat2$gender))

# sample size: 183
nrow(dat2)

# outcome: above vs. below control group's median willingness to choose veg alternative
cntrl.med = median( dat2$veg_1[ dat1$con == "no head"] )
dat2$Y = dat2$veg_1 > cntrl.med

get_rr_unadj(condition = "head",
             condition.var.name = "con",
             control.name = "no head",
             dat = dat2)

################################# 3838 ACE 2013a #################################

setwd(original.data.dir)
setwd('Animal Charity Evaluators 2013a, #3838')
setwd("Dataset and codebook")

dat = read_xlsx("ACE leafleting trial with group assignment.xlsx")

# total consumption frequency post-intervention
# higher scores are better (1 = 5x/day to 7 = Never)
dat$post.consump = dat$dairy...25 + dat$red...26 + dat$poultry...27 + dat$fish...28 + dat$eggs...29
# and pre-intervention (as a covariate)
dat$pre.consump = dat$dairy...5 + dat$red...6 + dat$poultry...7 + dat$fish...8 + dat$eggs...9

# interventions
# based on sample sizes given in text, "N" is control, "C" is "Compassionate Choices",
#  and "E" is "Even If You Like Meat"
table(dat$`GROUP ASSIGNMENT`)

( bl.med = median( dat$post.consump[ dat$`GROUP ASSIGNMENT` == "N" ], na.rm = TRUE ) )
dat$Y = dat$post.consump > bl.med  # higher scores are better

##### Compassionate Choices #####
# being above vs. below control group median, controlling for baseline consumption
get_rr_adj( condition.var.name = "GROUP ASSIGNMENT",
             control.name = "N",
             baseline.var.name = "pre.consump",
             .dat = dat[ dat$`GROUP ASSIGNMENT` != "E", ] )


##### Even If You Like Meat #####
# being above vs. below control group median, controlling for baseline consumption
get_rr_adj( condition.var.name = "GROUP ASSIGNMENT",
            control.name = "N",
            baseline.var.name = "pre.consump",
            .dat = dat[ dat$`GROUP ASSIGNMENT` != "C", ] )


################################# 3837 ACE 2013b #################################

setwd(original.data.dir)
setwd('Animal Charity Evaluators 2013b, #3837')
setwd("Dataset and codebook")

dat = read.spss("ACEhumaneeducationData.sav", to.data.frame = TRUE)

# total consumption frequency post-intervention
# higher scores are better (1 = 5x/day to 7 = Never)
dat$post.consump = dat$CurrentAnimalproductconsumption
# and pre-intervention (as a covariate)
dat$pre.consump = dat$Pastanimalproductconsumption

# interventions
# based on sample sizes given in text, "N" is control, "C" is "Compassionate Choices",
#  and "E" is "Even If You Like Meat"
table(dat$GROUPS)
# remove subjects who unintentionally received FF presentation
dat = dat %>% filter( GROUPS != "Control but attended FF presentation" )


# percent male
mean( dat$Sex == "Male", na.rm = TRUE )

# make outcome variable
( bl.med = median( dat$post.consump[ dat$GROUPS == "Control and did not attend FF presentation" ], na.rm = TRUE ) )
dat$Y = dat$post.consump > bl.med  # higher scores are better



# being above vs. below control group median, controlling for baseline consumption
get_rr_adj( condition.var.name = "GROUPS",
            control.name = "Control and did not attend FF presentation",
            baseline.var.name = "pre.consump",
            .dat = dat )


################################# 3862 FIAPO 2019 #################################

##### Study 2: 2D video #####
dc = read.spss("VO (control) data.sav", to.data.frame=TRUE)
dt = read.spss("VO (experimental) data.sav", to.data.frame=TRUE)


( res.c = dc %>% summarise( nv.mn = mean(Food_Non_Veg, na.rm = TRUE),
                            nv.sd = sd(Food_Non_Veg, na.rm = TRUE),
                            n = sum(!is.na(Food_Non_Veg))) )


( res.t = dt %>% summarise( nv.mn = mean(Non_Veg_Total_Score, na.rm = TRUE),
                            nv.sd = sd(Non_Veg_Total_Score, na.rm = TRUE),
                            n = sum(!is.na(Non_Veg_Total_Score))) )

# reported: 0.92
# close but slightly different
res.t$nv.mn - res.c$nv.mn



##### Study 3: 3D video #####

dc = read.spss("VR (control) data.sav", to.data.frame=TRUE)
dt = read.spss("VR (experimental) data.sav", to.data.frame=TRUE)


( res.c = dc %>% summarise( nv.mn = mean(Non_Veg_TotalScore_Control, na.rm = TRUE),
                            nv.sd = sd(Non_Veg_TotalScore_Control, na.rm = TRUE),
                            n = sum(!is.na(Non_Veg_TotalScore_Control))) )


( res.t = dt %>% summarise( nv.mn = mean(Non_Veg_TotalScore_Exp, na.rm = TRUE),
                            nv.sd = sd(Non_Veg_TotalScore_Exp, na.rm = TRUE),
                            n = sum(!is.na(Non_Veg_TotalScore_Exp))) )

# reported: 2.84 :)
res.t$nv.mn - res.c$nv.mn


################################# 3858 LACKNER 2019 #################################

setwd(original.data.dir)
setwd('Lackner 2019, #3858/Data from author')

dat = read_xlsx("ZL_Data_meta-analysis.xlsx")

# between-subjects manipulation of food processing stage (low vs. high)
#  with repeated measures for each subject
#  but each subject only rated their intentions once at the end
#  so keep only 1 row per subject
dat = dat[ !duplicated(dat$Subject_ID), ]

# analyzed N
nrow(dat)

# per paper, intention variable is coded such that lower scores are good
cntrl.med = median( dat$Intentions[dat$Processed_Stage == "high"] )
dat$Y = dat$Intentions < cntrl.med

# raw probabilites by group
dat %>% group_by(Processed_Stage) %>%
  summarise( Plow = mean(Y),
             n = n())


get_rr_unadj( condition = "low",
              condition.var.name = "Processed_Stage",
              control.name = "high",
              dat = dat )





################################# 3831 NORRIS 2018 #############################

###### Notes #####
# * Couldn't separate participants at Mesa Community College and at Arizona State
#   University - Tempe Campus, so we'll have to treat them as a single cluster.
#   (See the schools-adminstrators-codes.csv for details.)
# * There are some unclear codings: "identify_pre" and "identify_post" contains 0
#   and "progressCode_post" contains 2 but the code does not clearly align with
#   the definition in the code book.
# * Possibly related, the numbers of vegans, vegetarians and meat-eaters don't
#   quite align.
# * Based on the survey questions, both the pre and post tests were conducted
#   *after* receiving the intervention leaflet.
#
# animal_products_pre = c("oftenBeef_pre", "oftenChicken_pre", "oftenDairy_pre",
#   "oftenEggs_pre", "oftenFish_pre", "oftenPork_pre", "oftenTurkey_pre")
#
# animal_products_post = c("oftenBeef_post", "oftenChicken_post",
#   "oftenDairy_post", "oftenEggs_post", "oftenFish_post", "oftenPork_post",
#   "oftenTurkey_post")


##### Data loading #####
setwd(code.dir)
setwd(original.data.dir)
setwd('Norris 2019, #3831')
setwd("Data from author/Leafleting Effectiveness 2018")

# cluster codes
data_schools = read_csv('schools-adminstrators-codes.csv')

# pre- and post-data are in different files
data_pre = read.csv("LES Pre-test Results.csv"); nrow(dat.pre)
data_post = read.csv("LES Post-Test Results.csv"); nrow(dat.post)

# number of participants per group, extracted from report
n = dplyr::tibble(
  group=factor(c('Compassionate Choices', 'What is Speciesism','Control')),
  n=c(14000, 14000, 5838))


##### Data cleaning #####
# merge pre and post data
data_raw = dplyr::full_join(data_pre, data_post, by="code",
                            suffix=c('_pre', '_post'))

# Add treatment or control group and school cluster variables
data = dplyr::mutate(data_raw,
                     code = str_to_upper(code),

                     # extract the treatment or control group from "code"
                     group = stringr::str_sub(code, 0, 1),
                     group = dplyr::recode_factor(group,
                                                  C="Compassionate Choices",
                                                  X="Control",
                                                  P="What is Speciesism"),

                     # extract the school code from "code"
                     school_code = stringr::str_sub(code, 0, 3)) %>%

  # merge in the names of schools and other school-level data
  dplyr::left_join(data_schools, by='school_code') %>%

  # properly encode identity questions
  # -1 will be automatically convereted to NA here...and dplyr will insist on
  # warning despite there being no sanctioned solution with recode
  dplyr::mutate(
    identify_post = dplyr::recode_factor(identify_post,
                                         `0`="0",
                                         `1`="Meat-eater",
                                         `2`="Vegetarian",
                                         `3`="Vegan",
                                         .default='NA'),
    identify_pre = dplyr::recode_factor(identify_pre,
                                        `0`="0",
                                        `1`="Meat-eater",
                                        `2`="Vegetarian",
                                        `3`="Vegan",
                                        .default='NA'))


##### Validation #####
# Are any responses missing "code"?
any(is.na(data_raw$code)) # FALSE

# Are any non-null school codes duplicated?
as.logical(anyDuplicated(
  dplyr::filter(data_schools, !is.na(school_code)))) # FALSE

# XXX What does this coding mean?
dplyr::filter(data, identify_pre == "0" | identify_post == "0") # 3 observations

# Are any responses not matched to a school?
any(is.na(data$school)) # FALSE

# XXX How many observations are from the two inseperable clusters?
nrow(dplyr::filter(data, startsWith(code, 'CBA'))) # 20 observations

# Progress codes apply to each survey part, pre or post, independently
# -11 Control who tried to do survey after close of study.
# -10 Ejected for not reading leaflet.
#  -3 "database anomaly", exclude
#  -2 Participant completed multiple copies of the survey.
#     Apparently all their surveys will be flagged with this, not just extras.
#    Exclude.
#  -1 Completed all questions
#   0 Completed no questions
#   1...12 The number of the last survey completed. (Is this order in the
#     spreadsheet or in the survey?)
#  13 completed all but phone number
#
# The code 2 suggests that question 2 would be the last answered. However, in
# both cases it appears that questions 1 through 4 were not answered, but
# subsequent questions were.
dplyr::filter(data, !is.na(progressCode_pre) & !is.na(progressCode_post)) %>%
  dplyr::filter(progressCode_post > 0)  %>%
  dplyr::select(c("id_pre", "id_post", "code", "progressCode_pre",
                  "progressCode_post",
                  # questions in order asked in the survey instrument
                  "discuss_post", "followUp", "giftPreference_post", "identify_post",
                  "oftenBeans_post", "oftenBeef_post", "oftenChicken_post",
                  "oftenDairy_post"))


##### Replicate reported results #####
# Response rates, pre-test
dplyr::filter(data, progressCode_pre %in% c(-1, 13)) %>%
  dplyr::count(group, name='n_respondents') %>%
  dplyr::full_join(n, by='group') %>%
  dplyr::mutate(response_rate = round(100 * n_respondents / n, 1))
# Matches
# 11.8% expected for X
#  2.7% expected for C
#  2.5% expected for P

# Response rates, post-test
dplyr::filter(data, progressCode_post == -1) %>%
  dplyr::count(group, name='n_respondents') %>%
  dplyr::full_join(n, by='group') %>%
  dplyr::mutate(response_rate = round(100 * n_respondents / n, 1))
# Matches

# Rates of diet pattern change
dplyr::filter(data, data$progressCode_post == -1) %>%
  tidyr::pivot_longer(
    cols=c('identify_pre', 'identify_post'),
    names_to='timepoint',
    values_to='identify'
  ) %>%
  dplyr::group_by(group, timepoint, identify) %>%
  dplyr::tally() %>%
  tidyr::pivot_wider(names_from='timepoint', values_from='n')
# Matches roughly, but off by one or two in a couple spots.

# Calculate odds ratio of being nearly vegan in the post to the pre-test.
dplyr::filter(data, data$progressCode_post == -1) %>%
  dplyr::mutate(few_animal_products_post =
                  oftenBeef_post <= 1 & oftenChicken_post <= 1 & oftenDairy_post <= 1 &
                  oftenEggs_post <= 1 & oftenFish_post <= 1 & oftenPork_post <= 1 &
                  oftenTurkey_post <= 1) %>%
  dplyr::group_by(group) %>%
  dplyr::count(few_animal_products_post)

dplyr::filter(data, data$progressCode_post == -1) %>%
  dplyr::mutate(few_animal_products_pre =
                  oftenBeef_pre <= 1 & oftenChicken_pre <= 1 & oftenDairy_pre <= 1 &
                  oftenEggs_pre <= 1 & oftenFish_pre <= 1 & oftenPork_pre <= 1 &
                  oftenTurkey_pre <= 1) %>%
  dplyr::group_by(group) %>%
  dplyr::count(few_animal_products_pre)

# XXX Manually transcribed from above calculation
or_control = (15 / 358) / (7 / 366)
or_cc = (19 / 181) / (13 / 187)
or_sp = (14 / 187) / (6 / 195)
# Matches figure


##### Create Outcome Variable #####
# sum of consumption frequences of each animal product
# per codebook PDF, higher scores are more consumption
data$pre.consump = data$oftenBeef_pre +
  data$oftenChicken_pre +
  data$oftenDairy_pre +
  data$oftenEggs_pre +
  data$oftenFish_pre +
  data$oftenPork_pre +
  data$oftenTurkey_pre

data$post.consump = data$oftenBeef_post +
  data$oftenChicken_post +
  data$oftenDairy_post +
  data$oftenEggs_post +
  data$oftenFish_post +
  data$oftenPork_post +
  data$oftenTurkey_post


cntrl.med = median( data$post.consump[ data$group == "Control" ], na.rm = TRUE )
data$Y = data$post.consump < cntrl.med

data %>% group_by(group) %>%
  summarise( Y = mean(Y, na.rm = TRUE) )  # per codebook, 2 is male


##### Compassionate Choices: Calculate Main RR #####
# controlling for subject's own consumption at baseline
mod = glm( Y ~ ( group == "Compassionate Choices" ) + pre.consump,
           data = data[ data$group != "What is Speciesism", ],
           family = "poisson" )
summary(mod)
vcovHC(mod, type="HC0", cluster = "school_code")

# percent male and analyzed N
( agg.cc = data %>% filter(group != "What Is Speciesism") %>%
    summarise( N = sum( !is.na(post.consump) & !is.na(pre.consump) ),
               male = mean(gender == 2) ) ) # per codebook, 2 is male


##### Speciesism: Calculate Main RR #####
# controlling for subject's own consumption at baseline
mod = glm( Y ~ ( group == "What is Speciesism" ) + pre.consump,
           data = data[ data$group != "Compassionate Choices", ],
           family = "poisson" )
summary(mod)
vcovHC(mod, type="HC0", cluster = "school_code")

# percent male and analyzed N
( agg.s = data %>% filter(group != "Compassionate Choices") %>%
    summarise( N = sum( !is.na(post.consump) & !is.na(pre.consump) ),
               male = mean(gender == 2) ) ) # per codebook, 2 is male


##### Overall Analyzed N and Missing Data #####
sum( !is.na(data$post.consump) & !is.na(data$pre.consump) )

# missing data: based on number of booklets distributed in each condition in PDF
1 - (agg.cc$N / (14000 + 5838))
1 - (agg.s$N / (14000 + 5838))


################################# 3829 NORRIS 2014 #################################

setwd(original.data.dir)
setwd('Norris 2014, #3830')
setwd("Data from author/Pay Per Read Fall 2014")

dat.pre = read_xlsx("PPR 2014 Results pre-test.xlsx", sheet=3); nrow(dat.pre)
dat.post = read_xlsx("Post data MM cleaned.xlsx", sheet=1); nrow(dat.post)

# recode food-specific variables as numeric
dat.pre$`Think back to the meals and snacks you have had in the last 7 days. / How many different times did...-Red meat (beef, pork, etc.)-Not last week, but I sometimes eat`
# ughhhh
# they're in different variables

##### Recode Outcomes in Pre-Data #####
# outcome categories are in separate variables, always coded as NA or 1
( varsA = names(dat.pre)[ grepl( "7 or more times last week", names(dat.pre) ) ] )
( varsB = names(dat.pre)[ grepl( "4-6 times last week", names(dat.pre) ) ] )
( varsC = names(dat.pre)[ grepl( "1-3 times last week", names(dat.pre) ) ] )
( varsD = names(dat.pre)[ grepl( "Not last week", names(dat.pre) ) ] )

# recode so that higher scores = more consumption
dat.pre[ ,varsA ] = dat.pre[ ,varsA ] + 4  # so that the 1s for "7+ times/week" become 5s and the NAs stay NA
dat.pre[ ,varsB ] = dat.pre[ ,varsB ] + 3  # so that the 1s become 4s and the NAs stay NA
dat.pre[ ,varsC ] = dat.pre[ ,varsC ] + 2  # so that the 1s become 3s and the NAs stay NA
dat.pre[ ,varsD ] = dat.pre[ ,varsD ] + 1  # so that the 1s become 2s and the NAs stay NA
# leave the "Never" category alone

# sanity check
# View(head(dat.pre))

library(rccmisc)
# sum all of these variables
names(dat.pre[21:45])
dat.pre$pre.consump = psum( dat.pre[,21:45], na.rm=TRUE )


##### Recode Outcomes in Post-Data #####
( varsA = names(dat.post)[ grepl( "7 or more times last week", names(dat.post) ) ] )
( varsB = names(dat.post)[ grepl( "4-6 times last week", names(dat.post) ) ] )
( varsC = names(dat.post)[ grepl( "1-3 times last week", names(dat.post) ) ] )
( varsD = names(dat.post)[ grepl( "Not last week", names(dat.post) ) ] )
( varsE = names(dat.post)[ grepl( "Never", names(dat.post) ) ] )

# this one has "0" instead of NA
dat.post[, c(varsA, varsB, varsC, varsD, varsE) ][ dat.post[, c(varsA, varsB, varsC, varsD, varsE) ] == 0 ] = NA

dat.post[ ,varsA ] = dat.post[ ,varsA ] + 4 
dat.post[ ,varsB ] = dat.post[ ,varsB ] + 3 
dat.post[ ,varsC ] = dat.post[ ,varsC ] + 2  
dat.post[ ,varsD ] = dat.post[ ,varsD ] + 1  
# leave the "Never" category alone

# sanity check
# View(head(dat.post))

library(rccmisc)
# sum all of these variables
names(dat.post[16:40])
dat.post$post.consump = psum( dat.post[,16:40], na.rm=TRUE )


##### Merge Datasets #####
dat = inner_join( dat.pre,
                  dat.post, by = "What is your MTurk Worker ID?  We need this in order to / approve the HIT.  If you're not sure what...")
nrow(dat)


##### Make Intervention Variable #####
# need to merge the 3 "please read" columns
library(tidyr)

dat = dat %>% mutate( your.choice = dplyr::recode(`Please read the pamphlet below before continuing to the rest of /  the survey: /  /  Your Choice /  - PDF`,
                                           `1` = '"Your Choice"' ),
                      even.if = dplyr::recode( `Please read the pamphlet below before continuing to the rest of the / survey /  Even if /  you like Mea...`,
                                        `1` = '"Even If You Like Meat"'),
                      control = dplyr::recode( `Please read the pamphlet below before continuing to the rest of the / survey: /  Immigrant Detention...`,
                                        `1` = "control") )

dat$condition = coalesce( dat$your.choice, dat$even.if, dat$control )

# sanity check
# our N's are a bit larger than theirs
# reported: 134 EIYLM, 167 YC, 158 control
# could this be because they used different outcome?
table(dat$condition)

# binary outcome
# bm
bl.med = median( dat$post.consump[ dat$condition == "control" ] )
dat$Y = dat$post.consump < bl.med


##### Sex #####
mean( dat$`What is your gender?` == 1, na.rm = TRUE )

##### Missing Data #####

# control: 219 to 164
# Even If: 197 to 141
# Your Choice: 238 to 176

# percent remaining at F/U in each group
164/219
141/197
176/238
# appears nondifferential by group

1 - ( nrow(dat.post) / nrow(dat.pre) )

##### Finally Get the RRs #####
# "Your Choice"
get_rr_adj( condition.var.name = "condition",
            control.name = "control",
            baseline.var.name = "pre.consump",
            .dat = dat[ dat$condition != '"Even If You Like Meat"', ] )

# "Even If You Like Meat"
get_rr_adj( condition.var.name = "condition",
            control.name = "control",
            baseline.var.name = "pre.consump",
            .dat = dat[ dat$condition != '"Your Choice"', ] )

# # sanity check: why are the RRs in unexpected direction even though proportions
# #  below baseline median are slightly better for interventions vs. control? (per below)
# dat %>% group_by(condition) %>%
#   summarise( prop.below = mean(Y),
#              pre.consump = mean(pre.consump) )
# #  because the effect direction reverses upon controlling for pre-consumption
# temp = dat %>% filter( condition != '"Even If You Like Meat"')
# mod = glm( Y ~ condition,
#            data = temp,
#            family = "poisson" )
# summary(mod)

################################# 3829 NORRIS 2016 #############################

##### Notes from JP #####
# - I could not replicate the filtering from raw to final data exactly with the
#   provided information, so used the provided final data
# - Number of responses by arm is slightly off for the post-test
# - Includes respondents from many countries

##### Load data #####
setwd(original.data.dir)
setwd('Norris 2016, #3829')
setwd("Data from author/Pay Per Read Fall 2015")

raw_data = read_xlsx("PPR 2015 Fall 2016-05-03 post-test full cleaned.xlsx",
  sheet="Merged Data Baseline + Followup", .name_repair="universal")


##### Clean data #####
data = dplyr::mutate(raw_data,
  # count the number of comprehension checks passed
  count_passed_checks =
    (According.to.the.pamphlet..what.does.the.egg.industry.do.with.most.male.chicks.
      == "Kill them shortly after birth, often by grinding alive") +
    (What.kind.of.animal.does.not.have.a.photo.in.this.pamphlet. == "Whales") +
    (According.to.the.pamphlet..Emily.the.pig.
      == "Figured out how to unlatch her cage and then released other pigs from their cages"),

  Randomize = as.factor(Randomize)
) %>%

  dplyr::select(
    "Response.ID",
    "What.is.your.MTurk.Worker.ID.",
    "Country",
    "What.is.your.gender.",
    "Randomize",
    "According.to.the.pamphlet..what.does.the.egg.industry.do.with.most.male.chicks.",
    "According.to.the.pamphlet..Emily.the.pig.",
    "What.kind.of.animal.does.not.have.a.photo.in.this.pamphlet.",
    "count_passed_checks",
    dplyr::contains("In.the.past.3.months"),
    dplyr::contains("In.the.past.1.month"),
    -dplyr::starts_with("Non.dairy"),
    -dplyr::starts_with("Veggie.meats"),
    -dplyr::starts_with("Beans")
) %>%

  dplyr::rename(
    "pre.beef" = "Beef..hamburger..steak..roast.beef..etc...In.the.past.3.months..how.often.did.you.eat.the.following.Â..Â.",
    'pre.chicken' = "Chicken..fried.chicken..in.soup..grilled.chicken..etc...In.the.past.3.months..how.often.did.you.eat.the.following.Â..Â.",
    'pre.dair' = "Dairy..cheese..milk..yogurt..etc...In.the.past.3.months..how.often.did.you.eat.the.following.Â..Â.",
    'pre.eggs' = "Eggs..scrambled..omelet..egg.salad..etc...In.the.past.3.months..how.often.did.you.eat.the.following.Â..Â.",
    'pre.fish' = "Fish..salmon..tuna..fish.sticks..etc...In.the.past.3.months..how.often.did.you.eat.the.following.Â..Â.",
    'pre.pork' = "Pork..ham..pork.chops..ribs..etc...In.the.past.3.months..how.often.did.you.eat.the.following.Â..Â.",
    'pre.turkey' = "Turkey..turkey.dinner..turkey.sandwich..in.soup..etc...In.the.past.3.months..how.often.did.you.eat.the.following.Â..Â.",
    'post.beef' = "Beef..hamburger..steak..roast.beef..etc...In.the.past.1.month..how.often.did.you.eat.the.following.Â..Â.",
    'post.chicken' = "Chicken..fried.chicken..in.soup..grilled.chicken..etc...In.the.past.1.month..how.often.did.you.eat.the.following.Â..Â.",
    'post.dairy' = "Dairy..cheese..milk..yogurt..etc...In.the.past.1.month..how.often.did.you.eat.the.following.Â..Â.",
    'post.eggs' = "Eggs..scrambled..omelet..egg.salad..etc...In.the.past.1.month..how.often.did.you.eat.the.following.Â..Â.",
    'post.fish' = "Fish..salmon..tuna..fish.sticks..etc...In.the.past.1.month..how.often.did.you.eat.the.following.Â..Â.",
    'post.pork' = "Pork..ham..pork.chops..ribs..etc...In.the.past.1.month..how.often.did.you.eat.the.following.Â..Â.",
    'post.turkey' = "Turkey..turkey.dinner..turkey.sandwich..in.soup..etc...In.the.past.1.month..how.often.did.you.eat.the.following.Â..Â."
  )

##### Validation against report #####
# How many pre surveys in each arm?
dplyr::count(data, Randomize)
# Chicken Reduction Booklet – 628
# Even If You Like Meat – 634
# Speciesism – 601
# Your Choice – 592

# How many post surveys in each arm?
dplyr::filter(data,
  !is.na(post.beef)
) %>%
  dplyr::count(Randomize)
# Results are slightly off 406, 388, ...
# Chicken Reduction Booklet – 404
# Even If You Like Meat – 386
# Speciesism – 393
# Your Choice – 356


##### Attempt filtering data #####
# pretest_raw_data = read_xlsx(
#   "PPR 2015 Fall 2016-05-03 post-test full cleaned.xlsx",
#   sheet="Raw Data", .name_repair="universal")
#
# pretest_data =
#   dplyr::mutate(pretest_raw_data,
#     # count the number of comprehension checks passed
#     count_passed_checks =
#       (According.to.the.pamphlet..what.does.the.egg.industry.do.with.most.male.chicks.
#         == "Kill them shortly after birth, often by grinding alive") +
#       (What.kind.of.animal.does.not.have.a.photo.in.this.pamphlet. == "Whales") +
#       (According.to.the.pamphlet..Emily.the.pig.
#         == "Figured out how to unlatch her cage and then released other pigs from their cages"),
#
#     Randomize = as.factor(Randomize)
#   ) %>%
#
#   dplyr::filter(
#     # Passing at least 2 of 3 comprehension checks
#     (count_passed_checks > 1) &
#
#     # Remove test responses
#     (Time.Started >= '2015-11-27')
#
# )
#
# # Not sure why these two aren't identical
# pre_but_not_post = setdiff(pretest_data$Response.ID, raw_data$Response.ID)
#
# View(pretest_data[pretest_data$Response.ID %in% pre_but_not_post,])

##### Make Outcome Measure #####

# since some booklets made mixed appeals that included all animal products, 
#  use sum of all animal product consumption

# find all outcome variables (both pre and post)
y.inds = grepl( x = names(data), pattern = "beef|chicken|dair|egg|fish|turkey" )
y.names = names(data)[y.inds]
( y.pre.names = y.names[ grepl( x = y.names, pattern = "pre" ) ] )
( y.post.names = y.names[ grepl( x = y.names, pattern = "post" ) ] )


# recode all food-specific outcomes as simple ordinal scales
# higher scores are worse (more consumption)
data = data %>% mutate_at( vars(y.names),
                           function(x) car::recode( x,
                                                      "'never' = 0;
                                                      'less than 1 time per week' = 1;
                                                      '1-6 times per week' = 2;
                                                      '1-3 times per day' = 3;
                                                      '4 or more times per day' = 4" ) )

# make aggregated consumption variable
data$pre.consump = rowSums( data[, y.pre.names ] )
data$post.consump = rowSums( data[, y.post.names ] )

# baseline median
bl.med = median(data$pre.consump, na.rm = TRUE)

# rename conditions to match extracted qualitative data
data$condition = NA
data$condition[ data$Randomize == "A Simple Way to Help" ] = '"A Simple Way to Help"'
data$condition[ data$Randomize == "EIYLM" ] = '"Even If You Like Meat"'
data$condition[ data$Randomize == "Speciesism" ] = '"Speciesism"'
data$condition[ data$Randomize == "Your Choice" ] = '"Your Choice"'

# # metafor's underlying code for MPRR
# # see Zou 2007
# pi12 <- bi/ni
# pi21 <- ci/ni
# pi1. <- (ai+bi)/ni  # ni is total N for whole table
# pi.1 <- (ai+ci)/ni
# 
# yi <- log(pi1.) - log(pi.1)
# vi <- (pi12 + pi21) / (ni * pi1. * pi.1)

# effect sizes from raw data
draw = as.data.frame( matrix( ncol = 10, nrow = 0 ) )
names(draw) = c( "authoryear",
                 "substudy",
                 "desired.direction",
                 "effect.measure",
                 "interpretation",
                 "use.rr.analysis",
                 "use.grams.analysis",
                 "use.veg.analysis",
                 "yi",
                 "vi")

( all.conditions = unique( data$condition ) )

for (j in 1:length( all.conditions ) ) {
  temp = data[ data$condition == all.conditions[j], ]
  
  # print percent male
  print( paste( all.conditions[j], "percent male:",
                round( 100* mean( temp$What.is.your.gender. == "Male", na.rm = TRUE ), 1 ) ) )
  
  # print percent missing
  print( paste( all.conditions[j], "percent missing:",
                round( 100* mean( is.na(temp$post.consump), na.rm = TRUE ), 1 ) ) )
  
  cat("\n\n")
  
  tab = table(temp$pre.consump < bl.med, temp$post.consump < bl.med)
  
  es = escalc( "MPRR",
          ai = tab[1,1],
          bi = tab[1,2], 
          ci = tab[2,1],
          di = tab[2,2] )
  
  draw <<- add_row(draw,
                   authoryear = "Norris 2016",
                   substudy = all.conditions[j],
                   desired.direction = es$yi > 0,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high animal product consumption",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = as.numeric(es$yi),
                   vi = as.numeric(es$vi)
  )
}

write.csv(draw, "norris_2016_prepped_effect_sizes.csv")


################################# 3842 SCHWITZGEBEL 2019 #############################

setwd(original.data.dir)
setwd('Schwitzgebel 2019, #3842')
setwd("Data from author")

dat = read_xlsx("Data-Transactions-ReducedMathur-200115b.xlsx", sheet=1); nrow(dat)

# sanity check
# author said in email: 52% meat in control group vs. 45% in experimental group
dat %>% group_by(Condition) %>% 
  summarise( mean(Meat) )


##### Calculate Main RR #####
# whether they were above or below (baseline) median
#  for total meat consumption at time 3, controlling for
# (continuous) meat consumption at baseline
# since these are individual purchase data, Y is already binary and 
#  we don't need to dichotomize

mod = glm( Meat ~ Condition,
           data = dat,
           family = "poisson" )
summary(mod)

library(sandwich)
diag( vcovHC(mod, type="HC0", cluster = "UniqueIDNew") )

# surprisingly, there is almost no clustering of purchase type within subjects, 
#  which is why dropping clustering in above makes little difference
library(ICC)
ICCbareF(Meat, UniqueIDNew, dat)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                     EXCLUDED CHALLENGES                                             #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


################################# **#3826 CHALLENGE 22+ (ANIMALS NOW) #################################

setwd("~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review/*INCLUDED STUDIES/*Challenges/Challenge 22+ (Animals Now) 2018/Faunalytics 2019, #3826/Data from author")

# per the "legend" tab of the spreadsheet, the diet variables are Likert-type regarding
#  meat portions per week (1=daily to 6=vegan)
dat = read_xlsx("Cha22 Data.xlsx")

# high scores are less meat consumption
# per paper, coding scheme is as follows:
# 1 or 2 = consume meat >= 5X weekly
# 3 = consume 2-4X/week
# 4 = consume <=1X/week
# 5 = no meat
# ~~~ is option #6 vegan??
bl.med = median(dat$Diet_before)

# RR of being vegetarian after vs. before challenge
# (since they don't have a category for no animal product consumption)
# their "38.2% veg*ns" corresponds to survey options 5 and 6 combined
dat %>% summarise( mean(Diet_before >= 5),
                   mean(Diet_after >= 5) )

# ~~~ assume that option 6 is vegan
dat %>% summarise( pre.vegans = sum(Diet_before == 6),
                   pre.nonvegans = sum(Diet_before < 6),
                   post.vegans = sum(Diet_after == 6),
                   post.nonvegans = sum(Diet_after < 6) )





