
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

##### Effect Size #1: Main RR #####
##### Effect Size #2: No Meat vs. Any Meat #####
##### Effect Size #3: Grams of Meat ######


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

##### Effect Size #1: Main RR #####
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

##### Effect Size #2: No Meat vs. Any Meat #####
table(dat$ysize == 0)  # no one stopped eating meat entirely
dat$Y = dat$ysize == 0
get_rr_adj( condition.var.name = "CE",
            control.name = "Control",
            baseline.var.name = "size",
            .dat = dat )

##### Effect Size #2: Grams of Meat #####
dat$change = dat$ysize - dat$size
summary(dat$change)

escalc( m2i = mean( dat$change[dat$CE == "Control"] ),
              m1i = mean( dat$change[dat$CE == "Experimental"] ),
              sd2i = sd( dat$change[dat$CE == "Control"] ),
              sd1i = sd( dat$change[dat$CE == "Experimental"] ),
              n2i = length( dat$change[dat$CE == "Control"] ),
              n1i = length( dat$change[dat$CE == "Experimental"] ),
              measure = "MD" )

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


##### Effect Size #1: Main RR #####
# controlling for subject's own consumption in control condition
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

##### Effect Size #2: No Meat vs. Any Meat #####
table(dat$FFeaten == 0)  # no one refused to eat the sample entirely
mod = glm( FFeaten == 0 ~ CTeaten, 
           data = dat,
           family = "poisson" )
summary(mod)

diag( vcovHC(mod, type="HC0") )

##### Effect Size #3: Grams of Meat #####
# vs. same subject in control condition
dat$change = dat$FFeaten - dat$CTeaten
summary(dat$change)

# use LM to get SE of within-subject difference
summary( lm( change ~ 1, 
             data = dat) )


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


##### Effect Size #1: Main RR #####
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


##### Effect Size #2: No Meat vs. Any Meat #####

### "reduce" intervention
mod = glm( (FFQtotalSumMeat.3 == 0) ~ FFQtotalSumMeat.1 + treatment, 
           data = dat[ dat$treatment != "veg",],
           family = "poisson" )
summary(mod)
sqrt( diag( vcovHC(mod, type="HC0") ) )

### "eliminate" intervention
mod = glm( (FFQtotalSumMeat.3 == 0) ~ FFQtotalSumMeat.1 + treatment, 
           data = dat[ dat$treatment != "reduce",],
           family = "poisson" )
summary(mod)
sqrt( diag( vcovHC(mod, type="HC0") ) )


##### Effect Size #3: Grams of Meat #####
grams.per.serving = 85.0486
dat$grams.chg = ( dat$FFQtotalSumMeat.3 - dat$FFQtotalSumMeat.1 ) * grams.per.serving

agg = dat %>% group_by(treatment) %>%
  summarise( mean = mean(grams.chg),
             sd = sd(grams.chg),
             n = n() )


### both interventions
escalc( measure = "MD",

        m1i = agg$mean[2:3], # vector for the two outcomes: reduce, eliminate
        sd1i = agg$sd[2:3],
        n1i = agg$n[2:3],

        m2i = rep( agg$mean[1], 2 ),
        sd2i = rep( agg$sd[1], 2 ),
        n2i =  rep( agg$n[1], 2 ) )



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

##### Get RRs #####
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


##### Study 1, Effect Size #1: Main RR #####
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


##### Study 2, Effect Size #1: Main RR #####
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


##### Study 3, Effect Size #1: Main RR #####
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

##### Effect Size #1: Main RR #####
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




################################# COONEY 2015 ################################# 

# outcome is number of weekly meals containing animal products 
setwd(original.data.dir)
setwd("Cooney 2015, #3799")
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


##### Effect Size #1: Main RR #####
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
                   authoryear = "Cooney 2015",
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



##### Effect Size #2: No Meat vs. Any Meat #####

# rename outcome since below fn looks for a variable called "Y"
dat$Y = dat$Total_FollowupYN == 0
# only 5 subjects ate no meat

for (j in 1:length( all.conditions ) ) {
  
  es = get_rr_unadj( condition = all.conditions[j],
               condition.var.name = "condition",
               control.name = "control",
               dat = dat )
  
  draw <<- add_row(draw, 
                   authoryear = "Cooney 2015",
                   substudy = all.conditions[j],
                   desired.direction = es$yi > 0,  # Y coded such that positive is good
                   effect.measure = "log-rr",
                   interpretation = "No meat vs. any",
                   use.rr.analysis = 0,
                   use.grams.analysis = 0,
                   use.veg.analysis = 1,
                   yi = as.numeric(es$yi),
                   vi = as.numeric(es$vi)
  )
}

write.csv(draw, "cooney_2015_prepped_effect_sizes.csv")


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



##### Effect Size #1: Main RR #####
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


##### Effect Size #2: No Meat vs. Any Meat #####
# no GLM needed; there are no other covariates
tab = table( dat$group, dat$zeroServingsOfMeat )
prop.table(tab, margin = 1)  # matches what they reported :) 
escalc( measure = "RR",
        ai = tab[1,1],
        bi = tab[1,2],
        ci = tab[2,1],
        di = tab[2,2] )


##### Effect Size #3: Grams of Meat ######
servings = escalc( measure = "MD",
        
        m1i = agg$total.mean[ agg$group == "Control" ], # vector for the two outcomes: Meat2, MeatYesterday
        sd1i = agg$total.sd[ agg$group == "Control" ],
        n1i = sum( dat$group == "Control" ),
        
        m2i = agg$total.mean[ agg$group == "Treatment" ],
        sd2i = agg$total.sd[ agg$group == "Treatment" ],
        n2i = sum( dat$group == "Treatment" ) )

# no info on how they defined servings to subjects, so use the standard conversion
#  from Arndt
grams.per.serving = 85.0486
servings[1] * grams.per.serving
servings[2] * grams.per.serving^2



################################# KUNST 2016 ################################# 

setwd(original.data.dir)
setwd("Kunst 2016, #1453/Data from author/kunst_hohle_2016")


##### Study 2A, Effect Size #1: Main RR #####
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

##### Study 2B, Effect Size #1: Main RR #####
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


##### Study 3, Effect Size #1: Main RR #####
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


##### Study 5, Effect Size #1: Main RR #####

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




################################# 3831 NORRIS N.D. ################################# 

# ~~~ dataset seems to be from something else
#  the survey refers to "listening to a presentation" and dataset doesn't have a variable for which leaflet was received

setwd(original.data.dir)
setwd('Norris nd, #3831')
setwd("Data from author/Leafleting Effectiveness 2018")

# pre- and post-data are in different files
dat.pre = read.csv("LES Pre-test Results.csv"); nrow(dat.pre)
dat.post = read.csv("LES Post-Test Results.csv"); nrow(dat.post)

# per the "Schools and Dates" spreadhseet (see the bottom part called "Code Ranges"), 
#  the first letter of the "code" variable tells us which leaflet was received
# X = control; C = "Compassionate Choices"; "P" = "Speciesism"

dat.pre$code2 = substr( tolower(dat.pre$code), start = 1, stop = 1 )
table(dat.pre$code2)

5838 * .118  # expected count for X
14000*.027  # expected count for C
14000*.025  # expected count for P


# merge them
dat = merge( dat.pre, dat.post, by = "code" )

dat = merge( dat.pre, dat.post, by = "id" )


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
( varsA = names(dat.pre)[ grepl( "7 or more times last week", names(dat.pre) ) ] )
( varsB = names(dat.pre)[ grepl( "4-6 times last week", names(dat.pre) ) ] )
( varsC = names(dat.pre)[ grepl( "1-3 times last week", names(dat.pre) ) ] )
( varsD = names(dat.pre)[ grepl( "Not last week", names(dat.pre) ) ] )

dat.pre[ ,varsA ] = dat.pre[ ,varsA ] + 4  # so that the 1s are coded as 5s and the NAs stay NA
dat.pre[ ,varsB ] = dat.pre[ ,varsB ] + 3  # so that the 1s are coded as 5s and the NAs stay NA
dat.pre[ ,varsC ] = dat.pre[ ,varsC ] + 2  # so that the 1s are coded as 5s and the NAs stay NA
dat.pre[ ,varsD ] = dat.pre[ ,varsD ] + 1  # so that the 1s are coded as 5s and the NAs stay NA
# leave the "Never" category alone

# sanity check
View(head(dat.pre))

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

dat.post[ ,varsA ] = dat.post[ ,varsA ] + 4  # so that the 1s are coded as 5s and the NAs stay NA
dat.post[ ,varsB ] = dat.post[ ,varsB ] + 3  # so that the 1s are coded as 5s and the NAs stay NA
dat.post[ ,varsC ] = dat.post[ ,varsC ] + 2  # so that the 1s are coded as 5s and the NAs stay NA
dat.post[ ,varsD ] = dat.post[ ,varsD ] + 1  # so that the 1s are coded as 5s and the NAs stay NA
# leave the "Never" category alone

# sanity check
View(head(dat.post))

library(rccmisc)
# sum all of these variables
names(dat.post[16:40])
dat.post$post.consump = psum( dat.post[,16:40], na.rm=TRUE )

# binary outcome
bl.med = median( dat$post.consump[ dat$condition == "control" ] )
dat.post$Y = dat.post$post.consump < bl.med

##### Merge Datasets #####
dat = inner_join( dat.pre,
                  dat.post, by = "What is your MTurk Worker ID?  We need this in order to / approve the HIT.  If you're not sure what...")
nrow(dat)


##### Make Intervention Variable #####
# need to merge the 3 "please read" columns
library(tidyr)

dat = dat %>% mutate( your.choice = recode(`Please read the pamphlet below before continuing to the rest of /  the survey: /  /  Your Choice /  - PDF`,
                                           `1` = '"Your Choice"' ),
                      even.if = recode( `Please read the pamphlet below before continuing to the rest of the / survey /  Even if /  you like Mea...`,
                                        `1` = '"Even If You Like Meat"'),
                      control = recode( `Please read the pamphlet below before continuing to the rest of the / survey: /  Immigrant Detention...`,
                                        `1` = "control") )

dat$condition = coalesce( dat$your.choice, dat$even.if, dat$control )

# sanity check
# our N's are a bit larger than theirs
# reported: 134 EIYLM, 167 YC, 158 control
# could this be because they used different outcome?
table(dat$condition)

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

################################# 3829 NORRIS 2016 ################################# 

# ~~~ data also seem to be wrong 
setwd(original.data.dir)
setwd('Norris 2016, #3829')
setwd("Data from author/Pay Per Read Fall 2015")

dat.pre = read_xlsx("PPR 2015 Fall-12 pre-test data.xlsx")
dat.post = read_xlsx("PPR 2015 Fall 2016-05-03 post-test full cleaned.xlsx")

# some IDs are duplicated...not sure why
table(duplicated(dat.pre$`What is your MTurk Worker ID?`))
table(duplicated(dat.post$`What is your MTurk Worker ID?`))
# remove these
bad.ids = c( dat.pre$`What is your MTurk Worker ID?`[ duplicated(dat.pre$`What is your MTurk Worker ID?`) ],
             dat.post$`What is your MTurk Worker ID?`[ duplicated(dat.pre$`What is your MTurk Worker ID?`) ] )
dat.pre = dat.pre[ !dat.pre$`What is your MTurk Worker ID?` %in% bad.ids, ]
dat.post = dat.post[ !dat.post$`What is your MTurk Worker ID?` %in% bad.ids, ]


# recode food-specific variables as numeric
( pre.food.vars = c(names(dat.pre)[35:39], names(dat.pre)[41:42]) )
names(dat.post)

# so this was carried forward from pre-data
table(dat$`Beef (hamburger, steak, roast beef, etc.):In the past 3 months, how often did you eat the following?   ` == dat$`Beef (hamburger, steak, roast beef, etc.):In the past 3 months, how often did you eat the following?Â  Â `)


# merge the datasets
dat = inner_join( dat.pre,
                 dat.post, by = "What is your MTurk Worker ID?")
nrow(dat)

# sanity check: expected number of rows
length( intersect( unique(dat.post$`What is your MTurk Worker ID?`),
                   unique(dat.pre$`What is your MTurk Worker ID?`) ) )




dat$pre.consump = dat$`Beef (hamburger, steak, roast beef, etc.):In the past 3 months, how often did you eat the following?   `







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





