

# ~~~ make sure we're using the appropriate outcome now that it is supposed to match the 
#  intervention's scope

# look for the following in each study:
#  - our main RR outcome
#  - raw difference in servings
#  - vegetarian vs not
#  give all of these the same unique ID so that we know not to include them in same analysis

##### Effect Size #1: Main RR #####
##### Effect Size #2: No Meat vs. Any Meat #####
##### Effect Size #3: Grams of Meat ######

################################# AMIOT ################################# 

original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review"
setwd(original.data.dir)
setwd("Amiot 2016")

library(foreign)
dat = read.spss("data.sav", to.data.frame=TRUE)

# confirm which variables are total meat consumption at each time point
attr(dat, "variable.labels")[names(dat)=="size"]
attr(dat, "variable.labels")[names(dat)=="xsize"]
attr(dat, "variable.labels")[names(dat)=="ysize"]
# also corroborated by their code (codefile2, line 224)

# sanity check vs. Table 2 stats
# agrees! :)
library(dplyr)
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
dat$lo.t3 = dat$ysize < bl.med
table(dat$lo.t3, dat$CE)

mod = glm( lo.t3 ~ size + CE, 
           data = dat,
           family = "poisson" )
summary(mod)

library(sandwich)
sqrt( diag( vcovHC(mod, type="HC0") ) )
# here the sandwich SEs are smaller than naive
# this makes sense since the outcome is common; see this from McNutt paper:
# "For studies of common outcomes, Poisson regression is likely to
# compute a confidence interval(s) that is conservative,
# suggesting less precision than is true. Poisson errors are overestimates of binomial errors
# when the outcome is common (Poisson errors approximately
# equal binomial errors when the outcome (disease) is rare)."


##### Effect Size #2: No Meat vs. Any Meat #####
table(dat$ysize == 0)  # no one stopped eating meat entirely
mod = glm( dat$ysize == 0 ~ size + CE, 
           data = dat,
           family = "poisson" )
summary(mod)

library(sandwich)
sqrt( diag( vcovHC(mod, type="HC0") ) )


##### Effect Size #2: Grams of Meat #####
dat$change = dat$ysize - dat$size
summary(dat$change)

library(metafor)
escalc( m2i = mean( dat$change[dat$CE == "Control"] ),
              m1i = mean( dat$change[dat$CE == "Experimental"] ),
              sd2i = sd( dat$change[dat$CE == "Control"] ),
              sd1i = sd( dat$change[dat$CE == "Experimental"] ),
              n2i = length( dat$change[dat$CE == "Control"] ),
              n1i = length( dat$change[dat$CE == "Experimental"] ),
              measure = "MD" )

################################# ANDERSON 2016 (PLOS) ################################# 

original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review"
setwd(original.data.dir)
setwd("Anderson 2016")


# only Study 3 is eligible
library(foreign)
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
sqrt( diag( vcovHC(mod, type="HC0") ) )
# here the sandwich SEs are smaller than naive

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

library(sandwich)
sqrt( diag( vcovHC(mod, type="HC0") ) )

##### Effect Size #3: Grams of Meat #####
# vs. same subject in control condition
dat$change = dat$FFeaten - dat$CTeaten
summary(dat$change)

# use LM to get SE of within-subject difference
summary( lm( change ~ 1, 
             data = dat) )

escalc( m2i = mean( dat$change[dat$CE == "Control"] ),
        m1i = mean( dat$change[dat$CE == "Experimental"] ),
        sd2i = sd( dat$change[dat$CE == "Control"] ),
        sd1i = sd( dat$change[dat$CE == "Experimental"] ),
        n2i = length( dat$change[dat$CE == "Control"] ),
        n1i = length( dat$change[dat$CE == "Experimental"] ),
        measure = "MD" )




################################# ANDERSON 2017 ################################# 


# they fit a cumulative logit mixed model

original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review"
setwd(original.data.dir)
setwd("Anderson 2017 (iAnimal)")

library(data.table)
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

# instead use high vs. low
# robust SEs already take care of campus clustering
#  so RE is omitted
mod.2d = glm( low.t2 ~ cond.f + diet.f1,
                data = dat[cond.f != 2],
                family = "poisson")
summary(mod.2d)
sqrt( diag( vcovHC(mod.2d, type="HC0", cluster = "campus") ) )


mod.3d = glm( low.t2 ~ cond.f + diet.f1,
                data = dat[cond.f != 1],
                family = "poisson" )
summary(mod.3d)
sqrt( diag( vcovHC(mod.3d, type="HC0", cluster = "campus") ) )

# uses Zhang method
# not in use because Poisson now works
# # convert directly to RR
# library(sjstats)
# # get SE from CI limit
# library(MetaUtility)
# scrape_meta( type = "RR",
#              est = odds_to_rr(mod.2d)["cond.f", "RR"],
#              hi = odds_to_rr(mod.2d)["cond.f", "upper.ci"] )
# 
# scrape_meta( type = "RR",
#              est = odds_to_rr(mod.3d)["cond.f", "RR"],
#              hi = odds_to_rr(mod.3d)["cond.f", "upper.ci"] )



################################# MACDONALD 2016 ################################# 

# note this also has outcome at a shorter time lag; using the longer time lag only
setwd(original.data.dir)
setwd("MacDonald 2016")

dat = read.csv("all_waves_cleaned.csv")

names(dat)[ grepl("FFQ", names(dat))]

# confirm coding scheme
table( dat$FFQtotalSumMeat_chg == dat$FFQtotalSumMeat.3 - dat$FFQtotalSumMeat.1,
       useNA = "no")

# reproduce Table 12 means
# ~~~ finish
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
bl.med = median(dat$FFQtotalSumMeat.1)
dat$Y = dat$FFQtotalSumMeat.3 < bl.med
table(dat$Y, dat$treatment)

### "reduce" intervention
mod = glm( Y ~ FFQtotalSumMeat.1 + treatment, 
           data = dat[ dat$treatment != "veg",],
           family = "poisson" )
summary(mod)
library(sandwich)
sqrt( diag( vcovHC(mod, type="HC0") ) )

### "eliminate" intervention
mod = glm( Y ~ FFQtotalSumMeat.1 + treatment, 
           data = dat[ dat$treatment != "reduce",],
           family = "poisson" )
summary(mod)
library(sandwich)
sqrt( diag( vcovHC(mod, type="HC0") ) )


##### Effect Size #2: No Meat vs. Any Meat #####

### "reduce" intervention
mod = glm( (FFQtotalSumMeat.3 == 0) ~ FFQtotalSumMeat.1 + treatment, 
           data = dat[ dat$treatment != "veg",],
           family = "poisson" )
summary(mod)
library(sandwich)
sqrt( diag( vcovHC(mod, type="HC0") ) )

### "eliminate" intervention
mod = glm( (FFQtotalSumMeat.3 == 0) ~ FFQtotalSumMeat.1 + treatment, 
           data = dat[ dat$treatment != "reduce",],
           family = "poisson" )
summary(mod)
library(sandwich)
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


################################# PALOMO-VELEZ ################################# 


##### Study 1, Effect Size #1: Main RR #####
setwd(original.data.dir)
setwd("Palomo-Velez")
library(foreign)
dat1 = read.spss("Study1.sav", to.data.frame=TRUE)

# sanity check (compare visually to Fig 1)
library(dplyr)
dat1 %>% group_by(CONDITION) %>%
  summarise(mean = mean(MEAT_BUYINGLIKEHOOD),
            sd = sd(MEAT_BUYINGLIKEHOOD))


cntrl.med = median( dat1$MEAT_BUYINGLIKEHOOD[dat1$CONDITION == "Neutral essay"] )
dat1$lo = dat1$MEAT_BUYINGLIKEHOOD < cntrl.med

# no GLM needed; there are no other covariates
dat1 = dat1[ dat1$CONDITION %in% c("Neutral essay", "Moral essay"), ]
dat1 = droplevels(dat1)
tab = table( dat1$CONDITION, dat1$lo )

library(metafor)
escalc( measure = "RR",
        ai = tab[1,1],
        bi = tab[1,2],
        ci = tab[2,1],
        di = tab[2,2] )


##### Study 2, Effect Size #1: Main RR #####
setwd("From author")
library(foreign)
dat2 = read.spss("Study 2 with ratings created.sav", to.data.frame=TRUE)

# filter out vegetarians per author's email (saved)
dat2 = dat2[ dat2$filter_. == "Selected",]

# remove non-animal conditions
dat2 = dat2[ dat2$CONDITION %in% c("CONTROL", "ANIMAL WELFARE"), ]
dat2 = droplevels(dat2)

cntrl.med = median( dat2$MEAT_BUYINGLIKEHOOD[dat2$CONDITION == "CONTROL"] )
dat2$Y = dat2$MEAT_BUYINGLIKEHOOD < cntrl.med

# no GLM needed; there are no other covariates
tab = table( dat2$CONDITION, dat2$Y )

library(metafor)
escalc( measure = "RR",
        ai = tab[1,1],
        bi = tab[1,2],
        ci = tab[2,1],
        di = tab[2,2] )


##### Study 2, Effect Size #1: Main RR #####
library(foreign)
dat3 = read.spss("Study 3. with ratings created.sav", to.data.frame=TRUE)

# filter out vegetarians per author's email (saved)
dat3 = dat3[ dat3$filter_. == "Selected",]

# remove non-animal conditions
dat3 = dat3[ dat3$CONDITION %in% c("CONTROL", "ANIMAL WELFARE"), ]
dat3 = droplevels(dat3)

cntrl.med = median( dat3$MEAT_BUYINGLIKEHOOD[dat3$CONDITION == "CONTROL"] )
dat3$Y = dat3$MEAT_BUYINGLIKEHOOD < cntrl.med

# no GLM needed; there are no other covariates
tab = table( dat3$CONDITION, dat3$Y )

library(metafor)
escalc( measure = "RR",
        ai = tab[1,1],
        bi = tab[1,2],
        ci = tab[2,1],
        di = tab[2,2] )

# 
# #### ~~ old
# ##### Study 2 #####
# library(foreign)
# dat2 = read.spss("Study 2.sav", to.data.frame=TRUE)
# 
# # they didn't save the outcome variable; make it as in their code
# dat2$MEAT_BUYINGLIKEHOOD = (dat2$BEEF_BUY + dat2$PORK_BUY + dat2$CHICKEN_BUY + dat2$RAWPORK_BUY + dat2$LAMB_BUY) / 5
# 
# # ~~ problem: these are factors, with some things coded as words and others as integers...
# cntrl.med = median( dat2$MEAT_BUYINGLIKEHOOD[dat2$CONDITION == "CONTROL"] )
# dat2$lo = dat2$MEAT_BUYINGLIKEHOOD < cntrl.med
# 
# # no GLM needed; there are no other covariates
# 
# dat2 = dat2[ dat2$CONDITION %in% c("Neutral essay", "Moral essay"), ]
# dat1 = droplevels(dat1)
# tab = table( dat1$CONDITION, dat1$lo )
# 
# library(metafor)
# escalc( measure = "RR",
#         ai = tab[1,1],
#         bi = tab[1,2],
#         ci = tab[2,1],
#         di = tab[2,2] )
# 
# # ~~~ stopped here -- contact them?


##### Study 2 #####
library(foreign)
dat3 = read.spss("Study 3.sav", to.data.frame=TRUE)

table(dat)


################################# REESE 2015 ################################# 

# this one has a ton of effect sizes, so will make its dataset automatically

setwd(original.data.dir)
setwd("Reese 2015")

library(readxl)


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
  
    es = get_rr( condition = all.conditions[j],
                 condition.var.name = "condition",
                 control.name = "control",
            dat = dats[[i]] )
    
    draw <<- add_row(draw, 
                   authoryear = "Reese 2015",
                   substudy = paste( "Study ", i+2, ", ", all.conditions[j], sep=""),
                   desired.direction = es$yi > 0,  # ~~ assumes that for raw data, we also code Y such that positive is good
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

# ~~~ definitely hand-check a few of these


################################# COONEY 2015 ################################# 

# outcome is number of weekly meals containing animal products 
setwd(original.data.dir)
setwd("Cooney 2015")
dat = read.csv("raw_data.csv")

# group the booklets with same message type, as in JP's analysis
dat$condition = gsub('[0-9]+', '', dat$BookletDescrp) 

# check missing data %
prop.table( table( is.na(dat$Total_Chg) ) )

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

current.cols = names(dat)[grepl("Current", names(dat))][1:5]
table( apply( dat[,current.cols],
              1,
              sum ) ==
         dat$Total_Current )

# check how total_chg was calculated
table( dat$Total_Chg == dat$Total_FollowupYN - dat$Total_Current )
# total_chg is not always equal to the relevant difference
# often off by a sign

# fix the Total_Chg variable accordingly
dat$Total_Chg = dat$Total_FollowupYN - dat$Total_Current
 
# reduce vs. stay same or increase
dat$Y = dat$Total_Chg < 0

agg = dat %>% group_by(condition) %>%
  summarise( Preduce = mean(Y),
             N.tot = n(),
             N.reduce = sum(Y == 1) )
print(agg)


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
  
  es = get_rr( condition = all.conditions[j],
               condition.var.name = "condition",
               control.name = "control",
               dat = dat )
  
  draw <<- add_row(draw, 
                   authoryear = "Cooney 2015",
                   substudy = all.conditions[j],
                   desired.direction = es$yi > 0,  # ~~ assumes that for raw data, we also code Y such that positive is good
                   effect.measure = "log-rr",
                   interpretation = "Reduce vs. don't",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = as.numeric(es$yi),
                   vi = as.numeric(es$vi)
  )
}

##### Effect Size #2: No Meat vs. Any Meat #####

# rename outcome since below fn looks for a variable called "Y"
dat$Y = dat$Total_FollowupYN == 0
# only 5 subjects ate no meat

for (j in 1:length( all.conditions ) ) {
  
  es = get_rr( condition = all.conditions[j],
               condition.var.name = "condition",
               control.name = "control",
               dat = dat )
  
  draw <<- add_row(draw, 
                   authoryear = "Cooney 2015",
                   substudy = all.conditions[j],
                   desired.direction = es$yi > 0,  # ~~ assumes that for raw data, we also code Y such that positive is good
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
setwd("Cooney 2016")

# don't worry about warnings
library(readxl)
dat = read_xlsx("cleanImpactStudyData.ForAnalysis.xlsx")

#sanity checks
library(dplyr)
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
library(metafor)
escalc( measure = "RR",
        ai = tab[1,1],
        bi = tab[1,2],
        ci = tab[2,1],
        di = tab[2,2] )


##### Effect Size #2: No Meat vs. Any Meat #####
# no GLM needed; there are no other covariates
tab = table( dat$group, dat$zeroServingsOfMeat )
prop.table(tab, margin = 1)  # matches what they reported :) 
library(metafor)
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
setwd("Kunst 2016/Data from author/kunst_hohle_2016")



##### Study 2A, Effect Size #1: Main RR #####
library(foreign)
dat = read.spss("study2a.sav", to.data.frame=TRUE)

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

get_rr(condition = "head",
       condition.var.name = "con",
       control.name = "no-head",
       dat = dat)


##### Study 2B, Effect Size #1: Main RR #####

library(foreign)
dat = read.spss("study_2b.sav", to.data.frame=TRUE)

# look at variable codings
# View(attr(dat, "variable.labels"))
# here's the outcome we want
attr(dat, "variable.labels")[names(dat)=="veg_1"]

# reproduce bottom of pg. 764 - looks good
dat %>% group_by(con) %>%
  summarise( mean = mean(veg_1, na.rm = TRUE),
             n = sum( !is.na(con) & !is.na(veg_1) ) )

# outcome: above vs. below control group's median willingness to choose veg alternative
cntrl.med = median( dat$veg_1[ dat$con == "no-head"] )
dat$Y = dat$veg_1 > cntrl.med

get_rr(condition = "head",
       condition.var.name = "con",
       control.name = "no-head",
       dat = dat)


##### Study 3, Effect Size #1: Main RR #####
library(foreign)
dat = read.spss("study3.sav", to.data.frame=TRUE)

# look at variable codings
# View(attr(dat, "variable.labels"))
# here's the outcome we want
attr(dat, "variable.labels")[names(dat)=="eat_1"]

dat %>% group_by(con) %>%
  summarise( mean = mean(eat_1, na.rm = TRUE),
             n = sum( !is.na(con) & !is.na(eat_1) ) )

# outcome: below vs. above control group's median willingness to eat
cntrl.med = median( dat$eat_1[ dat$con == "control"] )
dat$Y = dat$eat_1 < cntrl.med

get_rr(condition = "animal shown",
       condition.var.name = "con",
       control.name = "control",
       dat = dat)


##### Study 5, Effect Size #1: Main RR #####

library(foreign)
dat = read.spss("study5.sav", to.data.frame=TRUE)

# look at variable codings
# View(attr(dat, "variable.labels"))
# here's the outcome we want
attr(dat, "variable.labels")[names(dat)=="veg_1"]

dat %>% group_by(con) %>%
  summarise( mean = mean(veg_1, na.rm = TRUE),
             n = sum( !is.na(con) & !is.na(veg_1) ) )

# outcome: above vs. below control group's median willingness to choose veg alternative
cntrl.med = median( dat$veg_1[ dat$con == "euphemisms"] )
dat$Y = dat$veg_1 > cntrl.med

get_rr(condition = "animal names",
       condition.var.name = "con",
       control.name = "euphemisms",
       dat = dat)


################################# KUNST 2018 ################################# 

setwd(original.data.dir)
setwd("Kunst 2016/Data from author/kunst_haugestad_2018")

library(foreign)
dat = read.spss("omnivores.sav", to.data.frame=TRUE)

# look at variable codings
# View(attr(dat, "variable.labels"))
# here's the outcome we want
attr(dat, "variable.labels")[names(dat)=="veg_1"]

dat %>% group_by(con) %>%
  summarise( mean = mean(veg_1, na.rm = TRUE),
             n = sum( !is.na(con) & !is.na(veg_1) ) )

# outcome: above vs. below control group's median willingness to choose veg alternative
cntrl.med = median( dat$veg_1[ dat$con == "no head"] )
dat$Y = dat$veg_1 > cntrl.med

get_rr(condition = "head",
       condition.var.name = "con",
       control.name = "no head",
       dat = dat)


