

# Notes: 
# 
#  - Analyzing on RR scale, not SMD, because that for many studies, the Chinn assumption of an underlying
#   normal RV seems not reasonable. For others, the common-outcome assumption seems questionable.
#   With this modification, we assume the SMDs are computed on normal data, which seems more reasonable. 
#   Used square-root transformation when outcome was rare or if we couldn't tell.  
# 
# - Tried to calculate RRs contrasting "high" vs. "low" meat consumption per each study's definition of those
#   terms. Sometimes these differed from study's own analyses (e.g., Anderson 2017, for which their model estimated 
#   cumulative logit ORs for ordinal categories of consumption). When the outcome was about reducing vs. increasing 
#   future meat consumption, we dichotomized at "staying the same". 
# 
# - For longitudinal controlled studies with pre/post measures, when available, used DID rather than just comparison
#  at final time point. Used latter if it was the only thing available. 
# 
# - For longitudinal studies with multiple post-intervention time points, we used the one with the longest
#   follow-up. 
# 
# - When there were multiple point estimates on same subjects, but our hierarchy didn't help decide between them, 
#   we averaged the effect sizes within a study assuming independence for a conservative SE (e.g., Hennessy).


# Idea2:
#  - Use the public datasets to compare vegetarianism as outcome vs. servings
#  - For studies with both self-reported and intended, calculate w/in study difference between 
#    these measures and compare


library(metafor)
library(dplyr)
library(testthat)

data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Data extraction"
code.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Data extraction/awr_data_extraction_git"
# location of original datasets and code for reproducible studies
original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review"
setwd(code.dir); source("helper.R")

d = as.data.frame( matrix( ncol = 6, nrow = 0 ) )
names(d) = c( "authoryear",
              "substudy",
              "desired.direction",
              "effect.measure",
              "yi",
              "vi")

############################### ENTER DATA FOR EACH STUDY ############################### 


##### Cordts 2014 #####
escalc_add_row( authoryear = "Cordts 2014",
                substudy = NA,
                desired.direction = 1,
                effect.measure = "log-rr",
                
                measure = "RR",
                
                ai = round(.28 * 150), # Tx intending to reduce meat
                bi = round( (1-.28) * 150),  # Tx not intending
                ci = round(.128 * 556),  # control intending to reduce
                di = round( (1-.128) * 556) )  # control not intending
# sanity check
expect_equal( exp( d$yi[ d$authoryear == "Cordts 2014" ] ),
              (42/150) / (71/556) )


##### Palomo-Velez 2018, Study 1 #####

escalc_add_row( authoryear = "Palomo-Velez 2018",
                substudy = "Study 1",
                desired.direction = 1,
                effect.measure = "smd",
                
                measure = "SMD",
                
                m1i = .72, # Table 1
                sd1i = 2.13, 
                n1i = 77, # N from df on pg 3
                
                m2i = .23,
                sd2i = 1.80,
                n2i = 76 )  # N from df on pg 3


##### Palomo-Velez 2018, Study 2 #####

escalc_add_row( authoryear = "Palomo-Velez 2018",
                substudy = "Study 2",
                desired.direction = 1,
                effect.measure = "smd",
                
                measure = "SMD",
                
                m1i = .17, # Table 1
                sd1i = 2.14, 
                n1i = 83, # directly reported on page 4
                
                m2i = -.49,
                sd2i = 1.90,
                n2i = 79 )  # N from df on pg 5

##### Palomo-Velez 2018, Study 3 #####

escalc_add_row( authoryear = "Palomo-Velez 2018",
                substudy = "Study 3",
                desired.direction = 1,
                effect.measure = "smd",
                
                measure = "SMD",
                
                m1i = .22, # Table 1
                sd1i = 1.86, 
                n1i = 135, # directly reported on page 5
                
                m2i = -.29,
                sd2i = 1.98,
                n2i = 135 )  # directly reported on page 5


##### Anderson 2017 (iAnimal), 360 video and 2D video ######
# they fit a cumulative logit mixed model
# for direct comparability with other meta-analyzed studies, 
#  we are instead dichotomizing at low vs. high consumption using Anderson's own criteria
#  (i.e., low = eating pork <=3X/month; high = more than that; see pg 22)
# see the separate file, recalculate_anderson_2017_estimates.R

d = dplyr::add_row(.data = d,
                   authoryear = "Anderson 2017",
                   substudy = "360-degree video",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   yi = 0.0481136,
                   vi = 0.000437124 )

d = dplyr::add_row(.data = d,
                   authoryear = "Anderson 2017",
                   substudy = "standard video",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   yi = 0.05666146,
                   vi = 0.00163489 )



##### Arndt (2016) [43] - Table 46, row 1
# need to email - see note on article


##### Bertolaso 2015, moral shocks & promotion #####
# Table 1
escalc_add_row( authoryear = "Bertolaso 2015",
                substudy = "moral shocks & promotion focus",
                desired.direction = 1,
                effect.measure = "smd",
                
                measure = "SMD",
                
                m1i = 1.6, # Table 1
                sd1i = 0.38, 
                n1i = 107, 
                
                m2i = 1.48,
                sd2i = 0.38,
                n2i = 106 ) 


##### Bertolaso 2015, moral shocks &  prevention #####
# Table 1
escalc_add_row( authoryear = "Bertolaso 2015",
                substudy = "moral shocks & prevention focus",
                desired.direction = 1,
                effect.measure = "smd",
                
                measure = "SMD",
                
                m1i = 1.6, # Table 1
                sd1i = 0.38, 
                n1i = 107, 
                
                m2i = 1.51,
                sd2i = 0.33,
                n2i = 108 ) 


##### Bertolaso 2015, individualization & promotion #####
# Table 1
escalc_add_row( authoryear = "Bertolaso 2015",
                substudy = "individualization & promotion focus",
                desired.direction = 1,
                effect.measure = "smd",
                
                measure = "SMD",
                
                m1i = 1.6, # Table 1
                sd1i = 0.38, 
                n1i = 107, 
                
                m2i = 1.48,
                sd2i = 0.31,
                n2i = 92 ) 


##### Bertolaso 2015, moral shocks & promotion #####
# Table 1
escalc_add_row( authoryear = "Bertolaso 2015",
                substudy = "individualization & prevention focus",
                desired.direction = 1,
                effect.measure = "smd",
                
                measure = "SMD",
                
                m1i = 1.6, # Table 1
                sd1i = 0.38, 
                n1i = 107, 
                
                m2i = 1.55,
                sd2i = 0.35,
                n2i = 98 ) 




##### Anderson 2015 (PLOS), Study 3 #####
# use SEs and n's in appendix
escalc_add_row( authoryear = "Anderson 2015",
                substudy = NA,
                desired.direction = 1,
                effect.measure = "smd",
                
                measure = "SMD",
                
                m1i = 72.88, 
                sd1i = 2.06 * sqrt(114),  # SE * sqrt(n) 
                n1i = 114, 
                
                m2i = 54.86,
                sd2i = 2.42 * sqrt(114),
                n2i =  114) 


##### Amiot 2018 (PLOS) #####
# ~~~ look into/think about different ways to get RRs here
# see separate code file
d = dplyr::add_row(.data = d,
                   authoryear = "Amiot 2018",
                   substudy = NA,
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   yi = -0.5259724,
                   vi = 0.6344842^2 )



##### Hennessy 2016, "why" leaflet #####
# Table 3
# "why" leaflet (intervention 1)
es = escalc( measure = "SMD",
        
        m1i = c(4.85, 0.80), # vector for the two outcomes: Meat2, MeatYesterday
        sd1i = c(2.37, 0.40),
        n1i = c(320, 320),
        
        m2i = c(4.86, .82),
        sd2i = c(2.15, 0.39),
        n2i =  c(318, 318) )

# average them assuming independence for conservative variance
smd = sum(es$yi/es$vi) / sum(1/es$vi)
smd.se = 0.5 * sqrt(es$vi[1] + es$vi[2])  # Borenstein with r = 0


d = dplyr::add_row(.data = d,
                   authoryear = "Hennessy 2016",
                   substudy = '"why" leaflet',
                   desired.direction = -1,
                   effect.measure = "smd",
                   yi = smd,
                   vi = smd.se^2 )


##### Hennessy 2016, "how" leaflet #####
# ~~~ this study also has intended behavior (going vegetarian), which I did not extract
#     but which might go in subgroup analyses
# Table 3
# "how" leaflet (intervention 2)
es = escalc( measure = "SMD",
             
             m1i = c(4.85, 0.80), # vector for the two outcomes: Meat2, MeatYesterday
             sd1i = c(2.37, 0.40),
             n1i = c(320, 320),
             
             m2i = c(5.01, .78),
             sd2i = c(2.19, .41),
             n2i =  c(318, 318) )

# average them assuming independence for conservative variance
smd = sum(es$yi/es$vi) / sum(1/es$vi)
smd.se = 0.5 * sqrt(es$vi[1] + es$vi[2])  # Borenstein with r = 0


d = dplyr::add_row(.data = d,
                   authoryear = "Hennessy 2016",
                   substudy = '"how" leaflet',
                   desired.direction = -1,
                   effect.measure = "smd",
                   yi = smd,
                   vi = smd.se^2 )


##### Cooney 2016 [36] 

# averaging the two eligible outcomes: meat consumption and going vegetarian

# there's also self-reported dietary change...tough calls here...
# decreased vs. increased

# should we have a rule saying the most granular outcome wins for main analyses?



# bm :)


############################### MERGE IN QUALITATIVE DATA ############################### 

# qualitative data entered into Excel
setwd(data.dir)
library(readxl)
d2 = read_xlsx("Extracted qualitative data.xlsx")

##### unique merger variable
d$unique = NA
d$unique[ is.na(d$substudy) ] = d$authoryear[ is.na(d$substudy) ]
d$unique[ !is.na(d$substudy) ] = paste( d$authoryear[ !is.na(d$substudy) ],
                                              d$substudy[ !is.na(d$substudy) ],
                                              sep = " ")
d$unique

# make pretty for evetual forest plot
d2$unique = NA
d2$unique[ is.na(d2$`Substudy #`) ] = paste( d2$`First author last name`[ is.na(d2$`Substudy #`) ],
                                             d2$Year[ is.na(d2$`Substudy #`) ],
                                             sep = " ")
d2$unique[ !is.na(d2$`Substudy #`) ] = paste( d2$`First author last name`[ !is.na(d2$`Substudy #`) ],
                                             d2$Year[ !is.na(d2$`Substudy #`) ],
                                             d2$`Substudy #`[ !is.na(d2$`Substudy #`) ],
                                             sep = " ")
d2$unique

# merge them
d = merge( d,
           d2,
           by.x = "unique",
           by.y = "unique" )

# minimal sanity check for absurd values
sort(d$yi)
sort(sqrt(d$vi))
sort(d$yi/sqrt(d$vi))



############################### CONVERT EFFECT SIZES - to RRs ###############################

# synchronize directions so that positive is always good
d$yi[ sign(d$yi) != sign(d$desired.direction) ] = -d$yi[ sign(d$yi) != sign(d$desired.direction) ]

table( d$effect.measure )

# analysis scale
d$logRR = NA
d$varlogRR = NA

##### RRs #####
d$logRR[ d$effect.measure == "log-rr" ] = d$yi[ d$effect.measure == "log-rr" ]
d$varlogRR[ d$effect.measure == "log-rr" ] = d$vi[ d$effect.measure == "log-rr" ]

##### SMDs #####
RR.stats = d_to_logRR( smd = d$yi[ d$effect.measure == "smd" ],
                       smd.se = sqrt(d$vi[ d$effect.measure == "smd" ]) )
d$logRR[ d$effect.measure == "smd" ] = RR.stats$logRR
d$varlogRR[ d$effect.measure == "smd" ] = RR.stats$varlogRR

##### ORs #####
RR.stats = logOR_to_logRR( logOR = d$yi[ d$effect.measure == "log-or" ],
                       varlogOR = d$vi[ d$effect.measure == "log-or" ] )
d$logRR[ d$effect.measure == "log-or" ] = RR.stats$logRR
d$varlogRR[ d$effect.measure == "log-or" ] = RR.stats$varlogRR

# CI limits
z.crit = qnorm(.975)
d$RR.lo = exp( d$logRR - z.crit * sqrt(d$varlogRR) )
d$RR.hi = exp( d$logRR + z.crit * sqrt(d$varlogRR) )

# ############################### CONVERT EFFECT SIZES - to SMDs ###############################
# 
# # synchronize directions so that positive is always good
# d$yi[ sign(d$yi) != sign(d$desired.direction) ] = -d$yi[ sign(d$yi) != sign(d$desired.direction) ]
# 
# table( d$effect.measure )
# 
# ##### SMDs #####
# d$smd = NA
# d$smd[ d$effect.measure == "smd" ] = d$yi[ d$effect.measure == "smd" ]
# d$smd.se[ d$effect.measure == "smd" ] = sqrt(d$vi[ d$effect.measure == "smd" ])
# 
# ##### Log-RRs #####
# RR = exp( d$yi[ d$effect.measure == "log-rr" ] )
# var.log.rr = d$vi[ d$effect.measure == "log-rr" ]
# OR = RR^2  # approximate OR with common outcome (~~make sure sensible for all papers!)
# smd = log(OR) * sqrt(3)/pi
# # use delta method to approximate SE of d
# # we have SE for log-RR, so letting x = RR,
# # the transformation to d via OR is log( x^2 ) * sqrt(3)/pi
# # derivative of that transformation is 2 * sqrt(3)/(pi*x)
# se = sqrt(var.log.rr) * 2 * sqrt(3)/(pi * RR)
# d$smd[ d$effect.measure == "log-rr" ] = smd
# d$smd.se[ d$effect.measure == "log-rr" ] = se
# 
# 
# 
# ##### Pearson's R #####
# # ~~~ only one such paper, and it has binary X, so can use point-biserial thing
# smd = r_to_d_ptbis( r = d$yi[ d$effect.measure == 'pearson.r' ],
#                     N = d$N[ d$effect.measure == 'pearson.r' ])$d
# smd.se = r_to_d_ptbis( r = d$yi[ d$effect.measure == 'pearson.r' ],
#                        N = d$N[ d$effect.measure == 'pearson.r' ])$se
# d$smd[ d$effect.measure == "pearson.r" ] = smd
# d$smd.se[ d$effect.measure == "pearson.r" ] = smd.se


############################### WRITE PREPPED DATA ############################### 

# rename the columns to be used in analysis first! 

setwd(data.dir)
write.csv(d, "prepped_data.csv", row.names = FALSE)
