

# Notes: 
# 
#  ** = had raw data
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
# 
# - When raw data were available, we calculated the RR for being below vs. above median meat consumption, where "median"
#   is the median at baseline among both control and treateds in a longitudinal study, or the median among controls in a 
#   a 2-group study. In longitudinal studies, we controlled for meat consumption at baseline (treated as continuous).
#   When a servings variable wasn't available, we tried to instead dichotomize at "reduce meat consumption" vs. "stay
#   the same or increase" (e.g., Reese 2015). 
#
#  - We considered two alternative effect size codings (grams of meat and "going vegetarian"). Some studies had raw data, 
#   so we could calculate multiple codings per point estimate (which were not included together in the same analysis, of course). 
#  For "going vegetarian", this only included studies in which subjects reported how much meat they had consumed
#  and excluded outcomes like "willingness to choose a vegetarian restaurant". 


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

d = as.data.frame( matrix( ncol = 10, nrow = 0 ) )
names(d) = c( "authoryear",
              "substudy",
              "desired.direction",
              "effect.measure",
              "interpretation",
              "use.rr.analysis",
              "use.grams.analysis",
              "use.veg.analysis",
              "yi",
              "vi")

############################### ENTER DATA FOR EACH STUDY ############################### 

# asterisks denote studies with raw data

##### **Amiot 2018 (PLOS) #####

# same study, 3 effect sizes for different analyses
d = dplyr::add_row(.data = d,
                   authoryear = "Amiot 2018",
                   substudy = NA,
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high 1-week consumption",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = 0.395127,
                   vi = 0.2381223399^2 )

d = dplyr::add_row(.data = d,
                   authoryear = "Amiot 2018",
                   substudy = NA,
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "No meat vs. any",
                   use.rr.analysis = 0,
                   use.grams.analysis = 0,
                   use.veg.analysis = 1,
                   yi = 1.018e-14,
                   vi = 0.3554550562^2 )


d = dplyr::add_row(.data = d,
                   authoryear = "Amiot 2018",
                   substudy = NA,
                   desired.direction = 1,
                   effect.measure = "rmd",
                   interpretation = "Grams of meat",
                   use.rr.analysis = 0,
                   use.grams.analysis = 1,
                   use.veg.analysis = 0,
                   yi = -64.0625,
                   vi = 19538.9216 )

##### **Anderson 2016 (PLOS), Study 3 ######
# same study, 3 effect sizes for different analyses
d = dplyr::add_row(.data = d,
                   authoryear = "Anderson 2016",
                   substudy = NA,
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high consumption",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = -0.5397,
                   vi = 0.07832975^2 )

d = dplyr::add_row(.data = d,
                   authoryear = "Anderson 2016",
                   substudy = NA,
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "No meat vs. any",
                   use.rr.analysis = 0,
                   use.grams.analysis = 0,
                   use.veg.analysis = 1,
                   yi = -1.038e-14,
                   vi = 0.21344062^2 )

d = dplyr::add_row(.data = d,
                   authoryear = "Anderson 2016",
                   substudy = NA,
                   desired.direction = 1,
                   effect.measure = "rmd",
                   interpretation = "Grams of meat",
                   use.rr.analysis = 0,
                   use.grams.analysis = 1,
                   use.veg.analysis = 0,
                   yi = -0.1084,
                   vi = 0.0901^2 )



##### Arndt (2016)
# ~~~ asked for raw data; this is hopefully temporary
# bm: 
# these are in terms of servings, not grams
# convert by multiplying by grams/serving

# total sample size in paper (pg 52): 179 (Table 13) + 338 (Table 46)

# Table 13, personalized & not tailored
escalc_add_row( authoryear = "Arndt 2016",
                substudy = "Study 1, personalized & not tailored",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Servings SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 2.57, 
                sd1i = 2.68,
                n1i = 37,  # sample sizes on pg 52
                
                m2i = 2.63,
                sd2i = 2.02,
                n2i = 40 )

# Table 13, generalized & not tailored
escalc_add_row( authoryear = "Arndt 2016",
                substudy = "Study 1, generalized & not tailored",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Servings SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 2.21, 
                sd1i = 1.63,
                n1i = 37,
                
                m2i = 2.63,
                sd2i = 2.02,
                n2i = 40 )

# Table 46, self-schema & not tailored
# ~~~ using 338/9 for all sample sizes in this table
escalc_add_row( authoryear = "Arndt 2016",
                substudy = "Study 2, self-schema & not tailored",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Servings SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 2.13, 
                sd1i = 1.96,
                n1i = round(338/9),
                
                m2i = 2.75,
                sd2i = 2,
                n2i = round(338/9) )

# Table 46, altruistic & not tailored
escalc_add_row( authoryear = "Arndt 2016",
                substudy = "Study 2, altruistic & not tailored",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Servings SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 2.24, 
                sd1i = 1.96,
                n1i = round(338/9),
                
                m2i = 2.75,
                sd2i = 2,
                n2i = round(338/9) )

# Table 46, egotistic & not tailored
escalc_add_row( authoryear = "Arndt 2016",
                substudy = "Study 2, egotistic & not tailored",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Servings SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 1.92, 
                sd1i = 1.32,
                n1i = round(338/9),
                
                m2i = 2.75,
                sd2i = 2,
                n2i = round(338/9) )

# Table 46, non-specific & not tailored
escalc_add_row( authoryear = "Arndt 2016",
                substudy = "Study 2, non-specific & not tailored",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Servings SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 2.57, 
                sd1i = 2.3,
                n1i = round(338/9),
                
                m2i = 2.75,
                sd2i = 2,
                n2i = round(338/9) )

# for grams of meat conversion, using 3 oz/serving (see page 42)
grams.per.serving = 85.0486

new.rows = d %>% filter( authoryear == "Arndt 2016" ) %>%
  mutate( yi = yi * grams.per.serving,
          vi = vi * grams.per.serving^2,
          effect.measure = "rmd", 
          interpretation = "Grams of meat",
          use.rr.analysis = 0,
          use.grams.analysis = 1,
          use.veg.analysis = 0)
d = rbind(d, new.rows)



##### Caldwell 2016 #####
# reduce vs. keep consumption the same 
# see their xlsx file
# no servings variable

# egg policy article
escalc_add_row( authoryear = "Caldwell 2016",
                substudy = "egg policy article",
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Reduce vs. don't",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = round(0.07291667 * 193), # Tx intending to reduce meat
                bi = round( (1-0.07291667) * 193),  # Tx not intending
                ci = round(0.045 * 200),  # control intending to reduce
                di = round( (1-0.045) * 200) )  # control not intending


# egg legislation article
escalc_add_row( authoryear = "Caldwell 2016",
                substudy = "egg legislation article",
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Reduce vs. don't",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = round(0.08717949 * 195), # Tx intending to reduce meat
                bi = round( (1-0.08717949) * 195),  # Tx not intending
                ci = round(0.004901961 * 205),  # control intending to reduce
                di = round( (1-0.004901961) * 205) )  # control not intending


# pork policy article
escalc_add_row( authoryear = "Caldwell 2016",
                substudy = "pork policy article",
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Reduce vs. don't",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = round(0.1428571 * 197), # Tx intending to reduce meat
                bi = round( (1-0.1428571) * 197),  # Tx not intending
                ci = round(0.08629442 * 200),  # control intending to reduce
                di = round( (1-0.08629442) * 200) )  # control not intending


# pork legislation article
escalc_add_row( authoryear = "Caldwell 2016",
                substudy = "pork legislation article",
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Reduce vs. don't",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = round(0.1932367 * 207), # Tx intending to reduce meat
                bi = round( (1-0.1932367) * 207),  # Tx not intending
                ci = round(0.09405941 * 202),  # control intending to reduce
                di = round( (1-0.09405941) * 202) )  # control not intending


##### Caldwell 2017 #####
# reduce vs. keep consumption the same 
# no servings variable
# used the Tableau plots on their website for stats and the supplement for N's
# figure out the number of control subjects
nc = 2594 - (515+507+513+524)

# cruelty videos
escalc_add_row( authoryear = "Caldwell 2017",
                substudy = "cruelty videos",
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Reduce vs. don't",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = round(.278 * 515), # Tx intending to reduce meat
                bi = round( (1-.278) * 515),  # Tx not intending
                ci = round(.216 * nc),  # control intending to reduce
                di = round( (1-.216) * nc) )  # control not intending


# comparison videos
escalc_add_row( authoryear = "Caldwell 2017",
                substudy = "comparison videos",
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Reduce vs. don't",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = round(.294 * 507), # Tx intending to reduce meat
                bi = round( (1-.294) * 507),  # Tx not intending
                ci = round(.216 * nc),  # control intending to reduce
                di = round( (1-.216) * nc) )  # control not intending


# cute videos
escalc_add_row( authoryear = "Caldwell 2017",
                substudy = "cute videos",
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Reduce vs. don't",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = round(.24 * 513), # Tx intending to reduce meat
                bi = round( (1-.24) * 513),  # Tx not intending
                ci = round(.216 * nc),  # control intending to reduce
                di = round( (1-0.216) * nc) )  # control not intending

# lifestyle videos aren't animal welfare



##### Cordts 2014 ######
# Table 6
escalc_add_row( authoryear = "Cordts 2014",
                substudy = NA,
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Reduce vs. don't",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = round(.28 * 150), # Tx intending to reduce meat
                bi = round( (1-.28) * 150),  # Tx not intending
                ci = round(.128 * 556),  # control intending to reduce
                di = round( (1-.128) * 556) )  # control not intending
# sanity check
expect_equal( exp( d$yi[ d$authoryear == "Cordts 2014" ] ),
              (42/150) / (71/556) )


##### **Palomo-Velez 2018, Study 1 #####
# public data; used median split
escalc_add_row( authoryear = "Palomo-Velez 2018",
                substudy = "Study 1",
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Low vs. high meat buying likelihood",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = 30,  # these are from my code
                bi = 39,
                ci = 39,
                di = 36 )  # N from df on pg 3


##### **Palomo-Velez 2018, Study 2 #####
# data from author; used median split

d = dplyr::add_row(.data = d,
                   authoryear = "Palomo-Velez 2018",
                   substudy = "Study 2",
                   desired.direction = 1,
                   interpretation = "Low vs. high meat buying likelihood",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   effect.measure = "log-rr",
                   yi = 0.1093,
                   vi = 0.0242 )


##### **Palomo-Velez 2018, Study 2 #####
# data from author; used median split
d = dplyr::add_row(.data = d,
                   authoryear = "Palomo-Velez 2018",
                   substudy = "Study 3",
                   desired.direction = 1,
                   interpretation = "Low vs. high meat buying likelihood",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   effect.measure = "log-rr",
                   yi = 0.2492,
                   vi = 0.0188 )

##### **Anderson 2017 (iAnimal), 360 video and 2D video ######
# had raw data, so used median split
# see the separate file, recalculate_anderson_2017_estimates.R
d = dplyr::add_row(.data = d,
                   authoryear = "Anderson 2017",
                   substudy = "standard video",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high consumption frequency",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = 0.17653,
                   vi = 0.05059599^2 )

d = dplyr::add_row(.data = d,
                   authoryear = "Anderson 2017",
                   substudy = "360-degree video",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high consumption frequency",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = 0.07114,
                   vi = 0.02430246^2 )



##### Bertolaso 2015, moral shocks & promotion #####
# Table 1
escalc_add_row( authoryear = "Bertolaso 2015",
                substudy = "moral shocks & promotion focus",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Consumption frequency SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
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
                interpretation = "Consumption frequency SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
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
                interpretation = "Consumption frequency SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
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
                interpretation = "Consumption frequency SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 1.6, # Table 1
                sd1i = 0.38, 
                n1i = 107, 
                
                m2i = 1.55,
                sd2i = 0.35,
                n2i = 98 ) 



##### **Cooney 2015 #####
# this one had a ton of effect sizes, so was prepped more automatically
original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review"
setwd(original.data.dir)
setwd("Cooney 2015")

d = rbind( d, read.csv("cooney_2015_prepped_effect_sizes.csv")[,-1] )


##### **Cooney 2016 #####
# there's also self-reported dietary change

# same study, 3 effect sizes for different analyses
d = dplyr::add_row(.data = d,
                   authoryear = "Cooney 2016",
                   substudy = NA,
                   desired.direction = -1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high 2-day consumption",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = -0.0107,
                   vi = 0.0026 )

d = dplyr::add_row(.data = d,
                   authoryear = "Cooney 2016",
                   substudy = NA,
                   desired.direction = -1,
                   effect.measure = "log-rr",
                   interpretation = "No meat vs. any",
                   use.rr.analysis = 0,
                   use.grams.analysis = 0,
                   use.veg.analysis = 1,
                     yi = -0.0107,
                   vi = 0.0026 )

d = dplyr::add_row(.data = d,
                   authoryear = "Cooney 2016",
                   substudy = NA,
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Grams of meat",
                   use.rr.analysis = 0,
                   use.grams.analysis = 1,
                   use.veg.analysis = 0,
                   yi = -17.38612,
                   vi = 485.8731 )



##### Hennessy 2016, "why" leaflet #####
# Table 3
# "why" leaflet (intervention 1)
# "Total" = sum of number of days over past week on which subject ate various animal products

# ~~~ ideally get raw data and use Meat2 (days over past week with meat consumption) to look at vegetarianism

# control N: 170 (page 30)
# "why" leafelet N: 79 (page 23)
# "how" leafelet N: 78 (page 23)

# number of days subject ate meat over past week
escalc_add_row( authoryear = "Hennessy 2016",
                substudy = 'why leaflet',
                desired.direction = -1,
                effect.measure = "smd",
                interpretation = "Consumption frequency SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 12.22, # Table 3
                sd1i = 4.25, 
                n1i = 79, 
                
                m2i = 11.64,
                sd2i = 4.14,
                n2i = 170 )


escalc_add_row( authoryear = "Hennessy 2016",
                substudy = 'how leaflet',
                desired.direction = -1,
                effect.measure = "smd",
                interpretation = "Consumption frequency SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 12.65, # Table 3
                sd1i = 3.92, 
                n1i = 78, 
                
                m2i = 11.64,
                sd2i = 4.14,
                n2i = 170 )



# page 29 reports on becoming vegetarian (based on 1-week meat consumption)
# 10 subjects in control group went veg and 1 in each treatment arm
# alternatively, we could have used "MeatYesterday" (did subject eat meat yesterday?), 
#  but we chose the longer 1-week follow-up per our decision hierarchy
escalc_add_row( authoryear = "Hennessy 2016",
                substudy = 'why leaflet',
                desired.direction = -1,
                effect.measure = "log-rr",
                interpretation = "No meat vs. any",
                use.rr.analysis = 0,
                use.grams.analysis = 0,
                use.veg.analysis = 1,
                measure = "RR",
                
                ai = 1, # Tx who went vegetarian
                bi = 79 - 1,  # Tx who did not go veg
                ci = 10,  # control intending to reduce
                di = 170 - 10  ) # control who did not go veg

# same stats as above (not a typo)
escalc_add_row( authoryear = "Hennessy 2016",
                substudy = 'how leaflet',
                desired.direction = -1,
                effect.measure = "log-rr",
                interpretation = "No meat vs. any",
                use.rr.analysis = 0,
                use.grams.analysis = 0,
                use.veg.analysis = 1,
                measure = "RR",
                
                ai = 1, # Tx who went vegetarian
                bi = 79 - 1,  # Tx who did not go veg
                ci = 10,  # control intending to reduce
                di = 170 - 10  ) # control who did not go veg


##### **MacDonald 2016 #####
# low vs. high (using baseline median)
d = dplyr::add_row(.data = d,
                   authoryear = "MacDonald 2016",
                   substudy = "reduce",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high 1-week consumption",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = 0.060872,
                   vi = 0.049757488^2 )

d = dplyr::add_row(.data = d,
                   authoryear = "MacDonald 2016",
                   substudy = "eliminate",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high 1-week consumption",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = 0.077449,
                   vi = 0.048763497^2 )

# go vegetarian
d = dplyr::add_row(.data = d,
                   authoryear = "MacDonald 2016",
                   substudy = "reduce",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "No meat vs. any",
                   use.rr.analysis = 0,
                   use.grams.analysis = 0,
                   use.veg.analysis = 1,
                   yi = 0.07387,
                   vi = 0.1680007^2 )

d = dplyr::add_row(.data = d,
                   authoryear = "MacDonald 2016",
                   substudy = "eliminate",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "No meat vs. any",
                   use.rr.analysis = 0,
                   use.grams.analysis = 0,
                   use.veg.analysis = 1,
                   yi = 0.05470,
                   vi = 0.1389741^2 )

# grams of meat
d = dplyr::add_row(.data = d,
                   authoryear = "MacDonald 2016",
                   substudy = "reduce",
                   desired.direction = 1,
                   effect.measure = "rmd",
                   interpretation = "Grams of meat (1 week)",
                   use.rr.analysis = 0,
                   use.grams.analysis = 1,
                   use.veg.analysis = 0,
                   yi = -68.7170,
                   vi = 1789.3185 )

d = dplyr::add_row(.data = d,
                   authoryear = "MacDonald 2016",
                   substudy = "eliminate",
                   desired.direction = 1,
                   effect.measure = "rmd",
                   interpretation = "Grams of meat (1 week)",
                   use.rr.analysis = 0,
                   use.grams.analysis = 1,
                   use.veg.analysis = 0,
                   yi = -84.8222,
                   vi = 1783.7241 )


##### **Reese 2015 #####

# reduce vs. not
# this one had a ton of effect sizes, so was prepped more automatically
original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review"
setwd(original.data.dir)
setwd("Reese 2015")

d = rbind( d, read.csv("reese_prepped_effect_sizes.csv")[,-1] )


##### **Kunst 2016 #####

d = dplyr::add_row(.data = d,
                   authoryear = "Kunst 2016",
                   substudy = "Study 2A",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high willingness to eat specific meat sample",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = 0.3735,
                   vi = 0.0170 )

d = dplyr::add_row(.data = d,
                   authoryear = "Kunst 2016",
                   substudy = "Study 2B",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high willingness to choose vegetarian dish",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = 0.3167,
                   vi = 0.0290 )

d = dplyr::add_row(.data = d,
                   authoryear = "Kunst 2016",
                   substudy = "Study 3",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high willingness to eat specific meat sample",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = 0.3866,
                   vi = 0.0148 )

d = dplyr::add_row(.data = d,
                   authoryear = "Kunst 2016",
                   substudy = "Study 5",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high willingness to choose vegetarian dish",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = 0.1361,
                   vi = 0.0184 )


##### **Kunst 2018 #####

d = dplyr::add_row(.data = d,
                   authoryear = "Kunst 2018",
                   desired.direction = 1,
                   effect.measure = "log-rr",
                   interpretation = "Low vs. high willingness to choose vegetarian dish",
                   use.rr.analysis = 1,
                   use.grams.analysis = 0,
                   use.veg.analysis = 0,
                   yi = 0.3122,
                   vi = 0.0083 )



##### Silva (2016) #####

# measured Fig 1.2 in pixels (obviously arbitrary since it depends on zooming,
#  but it's the relative bar measurements that matter)
# bar heights in pixels:
# high cuteness: 217
# low cuteness: 209
# control: 180
# distance from 1 to 5 on y-axis: 481 px

# start at 1, the lowest point on x-axis, and add the proportion of the 
#  x-axis occupied by the bar
mean.hi.cute = 1 + (217 / 481) * (5-1)
mean.lo.cute = 1 + (209 / 481) * (5-1)
mean.cntrl = 1 + (180 / 481) * (5-1)
sd.marg = 1.33  # conservatively use marginal SD for all groups since it's all we have

escalc_add_row( authoryear = "Silva 2016",
                substudy = "cute pig",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Willingness to avoid meat SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = mean.hi.cute, # Table 1
                sd1i = sd.marg, 
                n1i = 68,  # all n's on pg 22
                
                m2i = mean.cntrl,
                sd2i = sd.marg,
                n2i = 62 ) 

escalc_add_row( authoryear = "Silva 2016",
                substudy = "non-cute pig",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Willingness to avoid meat SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = mean.lo.cute, # Table 1
                sd1i = sd.marg, 
                n1i = 71,  # all n's on pg 22
                
                m2i = mean.cntrl,
                sd2i = sd.marg,
                n2i = 62 ) 


##### Spanikova (2015) #####

# n on pg. 15: 178 for restaurant task
n.group = floor(178/8)  # assume equal sample size in each group

# Table 5 (a weird, not-saturated logistic regression model)
expit = function(p) { exp(p) / (1 + exp(p))}
# P( choose vegetarian | control )
p.cntrl = expit( -1.12 + 0.95 )
# P( choose vegetarian | positive animal welfare intervention )
p.pos = expit( -1.12 + 0.61 )
# P( choose vegetarian | negative animal welfare intervention )
p.neg = expit( -1.12 + 0.61 - 0.33 )

escalc_add_row( authoryear = "Spanikova 2015",
                substudy = "positive framing",
                desired.direction = -1,
                effect.measure = "log-rr",
                interpretation = "Chose vegetarian vs. non-vegetarian restaurant",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = round(p.pos * n.group), # Tx choosing vegetarian restaurant
                bi = round( (1-p.pos) * n.group),  # Tx not choosing veg
                ci = round(p.cntrl * n.group),  # control choosing veg restaurant
                di = round( (1-p.cntrl) * n.group) )  # control not choosing veg

escalc_add_row( authoryear = "Spanikova 2015",
                substudy = "negative framing",
                desired.direction = -1,
                effect.measure = "log-rr",
                interpretation = "Chose vegetarian vs. non-vegetarian restaurant",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = round(p.neg * n.group), # Tx choosing vegetarian restaurant
                bi = round( (1-p.neg) * n.group),  # Tx not choosing veg
                ci = round(p.cntrl * n.group),  # control choosing veg restaurant
                di = round( (1-p.cntrl) * n.group) )  # control not choosing veg


##### Norris n.d. (Leafleting and Booklet Effectiveness) #####

# measure the purple bars in the saved figure (post vs. pre ORs in each group)
# start at 1, the lowest point on x-axis, and add the proportion of the 
#  x-axis occupied by the bar
OR.cntrl = (245/167) * 1.5
OR.compass = 1.5  # almost exactly on a gridline = 167 px
OR.species = (272 / 167) * 1.5

# CI half-width
OR.unit = 112  # px
hw.cntrl = (147/OR.unit)
hw.compass = (88/OR.unit)
hw.species = (178/OR.unit)

# sanity check: calculate lower CI limits - looks good
OR.cntrl - hw.cntrl
OR.compass - hw.compass
OR.species - hw.species

library(MetaUtility)
MetaUtility::scrape_meta(type = "RR",
                         est = )

# ~~ bm

# d = dplyr::add_row(.data = d,
#                    authoryear = "Palomo-Velez 2018",
#                    substudy = "Study 3",
#                    desired.direction = 1,
#                    interpretation = "Low vs. high meat buying likelihood",
#                    use.rr.analysis = 1,
#                    use.grams.analysis = 0,
#                    use.veg.analysis = 0,
#                    effect.measure = "log-rr",
#                    yi = 0.2492,
#                    vi = 0.0188 )



##### Norris 2016 #####

# this one gives the baseline probability (albeit combined for all treatment arms),
#  so can work with the ORs
p0 = .028

# odds ratios scraped from purple bars of plot
OR.unit = 118  # px
# booklet order as in plot (Simple, Even, Species, Your Choice)
OR = c(162/OR.unit, 181/OR.unit, 232/OR.unit, 134/OR.unit)
n0 = c(628, 634, 601, 592)
n1 = c(404, 386, 393, 356)

substudy = c('"A Simple Way to Help"', 
             '"Even If You Like Meat"',
             '"Speciesism"',
             '"Your Choice"')

# ~~ this analysis isn't ideal because we're treating baseline and F/U as independent instead of
#  paired, so SEs will be conservative assuming positive correlation between baseline and F/U
#  behavior
for (i in 1:4) {
  # calculate probability of avoidance at F/U from OR and p0
  # from Wolfram: solve (P/(1-P)) / (p/(1-p)) = \delta for P
  p1 = (OR[i] * p0) / ( (OR[i] - 1) * p0 + 1 )
  
  escalc_add_row( authoryear = "Norris 2016",
                  substudy = substudy[i],
                  desired.direction = 1,
                  effect.measure = "log-rr",
                  interpretation = "Eating any animal product less than vs. more than once weekly",
                  use.rr.analysis = 1,
                  use.grams.analysis = 0,
                  use.veg.analysis = 0,
                  measure = "RR",
                  
                  ai = round(p1 * n1[i]), # F/U avoiding animal products
                  bi = round( (1-p1) * n1[i]),  # F/U not avoiding animal products
                  ci = round(p0 * n0[i]),  # baseline avoiding animal products
                  di = round( (1-p0) * n0[i]) )  # baseline not intending
}

##### Byrd-Bredbenner 2010 #####

# they measured pre-post changes, but change scores aren't reported, so 
#  just using means at F/U
escalc_add_row( authoryear = "Byrd-Bredbenner 2010",
                substudy = NA,
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Intention to go vegetarian SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 1.94, # Table 1
                sd1i = 0.09 * sqrt(34),  # convert SE to SD
                n1i = 34,
                
                m2i = 1.66,
                sd2i = 0.13 * sqrt(37),
                n2i = 37 )


##### Moleman 2018 #####

# pre-intervention: 4% vegans
# post-intervention: 20% vegans
# hence the absurdly large point estimate

# because this is self-description as vegan (not based on consumption), not using in 
#  the vegetarian secondary analysis
escalc_add_row( authoryear = "Moleman 2018",
                substudy = NA,
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Self-describing as vegan after vs. before challenge",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = 697, # pre-intervention vegans
                bi = 4727+7502+2573,  # pre-intervention non-vegans
                ci = 799+1155+530+697,  # post- vegans
                di = 1239+2473+4689+217+1658+2043 ) # post-non-vegans


##### Novotna 2019 #####
# extract post-intervention means and CI limits 
#  in WebPlotDigitizer
mn.cntrl = 2.96
hw.cntrl = (mn.cntrl-2.77)
n.cntrl = 34-8  # pg 26
sd.cntrl = ( hw.cntrl / qt(.975, df = n.cntrl-1) ) * sqrt(n.cntrl)
# sanity check
#qt(.975, df = n.cntrl-1) * (sd.cntrl / sqrt(n.cntrl)); hw.cntrl

mn.doc = 2.71
hw.doc = (mn.doc-2.54)
n.doc = 33-3  # pg 26
sd.doc = ( hw.doc / qt(.975, df = n.doc-1) ) * sqrt(n.doc)

mn.babe = 2.88
hw.babe = (mn.babe-2.71)
n.babe = 32-1  # pg 26
sd.babe = ( hw.babe / qt(.975, df = n.babe-1) ) * sqrt(n.babe)


escalc_add_row( authoryear = "Novotna 2019",
                substudy = "Documentary",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Meat and dairy consumption SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = mn.doc, # Figure 12
                sd1i = sd.doc,  
                n1i = n.doc,
                
                m2i = mn.babe, 
                sd2i = sd.babe,
                n2i = n.babe )

escalc_add_row( authoryear = "Novotna 2019",
                substudy = '"Babe" movie',
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Meat and dairy consumption SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = mn.babe, # Figure 13
                sd1i = sd.babe,  
                n1i = n.babe,
                
                m2i = mn.cntrl,
                sd2i = sd.cntrl,
                n2i = n.cntrl)


##### Vegan Outreach 2019 (#3828; "10 Weeks to Vegan") #####

escalc_add_row( authoryear = "Vegan Outreach 2019",
                substudy = NA,
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Avoiding all animal products over 1 month after vs. before challenge",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "RR",
                
                ai = 43, # pre-intervention vegans
                bi = 190+76,  # pre-intervention non-vegans
                ci = 77,  # post- vegans
                di = 140+92+77 ) # post-non-vegans

escalc_add_row( authoryear = "Vegan Outreach 2019",
                substudy = NA,
                desired.direction = 1,
                effect.measure = "log-rr",
                interpretation = "Avoiding meat over 1 month after vs. before challenge",
                use.rr.analysis = 0,
                use.grams.analysis = 0,
                use.veg.analysis = 1,
                measure = "RR",
                
                ai = 43 + 76, # pre-intervention vegans + vegetarians
                bi = 190,  # pre-intervention meat-eaters
                ci = 77 + 92,  # post- vegans + vegetarians
                di = 140 ) # post- meat-eaters

##### Tian (2016) #####

# Study 1, Chinese (Table 1, abbatoir condition)
escalc_add_row( authoryear = "Tian 2016",
                substudy = "Study 1, Chinese",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Willingness to eat meat SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 3.28, 
                sd1i = 1.46,
                n1i = round(277/2),
                
                m2i = 3.68,
                sd2i = 1.28,
                n2i = round(277/2) )


# Study 1, French (Table 1, abbatoir condition)
escalc_add_row( authoryear = "Tian 2016",
                substudy = "Study 1, French",
                desired.direction = 1,
                effect.measure = "smd",
                interpretation = "Willingness to eat meat SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 3.55, 
                sd1i = 1.30,
                n1i = round(243/2),
                
                m2i = 3.78,
                sd2i = 1.37,
                n2i = round(243/2) )

# Study 2, Chinese (Table 3, abbatoir condition)
escalc_add_row( authoryear = "Tian 2016",
                substudy = "Study 2, Chinese",
                desired.direction = -1,
                effect.measure = "smd",
                interpretation = "Willingness to eat meat SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 3.61, 
                sd1i = 1.29,
                n1i = round(217/2),
                
                m2i = 3.53,
                sd2i = 1.36,
                n2i = round(217/2) )

# Study 2, French (Table 3, abbatoir condition)
escalc_add_row( authoryear = "Tian 2016",
                substudy = "Study 2, French",
                desired.direction = -1,
                effect.measure = "smd",
                interpretation = "Willingness to eat meat SMD",
                use.rr.analysis = 1,
                use.grams.analysis = 0,
                use.veg.analysis = 0,
                measure = "SMD",
                
                m1i = 3.84, 
                sd1i = 1.30,
                n1i = round(301/2),
                
                m2i = 3.82,
                sd2i = 1.24,
                n2i = round(301/2) )

setwd(data.dir)
write.csv(d, "data_prepped_stage1.csv", row.names = FALSE)
d = read.csv("data_prepped_stage1.csv")

############################### MERGE IN QUALITATIVE DATA ############################### 

# how many unique studies?
length(unique(d$authoryear))
table(d$authoryear)

# qualitative data entered into Excel
setwd(data.dir)
library(readxl)
d2 = read_xlsx("Extracted qualitative data.xlsx")

# for some reason, reads in years in an absurd format (e.g., "2018.0" as a string)
library(tidyverse)
d2$Year = str_remove(d2$Year, "[.]0")

##### unique merger variable
d$unique = NA
d$unique[ is.na(d$substudy) ] = as.character(d$authoryear[ is.na(d$substudy) ])
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

# look for IDs from effect sizes that aren't in the qualitative data spreadsheet (should be none)
d$unique[ !d$unique %in% d2$unique ]

# studies in qualitative data that aren't in entered effect sizes
# can occur if we're awaiting author help
d2$unique[ !d2$unique %in% d$unique ]

# merge them
d = merge( d,
           d2,
           all.x = TRUE,
           by.x = "unique",
           by.y = "unique" )

# minimal sanity check for absurd values
round( sort(d$yi), 2 )
sort(sqrt(d$vi))
round( sort(d$yi/sqrt(d$vi)), 2 )  # z-scores

# synchronize directions so that positive is always good
d$yi[ sign(d$yi) != sign(d$desired.direction) ] = -d$yi[ sign(d$yi) != sign(d$desired.direction) ]


# very low-quality studies to exclude in main analyses
# Moleman is an extreme outlier
d$exclude.main = 0
d$exclude.main[ d$authoryear %in% c("Moleman 2018", "Vegan Outreach 2019") ] = 1



############################### CONVERT EFFECT SIZES - to RRs ###############################

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

# don't use these for the non-main-RR analyses
d$logRR[ d$use.rr.analysis == 0 ] = NA
d$varlogRR[ d$use.rr.analysis == 0 ] = NA
d$RR.lo[ d$use.rr.analysis == 0 ] = NA
d$RR.hi[ d$use.rr.analysis == 0 ] = NA


############################### MAKE NEW VARIABLES AND RENAME THE EXISTING ONES ############################### 

d$published = !is.na(d$`Journal/conference (if peer-reviewed)`)

d = d %>%
  rename( 
    year = Year,
    title = Title,
    journal = `Journal/conference (if peer-reviewed)`,
    other.source = `Other source (if not peer-reviewed)`,
    borderline = `Borderline inclusion`,
    mm.fave = `Among MM's favorites methodologically, exclusive of small sample size`,
    perc.male = `Percent male`,
    design = Design,
    n.paper = `N (total analyzed sample size in paper, combining all substudies included here)`, 
    x.has.text = `Intervention has text`,
    x.suffer = `Intervention has specific description or images of animal suffering`,
    x.has.visuals = `Intervention has visuals`,
    x.pure.animals = `Intervention is purely animal welfare`,
    x.suffer = `Intervention has specific description or images of animal suffering`,
    x.pushy = `Intervention pushiness (reduce, go vegan, go vegetarian, no request, other)`,
    x.tailored = `Intervention personally tailored`,
    x.min.exposed = `Total time exposed to intervention (minutes)`,
    y.cat = `Outcome category (purchase or consumption)`,
    y.lag.days = `Outcome time lag from intervention (days)`,
    #qual.exch = `Exchangeability (good, medium, bad)`,
    qual.y.prox = `Outcome proximity (intended, self-reported, actual)`,
    qual.missing = `Missing data (%)`,
    #qual.sdb = `Differemtial SDB and demand characteristics (good, medium, bad)`,
    #qual.self.select = `Avoidance of self-selection (e.g., by subjects already interested in animal welfare)`,
    #qual.stats = `Statistics quality as relevant for our extracted data (good, medium, bad)`,
    qual.prereg = Preregistered,
    qual.public.data = `Public data`,
    qual.public.code = `Public code`
  )

# get rid of columns with capital letters (not used in analysis)
has.caps = tolower(names(d)) != names(d)
d = d[ , has.caps == FALSE | names(d) %in% c("logRR", "varlogRR", "RR.lo", "RR.hi") ]


############################### RECODE SOME VARS ############################### 

# for hyphenated ranges, take the mean
d$y.lag.days = unname(hyphen_mean(d$y.lag.days))

make.numeric = c("perc.male",
                 "x.min.exposed",
                 "qual.missing")

d = d %>% 
  mutate_at( make.numeric, as.numeric )

# temp: look for coding issues
analysis.vars = c("effect.measure",
                  "perc.male",
                  "design",
                  "published",
                  "x.has.text",
                  "x.has.visuals",
                  "x.pure.animals",
                  "x.suffer",
                  "x.tailored",
                  "x.min.exposed",
                  "y.cat",
                  "y.lag.days" )

quality.vars = grepl("qual", names(d))
CreateTableOne(data=d[,analysis.vars])
CreateTableOne(data=d[,quality.vars])


# variables for moderator analysis
d$y.lag.wks = d$y.lag.days/7
d$y.long.lag = d$y.lag.days >= 7
d$rct = grepl("RCT", d$design)
d$reproducible = (d$qual.prereg == "Yes") & (d$qual.public.data == "Yes")
d$long.intervention = d$x.min.exposed >= 5


############################### WRITE PREPPED DATA ############################### 

setwd(data.dir)
write.csv(d, "prepped_data.csv", row.names = FALSE)