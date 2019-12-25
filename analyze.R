

# Impressions: I think all the top interventions don't make requests and are more informative or indirect

# to do: update analyze_one_meta to use the new Phat function :)

################################# PREP ################################# 

data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Data extraction"
code.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Data extraction/awr_data_extraction_git"
setwd(code.dir); source("helper.R")

# prepped dataset
setwd(data.dir)
d = read.csv("prepped_data.csv")

# should be none
d$unique[ is.na(d$logRR) & d$use.rr.analysis == 1]

# number of point estimates by analysis
table(d$use.rr.analysis)

# sanity check
table(!is.na(d$logRR), d$use.rr.analysis)

# make different datasets for different analyses
d.veg = d[ d$use.veg.analysis == 1,]
d.grams = d[ d$use.grams.analysis == 1,]
d = d[ d$use.rr.analysis == 1 & d$exclude.main == 0,]  # main dataset

# ~~ move this?
d$perc.male = as.numeric(d$perc.male)


################################# BASICS ################################# 

# number of studies in primary analysis
( n.studies = length(unique(d$authoryear)) )
# number of point estimates
( n.ests = nrow(d) )
# total number of unique subjects
n.paper = d$n.paper[ !duplicated(d$authoryear) ]
( n.total = sum(n.paper) )

# median n per paper
n.median = median(n.paper)

# ICC of point estimates clustered in papers
library(ICC)
ICCest(authoryear, logRR, d)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      MAIN ANALYSES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################# OVERALL META-ANALYSIS #################################

##### Robust Meta-Analysis ######
# also reproduced in the subset analyses, but need to run here for 
#  forest plotting joy 
# allows for correlated point estimates within studies
# and has no distributional assumptions
library(robumeta)
( meta.rob = robu( logRR ~ 1, 
                   data = d, 
                   studynum = as.factor(authoryear),
                   var.eff.size = varlogRR,
                   modelweights = "HIER",
                   small = TRUE) )


mu = meta.rob$b.r
t2 = meta.rob$mod_info$tau.sq
mu.lo = meta.rob$reg_table$CI.L
mu.hi = meta.rob$reg_table$CI.U
mu.se = meta.rob$reg_table$SE
mu.pval = meta.rob$reg_table$prob

exp(mu)
exp(mu.lo)
exp(mu.hi)


##### Check Normality ######
# use only main estimates here
std = (d$logRR - c(mu)) / sqrt(c(t2) + d$varlogRR)
hist(std, breaks=20)
shapiro.test(std)
# reasonable



################################# CALIBRATED ENSEMBLE ESTIMATES #################################

d$ens = my_ens(yi = d$logRR, 
               sei = sqrt(d$varlogRR))

ggplot( data = d,
            aes(x = exp(d$ens))) +
  
  geom_density() +
  
  geom_vline(xintercept = 1,
             color = "gray") +
  
  # pooled point estimate
  geom_vline(xintercept = exp(mu),
             color = "red",
             lty = 2) +
  
  xlab("Estimated relative risk of low vs. high meat") +
  ylab("Kernel density estimate") +
  
  scale_x_continuous( limits = c(0.8, 1.8),
                      breaks = seq(0.8, 1.8, .2) ) +
  theme_classic()



# 5 studies with best ensemble estimates
( best.ens = d$unique[ order(d$ens, decreasing = TRUE) ][1:8] )

# ...vs. 5 with best point estimates
( best.est = d$unique[ order(d$logRR, decreasing = TRUE) ][1:8] )

# interesting...only 6/8 best point estimates are among 5 best ensemble estimates!
sum(best.ens %in% best.est)


################################# NPPHAT :D #################################

library(MetaUtility)

# q.vec = c( log(1), log(1.1), log(1.2), log(1.5) )
# 
# prop_stronger( q = log(1.5),
#                tail = "above",
#                estimate.method = "calibrated",
#                ci.method = "calibrated",
#                dat = d,
#                yi.name = "yi",
#                vi.name = "vi" )



##### Make Plotting Dataframe #####
q.vec = seq( 0, log(2), 0.01 )
ql = as.list(q.vec)



# again pass threshold and calibrated estimates on log-RR scale
Phat.above.vec = lapply( ql,
                     FUN = function(.q) mean( d$ens > .q, na.rm = TRUE ) )

res = data.frame( q = q.vec,
                  Est = unlist(Phat.above.vec) )


##### Selective Bootstrapping #####

# look at just the values of q at which Phat jumps
#  this will not exceed the number of point estimates in the meta-analysis
res.short = res[ diff(res$Est) != 0, ]

library(dplyr)

# bootstrap a CI for each entry in res.short
temp = res.short %>% rowwise() %>%
  do( prop_stronger( q = .$q, 
                     tail = "above",
                     estimate.method = "calibrated",
                     ci.method = "calibrated",
                     dat = d,
                     R = 2000 ) )

# merge this with the full-length res dataframe, merging by Phat itself
res = merge( res, temp, by.x = "Est", by.y = "Est")



##### Make Plot #####
library(ggplot2)

ggplot( data = res,
        aes( x = exp(q),
             y = Est ) ) +
  theme_bw() +
  
  # # proprtion "r" line
  # geom_hline( yintercept = r, 
  #             lty = 2,
  #             color = "red" ) +
  # 
  # That line
  geom_vline( xintercept = exp(mu),
              lty = 2,
              color = "black" ) +
  
  scale_x_continuous( limits=c(1, 1.5), breaks=seq(1, 1.5, .1) ) +
  #scale_y_continuous(  breaks = seq(1, log(), .1) ) +
  geom_line(lwd=1.2) +
  
  xlab("Threshold (RR scale)") +
  ylab( paste( "Estimated proportion of studies with true RR above threshold" ) ) +
  
  geom_ribbon( aes(ymin=res$lo, ymax=res$hi), alpha=0.15, fill = "black" ) 

# # parametric CI
# geom_ribbon( aes(ymin=res$CI.lo.param, ymax=res$CI.hi.param), alpha=0.15, fill = "blue" )   

# 8 x 6 works well

# ~~~ look into jaggedyness


################################# FOREST PLOT #################################

# ~~ fix the fact that error bars go over the edges of forest plot

# relative weight of each study in meta-analysis
d$rel.wt = NA
d$rel.wt = 100 * (1/d$varlogRR) / sum(1/d$varlogRR )


# lean plotting df
dp = d

# sort by ensemble estimate
dp = dp[ order(dp$ens, decreasing = FALSE), ]
#dp = dp[ order(dp$logRR, decreasing = FALSE), ]


# add pooled point estimates as first rows
# arbitrary relative weight
library(dplyr)
dp = add_row( dp,
              .before = 1,
              logRR = meta.rob$b.r,
              ens = NA,
              varlogRR = meta.rob$reg_table$SE^2,
              RR.lo = exp(mu.lo),
              RR.hi = exp(mu.hi),
              #X.cat = "pooled",
              #Y.cat = "pooled",
              unique = "POOLED",
              borderline = 0,
              rel.wt = 5 )


# make sure POOLED is the first level of the factor variable so it's displayed at bottom
correct.order = dp$unique
dp$unique = factor(dp$unique, levels = correct.order)
levels(dp$unique)


dp$is.pooled = as.factor( c( "Pooled estimate", rep("Individual study", nrow(dp) - 1) ) )


#colors = c("red", "black", "blue")

shapes = c(10, 19, 15)
#breaks = seq(0.4, 6.0, .2)

breaks = c( seq(0.4, 1.3, .1),
            1.5,
            seq(1.5, 4, .5),
            seq(4, 6, 1) )

#breaks = exp( seq( log(0.4), log(6), .2 )

# for borderline status
colors = c("black", "orange")

library(ggplot2)
base = ggplot( data = dp, aes( x = exp(logRR), 
                               y = unique,
                               size = rel.wt,
                               shape = is.pooled,
                               color = as.factor(borderline) ) ) +
  geom_point() +
  #color = X.intensiveness ) ) +
  geom_errorbarh( aes(xmin = RR.lo,
                      xmax = RR.hi ),
                  lwd = .5,
                  height = .001) +
  
  # calibrated estimates
  geom_point( data = dp, aes( x = exp(ens),
                              y = unique ),
  size = 3,
  shape = 4,
  color = "red") +
  
  xlab( "Estimated relative risk of low vs. high meat" ) +
  ylab("") +
  
  geom_vline(xintercept = 1, lty = 2) +
  
  guides(size = guide_legend("% weight in analysis") ) +
  
  scale_color_manual(values = colors,
                     name = "Borderline inclusion") +
  
  scale_shape_manual(values = shapes,
                     name = "") +
  #guide=FALSE) +
  
  # scale_x_continuous( breaks = breaks,
  #                     lim = c(breaks[1], breaks[length(breaks)] ),
  #                     trans = "log10") +
  
  scale_x_continuous( breaks = breaks, trans = "log10") +
  # https://michaelbach.de/2012/07/22/R-ggplot2-and-axis-limits-unexpected-behaviour-solution.html
  coord_cartesian( xlim = c(breaks[1], breaks[length(breaks)] ) ) +
  
  theme_bw()

#facet_wrap( ~Y.cat)  # to facet by outcome measure type

base

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                SECONDARY EFFECT SIZE CODINGS          
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

my_robu( dat = d.veg,
         # yi.name = "yi",
         # vi.name = "vi",
         take.exp = TRUE )


my_robu( dat = d.grams,
         take.exp = FALSE )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              MODERATORS AND STUDY QUALITY            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


################################# STUDY CHARACTERISTICS AND QUALITY TABLE #################################


analysis.vars = c("effect.measure",
                  "perc.male",
                  "design",
                  "published",
                  "x.has.text",
                  "x.has.visuals",
                  "x.suffer",
                  "x.pure.animals",
                  "x.tailored",
                  "x.min.exposed",
                  "y.cat",
                  "y.lag.days"
                  )

quality.vars = grepl("qual", names(d))

library(tableone)

# ** will become Tables 1-2 for paper
CreateTableOne(data=d[,analysis.vars], includeNA = TRUE)
CreateTableOne(data=d[,quality.vars], includeNA = TRUE)

# how many meet bar of high quality?
d = d %>% mutate( hi.qual = grepl("RCT", design) == TRUE & 
                    qual.y.prox %in% c("Self-reported", "Actual behavior") )
table(d$hi.qual)
d$authoryear[ d$hi.qual == TRUE ]



################################# RUN ALL MODERATOR AND SUBSET ANALYSES #################################

if( exists("resE") ) rm(resE)

# for Bonferroni
n.tests = 10

# for Phat
ql = c(log(1), log(1.1), log(1.2))
boot.reps = 500  # ~~ increase later

digits = 2


# ~~ move this?
# recode percent male as a 10-percentage point increase
d$perc.male.10 = d$perc.male/10
library(car)
d$x.pushy = recode_factor( d$x.pushy,
                    "No request" = "a.No request",
                    "Reduce" = "b.Reduce",
                    "Go vegetarian" = "c.Go vegetarian",
                    "Go vegan" = "d.Go vegan",
                    "Mixed" = "e.Mixed")

##### Overall and Subset Analyses #####
subsets = list( d,
                d[d$borderline==0,],
                d[d$hi.qual == 1,],
                d[ grepl("RCT", d$design) == 1, ],
                d[d$reproducible ==1,] )

subset.labels = c( "Overall",
                   "Non-borderline",
                   "High quality",
                   "Randomized",
                   "Preregistered with open data")

for (i in 1:length(subsets)) {
  analyze_one_meta( dat = subsets[[i]],
                    meta.name = subset.labels[i],
                    yi.name = "logRR",
                    vi.name = "varlogRR",
                    digits = digits,
                    boot.reps = boot.reps, 
                    ql = ql,
                    take.exp = TRUE,
                    n.tests = n.tests)
}


##### Moderators #####
if( exists("resE") ) rm(resE)
moderators = c("published",
               "x.has.text",
               "x.has.visuals",
               "x.suffer",
               "x.pushy",
               "y.long.lag",
               "long.intervention",
               "perc.male.10")

moderator.labels = c("Published",
                     "Intervention has text",
                     "Intervention has visuals",
                     "Intervention depicts suffering",
                     "Intervention pushiness",
                     "Outcome measured >1 week after intervention",
                     "Intervention took at least 5 min",
                     "10 percentage pt. increase in males")

mod.continuous = c( rep(0,7), 1 )

# bm
for (i in 1:length(moderators)) {
  analyze_one_meta( dat = d,
                    meta.name = moderator.labels[i],
                    yi.name = "logRR",
                    vi.name = "varlogRR",
                    digits = digits,
                    moderator = moderators[i],
                    mod.continuous = mod.continuous[i],
                    boot.reps = boot.reps, 
                    ql = ql,
                    take.exp = TRUE,
                    n.tests = n.tests)
}


# remove the baseline levels for binary moderators
View(resE %>% filter(Level != 0 & Level != FALSE))
# note that the moderators' estimates and p-values are vs. reference

# bm


################################# CONTINUOUS MODERATOR PLOTS #################################

##### time lag from intervention to outcome measurement #####
ggplot( data = dp, aes( x = y.lag.days,
                        y = exp(ens),
                        color = authoryear,
                        shape = authoryear) ) + 
  geom_point(size = 4,
             alpha = 1) + 
  geom_smooth() +
  xlab("Days elapsed between intervention and outcome") +
  ylab("True effect estimate (RR)") +
  geom_hline(yintercept = 1, lty = 2) +
  geom_vline(xintercept = 7, lty = 2, color = "red") +
  scale_x_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
  scale_shape_manual(values = 1:50) +
  #scale_colour_brewer(palette = "Set1") +
  theme_bw()


##### time exposed to intervention
ggplot( data = dp, aes( x = x.min.exposed,
                        y = exp(ens),
                        color = authoryear,
                        shape = authoryear) ) + 
  geom_point(size = 4) + 
  geom_smooth() +
  xlab("Total time exposed to intervention (min)") +
  ylab("True effect estimate (RR)") +
  geom_hline(yintercept = 1, lty = 2) +
  scale_shape_manual(values = 1:50) +
  scale_x_continuous(limits = c(0,110),
                     breaks = seq(0,95,5)) +
  theme_bw()


( meta.intens = robu( logRR ~ x.min.exposed, 
                      data = d, 
                      studynum = as.factor(authoryear),
                      var.eff.size = varlogRR,
                      modelweights = "HIER",
                      small = TRUE) )

##### percent male subjects


# mu = meta.rob$b.r
# t2 = meta.rob$mod_info$tau.sq
# mu.lo = meta.rob$reg_table$CI.L
# mu.hi = meta.rob$reg_table$CI.U
# mu.se = meta.rob$reg_table$SE
# mu.pval = meta.rob$reg_table$prob


################################# OTHER #################################

( meta.push = robu( logRR ~ x.pushy, 
                      data = d, 
                      studynum = as.factor(authoryear),
                      var.eff.size = varlogRR,
                      modelweights = "HIER",
                      small = TRUE) )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              OTHER META-ANALYSIS MEASURES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



################################# PUBLICATION BIAS ################################# 

##### Significance Funnel ######
library(PublicationBias)
significance_funnel(yi = d$logRR,
                    vi = d$varlogRR)

# affirmative vs. non-affirmative
d$pval = 2 * ( 1 - pnorm( abs(d$logRR) / sqrt(d$varlogRR) ) )
d$affirm = d$pval < 0.05 & d$logRR > 0
table(d$affirm)

# meta-analyze only the nonaffirmatives
# 2-sided pval
( meta.worst = robu( yi ~ 1, 
                     data = d[ d$affirm == FALSE, ], 
                     studynum = authoryear,
                     var.eff.size = varlogRR,
                     modelweights = "HIER",
                     small = TRUE) )
# mu = meta.rob$b.r
# t2 = meta.rob$mod_info$tau.sq
# mu.lo = meta.rob$reg_table$CI.L
# mu.hi = meta.rob$reg_table$CI.U
# mu.se = meta.rob$reg_table$SE

# s-values to reduce to null
res = svalue( yi = d$logRR,
              vi = d$varlogRR,
              q = log(1), 
              clustervar = d$authoryear,
              model = "robust" )

res$svals


# s-values to reduce effect size to RR=1.1
res = svalue( yi = d$logRR,
              vi = d$varlogRR,
              q = log(1.1), 
              clustervar = d$authoryear,
              model = "robust" )

res$svals



##### Selection Model #####
# be careful about inference
# ~~ check ICC within studies

library(weightr)

# this line breaks for "data_25360660.csv"
( m1 = weightfunct( effect = d$logRR,
                    v = d$varlogRR,
                    steps = c(0.025, 1),
                    table = TRUE
) )
# actually makes the estimate larger! 



# ~~~~ below not modified from Bianchi work


