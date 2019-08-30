

# Impressions: I think all the top interventions don't make requests and are more informative or indirect

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

# number of studies
length( unique(d$authoryear) )

# make different datasets for different analyses
d.veg = d[ d$use.veg.analysis == 1,]
d.grams = d[ d$use.grams.analysis == 1,]
d = d[ d$use.rr.analysis == 1,]  # main dataset


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      MAIN ANALYSES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################# OVERALL META-ANALYSIS #################################

##### First Try Regular RE Meta-Analysis ######
# ignores potential correlation of point estimates within papers
# and assumes normality
library(metafor)

( meta = rma.uni( yi = d$logRR,
                  vi = d$varlogRR, 
                  method = "REML",
                  knha = TRUE ) )


##### Check Normality ######
# use only main estimates here
std = (d$logRR - c(meta$b)) / sqrt(c(meta$tau2) + d$varlogRR)
hist(std, breaks=20)
shapiro.test(std)
# reasonable


##### Robust Meta-Analysis ######
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



##### Proportion above RR = 1.1 ######
# parametric: 100%
library(MetaUtility)
# SOMETHING IS WRONG HERE; SHAPIRO PVAL DOESN'T AGREE
#  PROBABLY HAS TO 
MetaUtility::prop_stronger( q = log(1.28), 
               M = mu,
               t2 = t2,
               se.M = NA,
               se.t2 = NA,
               tail = "above",
               boot = "never",  # no inference because of clustering
               dat = ,
               yi.name = "logRR",
               vi.name = "varlogRR")  

# nonparametrically using quantiles of ensemble estimates: 59%
sum( d$ens[ d$es.group == "main"] > log(1.1) ) / length( d$ens[ d$es.group == "main"] )


##### Proportion above RR=1.25 ######
# parametric: 12%
library(MetaUtility)
prop_stronger( q = log(1.28), 
               M = mu,
               t2 = t2,
               se.M = NA,
               se.t2 = NA,
               tail = "above",
               dat = d,
               yi.name = "logRR",
               vi.name = "varlogRR",
               boot = "never",
               R = 500)  

# nonparametrically: 51%
sum( d$ens[ d$es.group == "main"] > log(1.25) ) / length( d$ens[ d$es.group == "main"] )




################################# CALIBRATED ENSEMBLE ESTIMATES #################################

# which individual interventions appeared most effective?
d$ens = my_ens( yi = d$logRR, sei = sqrt(d$varlogRR) )

plot( density(exp(d$ens)), main = "Ensemble estimates' KDE" )


# 5 studies with best ensemble estimates
best.ens = d$unique[ order(d$ens, decreasing = TRUE) ][1:5]

# ...vs. 5 with best point estimates
best.est = d$unique[ order(d$logRR, decreasing = TRUE) ][1:5]

# interesting...only 3/5 best point estimates are among 5 best ensemble estimates!
best.ens %in% best.est

# correlation between point estimate rank and ensemble rank
d$logRR.rank = rank(d$logRR)
d$ens.rank = rank(d$ens)
cor(d$logRR.rank, d$ens.rank)

################################# FOREST PLOT #################################

# ~~ fix the fact that error bars go over the edges of forest plot

# relative weight of each study in meta-analysis
d$rel.wt = NA
d$rel.wt[d$es.group == "main"] = 100 * (1/d$varlogRR[d$es.group == "main"]) / sum(1/d$varlogRR[d$es.group == "main"] )


# lean plotting df
# keepers = c("logRR",
#             "ens",
#             "varlogRR",
#             "RR.lo",
#             "RR.hi",
#             "authoryear",
#             "unique",
#             "rel.wt")
# dp = d[ , keepers ]
dp = d[ d$es.group == "main", ]

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

library(ggplot2)
base = ggplot( data = dp, aes( x = exp(logRR), 
                               y = unique,
                               size = rel.wt,
                               shape = is.pooled ) ) +
  #color = X.intensiveness ) ) +
  geom_errorbarh( aes(xmin = RR.lo,
                      xmax = RR.hi ),
                  lwd = .5,
                  height = .001,
                  color = "black") +
  
  geom_point() +
  
  geom_point( data = dp, aes( x = exp(ens),
                              y = unique
  ),
  size = 3,
  shape = 4,
  color = "red") +
  
  xlab( "Estimated relative risk of low vs. high meat" ) +
  ylab("") +
  
  geom_vline(xintercept = 1, lty = 2) +
  
  guides(size = guide_legend("% weight in analysis") ) +
  
  # scale_color_manual(values = colors,
  #                    name = "") +
  
  scale_shape_manual(values = shapes,
                     name = "") +
  #guide=FALSE) +
  
  scale_x_continuous( breaks = breaks,
                      lim = c(breaks[1], breaks[length(breaks)] ),
                      trans = "log10") +
  
  theme_bw()

#facet_wrap( ~Y.cat)  # to facet by outcome measure type

base


##### By Outcome Type #####


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
#                              EXCLUDE BORDERLINE STUDIES            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              MODERATORS AND STUDY QUALITY            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


################################# SUMMARIZE MODERATORS AND QUALITY VARIABLES #################################


analysis.vars = c("effect.measure",
                  "perc.male",
                  "design",
                  "published",
                  "x.has.text",
                  "x.has.visuals",
                  "x.pure.animals",
                  "x.tailored",
                  "x.min.exposed",
                  "y.cat",
                  "y.lag.days"
)

quality.vars = grepl("qual", names(d))

library(tableone)


CreateTableOne(data=d[,analysis.vars], includeNA = TRUE)
CreateTableOne(data=d[,quality.vars], includeNA = TRUE)

################################# TIME LAG #################################

ggplot( data = dp, aes( x = y.lag.days,
                        y = exp(ens) ) ) + 
  geom_point() + 
  geom_smooth() +
  xlab("Days elapsed between intervention and outcome") +
  ylab("True effect estimate (RR)") +
  geom_hline(yintercept = 1, lty = 2) +
  theme_bw()

# put exposure in terms of weeks
d$y.lag.wks = d$y.lag.days/7
( meta.time = robu( logRR ~ y.lag.wks >= 1, 
                    data = d, 
                    studynum = as.factor(authoryear),
                    var.eff.size = varlogRR,
                    modelweights = "HIER",
                    small = TRUE) )


# mu = meta.rob$b.r
# t2 = meta.rob$mod_info$tau.sq
# mu.lo = meta.rob$reg_table$CI.L
# mu.hi = meta.rob$reg_table$CI.U
# mu.se = meta.rob$reg_table$SE
# mu.pval = meta.rob$reg_table$prob


################################# TIME EXPOSED TO INTERVENTION #################################

ggplot( data = dp, aes( x = x.min.exposed,
                        y = exp(ens) ) ) + 
  geom_point() + 
  geom_smooth() +
  xlab("Days elapsed between intervention and outcome") +
  ylab("True effect estimate (RR)") +
  geom_hline(yintercept = 1, lty = 2)
theme_bw()


( meta.intens = robu( logRR ~ x.min.exposed, 
                      data = d, 
                      studynum = as.factor(authoryear),
                      var.eff.size = varlogRR,
                      modelweights = "HIER",
                      small = TRUE) )


# mu = meta.rob$b.r
# t2 = meta.rob$mod_info$tau.sq
# mu.lo = meta.rob$reg_table$CI.L
# mu.hi = meta.rob$reg_table$CI.U
# mu.se = meta.rob$reg_table$SE
# mu.pval = meta.rob$reg_table$prob

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


