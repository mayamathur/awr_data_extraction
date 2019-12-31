



############################### MERGE IN QUALITATIVE DATA ############################### 

# read it back in
setwd(data.dir)
d = read.csv("data_prepped_stage1.csv")

# how many unique studies?
length(unique(d$authoryear))
table(d$authoryear)

# how many point estimates?
nrow(d)

# qualitative data entered into Excel
setwd(data.dir)
library(readxl)
d2 = read_xlsx("Extracted qualitative data.xlsx")
# remove missing rows
d2 = d2 %>% filter(!is.na(`First author last name`))

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

# make pretty for eventual forest plot
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
d$yi[ d$desired.direction == 1 ] = abs(d$yi[ d$desired.direction == 1 ])
d$yi[ d$desired.direction == 0 ] = -abs(d$yi[ d$desired.direction == 0 ])



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

# ##### ORs #####
# RR.stats = logOR_to_logRR( logOR = d$yi[ d$effect.measure == "log-or" ],
#                            varlogOR = d$vi[ d$effect.measure == "log-or" ] )
# d$logRR[ d$effect.measure == "log-or" ] = RR.stats$logRR
# d$varlogRR[ d$effect.measure == "log-or" ] = RR.stats$varlogRR

##### Calculate Approximate CI limits for Forest Plot #####
z.crit = qnorm(.975)
d$RR.lo = exp( d$logRR - z.crit * sqrt(d$varlogRR) )
d$RR.hi = exp( d$logRR + z.crit * sqrt(d$varlogRR) )

# sanity check
# fine for RMDs to be NA since those are for the grams analysis
table( is.na(d$logRR), d$effect.measure)

# don't use these for the non-main-RR analyses
d$logRR[ d$use.rr.analysis == 0 ] = NA  # e.g., could have log-RRs for going vegetarian vs. not, but those aren't for main analysis
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
    exclude.main = `Excluded challenge`,
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

library(tableone)
CreateTableOne(data=d[,analysis.vars])
CreateTableOne(data=d[,quality.vars])


# variables for moderator analysis
d$y.lag.wks = d$y.lag.days/7
d$y.long.lag = d$y.lag.days >= 7
d$rct = grepl("RCT", d$design)
d$reproducible = (d$qual.prereg == "Yes") & (d$qual.public.data == "Yes")
d$x.long = d$x.min.exposed >= 5

# recode missing data
d[ d == "NR" ] = NA

############################### WRITE PREPPED DATA ############################### 

setwd(data.dir)
write.csv(d, "prepped_data.csv", row.names = FALSE)