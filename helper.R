
################################ FOR ANALYSIS ################################

# analyze a subset or a moderator
# n.tests: for Bonferroni
analyze_one_meta = function( dat,
                             yi.name,
                             vi.name,
                             meta.name,
                             moderator = "",
                             mod.continuous = FALSE,
                             ql,
                             take.exp,
                             boot.reps = 2000,
                             n.tests=1,
                             digits = 2) {
  
  dat$yi = dat[[yi.name]]
  dat$vyi = dat[[vi.name]]
  
  ##### Regular Meta-Analysis (Possibly of Moderator) #####
  if ( moderator == "" ) {
    
    library(robumeta)
    ( meta = robu( logRR ~ 1, 
                       data = dat, 
                       studynum = as.factor(authoryear),
                       var.eff.size = varlogRR,
                       modelweights = "HIER",
                       small = TRUE) )
    
    est = meta$b.r
    t2 = meta$mod_info$tau.sq
    mu.lo = meta$reg_table$CI.L
    mu.hi = meta$reg_table$CI.U
    mu.se = meta$reg_table$SE
    mu.pval = meta$reg_table$prob
    
 
    # Phat from calibrated estimates
    Phat.l = lapply( ql,
                     FUN = function(q) {
                       
                       ens = my_ens( yi = dat$yi, 
                                     sei = sqrt(dat$vyi) )
                       
                       # set tail based on sign of q
                       if (q >= 0) tail = "above"
                       else tail = "below"
                       if ( tail == "above" ) Phat.NP.ens = sum(ens > c(q)) / length(ens)
                       if ( tail == "below" ) Phat.NP.ens = sum(ens < c(q)) / length(ens)
                       
                       library(boot)
                       Note = NA
                       tryCatch({
                         boot.res.ens = boot( data = dat, 
                                              parallel = "multicore",
                                              R = boot.reps, 
                                              statistic = function(original, indices) {
                                                
                                                b = original[indices,]
                                                
                                                ens.b = my_ens( yi = b$yi, 
                                                                sei = sqrt(b$vyi) )
                                                if ( tail == "above" ) return( sum(ens.b > c(q)) / length(ens.b) )
                                                if ( tail == "below" ) return( sum(ens.b < c(q)) / length(ens.b) )
                                              }
                                            )
                         
                         bootCIs.ens = boot.ci(boot.res.ens, type="bca")
                         boot.lo.ens = bootCIs.ens$bca[4]
                         boot.hi.ens = bootCIs.ens$bca[5]
                         
                       }, error = function(err){
                         boot.lo.ens <<- NA
                         boot.hi.ens <<- NA
                         Note <<- err$message
                       } )  # end tryCatch
                       
                       return( data.frame( Est = Phat.NP.ens,
                                           lo = boot.lo.ens,
                                           hi = boot.hi.ens,
                                           boot.note = Note ) )
                     } )  # end lapply
    

    Phat.df = do.call( rbind, 
                       Phat.l )
    Phat.df$string = paste( round( 100*Phat.df$Est,
                                         digits = 0 ),
                            format_CI( 100*Phat.df$lo, 
                                       100*Phat.df$hi,
                                       digits = 0 ),
                            sep = " " )
    
    levels = ""
    k = nrow(dat)
  }
  
  ##### Meta-Regression #####
  if (moderator != "") {
    library(robumeta)
    ( meta = robu( logRR ~ d[[moderator]], 
                   data = d, 
                   studynum = as.factor(authoryear),
                   var.eff.size = varlogRR,
                   modelweights = "HIER",
                   small = TRUE) )
    
    # for factor moderator, include each level
    if (! mod.continuous ) {
      levels = levels( as.factor(d[[moderator]]) )
      est = meta$b.r  # all of these are vectors
      t2 = meta$mod_info$tau.sq
      mu.lo = meta$reg_table$CI.L
      mu.hi = meta$reg_table$CI.U
      mu.se = meta$reg_table$SE
      mu.pval = meta$reg_table$prob
      k = as.numeric( table( as.factor(d[[moderator]]) ) ) # k in the relevant level
    } 
    
    # for continuous moderator, avoid the intercept
    else {
      levels = "1-unit increase"
      est = meta$b.r[2]
      t2 = meta$mod_info$tau.sq[2]
      mu.lo = meta$reg_table$CI.L[2]
      mu.hi = meta$reg_table$CI.U[2]
      mu.se = meta$reg_table$SE[2]
      mu.pval = meta$reg_table$prob[2]
      k = nrow(dat)
    }

  }

  
  ##### Put Results in Dataframe #####
  if (take.exp == TRUE) {
    est = exp(est)
    lo = exp(mu.lo)
    hi = exp(mu.hi)
  }
  
  est.string = paste( round( est, digits ),
                      format_CI( lo, 
                                 hi,
                                 digits),
                      sep = " " )
  
  tau.string = round( sqrt(t2), digits)
  
  
  new.row = data.frame( Meta = meta.name,
                        Moderator = moderator,
                        Level = levels,
                        k = k,
                        Est = est.string,
                        Pval = format_stat(mu.pval),
                        Pval.Bonf = format_stat( pmin(mu.pval*n.tests, 1) ),
                        Tau = tau.string
                        )
  
  # tail is now just for string purposes
  if ( moderator == "" ) {
    tail = rep("above", length(unlist(ql)))
    tail[unlist(ql) < 0] = "below"
    if (take.exp == TRUE) q.vec = exp(unlist(ql)) else q.vec = unlist(ql)
    Phat.names = paste( "Percent ", tail, " ", q.vec, sep = "" )
   # new.row[, Phat.names ] = NA
    
    new.row[ , Phat.names ] = Phat.df$string
  }

  
  # this should be a global variable
  if ( !exists("resE") ){
    resE <<- new.row
  } else {
    library(plyr)
    resE <<- rbind.fill(resE, new.row)
    detach("package:plyr", unload=TRUE)
  }
} 


################################ MISCELLANEOUS ################################

# arguments same as those of escalc, except for "unique"
# df called "d" assumed to be global var
escalc_add_row = function(authoryear,
                          substudy,
                          effect.measure,
                          desired.direction,
                          interpretation,
                          use.rr.analysis,
                          use.grams.analysis,
                          use.veg.analysis,
                          ...){
  
  es = as.data.frame( escalc(...) )
  d <<- dplyr::add_row(.data = d,
                       authoryear = authoryear, 
                       substudy = substudy,
                       desired.direction = desired.direction,
                       effect.measure = effect.measure, # NOT passed to escalc, but used for later conversions
                       interpretation = interpretation,
                       use.rr.analysis,
                       use.grams.analysis,
                       use.veg.analysis,
                       yi = es$yi,
                       vi = es$vi )
  return(d)
}

# gets the raw RR 
# for extracting many point estimates from the bigger articles
# assumes outcome is called "Y"
get_rr_unadj = function(condition,
                  condition.var.name = "condition",
                  control.name = "control",
                  dat) {

  # remove other interventions in case the study was more than 2 arms
  temp = droplevels( dat[ dat[[condition.var.name]] %in% c(condition, control.name), ] )
  
  tab = table( temp[[condition.var.name]], temp$Y )
  
  library(metafor)
  es = escalc( measure = "RR",
               ai = tab[condition, 2], # X=1, Y=1
               bi = tab[condition, 1],  # X=1, Y=0
               ci = tab[control.name, 2], # X=0, Y=1
               di = tab[control.name, 1] ) # X=0, Y=0
  
  return(es)
}


# gets the RR, controlling for baseline consumption
# assumes outcome is called "Y"

# note that the sandwich SEs will often be smaller than naive
# this makes sense since the outcome is common; see this from McNutt paper:
# "For studies of common outcomes, Poisson regression is likely to
# compute a confidence interval(s) that is conservative,
# suggesting less precision than is true. Poisson errors are overestimates of binomial errors
# when the outcome is common (Poisson errors approximately
# equal binomial errors when the outcome (disease) is rare)."
get_rr_adj = function( 
                       condition.var.name = "condition",
                       control.name = "control",
                       baseline.var.name,
                       .dat ) {
  
  # set levels so that contrast is treatment vs. control, not the other way around
  current.levels = levels( factor( .dat[[condition.var.name]] ) )
  .dat[[condition.var.name]] = factor( .dat[[condition.var.name]],
                                       levels = c( control.name,
                                                   current.levels[ !current.levels == control.name ] ) )
  
  # controlling for subject's own consumption in control condition
  mod = glm( Y ~ .dat[[condition.var.name]] + .dat[[baseline.var.name]], 
             data = .dat,
             family = "poisson" )
  yi = coef(mod)[2]
  
  library(sandwich)
  vi = diag( vcovHC(mod, type="HC0") )[2]
  
  return( data.frame(yi, vi) )
}



# for strings entered as "14-28", return the mean of the two values
hyphen_mean = Vectorize( function(string) {

  # if string doesn't contain hyphen, leave alone
  if ( grepl("-", string) == FALSE ) return(as.numeric(string))
  
  library(stringr)
  lo.num = as.numeric( str_split(string, "-")[[1]][1] )
  hi.num = as.numeric( str_split(string, "-")[[1]][2] )
  return( unname( (lo.num+hi.num)/2 ) )
  
}, vectorize.args = "string" )

fake = hyphen_mean(c("14-28", "28"))

################################ EFFECT-SIZE CONVERSIONS ################################
# Borenstein conversion
r_to_d_ptbis = function(r, N=NA) {
  
  d = (2 * r) / sqrt(1 - r^2)
  
  if( !is.na(N) ) {
    # variance of r (Borenstein pg. 41)
    Vr = ( 1 - r^2 )^2 / (N-1)
    Vd = 4 * Vr / ( 1 - r^2 )^3
    se = sqrt(Vd)
    lo = d - qnorm(.975) * se
    hi = d + qnorm(.975) * se
    
  } else {
    se = NA
    lo = NA
    hi = NA
  }
  return( list( d=d, se=se, lo=lo, hi=hi ) )
}
#, vectorize.args = c("r", "N") )

# variance of SMD
# e.g., https://stats.stackexchange.com/questions/144084/variance-of-cohens-d-statistic
smd_var = function(N = NULL,
                   n1 = NULL, 
                   n0 = NULL, 
                   smd) {
  if ( is.null(n1) | is.null(n0) ) {
    # assume equal sample sizes in each group
    n1 = N/2
    n0 = N/2
  }
  
  term1 = (n1 + n0) / (n1 * n0)
  term2 = smd^2 / ( 2 * (n1 + n0) )
  
  return( term1 + term2 )
}

# uses square-root transformation (assumes common outcome; otherwise conservative)
logOR_to_logRR = function( logOR,
                           varlogOR ) {
  logRR = log( sqrt( exp(logOR) ) )

  # delta method
  # let x = logOR
  # derivative of log( sqrt( exp(x) ) ) = 0.5
  varlogRR = 0.5^2 * varlogOR 
  return( list(logRR = logRR, varlogRR = varlogRR) )
}


# uses square-root transformation (assumes common outcome; otherwise conservative)
# and Chinn conversion
d_to_logRR = function( smd, smd.se ) {
  logOR = smd * (pi / sqrt(3))
  varlogOR = smd.se^2 * (pi^2 / 3)
  
  RR.stats = logOR_to_logRR( logOR = logOR,
                             varlogOR = varlogOR)
  return( list(logRR = RR.stats$logRR, varlogRR = RR.stats$varlogRR) )
}




################################ ENSEMBLE ESTIMATES ################################

# my calculation of ensemble estimates
# see Wang paper
my_ens = function(yi,
                  sei ) {
  
  meta = rma.uni( yi = yi, 
                  sei = sei, 
                  method = "DL" )
  
  muhat = meta$b
  t2 = meta$tau2
  
  # return ensemble estimates
  c(muhat) + ( c(t2) / ( c(t2) + sei^2 ) )^(1/2) * ( yi - c(muhat) )
}