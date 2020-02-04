

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
  
  # state sample size
  print( paste( "Analyzed N:", nrow(temp) ) )
  
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
  
  # number analyzed
  # null model only has an intercept
  print( paste("N analyzed:", mod$df.null + 1) )
  
  # percent missing
  prop.missing = 1 - ( (mod$df.null + 1) / nrow(.dat) )
  print( paste("Percent missing:", round( 100 * prop.missing, 2) ) )
  
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

# agrees with http://www.campbellcollaboration.org/escalc/html/EffectSizeCalculator-SMD11.php
r_to_d_ptbis = function(r, n0 = NA, n1 = NA) {
  
  # Jacobs & Viechtbauer, Eq (5)
  # note the h term is slightly different from Borenstein
  # because latter assumes equal n in each group
  h = ( (n0 + n1) / n0 ) + ( (n0 + n1) / n1 )
  d = ( sqrt(h) * r ) / ( sqrt(1 - r^2) ) 
  
  
  if( !is.na(n0) & !is.na(n1) ) {
    N = n0 + n1
    # variance of r (Borenstein pg. 41 or Jacobs & Viechtbauer, Eq. (10))
    # Campbell Collaboration also uses this (http://www.campbellcollaboration.org/escalc/html/EffectSizeCalculator-SMD11.php)
    Vr = ( 1 - r^2 )^2 / (N-1)
    
    # as in Borenstein
    # this is from the delta method: https://www.wolframalpha.com/input/?i=derivative+of+2r+%2F+sqrt%281-r%5E2%29+wrt+r
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


# # uses square-root transformation (assumes common outcome; otherwise conservative)
# logOR_to_logRR = function( logOR,
#                            varlogOR ) {
#   logRR = log( sqrt( exp(logOR) ) )
# 
#   # delta method
#   # let x = logOR
#   # derivative of log( sqrt( exp(x) ) ) = 0.5
#   varlogRR = 0.5^2 * varlogOR 
#   return( list(logRR = logRR, varlogRR = varlogRR) )
# }


# given OR and denominator probability, solve for numerator probability
# Wolfram Alpha: solve r = P/(1-P) / (p/(1-p)) for P
OR_to_p1 = Vectorize( function(OR, p0) {
  return( (p0 * OR) / ( p0 * (OR-1) + 1 ) )
}, vectorize.args = c("OR", "p0") )


# uses square-root transformation (assumes common outcome; otherwise conservative)
# and Chinn conversion
# gets variance by delta method
d_to_logRR = function( smd, smd.se ) {
  
  # simplified the math
  # Chinn conversion to log-OR followed by TVW's square-root transformation
  #  to RR so that we can imagine dichotomizing near median
  logRR = log( sqrt( exp( smd * pi / sqrt(3) ) ) )
  varlogRR = ( pi^2 / 12 ) * smd.se^2
  
  return( list(logRR = logRR, varlogRR = varlogRR) )
}

# d_to_logRR( smd = .2, smd.se = .3 )






