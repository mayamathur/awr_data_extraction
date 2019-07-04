



# arguments same as those of escalc, except for "unique"
# df called "d" assumed to be global var
escalc_add_row = function(authoryear,
                          substudy,
                          effect.measure,
                          desired.direction,
                          ...){
  
  es = as.data.frame( escalc(...) )
  d <<- dplyr::add_row(.data = d,
                       authoryear = authoryear, 
                       substudy = substudy,
                       desired.direction = desired.direction,
                       effect.measure = effect.measure, # NOT passed to escalc, but used for later conversions
                       yi = es$yi,
                       vi = es$vi )
  return(d)
}


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