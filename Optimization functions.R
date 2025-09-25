##optimization functions

### Optimization functions ####

pitch_logprob <- function(sigx,sigz, x, z, sz_top, sz_bot, called_strike){
  xprob <- pnorm(0.83-x, 0, sd = sigx) - pnorm(-0.83 - x, 0, sd = sigx)
  zprob <- pnorm(sz_top - z, 0, sd = sigz) - pnorm(sz_bot - z, 0, sd = sigz)
  log(called_strike*(xprob*zprob) + (1-called_strike)*(1-xprob*zprob))
}

pitch_logprob_BIAS_STANCE <- function(sigx,sigz, b1_r, b1_l, b2_r, b2_l, b3, b4,
                                      x, z, sz_top, sz_bot, called_strike, bh){
  xprob_l <- pnorm(0.83-x + b1_l, 0, sd = sigx) -
    pnorm(-0.83 - x + b2_l, 0, sd = sigx)
  xprob_r <- pnorm(0.83-x + b1_r, 0, sd = sigx) -
    pnorm(-0.83 - x + b2_r, 0, sd = sigx)
  xprob <- ifelse(bh=="R",xprob_r, xprob_l)
  zprob <- pnorm(sz_top - z + b3, 0, sd = sigz) -
    pnorm(sz_bot - z + b4, 0, sd = sigz)
  
  prob <- called_strike*(xprob*zprob) + (1-called_strike)*(1-xprob*zprob)
  ifelse(prob==0,-10000,log(prob))
}

pitch_logprob_BIAS_STANCE2 <- function(sigx,sigz, b1_r, b1_l, b2_r, b2_l, b3, b4,
                                       ds){
  x <- ds$plate_x
  z <- ds$plate_z
  sz_top <- ds$sz_top
  sz_bot <- ds$sz_bot
  called_strike <- ds$predicted
  bh <- ds$stand
  xprob_l <- pnorm(0.83-x + b1_l, 0, sd = sigx) -
    pnorm(-0.83 - x + b2_l, 0, sd = sigx)
  xprob_r <- pnorm(0.83-x + b1_r, 0, sd = sigx) -
    pnorm(-0.83 - x + b2_r, 0, sd = sigx)
  xprob <- ifelse(bh=="R",xprob_r, xprob_l)
  zprob <- pnorm(sz_top - z + b3, 0, sd = sigz) -
    pnorm(sz_bot - z + b4, 0, sd = sigz)
  
  prob <- called_strike*(xprob*zprob) + (1-called_strike)*(1-xprob*zprob)
  ifelse(prob==0,-10000,log(prob))
}

opt_function_BIAS_STANCE <- function(pars, use_prior=0, sig=100, ds=game_ex){
  loglik <- -1*sum(pitch_logprob_BIAS_STANCE(pars[1], pars[2], pars[3], pars[4], pars[5],
                                             pars[6], pars[7], pars[8], ds$plate_x, ds$plate_z,
                                             ds$sz_top, ds$sz_bot, ds$predicted, ds$stand))
  
  logprior <- sum(dnorm(pars[3:6],0,sig, log=TRUE))
  
  loglik - use_prior*logprior
}
