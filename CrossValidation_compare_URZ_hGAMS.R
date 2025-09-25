### Hoberg Evaluation

library(caret)
library(mgcv)

##Make this general

CV_results <- data.frame(ump_name = Res23$ump_name, MSE_baseline_avg=0,
                         MSE_avg=0, MSE_se=0, MSE_hg_avg=0, MSE_hg_se=0,
                         hg_edf = 0)

for (j in c(1:8,10)){

  start <- Sys.time()
  ump_games <- SCGT$game_pk[SCGT$ump==CV_results$ump_name[j]]
  df <- df_called[df_called$game_pk %in% ump_games,]
  df$zs <- 2*((df$plate_z-df$sz_bot)/(df$sz_top-df$sz_bot)) + 1.5

  N <- nrow(df)

  folds <- createFolds(y=as.factor(df$game_pk),k = 5)

  MSE <- numeric(5)
  MSE_baseline <- numeric(5)
  MSE_hg <- numeric(5)

  for(i in 1:5){
    train1 <-df[-c(folds[[i]]),] ##For fold 1
    val1 <- df[folds[[i]],]

    pitches <- data.frame(x = train1$plate_x, z=train1$zs,
                             called_strike=train1$predicted,
                             batter_hand=train1$stand,
                             pitcher_hand=train1$p_throws)

    pitches_val1 <- data.frame(x = val1$plate_x, z=val1$zs,
                        called_strike=val1$predicted,
                        batter_hand=val1$stand,
                        pitcher_hand=val1$p_throws)

    subset_RHB_RHP <- subset(pitches, batter_hand == "R" & pitcher_hand == "R")
    subset_RHB_LHP <- subset(pitches, batter_hand == "R" & pitcher_hand == "L")
    subset_LHB_RHP <- subset(pitches, batter_hand == "L" & pitcher_hand == "R")
    subset_LHB_LHP <- subset(pitches, batter_hand == "L" & pitcher_hand == "L")
    # Fit GAM models for each group
    gam_RHB_RHP <- gam(called_strike ~ s(x, z), family = binomial, data = subset_RHB_RHP)
    gam_RHB_LHP <- gam(called_strike ~ s(x, z), family = binomial, data = subset_RHB_LHP)
    gam_LHB_RHP <- gam(called_strike ~ s(x, z), family = binomial, data = subset_LHB_RHP)
    gam_LHB_LHP <- gam(called_strike ~ s(x, z), family = binomial, data = subset_LHB_LHP)

    subset_RHB_RHP_val <- subset(pitches_val1, batter_hand == "R" & pitcher_hand == "R")
    subset_RHB_LHP_val <- subset(pitches_val1, batter_hand == "R" & pitcher_hand == "L")
    subset_LHB_RHP_val <- subset(pitches_val1, batter_hand == "L" & pitcher_hand == "R")
    subset_LHB_LHP_val <- subset(pitches_val1, batter_hand == "L" & pitcher_hand == "L")

    MSE_hg[i] <- (sum((subset_RHB_RHP_val$called_strike - predict(gam_RHB_RHP,
                  newdata = subset_RHB_RHP_val[,1:2],type="response"))^2) +
              sum((subset_RHB_LHP_val$called_strike - predict(gam_RHB_LHP,
                   newdata = subset_RHB_LHP_val[,1:2],type="response"))^2) +
              sum((subset_LHB_RHP_val$called_strike - predict(gam_LHB_RHP,
                   newdata = subset_LHB_RHP_val[,1:2],type="response"))^2) +
              sum((subset_LHB_LHP_val$called_strike - predict(gam_LHB_LHP,
                   newdata = subset_LHB_LHP_val[,1:2],type="response"))^2))/nrow(val1)

    DE_obj_PH<- DEoptim(opt_function_BIAS_STANCE,
                      lower = c(1e-5,1e-5,-.5,-.5,-.5,-.5, -.5, -.5),
                      upper = c(1,1,.5,.5,.5,.5, .5, .5),
                      control = DEoptim.control(
                        trace = FALSE  # or trace = 0
                      ),
                      use_prior=0,
                      sig=0.25,
                      ds=train1)

    pars <- DE_obj_PH$optim$bestmem
    lp<-pitch_logprob_BIAS_STANCE2(pars[1], pars[2], pars[3], pars[4], pars[5], pars[6],
                            pars[7], pars[8], val1)
    MSE[i] <- mean((1-exp(lp))^2)
    MSE_baseline[i] <- nrow(val1[val1$actual != val1$predicted,])/nrow(val1)
    print(c("fold ",i, " complete"))
  }

  CV_results$MSE_baseline_avg[j] <- mean(MSE_baseline)
  CV_results$MSE_avg[j] <- mean(MSE)
  CV_results$MSE_se[j] <- sd(MSE)/sqrt(5)
  CV_results$MSE_hg_avg[j] <- mean(MSE_hg)
  CV_results$MSE_hg_se[j] <- sd(MSE_hg)/sqrt(5)
  CV_results$hg_edf[j] <- summary(gam_RHB_RHP)$edf + summary(gam_RHB_LHP)$edf +
                       summary(gam_LHB_RHP)$edf + summary(gam_LHB_LHP)$edf

  print(Sys.time() - start)
}

write.csv(CV_results,"CV_results_ALL.csv")

temp_cv <- read.csv("CV_results.csv")

uset <- CV_results[CV_results$MSE_baseline_avg>0,]
uset$num_calls <- 0
for(i in 1:nrow(uset)){
  uset$num_calls[i] <- Res23$num_calls[Res23$ump_name==uset$ump_name[i]]
}


