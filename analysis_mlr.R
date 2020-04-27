library(glmnet)
library(car)
library(janitor)
library(mgcv)

year1920 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201920.csv', header = TRUE)
year1819 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201819.csv', header = TRUE)
year1718 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201718.csv', header = TRUE)
year1617 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201617.csv', header = TRUE)
year1516 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201516.csv', header = TRUE)
year1415 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201415.csv', header = TRUE)
year1314 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201314.csv', header = TRUE)
year1213 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201213.csv', header = TRUE)

year_tot <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-tot.csv', header = TRUE)

### Headers
# Rk/Player/Link/Pos/Age/Tm/G/GS/MP/FG/FGA/FG.
# X3P/X3PA/X3P./X2P/X2PA/X2P./eFG./FT/FTA/FT.
# 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS'
headers = c('Rk','Player', 'Link', 'Pos', 'Age', 'Tm', 'G', 'GS', 'MP', 'FG', 'FGA', 'FG%', '3P', '3PA', '3P%', '2P', '2PA', '2P%', 'eFG%', 'FT', 'FTA', 'FT%', 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS')
mheaders = c('Age', 'G', 'GS', 'MP', 'FG', 'FGA', 'FG.', 'X3P', 'X3PA', 'X3P.', 'X2P', 'X2PA', 'X2P.', 'eFG.', 'FT', 'FTA', 'FT.', 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS')
mheaders_post = c('Pos', 'Age', 'G', 'GS', 'MP', 'FG', 'FGA', 'FG.', 'X3P', 'X3PA', 'X3P.', 'X2P', 'X2PA', 'X2P.', 'eFG.', 'FT', 'FTA', 'FT.', 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS')
years_dat = list(year1920, year1819, year1718, year1617, year1516, year1415, year1314, year1213)

### Analysis
c_stat_mlr <- function(year, ydata) {
  ### Plots the data for all statistics for a specific year
  # ydata: Entire dataset for specific year
  # logsal: Log 10 salary data
  # year: Specific year (string)

  Pos = ydata$Pos
  Age = ydata$Age
  G = ydata$G
  GS = ydata$GS
  MP = ydata$MP
  FG = ydata$FG
  FGA = ydata$FGA
  FG. = ydata$FG.
  X3P = ydata$X3P
  X3PA = ydata$X3PA
  X3P. = ydata$X3P.
  X2P = ydata$X2P
  X2PA = ydata$X2PA
  X2P. = ydata$X2P.
  eFG. = ydata$eFG.
  FT = ydata$FT
  FTA = ydata$FTA
  FT. = ydata$FT.
  ORB = ydata$ORB
  DRB = ydata$DRB
  TRB = ydata$TRB
  AST = ydata$AST
  STL = ydata$STL
  BLK = ydata$BLK

  #xdata = cbind(Pos, Age, G, GS, MP, FG, FGA, FG., X3P, X3PA, X3P., X2P, X2PA, X2P., eFG., FT, FTA, FT., ORB, DRB, TRB, AST, STL, BLK)
  dat = model.matrix( ~ Pos + Age + G + GS + MP + FG + FGA + FG. + X3P + X3PA + X3P. + X2P + X2PA + X2P. + eFG. + FT + FTA + FT. + ORB + DRB + TRB + AST + STL + BLK + Salary - 1, data = ydata)
  xdata = dat[,1:28]
  ydat = dat[,29]
  # print(xdata)
  # print(ydata)
  # print(dim(xdata))
  # print(length(ydata))
  logsal <- log10(ydat)
  # print(typeof(logsal))
  statyear <- lm(logsal ~ xdata)
  
  #sink("lm.txt")
  print(summary(statyear))
  #sink()  # returns output to the console
  
  #png("diagplot.png")
  par(mfcol = c(2,2))
  plot(statyear, main = paste0(year,": All players"))
  #while (!is.null(dev.list()))  dev.off()
  
  r2 <- summary(statyear)$adj.r.squared
  
  #----------------------------------------------------------------------------
  # Ridge Regression
  #----------------------------------------------------------------------------
  
  # Response:
  y = statyear$model[,1]
  
  # Predictors 
  X = as.matrix(statyear$model[,-1])
  
  # Now ridge regression:
  statyear1 = glmnet(X,  # Matrix of predictors (w/o intercept)
                y,  # Response
                alpha=0, # Corresponds to the penalty (0 for ridge, 1 for lasso)  
                lambda = seq(100, 0, by = -0.01) # lambda sequence
  )

  # Plot the estimated coefficients for each (log) lamda
  
  #png("lambda_r.png")
  par(mfcol = c(1,1))
  plot(statyear1, xvar = "lambda", lwd=2, main = paste0(year,": All players"))
  # Reference line at zero:
  abline(h = 0, lwd = 3, lty=2)
  # Add OLS fits:
  lines(rep(log(.01), ncol(X)), statyear$coefficients[-1], type='p', pch=4, cex = 2, lwd=3)
  #while (!is.null(dev.list()))  dev.off()
  
  #png("L1 Norm_r.png")
  par(mfcol = c(1,1))
  plot(statyear1, main = paste0(year,": All players"))
  #while (!is.null(dev.list()))  dev.off()
  
  # Now find an "optimal" lambda using cross-validation: 
  fit.cv = cv.glmnet(X,  # Matrix of predictors (w/o intercept)
                     y,  # Response
                     alpha=0, # Corresponds to the penalty (0 for ridge, 1 for lasso)  
                     lambda = seq(100, 0, by = -0.01) # lambda sequence
  )
  #png("loglambda_r.png")
  par(mfcol = c(1,1))
  plot(fit.cv, main = paste0(year,": All players"))
  #while (!is.null(dev.list()))  dev.off()
  
  # Pick a reasonable value for lambda:
  #lambda = fit.cv$lambda.min # Minimizes CV error^2
  lambda = fit.cv$lambda.1se # largest lambda w/in 1 SE of lambda.min
  
  # Fit the model at the selected lambda:
  fit2 = glmnet(X,  # Matrix of predictors (w/o intercept)
                y,  # Response
                alpha=0, # Corresponds to the penalty (0 for ridge, 1 for lasso)  
                lambda = lambda)
  
  # Estimated coefficients 
  beta_hat_ridge = c(fit2$a0, # Intercept
                     as.numeric(fit2$beta))
  
  # Compare to OLS:
  #sink("ols_r.txt")
  print(cbind(beta_hat_ridge, coef(statyear)))
  #sink()  # returns output to the console

  # Re-do plot from before, but now with ridge estimates marked:
  #png("lambda_ridge.png")
  par(mfcol = c(1,1))
  plot(statyear1, xvar = "lambda", lwd=4, main = paste0(year,": All players")); abline(h = 0, lwd = 3, lty=2)
  lines(rep(log(.01), ncol(X)), statyear$coefficients[-1], type='p', pch=4, cex = 2, lwd=3)
  lines(rep(log(fit.cv$lambda.1se), ncol(X)), beta_hat_ridge[-1], type='p', pch=2, cex = 2, lwd=3)
  legend('topright', c('OLS', 'Ridge'), pch = c(4,2), cex=1.4, lwd=3)
  #while (!is.null(dev.list()))  dev.off()
  
  #----------------------------------------------------------------------------
  # Lasso Regression
  #----------------------------------------------------------------------------
  
  # Function will standardize for you
  fit1 = glmnet(X,  # Matrix of predictors (w/o intercept)
                y,  # Response
                alpha=1, # Corresponds to the penalty (0 for ridge, 1 for lasso)  
                lambda = seq(100, 0.001, by = -0.01) # lambda sequence
  )
  
  # Plot the estimated coefficients for each (log) lamda
  #png("lambda_l.png")
  par(mfcol = c(1,1))
  plot(fit1, xvar = "lambda", lwd=4, main = paste0(year,": All players"))
  # Reference line at zero:
  abline(h = 0, lwd = 3, lty=2)
  # Add OLS fits:
  lines(rep(log(.01), ncol(X)), statyear$coefficients[-1], type='p', pch=4, cex = 2, lwd=3)
  #while (!is.null(dev.list()))  dev.off()
  
  #png("L1 Norm_l.png")
  par(mfcol = c(1,1))
  plot(statyear1, main = paste0(year,": All players"))
  #while (!is.null(dev.list()))  dev.off()
  
  # Now find an "optimal" lambda using cross-validation: 
  fit.cv = cv.glmnet(X,  # Matrix of predictors (w/o intercept)
                     y,  # Response
                     alpha=1, # Corresponds to the penalty (0 for ridge, 1 for lasso)  
                     lambda = seq(100, 0.001, by = -0.01) # lambda sequence
  )
  #png("loglambda_l.png")
  par(mfcol = c(1,1))
  plot(fit.cv, main = paste0(year,": All players"))
  #while (!is.null(dev.list()))  dev.off()
  
  # Pick a reasonable value for lambda:
  #lambda = fit.cv$lambda.min # Minimizes CV error^2
  lambda = fit.cv$lambda.1se # largest lambda w/in 1 SE of lambda.min
  
  # Fit the model at the selected lambda:
  fit2 = glmnet(X,  # Matrix of predictors (w/o intercept)
                y,  # Response
                alpha = 1, # Corresponds to the penalty (0 for ridge, 1 for lasso)  
                lambda = lambda)
  
  # Estimated coefficients 
  beta_hat_lasso = c(fit2$a0, # Intercept
                     as.numeric(fit2$beta))
  
  # Compare to OLS:
  #sink("ols_l.txt")
  print(cbind(beta_hat_lasso, coef(statyear)))
  #sink()  # returns output to the console
  
  # Re-do plot from before, but now with lasso estimates marked:
  #png("lambda_lasso.png")
  par(mfcol = c(1,1))
  plot(fit1, xvar = "lambda", lwd=4, main = paste0(year,": All players")); abline(h = 0, lwd = 3, lty=2)
  lines(rep(log(.001), ncol(X)), statyear$coefficients[-1], type='p', pch=4, cex = 2, lwd=3)
  lines(rep(log(lambda), ncol(X)), beta_hat_lasso[-1], type='p', pch=2, cex = 2, lwd=3)
  legend('topright', c('OLS', 'Lasso'), pch = c(4,2), cex=1.4, lwd=3)
  #while (!is.null(dev.list()))  dev.off()
  
  # Which variables were selected?
  #sink("lasso_select.txt")
  print(colnames(X)[which(fit2$beta != 0)])
  #sink()  # returns output to the console
  
  #----------------------------------------------------------------------------
  # GAM
  #----------------------------------------------------------------------------
  
  # For GAM, it's easier here to use the variable names:
  PosC = X[,1]
  PosPF = X[,2]
  PosSF = X[,3]
  PosSG = X[,4]
  PosPG = X[,5]
  Age = X[,6]
  G = X[,7]
  GS = X[,8]
  MP = X[,9]
  FG = X[,10]
  FGA = X[,11]
  FG. = X[,12]
  X3P = X[,13]
  X3PA = X[,14]
  X3P. = X[,15]
  X2P = X[,16]
  X2PA = X[,17]
  X2P. = X[,18]
  eFG. = X[,19]
  FT = X[,20]
  FTA = X[,21]
  FT. = X[,22]
  ORB = X[,23]
  DRB = X[,24]
  TRB = X[,25]
  AST = X[,26]
  STL = X[,27]
  BLK = X[,28]
  
  ### Would have fit the models using all predictors but ran into an error
  if (year == "2020") {
    gam_fit = gam(logsal ~ s(Age) + s(MP) + s(FGA) + s(X2PA) + s(FTA) + s(DRB) + s(AST) + s(BLK),
                  method = "REML")
    
    # sink("gam.txt")
    print(gam_fit)
    #sink()  # returns output to the console
    
    pred = c("Age", "MP", "FGA", "X2PA", "FTA", "DRB", "AST", "BLK")
    
    for (idx in 1:length(pred)) {
      #png(paste0("gam_lasso-",pred[idx],".png"))
      plot(gam_fit, shade = TRUE, main = paste0(year,": GAM on Lasso predictors"), select = idx)
      #while (!is.null(dev.list()))  dev.off()
    }
    
  } else if (year == "2019") {
    gam_fit = gam(logsal ~ s(Age) + s(G) + s(MP) + s(FG) + s(FT) + s(DRB),
                  method = "REML")
    
    #sink("gam.txt")
    print(gam_fit)
    #sink()  # returns output to the console
    
    pred = c("Age", "G", "MP", "FG", "FT", "DRB")
    
    for (idx in 1:length(pred)) {
      #png(paste0("gam_lasso-",pred[idx],".png"))
      plot(gam_fit, shade = TRUE, main = paste0(year,": GAM on Lasso predictors"), select = idx)
      #while (!is.null(dev.list()))  dev.off()
    }
    
  } else if (year == "2018") {
    gam_fit = gam(logsal ~ s(Age) + s(G) + s(GS) + s(MP) + s(FG) + s(FT) + s(TRB),
                  method = "REML")
    
    #sink("gam.txt")
    print(gam_fit)
    #sink()  # returns output to the console
    
    pred = c("Age", "G", "GS", "MP", "FG", "FT", "TRB")
    
    for (idx in 1:length(pred)) {
      #png(paste0("gam_lasso-",pred[idx],".png"))
      plot(gam_fit, shade = TRUE, main = paste0(year,": GAM on Lasso predictors"), select = idx)
      #while (!is.null(dev.list()))  dev.off()
    }
    
  } else if (year == "2017") {
    gam_fit = gam(logsal ~ s(Age) + s(G) + s(GS) + s(FGA) + s(DRB),
                  method = "REML")
    
    #sink("gam.txt")
    print(gam_fit)
    #sink()  # returns output to the console
    
    pred = c("Age", "G", "GS", "FGA", "DRB")
    
    for (idx in 1:length(pred)) {
      #png(paste0("gam_lasso-",pred[idx],".png"))
      plot(gam_fit, shade = TRUE, main = paste0(year,": GAM on Lasso predictors"), select = idx)
      #while (!is.null(dev.list()))  dev.off()
    }
    
  } else if (year == "2016") {
    gam_fit = gam(logsal ~ s(Age) + s(G) + s(GS) + s(MP) + s(FG) + s(X2PA) + s(FT) + s(DRB),
                  method = "REML")
    
    #sink("gam.txt")
    print(gam_fit)
    #sink()  # returns output to the console
    
    pred = c("Age", "G", "GS", "MP", "FG", "X2PA", "FT", "DRB")
    
    for (idx in 1:length(pred)) {
      #png(paste0("gam_lasso-",pred[idx],".png"))
      plot(gam_fit, shade = TRUE, main = paste0(year,": GAM on Lasso predictors"), select = idx)
      #while (!is.null(dev.list()))  dev.off()
    }
    
  } else if (year == "2015") {
    gam_fit = gam(logsal ~ s(Age) + s(G) + s(MP) + s(FG) + s(DRB),
                  method = "REML")
    
    #sink("gam.txt")
    print(gam_fit)
    #sink()  # returns output to the console
    
    pred = c("Age", "G", "MP", "FG", "DRB")
    
    for (idx in 1:length(pred)) {
      #png(paste0("gam_lasso-",pred[idx],".png"))
      plot(gam_fit, shade = TRUE, main = paste0(year,": GAM on Lasso predictors"), select = idx)
      #while (!is.null(dev.list()))  dev.off()
    }
    
  } else if (year == "2014") {
    gam_fit = gam(logsal ~ s(Age) + s(GS) + s(MP) + s(X2PA) + s(DRB),
                  method = "REML")
    
    #sink("gam.txt")
    print(gam_fit)
    #sink()  # returns output to the console
    
    pred = c("Age", "GS", "MP", "X2PA", "DRB")
    
    for (idx in 1:length(pred)) {
      #png(paste0("gam_lasso-",pred[idx],".png"))
      plot(gam_fit, shade = TRUE, main = paste0(year,": GAM on Lasso predictors"), select = idx)
      #while (!is.null(dev.list()))  dev.off()
    }
    
  } else if (year == "2013") {
    gam_fit = gam(logsal ~ s(Age) + s(G) + s(MP) + s(FGA) + s(X2PA) + s(FTA) + s(DRB),
                  method = "REML")
    
    #sink("gam.txt")
    print(gam_fit)
    #sink()  # returns output to the console
    
    pred = c("Age", "G", "MP", "FGA", "X2PA", "FTA", "DRB")
    
    for (idx in 1:length(pred)) {
      #png(paste0("gam_lasso-",pred[idx],".png"))
      plot(gam_fit, shade = TRUE, main = paste0(year,": GAM on Lasso predictors"), select = idx)
      #while (!is.null(dev.list()))  dev.off()
    }
    
  } else {
    print("Should not reach this case")
  }
  
  
  return(r2)
}


c_y_stat_mlr <- function(ydata) {
  ### Plots the data for each statistic for all years
  # ydata: Entire dataset for specific year

  year <- ydata$Year[1]

  #mainDir <- "/Users/alexanderxiong/Documents/STAT 410/stat410-final"
  #subDir <- year
  #dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  #setwd(file.path(mainDir, subDir))

  #mdir_pos <- getwd()
  #subdir_pos <- "mlr"
  #dir.create(file.path(mdir_pos, subdir_pos), showWarnings = FALSE)
  #setwd(file.path(mdir_pos, subdir_pos))
  
  r2 <- c_stat_mlr(year, ydata)
  
  #setwd(mainDir)
  
  return(r2)
}

### Run this code: Be wary of the file saving above
c_y_stat_mlr(year1920)
c_y_stat_mlr(year1819)
c_y_stat_mlr(year1718)
c_y_stat_mlr(year1617)
c_y_stat_mlr(year1516)
c_y_stat_mlr(year1415)
c_y_stat_mlr(year1314)
c_y_stat_mlr(year1213)
###

