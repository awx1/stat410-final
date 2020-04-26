library(glmnet)
library(car)
library(janitor)

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
c_stat_mlr <- function(year, logsal, ydata) {
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
  xdat = model.matrix( ~ Pos + Age + G + GS + MP + FG + FGA + FG. + X3P + X3PA + X3P. + X2P + X2PA + X2P. + eFG. + FT + FTA + FT. + ORB + DRB + TRB + AST + STL + BLK, data = ydata)
  print(dim(xdat))
  print(length(logsal))
  print(length(remove_empty(ydata, which = "rows", quiet = TRUE)$Salary))
  statyear <- lm(logsal ~ xdat)
  sink("lm.txt")
  print(summary(statyear))
  sink()  # returns output to the console
  
  # Check variance inflation factors:
  vif(statyear)
  # Any cause for concern?
  any(vif(statyear) > 5)
  
  png("diagplot.png")
  par(mfcol = c(2,2))
  plot(statyear, main = paste0(year,": All players"))
  
  r2 <- summary(statyear)$adj.r.squared
  
  # Response:
  y = statyear$model[,1]

  # Predictors 
  X = model.matrix( ~ Pos + Age + G + GS + MP + FG + FGA + FG. + X3P + X3PA + X3P. + X2P + X2PA + X2P. + eFG. + FT + FTA + FT. + ORB + DRB + TRB + AST + STL + BLK - 1, data = ydata)
  
  # Now ridge regression:
  statyear1 = glmnet(X,  # Matrix of predictors (w/o intercept)
                y,  # Response
                alpha=0, # Corresponds to the penalty (0 for ridge, 1 for lasso)  
                lambda = seq(100, 0, by = -0.01) # lambda sequence
  )
  
  # Plot the estimated coefficients for each (log) lamda
  
  png("lambda.png")
  par(mfcol = c(1,1))
  plot(statyear1, xvar = "lambda", lwd=2, main = paste0(year,": All players"))
  # Reference line at zero:
  abline(h = 0, lwd = 3, lty=2)
  # Add OLS fits:
  print(statyear$coefficients)
  print(X)
  lines(rep(log(.01), ncol(X)), statyear$coefficients[-1], type='p', pch=4, cex = 2, lwd=3)
  print(1)
  png("L1 Norm.png")
  par(mfcol = c(1,1))
  plot(statyear1, main = paste0(year,": All players"))
  print(1)
  # Now find an "optimal" lambda using cross-validation: 
  fit.cv = cv.glmnet(X,  # Matrix of predictors (w/o intercept)
                     y,  # Response
                     alpha=0, # Corresponds to the penalty (0 for ridge, 1 for lasso)  
                     lambda = seq(100, 0, by = -0.01) # lambda sequence
  )
  png("loglambda.png")
  par(mfcol = c(1,1))
  plot(fit.cv, main = paste0(year,": All players"))
  print(1)
  while (!is.null(dev.list()))  dev.off()
  print(1)
  # Pick a reasonable value for lambda:
  #lambda = fit.cv$lambda.min # Minimizes CV error^2
  lambda = fit.cv$lambda.1se # largest lambda w/in 1 SE of lambda.min
  print(1)
  # Fit the model at the selected lambda:
  fit2 = glmnet(X,  # Matrix of predictors (w/o intercept)
                y,  # Response
                alpha=0, # Corresponds to the penalty (0 for ridge, 1 for lasso)  
                lambda = lambda)
  print(1)
  # Estimated coefficients 
  beta_hat_ridge = c(fit2$a0, # Intercept
                     as.numeric(fit2$beta))
  print(1)
  # Compare to OLS:
  cbind(beta_hat_ridge, coef(statyear))
  print(1)
  # Re-do plot from before, but now with ridge estimates marked:
  plot(statyear1, xvar = "lambda", lwd=4); abline(h = 0, lwd = 3, lty=2)
  lines(rep(log(.01), ncol(X)), statyear$coefficients[-1], type='p', pch=4, cex = 2, lwd=3)
  lines(rep(log(fit.cv$lambda.1se), ncol(X)), beta_hat_ridge[-1], type='p', pch=2, cex = 2, lwd=3)
  legend('topright', c('OLS', 'Ridge'), pch = c(4,2), cex=1.4, lwd=3)
  
  return(r2)
}


c_y_stat_mlr <- function(ydata) {
  ### Plots the data for each statistic for all years
  # ydata: Entire dataset for specific year

  year <- ydata$Year[1]

  mainDir <- "/Users/alexanderxiong/Documents/STAT 410/stat410-final"
  subDir <- year
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  # pos <- c('C', 'PF', 'SF', 'SG', 'PG')
  # ydat_mod <- ydata[ydata$Pos == p, ]
  # logsal = log10(ydat_mod$Salary)
  logsal = log10(ydata$Salary)

  mdir_pos <- getwd()
  subdir_pos <- "mlr"
  dir.create(file.path(mdir_pos, subdir_pos), showWarnings = FALSE)
  setwd(file.path(mdir_pos, subdir_pos))
  
  r2 <- c_stat_mlr(year, logsal, ydata)
  
  setwd(mainDir)
  
  return(r2)
}

c_y_stat_mlr(year1920)
c_y_stat_mlr(year1819)
c_y_stat_mlr(year1718)
c_y_stat_mlr(year1617)
c_y_stat_mlr(year1516)
c_y_stat_mlr(year1415)
c_y_stat_mlr(year1314)
c_y_stat_mlr(year1213)

