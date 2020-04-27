year1920 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201920.csv', header = TRUE)
year1819 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201819.csv', header = TRUE)
year1718 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201718.csv', header = TRUE)
year1617 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201617.csv', header = TRUE)
year1516 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201516.csv', header = TRUE)
year1415 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201415.csv', header = TRUE)
year1314 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201314.csv', header = TRUE)
year1213 <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-201213.csv', header = TRUE)

year_tot <- read.csv('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-tot.csv', header = TRUE)
years_dat = list(year1920, year1819, year1718, year1617, year1516, year1415, year1314, year1213)

### Generate Table 1
p_num <- function(year) {
  num = c(0,0,0,0,0)
  for (y in year$Pos){
    if (y == 'C') {
      num[1] = num[1] + 1
    }
    if (y == 'PF') {
      num[2] = num[2] + 1
    }
    if (y == 'SF') {
      num[3] = num[3] + 1
    }
    if (y == 'SG') {
      num[4] = num[4] + 1
    }
    if (y == 'PG') {
      num[5] = num[5] + 1
    }
  }
  return(num)
}
print(p_num(year_tot))
     
# Generate Figure 1.
par(mfcol = c(2,2))
hist(log10(year_tot$Salary), breaks = seq(3.8, 8, by = 0.05), xlab = "log10(Salary)", main = "Frequency of Log-based NBA salary")
hist(year_tot$Age, breaks = seq(15, 45, by = 1), ylim = c(0,400), xlab = "Age", main = "Frequency of NBA player age")
barplot(table(year_tot$MP), xlab = " Minutes per game", main = " Frequency of MP per game")
barplot(table(year_tot$PTS), ylim = c(0,50), xlab = "PTS per game", main = "Frequency of PTS per game")

### Headers
# Rk/Player/Link/Pos/Age/Tm/G/GS/MP/FG/FGA/FG.
# X3P/X3PA/X3P./X2P/X2PA/X2P./eFG./FT/FTA/FT.
# 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS'
headers = c('Rk','Player', 'Link', 'Pos', 'Age', 'Tm', 'G', 'GS', 'MP', 'FG', 'FGA', 'FG%', '3P', '3PA', '3P%', '2P', '2PA', '2P%', 'eFG%', 'FT', 'FTA', 'FT%', 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS')
mheaders = c('Age', 'G', 'GS', 'MP', 'FG', 'FGA', 'FG.', 'X3P', 'X3PA', 'X3P.', 'X2P', 'X2PA', 'X2P.', 'eFG.', 'FT', 'FTA', 'FT.', 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS')
years_dat = list(year1920, year1819, year1718, year1617, year1516, year1415, year1314, year1213)

### Analysis
c_stat <- function(year, stat, logsal, ydata) {
  ### Plots the data for each statistic for a specific year
  # ydata: Entire dataset for specific year
  # logsal: Log 10 salary data
  # stat: Specific statistic examining (string)
  # year: Specific year (string)
  
  statyear <- lm(logsal ~ ydata[,stat])
  r2 <- summary(statyear)$adj.r.squared
  #png(paste0(stat,"-slr.png"), width = 480, height = 480, units = "px")
  #png(paste0(stat,"-slr.png"))
  par(mfcol = c(1,1))
  plot(ydata[,stat], logsal, xlab=stat, ylab='Salary (log10-scale)', main = paste0(year, ": Salary (log10-scale) vs. ", stat))
  if (r2 < 0.1) {
    abline(statyear, col='red', lwd = 3)
  } else if (r2 < 0.3) {
    abline(statyear, col='orange', lwd = 3)
  } else {
    abline(statyear, col='green', lwd = 3)
  }
  #png(paste0(stat,"-diagplot.png"), width = 480, height = 480, units = "px")
  #png(paste0(stat,"-diagplot.png"))
  par(mfcol = c(2, 2))
  plot(statyear, lwd = 3)
  #while (!is.null(dev.list()))  dev.off()
  
  return(r2)
}

c_y_stat <- function(ydata) {
  ### Plots the data for each statistic for all years
  # ydata: Entire dataset for specific year
  
  logsal = log10(ydata$Salary)
  year <- ydata$Year[1]
  ar2 <- rep(26,0)
  
  #mainDir <- "/Users/alexanderxiong/Documents/STAT 410/stat410-final"
  #subDir <- year
  #dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  #setwd(file.path(mainDir, subDir))
  
  for (idx in 1:length(mheaders)) {
    #mdir <- getwd()
    #subdir <- paste0(mheaders[idx],"-plots")
    #dir.create(file.path(mdir, subdir), showWarnings = FALSE)
    #setwd(file.path(mdir, subdir))
    r2 <- c_stat(year, mheaders[idx], logsal, ydata)
    #setwd(mdir)
    
    ar2[idx] <- r2
  }
  #setwd(mainDir)
  
  sig = c()
  insig = c()
  
  for (idx in 1:length(ar2)) {
    if (ar2[idx] >= 0.2551) {
      sig = append(sig, mheaders[idx])
    } else if (ar2[idx] <= 0.0678) {
      insig = append(insig, mheaders[idx])
    }
  }
  
  print(year)
  print(paste("Significant:", sig))
  print(paste("Insignificant:", insig))
  return(ar2)
}

### Run this code: Be wary of the file saving above
## Store the r^2 values
r2_tot <- list()
idx = 1
for (year_dat in years_dat) {
  ## Function call
  aggr_r2 <- c_y_stat(year_dat)
  r2_tot[[idx]] = aggr_r2
  
  idx = idx + 1
}
####

## Store quantiles
first <- c()
median <- c()
third <- c()
for (r2 in r2_tot) {
  ## Calculate quantiles
  bp <- quantile(r2, c(0.25,0.5,0.75))
  first <- append(first, bp[[1]])
  median <- append(median, bp[[2]])
  third <- append(third, bp[[3]])
}

## Mean of quantiles: this is how I set my bounds for significance
mean(first) ## 0.06697005
mean(median) ## 0.1700509
mean(third) ## 0.2502852
tot <- unlist(r2_tot)
quantile(tot, c(0.25,0.5,0.75))
# 0.06780191 0.17282437 0.25510792 


