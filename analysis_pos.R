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
years_dat = list(year1920, year1819, year1718, year1617, year1516, year1415, year1314, year1213)

### Analysis
c_stat <- function(year, stat, logsal, ydata, pos) {
  ### Plots the data for each statistic for a specific year, split by position
  # ydata_pos: Entire dataset for specific year
  # logsal: Log 10 salary data
  # stat: Specific statistic examining (string)
  # year: Specific year (string)
  # pos: Position of player (string)
  
  statyear <- lm(logsal ~ ydata[,stat])
  r2 <- summary(statyear)$adj.r.squared
  #png(paste0(stat,"-slr.png"))
  par(mfcol = c(1,1))
  plot(ydata[,stat], logsal, xlab=stat, ylab='Salary (log10-scale)', main = paste0(year, " ", pos, ": Salary (log10-scale) vs. ", stat))
  if (r2 < 0.1) {
    abline(statyear, col='red', lwd = 3)
  } else if (r2 < 0.3) {
    abline(statyear, col='orange', lwd = 3)
  } else {
    abline(statyear, col='green', lwd = 3)
  }
  #png(paste0(stat,"-diagplot.png"))
  par(mfcol = c(2, 2))
  plot(statyear, lwd = 3)
  #while (!is.null(dev.list()))  dev.off()
  
  return(r2)
}

c_y_stat <- function(ydata) {
  ### Plots the data for each statistic for all years
  # ydata: Entire dataset for specific year
  
  year <- ydata$Year[1]
  ar2_pos <- list()
  #mainDir <- "/Users/alexanderxiong/Documents/STAT 410/stat410-final"
  #subDir <- year
  #dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  #setwd(file.path(mainDir, subDir))
  
  pos <- c('C', 'PF', 'SF', 'SG', 'PG')
  idx_p = 1
  for (p in pos) {
    ydat_mod <- ydata[ydata$Pos == p, ]
    logsal = log10(ydat_mod$Salary)
    ar2 <- rep(26,0)
    
    #mdir_pos <- getwd()
    #subdir_pos <- p
    #dir.create(file.path(mdir_pos, subdir_pos), showWarnings = FALSE)
    #setwd(file.path(mdir_pos, subdir_pos))
    
    for (idx in 1:length(mheaders)) {
      #mdir <- getwd()
      #subdir <- paste0(mheaders[idx],"-plots")
      #dir.create(file.path(mdir, subdir), showWarnings = FALSE)
      #setwd(file.path(mdir, subdir))
      
      r2 <- c_stat(year, mheaders[idx], logsal, ydat_mod, p)
      #setwd(mdir)
      
      ar2[idx] <- r2
    }
    
    #setwd(mdir_pos)
    
    sig = c()
    insig = c()
    for (idx in 1:length(ar2)) {
      if (ar2[idx] >= 0.28718003) {
        sig = append(sig, mheaders[idx])
      } else if (ar2[idx] <= 0.075479) {
        insig = append(insig, mheaders[idx])
      }
    }
    
    print(paste(year, "&", p))
    print(paste("Significant:", sig))
    print(paste("Insignificant:", insig))
    ar2_pos[[idx_p]] = ar2
    
    idx_p = idx_p + 1
  }
  #setwd(mainDir)
  
  return(ar2_pos)
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
###

## This store the r2 values as well as the actual parameter that are significant vs. insignificant
total_sig_insig_stats = list() ## total list store
idx = 1
for (year_dat in r2_tot) { ## iterate over each year (8 iterations)
  year_stats = list()
  for (idx_pos in 1:length(pos)) { ## should iterate through five times
    stats_sig_insig = list() ## list of significant & insignificant vectors
    stats_sig = c() ## store significant headers
    stats_insig = c() ## store insignificant headers
    
    for (idx_dat_pt in 1:length(year_dat[[idx_pos]])) { ## should iterate over headers (26 iterations)
      dat_pt = year_dat[[idx_pos]][idx_dat_pt]
      print(dat_pt)
      if (dat_pt >= 0.28718003) {
        stats_sig = append(stats_sig, mheaders[idx_dat_pt])
      } else if (dat_pt <= 0.075479) {
        stats_insig = append(stats_insig, mheaders[idx_dat_pt])
      }
    }
    stats_sig_insig[[1]] = stats_sig
    stats_sig_insig[[2]] = stats_insig
    year_stats[[idx_pos]] = stats_sig_insig
  }
  total_sig_insig_stats[[idx]] = year_stats
  idx = idx + 1
}

## Mean of quantiles: this is how I set my bounds for significance
quantile(unlist(r2_tot))
# 0.07547952  0.18993116  0.28718003


