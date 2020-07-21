# stat410-final: Multiple Linear Regression Approach for Analyzing NBA Player Salary via Performance
[![forthebadge made-with-python](http://ForTheBadge.com/images/badges/made-with-python.svg)](https://www.python.org/)
[![made-with-latex](https://img.shields.io/badge/Made%20with-LaTeX-1f425f.svg)](https://www.latex-project.org/)
[![MIT license](https://img.shields.io/badge/License-MIT-blue.svg)](https://lbesson.mit-license.org/)


## Table of Contents
* [General info](#general-info)
* [Process](#process)
* [Exploratory Analysis](#exploratory-analysis)
* [Technologies](#technologies)
* [Setup](#setup)

## General info
This project is to investigate the relationship between player salary and player features including various player per game performance metrics (e.g., points, rebounds, and shooting performance) and non-performance statistics (e.g., age and position). Specifically, I studied and extracted eight years of NBA player data from 2012 to 2020, and then applied linear regression models to determine which features play a vital role in player salary.

* To setup the model, I fit **simple linear regression** models and **multiple linear regression models** on the individual performance-salary data for each year.

* Due to the cross-correlation of performance statistics, **ridge** and **lasso** regression were applied to minimize multicollinearity and predictors, respectively.

* Using the subset of predictors obtained from lasso regression, we set up a **generalized additive model** for each performance-salary data.
	
## Process
I obtained the player performance data from the 2013 season to the 2020 season using https://www.basketball-reference.com/leagues/NBA_{YEAR}_per_game.html.

I obtained the player salary data from https://www.basketball-reference.com/contracts/players.html. Unfortunately, Basketball Reference only hosts the contracts for the current year; thus, to examine previous years contracts, we entered the link above into the Wayback Machine to access past snapshots of the url. This was the limiting factor as to why we only processed eight years of data. Basketball Reference had no data regarding contracts prior to the 2012-2013 NBA season.

I cleaned the data using preprocess.py whcih processes salary data and matches it to the correct player on the correct team in the correct year. I set a baseline for a player’s performance statistics to be included in this study.

<table>
<tr><th>Baseline Requirements</th></tr>
<tr><td>

|Header|Requirement|
|--|--|
|Games Played (GP)| > 5|
|Minutes per Game (MPG)| > 1|
|Field Goal Attempts (FGA) per Game| > 1|
|Free Throw Attempts (FTA) per Game| > 0.2|
|Average Points (PTS) per Game| > 1|

</td></tr> </table>

The processed csv files are located in the stats_sal directory. All box score data is averaged over a season fro each individual player.

<table>
<tr><th>Basic Info</th><th>Box Score</th></tr>
<tr><td>

|Header|
|--|
|'Rk'|
|'Player'|
|'Link'|
|'Pos'|
|'Age'|
|'Tm'|

</td><td>
  
|Header|Definition|
|--|--|
|'G'|Games Played|
|'GS'|Games Started|
|'MP'|Minutes per game|
|'FG'|Field Goals made|
|'FGA'|Field Goals attempted|
|'FG.'|Field Goal %|
|'X3P'|3 point attempts|
|'X3PA'|3 point makes|
|'X3P.'|3 point %|
|'FT'|Free Throws made|
|'FTA'|Free Throws attempted|
|'FT.'|Free Throw %|
|'ORB'|Offensive Rebounds|
|'DRB'|Defensive Rebounds|
|'TRB'|Total Rebounds|
|'AST'|Assists|
|'STL'|Steals|
|'BLK'|Blocks|
|'TOV'|Turnovers|
|'PF'|Personal Fouls|
|'PTS'|Points|

</td></tr> </table>

## Exploratory Analysis
Histograms of Salary, Minutes Played, Age, and Points over 8 years:
<p align="center">
  <img width="500" height="400" src="https://github.com/awx1/stat410-final/blob/master/explore.png">
</p>

### SLR Analysis
The relationship between salary and MP/TOV/FT% from different years:
2015:  Salary vs.  MP | 2020:  Salary vs.  TOV | 2018:  Salary vs.  FT%
:-------------------------:|:-------------------------:|:-------------------------:
![](https://github.com/awx1/stat410-final/blob/master/images/MP-slr.png)  |  ![](https://github.com/awx1/stat410-final/blob/master/images/TOV-slr.png)  |  ![](https://github.com/awx1/stat410-final/blob/master/images/FT.-slr.png)

Working with data from each season individually, I set up a linear regression model determining the correlation between a single performance statistic with the log-based salary data. Thus, for each of the eight years and for the 26 performance statistics (excluding position), I generated the SLR model plot and the diagnostic plot for that model. These figures can be found in each year directory under the specified performance statistic.

If the SLR model was a satisfactory fit by using the mean of quartile values of all the adjusted R-squared values from the models.  All the models with green SLR lines were in the 75% quartile of adjusted R2 values, all the models with orange SLR lines were in the inter-quartile range, and all the models with red SLR lines were below the 25% quartile. 

### MLR Analysis
MLR: lm output and diagnostic plot
2020:  MLR lm output | 2020:  MLR Diagnostic plot
:-------------------------:|:-------------------------:
![](https://github.com/awx1/stat410-final/blob/master/images/lm.png)  |  ![](https://github.com/awx1/stat410-final/blob/master/images/diagplot.png)

The predictors that have p-values less than 0.05 are PosC, Age, MP, FG%, eFg% and BLK. This output is interesting because it is contradictory to the hypothesis raised in SLR analysis. Although they  are percentage statistics, FG% and eFg% are deemed to be significant. Furthermore, Age, a predictor that was only deemed as insignificant in the SLR model, has the lowest p-value of any predictor. Looking through past years, Age is consistently the predictor with the lowest p-value and many of the significant predictors are percentage statistics

### MLR Ridge Regression
Ridge regression estimates without optimizing for cross-validation
2020:  Ridge regression lambda output | 2020:  Ridge regression plot
:-------------------------:|:-------------------------:
![](https://github.com/awx1/stat410-final/blob/master/images/lambda_r.png)  |  ![](https://github.com/awx1/stat410-final/blob/master/images/L1%20Norm_r.png)

Ridge regression estimates with optimizing for cross-validation
2020:  Ridge regression lambda output | 2020:  Ridge regression plot
:-------------------------:|:-------------------------:
![](https://github.com/awx1/stat410-final/blob/master/images/loglambda_r.png)  |  ![](https://github.com/awx1/stat410-final/blob/master/images/lambda_ridge.png)

### MLR Lasso Regression
Optimized lasso regression model for the 2020 data
2020:  Lasso regression lambda output | 2020:  Lasso regression plot
:-------------------------:|:-------------------------:
![](https://github.com/awx1/stat410-final/blob/master/images/loglambda_l.png)  |  ![](https://github.com/awx1/stat410-final/blob/master/images/lambda_lasso.png)

### MLR GAM
Optimized lasso regression model for the 2020 data
2020: GAM on Age | 2020: GAM on X2PA
:-------------------------:|:-------------------------:
![](https://github.com/awx1/stat410-final/blob/master/images/loglambda_l.png)|![](https://github.com/awx1/stat410-final/blob/master/images/gam_lasso-X2PA.png)

In-depth analysis on the graphs provided above can be found in STAT410 Final.pdf

## Technologies
Project is created with:
* Python 3.6, R
* csv: Python package for processing csv's
* glmnet: GAM analysis package in R

## Setup



