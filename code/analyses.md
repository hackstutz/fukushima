# Measuring and analysing the causal impact of the Fukushima Daiichi nuclear disaster

## Data

We use the International Social Survey Program (ISSP) data 2010 which added an environmental attitudes survey as a survey focus. The survey was conducted between 2010 and 2011 whereby the exact period varies by country. Data needs to be downloaded from ZACAT <http://zacat.gesis.org/webview/>.


```r
library(foreign)
setwd("C:/Users/Hackstutz/Dropbox/Fukushima/")
issp <- read.dta("issp_data/ZA5500_v2-0-0.dta")
```

```
## Warning: duplicated levels in factors are deprecated Warning: duplicated
## levels in factors are deprecated Warning: duplicated levels in factors are
## deprecated
```


For a few countries we have information about the month the interview took place but not the exact day. We suggest to impute these information using random uniform distributions.


```r
# random.org
set.seed(950075)

issp$randomDY <- NULL
howmany31 <- length(which(issp$DATEMO %in% c("January", "March", "May", "July", 
    "August", "October", "December")))
howmany30 <- length(which(issp$DATEMO %in% c("April", "June", "September", "November")))
howmany28 <- length(which(issp$DATEMO %in% c("February")))
# Plus cases missing DATEMO Sums to N (45199)

# Create random days that can be used to fill Missings
issp$randomDY[issp$DATEMO %in% c("January", "March", "May", "July", "August", 
    "October", "December")] <- floor(runif(howmany31, 1, 32))
issp$randomDY[issp$DATEMO %in% c("April", "June", "September", "November")] <- floor(runif(howmany30, 
    1, 31))
issp$randomDY[issp$DATEMO %in% c("February")] <- floor(runif(howmany28, 1, 29))

# Fill where the value of DATEDY is missing:
issp$DATEDY[is.na(issp$DATEDY)] <- issp$randomDY[is.na(issp$DATEDY)]

# Create a nice data variable
levels(issp$DATEYR) <- c("2009", "2010", "2011", "No answer")
issp$datestring <- paste(issp$DATEYR, issp$DATEMO, issp$DATEDY, sep = "-")
# Change the locale to apply strptime with english names for months
Sys.setlocale("LC_TIME", "us")
```

```
## [1] "English_United States.1252"
```

```r
issp$date <- strptime(issp$datestring, format = "%Y-%B-%d")
# issp$V4[is.na(issp$date)] #6471 Missings

# dependent variable
issp$nuclear <- issp$V45
issp$nuclear[issp$nuclear %in% c("Can't choose", "NA")] <- NA
issp$nuclear <- factor(issp$nuclear)

# independent variable: before/after fukushima
issp$after <- issp$date >= fukushima
```

```
## Error: Objekt 'fukushima' nicht gefunden
```

```r

# controls
issp$country <- issp$V4
issp$sex <- factor(issp$SEX)
issp$age <- issp$AGE
```


It is important to know which countries contribute information to which period of time.


```r
library(ggplot2)
fukushima <- strptime("2011-03-13", format = "%Y-%m-%d")
ggplot(issp, aes(x = date, y = V4)) + geom_point() + geom_vline(xintercept = as.numeric(fukushima), 
    color = "red")
```

```
## Warning: Removed 6481 rows containing missing values (geom_point).
```

![plot of chunk timelines](figure/timelines.png) 


## Method

### Regression Discontinuity Design (RDD)

RDD's causal identification strategy is to compare cases which are very close to a certain cutoff point of threshold which assigns data into a control and treatment group. Concerning the Fukushima accident, the assignment variable is time and the threshold is the day of the accident: 13th March 2011. Whether the interview was conducted before the 13th or after appears to be random so comparing individuals surveyed just before and just after the accident does control for most unobserved heterogeneity. RDD is illustrated in the following figure:


```r
library(ggplot2)
library(grid)  #for function arrow()
x <- 1:100
set.seed(123)
y <- rnorm(100)
y[50:100] <- y[50:100] + 2
group <- c(rep(0, 50), rep(1, 50))
qplot(x, y, group = group) + stat_smooth(se = FALSE) + geom_text(show_guide = FALSE, 
    x = 50, y = 1.2, label = "Treatment \neffect", color = "black") + geom_segment(aes(x = 50, 
    y = 1.5, xend = 50, yend = 2.2), arrow = arrow(), color = "black") + geom_segment(aes(x = 50, 
    y = 0.8, xend = 50, yend = 0.45), arrow = arrow(), color = "black") + ggtitle("RDD Visualization") + 
    theme_bw()
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using
## loess. Use 'method = x' to change the smoothing method.
```

![plot of chunk rddillustration](figure/rddillustration.png) 


### Difference-in-Difference Estimator (DiD)

DiD's causal identification strategy is to get rid of multiple sources of unobserved heterogeneity by controlling for country-characteristics (country dummies) and period-characteristics (period dummies) which in our case is accomplished by estimating a Country-Fixed-Effects Modell including a "before/after-Fukushima-Dummy". There are two key assumptions to mention. First, DiD assumes the timetrend of $y$, i.e. attitudes towards nuclear energy to be stable over time. Second, as there are countries which are completely before or completely after the accident, the covariance of all variables needs to be stable accross countries. (das müssen wir noch diskutieren). DiD is illustrated in the next figure:


```r
time <- rep(c(0, 1), 3)
observed <- c(30, 15)
control <- c(20, 35)
counterfactual <- c(30, 45)
group <- factor(c(1, 1, 2, 2, 3, 3), labels = c("observed", "control", "counterfactual"))
y <- c(observed, control, counterfactual)
qplot(time, y, group = group, colour = group, ylim = c(0, 50), xlim = c(0, 1.06)) + 
    geom_line(size = 1) + ggtitle("DiD Visualization") + geom_segment(aes(x = 1, 
    y = 27, xend = 1, yend = 15.5), arrow = arrow(), color = "black") + geom_segment(aes(x = 1, 
    y = 33, xend = 1, yend = 44.5), arrow = arrow(), color = "black") + geom_text(show_guide = FALSE, 
    x = 1, y = 30, label = "Treatment \neffect", color = "black") + theme_bw()
```

![plot of chunk didillustration](figure/didillustration.png) 



## Models

### RDD


```r
library(rdd)
# Efficiency gains can be made by including covariates. I.e. country
# dummies. Good to think about a lot more...  not yet working. bandwidth too
# small RDestimate(V45~as.numeric(date),
# cutpoint=as.numeric(fukushima),data=issp) plot(RDestimate(y~x|cov))
```


### DiD


```r
# We need ordered logit regression here. Fixed effects can be achieved by
# including country-dummies.
library(MASS)
# including country dummies in polr() throws an error..
# summary(polr(nuclear~after*(sex+age), data=issp))

# using OLS temporarily controlling for country trends
summary(lm(as.numeric(nuclear) ~ after * (sex + age) + country + country:as.numeric(date), 
    data = issp))
summary(lm(as.numeric(nuclear) ~ after * (sex + age) + country, data = issp))

```

