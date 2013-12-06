Fukushimas impact on attitudes towards nuclear energy - Analysing the structure of a causal effect
=====================

We analyse the impact of the Fukushima Accident on individuals attitudes to nuclear energy. Using ISSP data we try to estimate the causal effect using Regression Discontinuity Design (RDD) and a Difference-in-Difference Estimator (DiD). 

In a second step we analyse the structure of the effect. We can show that women and men as well as young and old people were affected differently by the accident.

Data and Software
---------

We use ISSP data. To get the data you need to sign up at ZACAT <http://zacat.gesis.org/webview/> and download the 2010 ISSP data file **ZA5500_v2-0-0**. All analyses were conducted using R (<http://www.r-project.org>).

Method
---------

### RDD

RDD is possible with ISSP data as people were interviewed both before and after the accident and data contains information about the exact date of the interview. Though the causal identification strategy is week as we can only use observations from five countries (Canada, Denmark, Slovenia, Spain and Switzerland). 

### DiD

DiD is a more powerful identification strategy, however it assumes stable trends accross countries as well as homogenous effects of covariates in all countries.

Result
----------

We find the Fukushima-effect. Moreover we can show, that women attitudes were affected significantly stronger than men. The Fukushima-effect is furthermore higher for older individuals who tended to be more optimistic with nuclear energy and now converge towards their younger counterparts.
