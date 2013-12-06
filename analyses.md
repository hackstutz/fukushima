# Fukushima-Effect Analyses

## Data

Data needs to be downloaded from ZACAT <http://zacat.gesis.org/webview/>.

```{dataimport}
library(foreign)
setwd("C:/Users/Hackstutz/Dropbox/Fukushima/")
issp <- read.dta("issp_data/ZA5500_v2-0-0.dta")
```

For a few countries we have information about the month the interview took place but not the exact day. We suggest to impute these information using random uniform distributions.

```{datapreperation}
#random.org
set.seed(950075)


```

