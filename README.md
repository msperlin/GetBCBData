## Motivation

The Central Bank of Brazil (BCB) offers access to its SGS system (sistema gerenciador de series temporais) with a official API available [here](http://www.bcb.gov.br/?sgs).

Package GetBCB offers a R interface to the API and many other advantages:

- Use of a caching system with package `memoise` to speed up repeated requests of data;
- User can utilize all cores of the machine (parallel computing) when fetching a large batch of time series;
- Error handling internally. Even if requested series does not exist, the function will still return all results.

## Installation

```
# CRAN (official release) - SOON
install.packages('GetBCBData')

# Github (dev version)
devtools::install_github('msperlin/GetBCBData')
```

## A simple example

```
library(GetBCBData)
library(tidyverse)

my.countries <- c('Germany', 'Canada', 'USA', 
                  'France', 'Italy', 'Japan')

my.ids <- c(3785:3790)

names(my.ids) <- paste0('Unemp. rate - ', my.countries)

df.bcb <- gbcbd_get_series(id = my.ids ,
                       first.date = '2000-01-01',
                       last.date = Sys.Date(),
                       format.data = 'long',
                       #series.name = 'ABC',
                       use.memoise = TRUE, 
                       cache.path = tempdir(), # use tempdir for cache folder
                       do.parallel = FALSE)

glimpse(df.bcb)

p <- ggplot(df.bcb, aes(x = ref.date, y = value) ) +
  geom_line() + 
  labs(title = 'Unemploymnent Rates Around the World', 
       subtitle = paste0(min(df.bcb$ref.date), ' to ', max(df.bcb$ref.date)),
       x = '', y = 'Percentage*100') + facet_wrap(~series.name)
  

print(p)
```

