## Motivation

The Central Bank of Brazil (BCB) offers access to its SGS system (sistema gerenciador de series temporais) with a official API available [here](http://www.bcb.gov.br/?sgs).

Package GetBCBData offers a R interface to the API and many other advantages:

- Use of a caching system with package `memoise` to speed up repeated requests of data;
- User can utilize all cores of the machine (parallel computing) when fetching a large batch of time series;
- Error handling internally. Even if requested series does not exist, the function will still return all results.


## Installation

```
# CRAN (official release)  
install.packages('GetBCBData')

# Github (dev version)
devtools::install_github('msperlin/GetBCBData')
```

## A simple example

```
library(GetBCBData)
library(tidyverse)

my_countries <- c('Germany', 'Canada', 'USA', 
                  'France', 'Italy', 'Japan')

my_ids <- c(3785:3790)

names(my_ids) <- paste0('Unemp. rate - ', my_countries)

df_bcb <- gbcbd_get_series(id = my_ids ,
                       first_date = '2000-01-01',
                       last_date = Sys.Date(),
                       format_data = 'long',
                       #series_name = 'ABC',
                       use_memoise = TRUE, 
                       cache_path = tempdir(), # use tempdir for cache folder
                       do_parallel = FALSE)

glimpse(df_bcb)

p <- ggplot(df_bcb, aes(x = ref_date, y = value) ) +
  geom_line() + 
  labs(title = 'Unemploymnent Rates Around the World', 
       subtitle = paste0(min(df_bcb$ref_date), ' to ', max(df_bcb$ref_date)),
       x = '', y = 'Percentage*100') + facet_wrap(~series_name)
  

print(p)
```

## Searching for series ids

If you don't know the id of a series, use `gbcbd_search_series()` to search the
BCB-SGS catalog by text:

```
df_search <- gbcbd_search_series('selic')
print(df_search)

# use the first match in gbcbd_get_series()
my_id <- df_search$id[1]
names(my_id) <- df_search$series_name[1]
df_bcb <- gbcbd_get_series(my_id)
```

