## ---- message=FALSE------------------------------------------------------
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


