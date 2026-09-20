## ----message=FALSE------------------------------------------------------------
library(GetBCBData)
library(dplyr)
library(ggplot2)

my_id <- c(selic = 432)

df_bcb <- gbcbd_get_series(id = my_id ,
                       first_date = '2000-01-01',
                       last_date = Sys.Date(),
                       format_data = 'long',
                       use_memoise = TRUE, 
                       cache_path = tempdir(), # use tempdir for cache folder
                       do_parallel = FALSE)

glimpse(df_bcb)

p <- ggplot(df_bcb, aes(x = ref_date, y = value/100) ) +
  geom_line() + 
  labs(title = 'Selic Rate', 
       subtitle = paste0(min(df_bcb$ref_date), ' to ', max(df_bcb$ref_date)),
       x = '', y = 'Interest Rate') + 
  theme_light()
  
print(p)


