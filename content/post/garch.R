
library(quantmod)
library(xts)
library(lubridate)
require(BatchGetSymbols)
library(dplyr)
require(data.table)
require(rugarch)

last.date <- Sys.Date()
first.date <- Sys.Date() - 10000
freq.data <- 'daily'
# set tickers
yahoo <<- c('^IBEX')

l.out <- BatchGetSymbols(tickers = yahoo, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) 
l.out$df.tickers

dta <- l.out$df.tickers[l.out$df.tickers$ref.date >= '2002-01-01', ]
dta_xt <- xts(dta$ret.adjusted.prices, order.by = as.Date(dta$ref.date)) %>% 
  na.omit()

ggplot(dta, aes(ref.date, ret.adjusted.prices)) + 
  geom_line() + 
  facet_wrap(~ticker)


all_poss <- expand.grid(p = 1:2, q = 1:2, 
                        model = c('sGARCH',  'eGARCH',
                                  'gjrGARCH', 'apARCH', 'iGARCH'), 
                        distribution = c('norm', 'snorm', 'std', 
                                         'sstd', 'ged', 'sged', 'jsu')
)
all_poss <- all_poss[!(all_poss$p == 0 & all_poss$q == 0), ]

mc <- 4

cl<-makeCluster(mc)
clusterExport(cl, ls()[!ls() %in% c('cl')])

N = nrow(all_poss)

t0 = Sys.time()
aic_ <- parLapply(cl = cl, X = 1:N,
          function(i, dta_xt, all_poss){
            print(i)
            params = all_poss[i,]
            tryCatch({
              spec <- rugarch::ugarchspec(
                variance.model = list(model = as.character(params$model),
                                      garchOrder = c(params$p, params$q)
                ), 
                mean.model = list(armaOrder = c(0,0), include.mean = F),
                distribution.model = as.character(params$distribution)
              )
              
              fit <- rugarch::ugarchfit(spec = spec, 
                                        data = dta_xt)
              
              return(rugarch::infocriteria(fit)[1,1])
            }, error = function(e){return(NA)})
          }, dta_xt = dta_xt, all_poss = all_poss)

t1 = Sys.time() - t0


require(flextable)

all_data <- data.table(all_poss, aic = as.numeric(aic_))
all_data$order = paste0('(', all_data$p, ', ', all_data$q, ')')
mod = c('sGARCH',  'eGARCH',
          'gjrGARCH', 'apARCH', 'iGARCH')

all_data %>% 
  tidyr::spread(key = distribution, value = aic) %>% 
  mutate(p = NULL, q = NULL) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  arrange(match(model, mod )) %>% 
  flextable() %>% 
  autofit() %>%
  theme_vanilla() %>% 
  merge_v(j=1) %>% 
  merge_h(part = 'header') %>% 
  align(align = 'center', part = 'all')

# Final model

spec <- rugarch::ugarchspec(
  variance.model = list(model = 'eGARCH',
                        garchOrder = c(2,1)
  ), 
  mean.model = list(armaOrder = c(0,0), include.mean = F),
  distribution.model = 'sstd'
)

fit <- rugarch::ugarchfit(spec = spec, 
                          data = na.omit(dta$ret.adjusted.prices))
fit
rugarch::plot(fit, which = 1)
rugarch::plot(fit, which = 9)

roll = ugarchroll(spec = spec, data = dta_xt, 
                  n.start = 3000, refit.every = 100)
plot(roll)


