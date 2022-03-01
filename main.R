library(tercen)
library(dplyr)

logit = function(x, pct = FALSE){
  if(pct) x = x/100
  eps = 1e-6
  if(any(x<eps))   x[x<0.025] = 0.025
  if(any(x>1-eps)) x[x>0.975] = 0.975
  result = log(x/(1-x))
}

ctx <- tercenCtx()

ctx  %>% 
  select(.y, .ci, .ri) %>% 
  group_by(.ci, .ri) %>%
  summarise( my = mean(.y)) %>%
  mutate(logit_transformed = logit(my)) %>%
  select(.ri, .ci, logit_transformed) %>%
  ctx$addNamespace() %>%
  ctx$save()

 