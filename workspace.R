library(tercen)
library(dplyr)

options("tercen.workflowId" = "0add2df8c4543198d0b9ab7b55003e76")
options("tercen.stepId"     = "d732d42f-cb7d-4402-9639-ca768ccba766")

getOption("tercen.workflowId")
getOption("tercen.stepId")

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
 