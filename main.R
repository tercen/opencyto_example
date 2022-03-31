library(tercen)
library(dplyr)
library(openCyto)

options("tercen.workflowId" = "f15e43167d5523d4b1b1f2cdbf002ca1")
options("tercen.stepId"     = "83ce5d1b-1be4-4991-b458-0f94f88d3b62")

getOption("tercen.workflowId")
getOption("tercen.stepId")

ctx <- tercenCtx()

ctx  %>%
  select(.y, .ci, .ri) %>%
  group_by(.ci, .ri) %>%
  summarise(mean = mean(.y)) %>%
  ctx$addNamespace() %>%
  ctx$save()
