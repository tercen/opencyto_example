library(tercen)
library(dplyr)
library(openCyto)
library(data.table)
library(flowWorkspace)
library(ncdfFlow)
library(ggcyto)

ctx <- tercenCtx()

data <- ctx %>% 
  as.matrix() %>%
  t()

colnames(data) <- ctx$rselect()[[1]]

data <- cbind(data,.ci =seq_len(nrow(data)) - 1)

flow.dat <- flowCore::flowFrame(as.matrix(data))
flow.set <- flowCore::flowSet(flow.dat)

gs <- GatingSet(flow.set)

gs_add_gating_method(gs, alias = "nonDebris",
                     pop = "+",
                     parent = "root",
                     dims = "FS-A",
                     gating_method = "gate_mindensity")

gs_add_gating_method(gs, alias = "singlets",
                     pop = "+",
                     parent = "nonDebris",
                     dims = "FS-A,SS-A",
                     gating_method = "singletGate")

data_get <- gh_pop_get_data(gs,"singlets")

filter_data <- data[,".ci"]%in% exprs(data_get)[,".ci"]

df <- data.frame(Openflag= ifelse(filter_data,"pass","fail"),.ci=  data[,".ci"])

df %>%
  ctx$addNamespace() %>%
  ctx$save()