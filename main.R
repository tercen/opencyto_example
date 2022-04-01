library(tercen)
library(dplyr)
library(openCyto)
library(data.table)
library(flowWorkspace)
library(ncdfFlow)
library(ggcyto)

ctx <- tercenCtx()

viable <- NULL
if(!is.null(ctx$op.value('viable')) && !ctx$op.value('viable') == "NULL") viable <- ctx$op.value('viable')

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
                     dims = "FSC-A",
                     gating_method = "gate_mindensity")

gs_add_gating_method(gs, alias = "singlets",
                     pop = "+",
                     parent = "nonDebris",
                     dims = "FS-A,SS-A",
                     gating_method = "singletGate")
if (is.null(viable)){
  data_get <- gh_pop_get_data(gs,"singlets")
} else{
  gs_add_gating_method(gs, alias = viable,
                       pop = "-",
                       parent = "singlets",
                       dims = viable,
                       gating_method = "gate_mindensity")
  
  data_get <- gh_pop_get_data(gs,viable)
}

filter_data <- data[,".ci"]%in% exprs(data_get)[,".ci"]

df <- data.frame(Openflag= ifelse(filter_data,"pass","fail"),.ci=  data[,".ci"])

df %>%
  ctx$addNamespace() %>%
  ctx$save()
