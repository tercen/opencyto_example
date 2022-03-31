library(tercen)
library(dplyr)
library(openCyto)
library(data.table)
library(flowWorkspace)
library(ncdfFlow)

flowset<-data_get

flowset_to_data = function(flowset) {
  data_fcs = exprs(flowset)
 
  names_parameters =  parameters(data_fcs)
  data = as.data.frame(exprs(data_fcs))
  col_names = colnames(data)
 
  data %>%
    mutate_if(is.logical, as.character) %>%
    mutate_if(is.integer, as.double) %>%
    mutate(.ci = as.integer(rep_len(0, nrow(.)))) %>%
    mutate(filename = rep_len(basename(filename), nrow(.)))
}



options("tercen.workflowId" = "0c9301e340e4822bf28879ed28006d54")
options("tercen.stepId"     = "c4f45c00-942a-475c-9bd5-2548e7f299a9")

getOption("tercen.workflowId")
getOption("tercen.stepId")

ctx <- tercenCtx()

data <- ctx %>% 
  as.matrix() %>%
  t()

colnames(data) <- ctx$rselect()[[1]]

flow.dat <- flowCore::flowFrame(as.matrix(data))
flow.set<-flowCore::flowSet(flow.dat)

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

data_get<-gh_pop_get_data(gs,"singlets")

data_get  %>%
  bind_rows() %>%
  ctx$addNamespace() %>%
  ctx$save()
