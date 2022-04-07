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

#PARAMETERS
First_param <- "FSC-A"
if(!ctx$op.value('FowardScatter') == "") First_param <- ctx$op.value('FowardScatter')

Second_param <- "SSC-A"
if( !ctx$op.value('SideScatter') == "") Second_param <- ctx$op.value('SideScatter')

Third_param <- "FSC-A,FSC-H"
if(!ctx$op.value('FowardScatter biplot') == "") Third_param <- ctx$op.value('FowardScatter biplot')

Fourth_param <- "SSC-A,SSC-H"
if(!ctx$op.value('SideScatter biplot') == "") Fourth_param <- ctx$op.value('SideScatter biplot')

#RANGE
First_range <- ""
if(ctx$op.value('FowardScatter gate range') == First_range) First_range <- paste("gate_range=c(",ctx$op.value('FowardScatter gate range'),")",sep="")

Second_range <- ""
if(ctx$op.value('SideScatter gate range') == Second_range) Second_range <- paste("gate_range=c(",ctx$op.value('SideScatter gate range'),")",sep="")


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
                      dims = First_param,
                      gating_method = "gate_mindensity",
                      gating_args = First_range)

gs_add_gating_method(gs, alias = "nonDebris2",
                      pop = "+",
                      parent = "nonDebris",
                      dims = Second_param,
                      gating_method = "gate_mindensity",
                      gating_args = Second_range)

gs_add_gating_method(gs, alias = "singlets",
                     pop = "+",
                     parent = "nonDebris2",
                     dims = Third_param,
                     gating_method = "singletGate")

gs_add_gating_method(gs, alias = "singlets2",
                     pop = "+",
                     parent = "singlets",
                     dims = Fourth_param,
                     gating_method = "singletGate")

if (is.null(viable)){
  data_get <- gh_pop_get_data(gs,"singlets2")
} else{
  gs_add_gating_method(gs, alias = viable,
                       pop = "-",
                       parent = "singlets2",
                       dims = viable,
                       gating_method = "tailgate")
  
  data_get <- gh_pop_get_data(gs,viable)
}

filter_data <- data[,".ci"]%in% exprs(data_get)[,".ci"]

df <- data.frame(Openflag= ifelse(filter_data,"pass","fail"),.ci=  as.integer(data[,".ci"]))

df %>%
  ctx$addNamespace() %>%
  ctx$save()
