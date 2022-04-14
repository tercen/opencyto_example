# OpenCyto operator

##### Description

The `OpenCyto operator` 

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, measurement value 
`row`           | factor, channel to be used for gating
`column`        | factor, observation (rowID)

Input parameters|.
---|---
`input_viable`        | parameter description of the channel used for the viability gate (select negative population)

Output relations|.
---|---
`flag`        | pass / fail flag, per column

##### Details

The operator is a wrapper of the `gs_add_gating_method()` function from the
`OpenCyto` R/Bioconductor package.
