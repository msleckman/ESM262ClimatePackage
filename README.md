# ESM262_ClimatePackage

Initial brainstorm of functions 

### Function 1: basic

Facet wrap plots of max/min temperature by seasonality 


### Function 2: multi-dimentional arrays

M-array where the dimensions are 
list of years 
columns months 
rows days 
number is daily precip 

array(dim = c(day, month, year))
array(dim = c(31, 12, ~50))


### Function 3: Temperature function 

make different dataframes for santa barbara species 
and then set suitability values based on temp and precipitaiton values
if heat is great/less than temp_stress_index = 3
if heat is great/less than temp_stress_index = 2
if heat is great/less than temp_stress_index = 1

### Function 4:  Water Stress Index  

Something like: 
if no precip for preceeding 5 days, than water_stress_index = 3
if no precip for preceeding more than 5 days, than water_stress_index = 2
if no precip for preceeding 2 days, than water_stress_index = 1


### Function 5:Impact metric 

If the temp average for consecutive days is above threshold, cost = $1000 to the city 
something like, the higher the temperature, the higher the cost of air conditioning.

