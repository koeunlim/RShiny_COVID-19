library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(shinythemes)
library(plotly)
library(googleVis)
library(tidyverse)
library(DT)
library(lubridate)
library(stringr)

# Load Data
data.covid = read.csv("./data/covid_Data_expanded_20201015.csv")
data.policy = read.csv("./data/state_policy_trimmed_20201021.csv")
data.covid.summary = read.csv("./data/covid_Data_summary_20201015.csv")
state.names.list = read.csv("./data/state_names_ID.csv")
state.policy.list = read.csv("./data/state_policy_list_20201015.csv")

data.covid$X = NULL
data.policy$X = NULL
data.covid.summary$X = NULL
state.names.list$X = NULL
state.policy.list$X = NULL

data.covid$date = as.Date(data.covid$date)
data.policy$date = as.Date(data.policy$date)

data.policy = data.policy[data.policy$state_id %in% state.names.list$ID,]
data.policy = data.policy[data.policy$policy_type %in%  state.policy.list$policy_type,]

state.names.list=state.names.list[order(state.names.list$Name),]

# shorten policy labels
data.policy$policy_type[grepl("Public Facing Businesses",data.policy$policy_type)] =
  "Mandate Face Mask Use In Public Facing Businesses"
data.policy$policy_type[grepl("In Public Spaces",data.policy$policy_type)] =
  "Mandate Face Mask Use In Public Spaces"
state.policy.list$policy_type[grepl("Public Facing Businesses",state.policy.list$policy_type)] =
  "Mandate Face Mask Use In Public Facing Businesses"
state.policy.list$policy_type[grepl("In Public Spaces",state.policy.list$policy_type)] =
  "Mandate Face Mask Use In Public Spaces"
data.covid.summary$policy.name[grepl("Public Facing Businesses",data.covid.summary$policy.name)] =
  "Mandate Face Mask Use In Public Facing Businesses"
data.covid.summary$policy.name[grepl("In Public Spaces",data.covid.summary$policy.name)] =
  "Mandate Face Mask Use In Public Spaces"

data.covid.summary = mutate(data.covid.summary, 
                           policy.label = paste0('[',toupper(data.covid.summary$start.stop),'] ',data.covid.summary$policy.name))

data.policy = mutate(data.policy,
                     policy.label = paste0('[',toupper(data.policy$start_stop),'] ',data.policy$policy_type))
                     
get_policydates = function(df,policy.name,start.stop){
  policy.dates = 
    data.frame(state_id = df$state_id[df$policy_type == policy.name &
                                        df$start_stop == start.stop],
               date = (df$date[df$policy_type == policy.name &
                                 df$start_stop == start.stop]))
  return(policy.dates)
}

get_coviddata = function(df,policy.dates,state.names){
  data.covid.sel = data.frame()
  for (i in 1:length(policy.dates[,1])){
    state = policy.dates$state_id[i]
    state.idx = as.integer(row.names(state.names)[state.names$ID == state])
    state_name = state.names$Name[state.idx]
    temp = df[df$state.name == state_name &
                df$date >= policy.dates$date[i] &
                df$date <= (policy.dates$date[i]+30),]
    
    temp = mutate(temp, days = c(0:30))
    data.covid.sel = rbind(data.covid.sel,temp)
  }
  return(data.covid.sel)
}

