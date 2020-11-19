#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

    ## Data Links =====
    url_covid = a("COVID-19 Data", href="https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip")
    url_policy = a("Policy Data", href="https://healthdata.gov/node/3281076/download")
    
    output$covid <- renderUI({
        tagList("Covid-19 Data/Estimates Link:", url_covid)
    })
    
    output$policy <- renderUI({
        tagList("Covid-19 Policy Link:", url_policy)
    })
    
    
    ## Data Tables =====
    output$table.covid.summary = DT::renderDataTable({
        datatable(data.covid.summary[,1:31], rownames = F) 
    })
    
    output$table.covid = DT::renderDataTable({
        datatable(data.covid, rownames = F) 
    })
    
    output$table.policy = DT::renderDataTable({
        datatable(data.policy[,1:6], rownames = F) 
    })

    ## Page 1. National Outcome Averages =====
    
    # Page 1 - Tab 1 - by Policy -----
    observe({
        policy_start_stop <- state.policy.list$start_stop[state.policy.list$policy_type == input$policy_name]
        updateSelectizeInput(
            session, "policy_start_stop",
            choices = policy_start_stop,
            selected = policy_start_stop[1])
    })
    
    covid.national1 <- reactive({
        data.covid.summary %>% 
            filter(policy.name == input$policy_name & start.stop == input$policy_start_stop)
    }) 
    
    output$Map_by_policy = renderPlotly({
        
        policy.dates = get_policydates(data.policy,input$policy_name,input$policy_start_stop)
        
        policy.months = c()
        for (i in 1:nrow(state.names.list)){
            month.i = policy.dates$date[policy.dates$state_id == state.names.list$ID[i]]
            if (length(month.i)>0) {
                policy.months[i] = month(as.Date(month.i))
            }
            else {policy.months[i] = NA}
        }
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            lakecolor = toRGB('white')
        )
        plot_geo() %>%
            add_trace(
                z = ~policy.months, text = state.name, span = I(1),
                locations = state.abb, locationmode = 'USA-states'
            ) %>%
            colorbar(title = "Policy Onset Month",limits = c(2,12)) %>%
            layout(geo = g) 
    })
    
    output$Counts_by_policy = renderPlotly({
        ggplotly(covid.national1() %>% ggplot(aes(x=days,y=Cases,color=Observations)) +
                     geom_line(aes(y=confirmed.case_mean,color='Confirmed Cases')) +
                     geom_line(aes(y=new.deaths_mean,color='New Deaths')) +
                     geom_line(aes(y=ICU_mean,color='ICU')) +
                     geom_line(aes(y=newICU_mean,color='New ICU')) +
                     geom_line(aes(y=ICUover_mean,color='ICU over')) +
                     geom_line(aes(y=Ventilator_mean,color='Ventilator')) +
                     ylab("Cases") + xlab("Days After the Policy Onset") 
        )
    })
    
    output$Rates_by_policy = renderPlotly({
        ggplotly(covid.national1() %>% ggplot(aes(x=days,y=Rates,color=Observations)) +
                     geom_line(aes(y=confirmed.case.rate_mean,color='Confirmed Cases')) +
                     geom_line(aes(y=new.deaths.rate_mean,color='New Deaths')) +
                     geom_line(aes(y=ICU.rate_mean,color='ICU')) +
                     geom_line(aes(y=newICU.rate_mean,color='New ICU')) +
                     geom_line(aes(y=ICUover.rate_mean,color='ICU over')) +
                     geom_line(aes(y=Ventilator.rate_mean,color='Ventilator')) +
                     ylab("Rates: Changes in the case counts") + xlab("Days After the Policy Onset")
        )
    })
    
    # Page 1 - Tab2 - by Observations -----
    output$Box_Dates_by_Obs = renderPlotly({
        policy.dates = data.frame(state.id = data.policy$state_id[data.policy$policy.label %in% input$policy_select],
                                  date = data.policy$date[data.policy$policy.label %in% input$policy_select],
                                  policy.label = data.policy$policy.label[data.policy$policy.label %in% input$policy_select])
        give.n <- function(x){
            return(c(y = min(x)-7, label = length(x)))
        }
        ggplotly(
            policy.dates %>% 
                ggplot(aes(x=policy.label, y=date,color=policy.label)) +
                geom_boxplot() +
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
                ylab("Policy Onset Date") + 
                labs(color = "State Policies") +
                stat_summary(fun.data = give.n, geom = "text")
        )
    })
    
    covid.national2 <- reactive({
        covid.national2i <- data.frame()
        for (i in 1:length(input$policy_select)){
            temp <- data.covid.summary[data.covid.summary$policy.label == input$policy_select[i],]
            covid.national2i <- rbind(covid.national2i,temp)
        }
        covid.national2i
    })
    
    output$Counts_by_Observation = renderPlotly({
        covid.national2_1 <- covid.national2()
        covid.national2_1 <- select(covid.national2_1,days,paste0(input$observation,'_mean'),policy.label)
        ggplotly(
            covid.national2_1 %>%
                ggplot(aes_string(x="days",y=colnames(covid.national2_1)[2],color="policy.label")) +
                geom_line() + 
                theme(legend.position = c(1, 1)) + 
                labs(color = "State Policies") +
                ylab("Cases") + xlab("Days After the Policy Onset")
        )
    })
    
    output$Rates_by_Observation = renderPlotly({
        covid.national2_1 <- covid.national2()
        covid.national2_1 <- select(covid.national2_1,days,paste0(input$observation,'.rate_mean'),policy.label)
        ggplotly(
            covid.national2_1 %>%
                ggplot(aes_string(x="days",y=colnames(covid.national2_1)[2],color="policy.label")) +
                geom_line() + 
                theme(legend.position = c(1, 1)) + 
                labs(color = "State Policies") +
                ylab("Rates: Changes in the case counts") + xlab("Days After the Policy Onset")
        )
    })
    
    ## Page 2. State vs. National Averages =====
    state_id_state <- reactive({
        state.names.list$ID[state.names.list$Name == input$state]
    })
    
    observe({
        state_id = state_id_state()
        policy_name_state <- sort(unique(data.policy$policy_type[data.policy$state_id == state_id]))
        updateSelectizeInput(
            session, "policy_name_state",
            choices = policy_name_state,
            selected = policy_name_state[1])
    })
    
    observe({
        policy_start_stop_state <- state.policy.list$start_stop[state.policy.list$policy_type == input$policy_name_state]
        updateSelectizeInput(
            session, "policy_start_stop_state",
            choices = policy_start_stop_state,
            selected = policy_start_stop_state[1])
    })
    
    policy_label_state <- reactive({
        paste0('[',toupper(input$policy_start_stop_state),'] ',
               input$policy_name_state)
    })
    policy_label_all <- reactive({
        sort(paste0('[',toupper(state.policy.list$start_stop),'] ',
                    state.policy.list$policy_type))
    })
    
    # Make ranked box output
    ranked_stats <- reactive({
        state_id_rank = state_id_state()
        policy_label_rank = policy_label_state()
        policy_dates_rank = data.frame(state.id = data.policy$state_id[data.policy$policy.label == policy_label_rank],
                                      date = data.policy$date[data.policy$policy.label == policy_label_rank],
                                      policy.label = data.policy$policy.label[data.policy$policy.label == policy_label_rank])
        data_covid_rank = data.frame()
        for (i in 1:length(policy_dates_rank$date)){
            state_name_i = state.names.list$Name[state.names.list$ID == policy_dates_rank$state.id[i]]
            temp = data.covid[data.covid$state.name == state_name_i &
                                  data.covid$date >= policy_dates_rank$date[i] &
                                  data.covid$date <= (policy_dates_rank$date[i]+30),]
            temp = mutate(temp, days = c(0:30))
            data_covid_rank = rbind(data_covid_rank,temp)
        }
        temp.colnames = colnames(data_covid_rank)
        idx.colnames = c(1:length(temp.colnames))
        idx.temp = idx.colnames[temp.colnames==input$observation_state]
        #temp = data_covid_rank %>% 
        #     select(state.name,confirmed.case)
        temp = data.frame(state.name = data_covid_rank[,1],
                          observation.temp = data_covid_rank[,idx.temp])
        temp %>% group_by(state.name) %>%
            summarise(mean = mean(observation.temp,na.rm = T), 
                      peak = max(observation.temp,na.rm = T)) %>%
            arrange(-mean)
    })
    
    output$all_stat1 = renderText({
        ranked.stats = ranked_stats()
        all.stat1 = as.integer(mean(ranked.stats$mean,na.rm=T)) 
        paste0("Mean: ", as.character(all.stat1))
    })
    
    output$all_stat12 = renderText({
        ranked.stats = ranked_stats()
        all.stat12 = as.integer(mean(ranked.stats$peak,na.rm=T)) 
        paste0("Peak: ", as.character(all.stat12))
    })
    
    output$state_rank0 = renderText({
        ranked.stats = ranked_stats()
        paste0("Rank ", as.character(row.names(ranked.stats)[ranked.stats$state.name == input$state]))
    })
    
    output$state_label0= renderText({
        input$state[1]
    })
    
    output$state_stat0 = renderText({
        ranked.stats = ranked_stats()
        state.stat0 = as.integer(ranked.stats$mean[ranked.stats$state.name == input$state]) 
        paste0("Mean: ", as.character(state.stat0))
    })
    
    output$state_stat02 = renderText({
        ranked.stats = ranked_stats()
        state.stat02 = as.integer(ranked.stats$peak[ranked.stats$state.name == input$state] )
        paste0("Peak: ", as.character(state.stat02))
    })
     
    output$state_label1= renderText({
        ranked.stats = ranked_stats()
        ranked.stats$state.name[1]
    })
    
    output$state_stat1 = renderText({
        ranked.stats = ranked_stats()
        paste0("Mean: ", as.character(as.integer(ranked.stats$mean[1])))
    })
    
    output$state_stat12 = renderText({
        ranked.stats = ranked_stats()
        paste0("Peak: ", as.character(as.integer(ranked.stats$peak[1])))
    })
    
    output$state_label2= renderText({
        ranked.stats = ranked_stats()
        ranked.stats$state.name[2]
    })
    
    output$state_stat2 = renderText({
        ranked.stats = ranked_stats()
        paste0("Mean: ", as.character(as.integer(ranked.stats$mean[2])))
    })
    
    output$state_stat22 = renderText({
        ranked.stats = ranked_stats()
        paste0("Peak: ", as.character(as.integer(ranked.stats$peak[2])))
    })
    
    output$state_label3= renderText({
        ranked.stats = ranked_stats()
        ranked.stats$state.name[3]
    })
    
    output$state_stat3 = renderText({
        ranked.stats = ranked_stats()
        paste0("Mean: ", as.character(as.integer(ranked.stats$mean[3])))
    })
    
    output$state_stat32 = renderText({
        ranked.stats = ranked_stats()
        paste0("Peak: ", as.character(as.integer(ranked.stats$peak[3])))
    })
    
    # Make a map comparisons of the stats
    output$Map_state = renderPlotly({
        ranked.stats = ranked_stats()
    
        rank.mean = c()
        rank.state = c()
        for (i in 1:nrow(state.names.list)){
            mean.i = ranked.stats$mean[ranked.stats$state.name == state.names.list$Name[i]]
            rank.state[i] = state.names.list$Name[i]
            if (length(mean.i)>0) {
                rank.mean[i] = as.integer(mean.i)
            }
            else {rank.mean[i] = NA}
        }
        
                g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            lakecolor = toRGB('white')
        )
        plot_geo() %>%
            add_trace(
                z = ~log10(rank.mean), text = state.name, span = I(1),
                locations = state.abb, locationmode = 'USA-states'
            ) %>%
            colorbar(title = "Mean Case Counts (Log10)") %>%
            layout(geo = g) 
    })
    
    # Make state vs. national average plots
    output$Box_Dates_state = renderPlotly({
        state_id = state_id_state()
        policy.label.all = policy_label_all()
        policy.dates.all = data.frame(state.id = data.policy$state_id[data.policy$policy.label %in% policy.label.all],
                                      date = data.policy$date[data.policy$policy.label %in% policy.label.all],
                                      policy.label = data.policy$policy.label[data.policy$policy.label %in% policy.label.all])
        policy.dates.state = 
            data.frame(state.id = data.policy$state_id[data.policy$policy.label %in% policy.label.all &
                                                           data.policy$state_id == state_id],
                       date = data.policy$date[data.policy$policy.label %in% policy.label.all &
                                                   data.policy$state_id == state_id],
                       policy.label = data.policy$policy.label[data.policy$policy.label %in% policy.label.all &
                                                                   data.policy$state_id == state_id])
        give.n <- function(x){
            return(c(y = min(x)-7, label = length(x)))
        }
        ggplotly(
            policy.dates.all %>% 
                ggplot(aes(x=policy.label, y=date, color=policy.label)) +
                geom_boxplot(alpha=0.9) +
                stat_summary(fun.data = give.n, geom = "text") +
                geom_point(data = policy.dates.state, 
                           aes(x=policy.label, y=date, color=state_id), 
                           shape=18, size=2) + 
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
                ylab("Policy Onset Date")  +
                labs(color = "State Policies") 
        )
    })
    
    output$Counts_state = renderPlotly({
        # select national average
        state_id = state_id_state()
        policy.label.state = policy_label_state()
        covid.national.state <- data.covid.summary %>% 
            filter(policy.label == policy.label.state) %>%
            select(days,paste0(input$observation_state,'_mean'),paste0(input$observation_state,'_CI95'))
        policy.date.state = data.policy$date[
            data.policy$state_id == state_id & data.policy$policy.label == policy.label.state]
        covid.state <- data.covid %>% 
            filter(state.name == input$state &
                       date >= policy.date.state & date <= policy.date.state+30) %>%
            select(input$observation_state) %>%
            mutate(days = c(0:30))
            
        ggplotly(
            covid.national.state %>%
                ggplot(aes_string(x="days",y=colnames(covid.national.state)[2])) +
                geom_ribbon(aes_string(ymin=paste0(colnames(covid.national.state)[2],"-",colnames(covid.national.state)[3]),
                                       ymax=paste0(colnames(covid.national.state)[2],"+",colnames(covid.national.state)[3])),
                            alpha=0.2) +
                geom_line(aes(color="National Average")) + 
                geom_line(data=covid.state,aes_string(x="days",y=input$observation_state,color="state_id")) +
                theme(legend.position = c(1, 1)) + 
                labs(color = "State vs. National Average") +
                ylab("Cases") + xlab("Days After the Policy Onset")
        )
    })
    
    output$Rates_state = renderPlotly({
        # select national average
        state_id = state_id_state()
        policy.label.state = policy_label_state()
        covid.national.state1 <- data.covid.summary %>% 
            filter(policy.label == policy.label.state) %>%
            select(days,paste0(input$observation_state,'.rate_mean'),paste0(input$observation_state,'.rate_CI95'))
        policy.date.state = data.policy$date[
            data.policy$state_id == state_id & data.policy$policy.label == policy.label.state]
        covid.state1 <- data.covid %>% 
            filter(state.name == input$state &
                       date >= policy.date.state & date <= policy.date.state+30) %>%
            select(paste0(input$observation_state,'.rate')) %>%
            mutate(days = c(0:30))
        
        ggplotly(
            covid.national.state1 %>%
                ggplot(aes_string(x="days",y=colnames(covid.national.state1)[2])) +
                geom_ribbon(aes_string(ymin=paste0(colnames(covid.national.state1)[2],"-",colnames(covid.national.state1)[3]),
                                       ymax=paste0(colnames(covid.national.state1)[2],"+",colnames(covid.national.state1)[3])),
                            alpha=0.2) +
                geom_line(aes(color="National Average")) + 
                geom_line(data=covid.state1,aes_string(x="days",y=colnames(covid.state1)[1],color="state_id")) +
                theme(legend.position = c(1, 1)) + 
                labs(color = "State vs. National Average") +
                ylab("Rates: Changes in the case counts") + xlab("Days After the Policy Onset")
        )
    })
})
