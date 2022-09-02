if (!require(shiny))
  install.packages("shiny", repos = "http://cran.us.r-project.org")

if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")

if (!require(plyr))
  install.packages("plyr", repos = "http://cran.us.r-project.org")

if (!require(DT))
  install.packages("DT", repos = "http://cran.us.r-project.org")

if (!require(shinyWidgets))
  install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")


library(shiny)
library(tidyverse)
library(plyr)
library(DT)
library(shinyWidgets)


setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/data"))


# Load any data you plan to use here (the app will use its home folder as the working directory)

load("globalclass.Rdata")
load("globalpos.Rdata")

global.data=as.data.frame(globalpos) %>%
  dplyr::rename(Longitude=V1,
                Latitude=V2) %>%
  dplyr::mutate(ThermalRegion=globalclass)

cnames <- c("Northern Frigid",
            "Northern Cool",
            "Northern Temperate",
            "Northern Warm",
            "Northern Hot",
            "Tropical Hot",
            "Southern Hot",
            "Southern Warm",
            "Southern Temperate")

colinfo <- as.data.frame(rbind(c(4,	'NF',	102,	102,	102),
                               c(5,	'NC',	69,   117,	180),
                               c(9,	'NT',	116,	173,	209),
                               c(1,	'NW',	255,	217,	47),
                               c(2,	'NH',	255,	127,	0),
                               c(3,	'TH',	228,	26,	  28),
                               c(6,	'SH',	231,	138,	195),
                               c(8,	'SW',	153,	112,	171), 
                               c(7,	'ST',	84,	  39,	  136)))

colnames(colinfo) <- c("GroupNum", "GroupCode", "R","G","B")

cols <- apply(colinfo, 1, function(x){rgb(red=x[3], green=x[4], blue=x[5],, maxColorValue=255)})
#mc=c("#0000CD", "#1E90FF","#ADD8E6","#EEC900" ,"#FF8C00","#EE0000", "#EEA9B8","#9ACD32", "#228B22")[order(c(4, 5, 9, 1, 2, 3, 6, 8, 7))]
mc=cols[order(c(4, 5, 9, 1, 2, 3, 6, 8, 7))]

global.data$ThermalRegionName=factor(case_when(global.data$ThermalRegion == 4 ~ "Northern Frigid",
                                               global.data$ThermalRegion == 5 ~ "Northern Cool",
                                               global.data$ThermalRegion == 9 ~ "Northern Temperate",
                                               global.data$ThermalRegion == 1 ~ "Northern Warm",
                                               global.data$ThermalRegion == 2 ~ "Northern Hot",
                                               global.data$ThermalRegion == 3 ~ "Tropical Hot",
                                               global.data$ThermalRegion == 6 ~ "Southern Hot",
                                               global.data$ThermalRegion == 8 ~ "Southern Warm",
                                               global.data$ThermalRegion == 7 ~ "Southern Temperate"),
                                     levels=cnames)


world.map=map_data("world")

jwp.sites=read.csv("JWP_submitted_expected_lakes - lakes.csv")




zoop.data=read.csv("PoconosZoopData.csv") %>%
  mutate(Country=case_when(Site=="Giles" ~ "USA",
                           Site=="Lacawac" ~ "Canada",
                           Site=="Waynewood" ~ "France"),
         Continent=case_when(Country %in% c("USA","Canada") ~ "North America",
                             Country=="France" ~ "Europe"),
         Variable=case_when(Variable=="Abundance" ~ "Abundance (indiv./L)",
                            Variable=="ProportionInLayer" ~ "Proportion in Layer (%)"),
         Layer=case_when(Layer=="meta_hypolimnion" ~ "metalimnion + hypolimnion",
                         Layer=="whole_column" ~ "whole water column",
                         TRUE ~ Layer))




### Define UI for application
ui <- fluidPage(navbarPage("Global Zooplankton Data Explorer",
                           tabPanel("General Information",
                                    "Details about collaborations, contributors, data, sources, funding, etc.",
                                    
                                    tabPanel("Geographic Coverage",
                                             addSpinner(plotOutput(outputId="zoopmap",height = "600px",width="1200px"), spin="fading-circle"))
                                    
                           ),
                           
                           tabPanel("Data Exploration",
                                    sidebarLayout(
                                      
                                      sidebarPanel(
                                        
                                        radioButtons(inputId="zoopvariable",label="Select a variable:",
                                                     choices=unique(zoop.data$Variable)),
                                        
                                        conditionalPanel(condition = "input.zoopvariable == 'Proportion in Layer (%)'",
                                                         radioButtons(inputId="zooplayer",label="Select a layer:",
                                                                      choices=c("epilimnion","metalimnion + hypolimnion"))),
                                        
                                        selectInput(inputId="zooptaxon",label="Select a taxon:",
                                                    choices=unique(zoop.data$Genus)),
                                        
                                        radioButtons(inputId="zoopgrouping",label="Select a geographic scale:",
                                                     choices=c("Global (all)","by Continent","by Country")),
                                        
                                        conditionalPanel(condition = "input.zoopgrouping == 'by Continent'",
                                                         radioButtons(inputId="zoopcontinent",label="Select a continent:",
                                                                      choices=c("Africa","Asia","Australia","Europe","North America","South America"))),
                                        
                                        conditionalPanel(condition = "input.zoopgrouping == 'by Country'",
                                                         radioButtons(inputId="zoopcountry",label="Select a country:",
                                                                      choices=unique(zoop.data$Country))),
                                        
                                        radioButtons(inputId="graphtype",label="Select graphic type:",
                                                     choices=c("histogram","time series")),
                                        
                                        numericInput(inputId="mydata",label=HTML(paste("Input your data to compare",
                                                                                       "(ensure compatible units!):",sep="<br/>")),
                                                     value=0,min=0),
                                        
                                        conditionalPanel(condition = "input.graphtype == 'time series'",
                                                         numericInput(inputId="myyear",label="Input year data was collected:",
                                                                      value=2022)),
                                        
                                        actionButton(inputId="zoopaction", label = "Go!")
                                      ),
                                      
                                      # Main panel typically used to display outputs
                                      mainPanel(
                                        # save a spot for your plots in the UI here
                                        tabPanel("Zooplankton Graph",plotOutput(outputId="zoopgraph",height = "450px")),
                                        
                                        tabPanel("Zooplankton Data Summary",dataTableOutput(outputId="zooptable")),
                                        
                                        uiOutput("zoopdownload")
                                        
                                       )
                                    )
                           )
))

                
                
### Define server behavior for application here
server <- function(input, output) {
  
  # put code for reactive elements here
  
  zoopgraph.func <- eventReactive(input$zoopaction, {
    
    if(input$zoopgrouping=="Global (all)"){
      zoop.sub=zoop.data %>%
        filter(Variable==input$zoopvariable,
               Genus==input$zooptaxon)
    }
    
    if(input$zoopgrouping=="by Continent"){
      zoop.sub=zoop.data %>%
        filter(Variable==input$zoopvariable,
               Continent==input$zoopcontinent,
               Genus==input$zooptaxon)
    }
    
    if(input$zoopgrouping=="by Country"){
      zoop.sub=zoop.data %>%
        filter(Variable==input$zoopvariable,
               Country==input$zoopcountry,
               Genus==input$zooptaxon)
    }
    
    if(input$zoopvariable=="Proportion in Layer (%)"){
      zoop.sub=zoop.sub %>%
        filter(Layer==input$zooplayer)
    }
    
    if(nrow(zoop.sub)==0){
      
      p1 <- ggplot() +
        geom_blank() +
        labs(title="No data available for current selection") +
        theme_bw() +
        theme(plot.title=element_text(size=20,face=4),
              panel.background=element_blank(),
              panel.border=element_blank())
      
    }else{
      if(input$graphtype=="histogram"){
        p1 <- ggplot() +
          geom_histogram(data=zoop.sub,aes(Value),fill="grey70",color="black") +
          geom_vline(xintercept=input$mydata,linetype=2,size=1,color="red3") +
          scale_y_continuous(expand=expand_scale(mult=c(0, 0.05))) +
          labs(x="Count",y=input$zoopvariable) +
          theme_bw() +
          theme(axis.text=element_text(size=15,color="black"),
                axis.title=element_text(size=15,color="black"),
                legend.text=element_text(size=15,color="black"),
                legend.title=element_text(size=15,color="black"),
                strip.text=element_text(size=15,color="black"))
      }
      
      if(input$graphtype=="time series"){
        p1 <- ggplot() +
          geom_smooth(data=zoop.sub,aes(x=Year,y=Value),color="black",span=0.95) +
          geom_point(data=zoop.sub,aes(x=Year,y=Value),fill="grey70",color="black",shape=21,size=2) +
          geom_point(aes(x=input$myyear,y=input$mydata),size=6,shape=23,fill="red3",color="black") +
          labs(x="Year",y=input$zoopvariable) +
          theme_bw() +
          theme(axis.text=element_text(size=15,color="black"),
                axis.title=element_text(size=15,color="black"),
                legend.text=element_text(size=15,color="black"),
                legend.title=element_text(size=15,color="black"),
                strip.text=element_text(size=15,color="black"))
      }
      
    }
    print(p1)
  })
  
  
  zooptable.func <- eventReactive(input$zoopaction, {
    
    if(input$zoopgrouping=="Global (all)"){
      zoop.sub=zoop.data %>%
        filter(Variable==input$zoopvariable,
               Genus==input$zooptaxon)
    }
    
    if(input$zoopgrouping=="by Continent"){
      zoop.sub=zoop.data %>%
        filter(Variable==input$zoopvariable,
               Continent==input$zoopcontinent,
               Genus==input$zooptaxon)
    }
    
    if(input$zoopgrouping=="by Country"){
      zoop.sub=zoop.data %>%
        filter(Variable==input$zoopvariable,
               Country==input$zoopcountry,
               Genus==input$zooptaxon)
    }
    
    if(input$zoopvariable=="Proportion in Layer (%)"){
      zoop.sub=zoop.sub %>%
        filter(Layer==input$zooplayer)
    }
    
    if(nrow(zoop.sub)==0){
      
    }else{
      zoop.summary=zoop.sub %>%
        summarize(Min=format(min(Value,na.rm=T),digits=2,nsmall=2),
                  Q25=format(quantile(Value,0.25,na.rm=T),digits=2,nsmall=2),
                  Median=format(median(Value,na.rm=T),digits=2,nsmall=2),
                  Mean=format(mean(Value,na.rm=T),digits=2,nsmall=2),
                  Q75=format(quantile(Value,0.75,na.rm=T),digits=2,nsmall=2),
                  Max=format(max(Value,na.rm=T),digits=2,nsmall=2),
                  N=NROW(which(!is.na(Value))),
                  Percentile=format(ecdf(Value)(input$mydata)*100,digits=2,nsmall=2))
    }
  })
  
  output$zoopmap <- renderPlot({
    
    ggplot() +
      geom_point(data=global.data,aes(x=Longitude,y=Latitude,color=ThermalRegionName),
                 shape=15,size=3.5) +
      geom_path(data=world.map,aes(x=long,y=lat,group=group)) +
      geom_point(data=jwp.sites,aes(x=waterbody_lon_decdeg,y=waterbody_lat_decdeg),
                 shape=21,fill="white",color="black",size=2.5,stroke=1) +
      scale_color_manual(values=cols,name="Thermal Region:",na.translate=FALSE) +
      scale_x_continuous(breaks=seq(-180,180,by=20),labels=NULL) +
      scale_y_continuous(breaks=seq(-80,80,by=20),labels=NULL) +
      labs(x=NULL,y=NULL) +
      coord_cartesian(xlim=c(-165,175),ylim=c(-55,80)) +
      theme_bw() +
      theme(axis.text=element_text(size=15,color="black"),
            axis.title=element_text(size=15,color="black"),
            legend.text=element_text(size=15,color="black"),
            legend.title=element_text(size=15,color="black"),
            legend.position="none",
            legend.background=element_rect(color="black"),
            panel.grid=element_blank(),
            axis.line=element_blank(),
            axis.ticks=element_blank(),
            panel.border=element_blank(),
            panel.background=element_rect(fill="transparent"),
            plot.background=element_rect(fill="transparent"))  
  })
  
  output$zoopgraph <- renderPlot({
    zoopgraph.func()
  })
  
  output$zooptable <- renderDataTable({
    zooptable.func()
  })
  
  output$zoopdownload <- renderUI({
    req(input$zoopaction)
    downloadButton("datadownload", "Download Data")
  })
}




### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)