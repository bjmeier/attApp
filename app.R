library(shiny)
library(leaflet)
library(zoo)
library(magrittr)
library(plyr)
library(RColorBrewer)
library(colorRamps)
library(grDevices)
library(stats)
library(graphics)
library(reshape)
library(reshape2)
library(ggplot2)
library(DT)
library(moments)
source("helpers.R")

att=read.csv('SoccerAttendance.csv')
attIn=att[c(3,1,4,2)]

attLeague=ddply(att,.(League,Year),summarize,
        numTeams      =length(Attendance),
        Mean      =round(mean    (Attendance),0),
        Sd        =round(sd      (Attendance),0),
        Skew      =round(skewness(Attendance),2),
        Kurt      =round(kurtosis(Attendance),2),
        Min       =min(Attendance),
        LowerQuart=round(quantile(Attendance,probs=0.25),0),
        Median    =round(median  (Attendance),0),
        UpperQuart=round(quantile(Attendance,probs=0.75),0),
        Max   =max(Attendance)
        )

attMapIn=read.csv('Stadium.csv')

resInfo=join(x=attMapIn,y=attIn, by = 'Club',type='full')
resInfo$addStr=NULL
resInfo$Add1=NULL
resInfo$Add2=NULL
resInfo$Add3=NULL
resInfo=resInfo[c(2,1,7,6,3,4,5)]

att=melt(att,id=c("Club","League","Year"))
att=cast(att,Year~Club)
att[is.na(att)]=0
yrS=att$Year[1]
att$Year=NULL

attTs=ts(data=att,start=yrS,frequency=1)
attTs=zoo(attTs)
attChTs=diff(attTs,na.pad=TRUE)/(0.001+0.5*lag(attTs,k=-1,na.pad=TRUE)+0.5*attTs)
x=attTs
xA=attChTs

ui <- bootstrapPage(title="Association Football Attendance",
                    
                   tabsetPanel(
                      
                      tabPanel("Map",
                               tags$style(type="text/css","html,body,#mymap {height: 100%;width: 100%}"),
                               leafletOutput('mymap'),
                               absolutePanel(top=00,right=10,
                                             draggable=TRUE,
                                             selectInput("series","Attendance (att) or change in Attendance
                            (att[yr]-att[yr-1])/(att[yr]+att[yr-1])/2:",
                                                         c("Attendance","Change in Attendance"),
                                                         width='316'),
                                             sliderInput("Range","Year Season Started:",
                                                         min(index(attTs)),
                                                         max(index(attTs)),
                                                         value=max(index(attTs)),
                                                         step=1,
                                                         sep="",
                                                         animate=animationOptions(interval=500))
                               )
                      ),
                      tabPanel("Box and Jitter Plot",
                               sidebarLayout(
                                 sidebarPanel(
                                   sliderInput("plotSl","Year Season Started:",
                                               min(index(attTs)),
                                               max(index(attTs)),
                                               value=c(min(index(attTs)),max(index(attTs))),
                                               ##animate=TRUE,
                                               dragRange=TRUE,
                                               step=1,
                                               sep="",
                                               animate=animationOptions(interval=1000)
                                   ),
                                   checkboxGroupInput(
                                     inputId="leagueChk",
                                     label="Check Leagues to View:",
                                     choices=c("Bundlesliga","EPL","La Liga","MLS","Serie A"),
                                     selected=c("Bundlesliga","EPL","La Liga","MLS","Serie A")
                                   ),
                                   dataTableOutput("infoBr")
                                 ),
                                 mainPanel(
                                   plotOutput("plotBox",
                                              click="plot_click",
                                              brush=brushOpts(
                                                id="plot_brush",
                                                resetOnNew=TRUE),
                                              dblclick="plot_dblclick"),
                                   dataTableOutput("info")
                                 )
                               )
                      ),
                      tabPanel("Club Line Plot",
                               sidebarLayout(
                                 sidebarPanel(
                                   checkboxGroupInput(
                                     inputId="leagueChkLi",
                                     label="Check Leagues to View:",
                                     choices=c("Bundlesliga","EPL","La Liga","MLS","Serie A"),
                                     selected=c("Bundlesliga","EPL","La Liga","MLS","Serie A")
                                   ),
                                   dataTableOutput("infoClBrush")
                                 ),
                                 mainPanel(
                                   plotOutput("linePlot",
                                              click="plot_clickLi",
                                              dblclick="linePlot_dblclick",
                                              brush=brushOpts(
                                                id="plot_brushLi",
                                                resetOnNew=TRUE)),
                                   dataTableOutput("infoClLine")
                                 )
                               )
                      ),
                      tabPanel("Club Data Table",
                               fluidPage(DT::dataTableOutput("sumInfo"))
                      ),
                      tabPanel("League Data Table",
                               fluidPage(DT::dataTableOutput("sumInfoLeague"))
                      ),
                      tabPanel("Notes",fluidPage(
                               uiOutput("sources")))
                    )
)


server <- function(input, output, session) {
  output$sources=renderUI(HTML(
    "<br>
     <strong>Sources:</strong><br>
     Wikipedia:  <a href='https://en.wikipedia.org/wiki/Major_League_Soccer_attendance' target='_blank'>MLS attendance</a><br>
     Wikipedia:  <a href='https://en.wikipedia.org/wiki/List_of_Major_League_Soccer_stadiums' target='_blank'>MLS stadiums</a><br>
     Statbunker: <a href='http://www.statbunker.com/' target='_blank'>European attendance and stadiums</a><br>
     Doogal:     <a href='http://www.doogal.co.uk/FootballStadiums.php' target='_blank'>EPL stadium geocodes</a><br>
    Google was used to search for stadium addresses.<br>
    For other leagues, a combination of the above links and Google were used to find stadium addresses.
Geocodes were first attempted to be obtained with ggmap::geocode.<br>For those not found using ggmap::geocode, 
     <a href='http://mygeoposition.com/' target='_blank'> mygeoposition</a> was used.<br><br>
    <strong>Definitions:</strong><br>
    Attendance: Average home attendance for a given club in a given season.<br>
    Season year:  Year in which a season started.  <br>
    &nbsp For MLS clubs, this is also the year it finished.<br> 
    &nbsp For European clubs, the season finished in following year -- 2010 is the 2010/2011 season.<br><br>
    <em>All data was obtained in September, 2015.</em>
    "
      
  )
  )
  ranges <- reactiveValues(x = NULL, y = NULL)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  output$linePlot = renderPlot({
    attInLi=subset(attIn,attIn$League %in% input$leagueChkLi)
    if (length(unique(attInLi$League))>1){
      p=ggplot(attInLi,aes(x=Year,y=Attendance))
      p=p+geom_line(aes(color=League,group=Club))    
      p=p+geom_point(aes(color=Club))
      p=p+ theme(legend.position="none")
      p=p+coord_cartesian(xlim=ranges$x,ylim=ranges$y)
      p=p+xlab("Brush and double click to zoom, double click to reset.  Brush or click to show data.")
    }
    else
    {
      p=ggplot(attInLi,aes(x=Year,y=Attendance))
      p=p+geom_point(aes(color=Club))
      p=p+geom_line(aes(color=Club,group=Club))
      p=p+guides(col=guide_legend(nrow=20,byrow=FALSE))
      p=p+coord_cartesian(xlim=ranges$x,ylim=ranges$y)
      p=p+xlab("Brush and double click to zoom, double click to reset.  Brush or click to show data.")
    }
    return(p)
  })
  observeEvent(input$linePlot_dblclick, {
    brush <- input$plot_brushLi
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  output$infoClLine=renderDataTable({
    res=nearPoints(attIn,input$plot_clickLi)
    if(nrow(res)==0)
      return()
    res
  })
  output$infoClBrush=renderDataTable({
    res=brushedPoints(attIn,input$plot_brushLi)
    if(nrow(res)==0)
      return()
    res
  })
  
  colorpal=reactive({
    if(input$series=="Attendance")
    {return(colorNumeric(palette="YlOrRd",
                         domain=c(0,90000)))}
    else
    {return(colorNumeric(palette=colorRamp(c("blue4","blue4",
                                             "blue4","blue4",
                                             "blue4","blue4",
                                             "blue4","blue4",
                                             "blue4","blue4",
                                                    "blue", 
                                             "white",
                                             "red",
                                             "red4","red4",
                                             "red4","red4",
                                             "red4","red4",
                                             "red4","red4",
                                            "red4","red4")),
                         domain=c(-2,2)))}
  })
  
  attMap=reactive({
    serStr=input$series
    Time=input$Range
    x=attTs
    if(input$series=="Attendance")
      xA=attTs
    else
      xA=attChTs
    i=as.integer(1+Time-yrS)
    return(getAttMap(x,xA,i,serStr,attMapIn))
  })
  
  output$mymap <- renderLeaflet({
    leaflet(data=attMap) %>% addTiles()
  })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)

  output$plotBox = renderPlot({
    yr1=input$plotSl[1]
    yr2=input$plotSl[2]
    attInSl=subset(attIn,attIn$Year>=yr1)
    attInSl=subset(attInSl,attInSl$Year<=yr2)
    attInSl=subset(attInSl,attInSl$League %in% input$leagueChk)
    p=ggplot(attInSl,aes(League,Attendance))
    p=p+geom_boxplot()
    p=p+geom_jitter(aes(colour=League),position=position_jitter(height=0))
    p=p+facet_grid(~Year)
    if (yr2-yr1>3)
      p=p+scale_x_discrete(labels=NULL)
    p=p+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
    p=p+labs(x="Brush and double click to zoom, double click to reset.  Select all leagues to enable bush and click link to data.")
    return(p)
  })
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  output$info=renderDataTable({
    res=nearPoints(attIn,input$plot_click)
    if(nrow(res)==0)
      return()
    if (length(input$leagueChk)<5)
      return()
    res
  })
  output$infoBr=renderDataTable({
    res=brushedPoints(attIn,input$plot_brush)
    if(nrow(res)==0)
      return()
    if (length(input$leagueChk)<5)
        return()
    res
  })
  
  output$sumInfo=DT::renderDataTable(
    resInfo, filter = 'top', 
    rownames=FALSE,
    options = list(
      pageLength = 25, 
      autoWidth = TRUE))
  
  output$sumInfoLeague=DT::renderDataTable(
    attLeague, filter = 'top',
    rownames=FALSE,
    options = list(
      pageLength = 25, 
      autoWidth = TRUE))
  
  observe({
    pal=colorpal()
    
    leafletProxy("mymap",data=attMap())%>%
      clearMarkers() %>%
      addCircleMarkers(radius= ~y^.5/15,
                       color=~pal(yA),
                       stroke=TRUE,
                       fillOpacity=.75,
                       popup = ~popup) %>%
      clearControls%>%
      addLegend(position="bottomright",
                pal=pal,
                values=~yA,
                title=input$series,
                bins=9)
  })    
}

shinyApp(ui=ui,server=server)
