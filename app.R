library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)

server <-function(input, output, session) {
  
  output$plot <-renderPlot({
    qplot(Installs,Reviews,data=googleplaystore)
  })
  diam <- reactive({
    user_brush<-input$user_brush
    sel<-brushedPoints(googleplaystore, user_brush)
    return(sel)
  })
  output$table <-DT::renderDataTable(DT::datatable(diam(),
                                                   rownames=F,
                                                   filter="top")%>%
                                       formatStyle("Rating",color="green"))
  output$mydownload<- downloadHandler(
    filename="plotextract.csv",
    content=function(file){
      write.csv(diam(),file)})
  
  output$plot1 <- renderPlot({
    dat <- googleplaystore[1:input$myslider,]
    ggplot(dat, aes(factor(Category), Rating))+ geom_boxplot(aes(fill = factor(Category)))+ theme(legend.position="bottom", axis.text.x=element_blank())+ labs(title="Distribution of rating per category", x="Category")
  })
  
  output$plot3 <- renderPlot({
    dat <- googleplaystore[1:input$myslider,]
    ggplot(data=dat,aes(x=Category, fill=Category))+geom_bar()+facet_grid(.~cut(Rating, breaks=5))+ theme(legend.position="bottom", axis.text.x=element_blank())+labs(title="General rating", y="reviews", x="rating")
  })    
  
  output$plot2 <- renderPlot({
    dat <- googleplaystore[1:input$myslider,]
    ggplot(data=dat,aes(x=cut(Rating, breaks=5), fill= Category))+geom_bar()+facet_grid(.~Type)+ theme(legend.position="bottom")+labs(title="General rating", y="reviews", x="rating")
    
  })
}
ui<- fluidPage(theme=shinytheme("sandstone"),
               titlePanel(strong("Google Play Store User Review")),
               sidebarLayout(
                 
                 sidebarPanel(
                   withTags(
                     div(
                       img(src="google.png", width="300px", height="300px"),
                       br(),br(),
                       h2("Play Your Heart Out",align = "center",style = "color:deepskyblue"),
                       br(),
                       hr(),
                       br(),
                       p(em("Google Play is a digital distribution service operated and developed by Google LLC.")),
                       p(em("It serves as the official app store for the Android operating system, 
                            allowing users to browse and download applications developed with the Android software development kit.")), 
                       p(em("Google Play also serves as a digital media store, offering music, books, movies, and television programs."))
                       
                       ))),
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("About",
                              h2(span("App developer", style = "color:red")),
                              hr(),
                              tags$img(src="IMG_0543.JPG",width="100px",height="100px"),
                              br(), br(),
                              p(strong("Ngoc B. Dao"), "is a Vietnamese student who are pursuing her Master's program in  ", 
                                em("Big Data"),"at",
                                strong("SGH Warsaw School of Economics.")),
                              p("Before Big Data, she had background in Finance and Accounting with 2 years experience working in a bank."),
                              p("For more information, visit her",
                                a("Linkedin.", 
                                  href = "https://www.linkedin.com/in/gemmie/")),
                              br(),
                              h2(span("Web App", style = "color:red")),
                              hr(),
                              p("This app is created as part of final project for subject", 
                                em("Querrying, data presentation, data visualisation"), 
                                "lead by", strong("Dr. Jaroslaw OLEJNICZAK.")),
                              p("This is the first application created by", em("Ngoc"), "using programing language R."), 
                              p("The dataset was taken from the", 
                                strong("Kaggle website"),
                                " which can be downloaded",
                                a("HERE.", 
                                  href = "https://www.kaggle.com/lava18/google-play-store-apps")
                                
                              )),
                     
                     tabPanel("Geogle Play", tags$video(src="video1.mp4",type="video/mp4",autoplay = F, controls = F,
                                                        width="700px", height="600px")),
                     
                     tabPanel("Kaggle Dataset", 
                              plotOutput("plot",width='100%', brush="user_brush"),
                              dataTableOutput("table"),
                              downloadButton(outputId ="mydownload", label="Download Table")),
                     
                     tabPanel("Data visualization", 
                              sliderInput("myslider", "Number of observations:", 1, 10841, 5000, width='100%'),
                              plotOutput("plot1",height = 500),
                              plotOutput("plot2",height = 500))
                   )
                 )))

shinyApp(ui=ui, server=server)
