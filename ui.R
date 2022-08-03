dashboardPage(
  
  # SKIN
  skin = "purple",
  
  
  
  # DASHBOARD HEADER
  dashboardHeader(
    title = "Premier League Analysis",
    titleWidth = 320
    
    
  ),
  dashboardSidebar(
    width = 320,
    sidebarMenu(
    
    menuItem("Top 10 Team Goal", tabName = "page1", icon = icon("arrow-up", lib = "glyphicon")),
    menuItem("Team Match Result Analysis", tabName = "page2", icon = icon("search", lib = "glyphicon")),
    menuItem("Standings", tabName = "page3", icon = icon("stats", lib = "glyphicon"))
    
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    tabItems(
      
      # PAGE 1
      
      tabItem(tabName = "page1",
        
        # ROW 1
        
        fluidRow(
          
          box(width = 2,
              sliderInput(inputId= "bins",
                          label = "Number of rows:",
                          min = 1,
                          max = 39,
                          value = 10,
                          step = 1)
              
               # radioButtons(inputId = "sort", 
               #             label = "Sorting",
               #            choices = c(top10_home_order %>% arrange(home_goals),
               #                        top10_home_order %>% arrange(-home_goals))),
               
              ),
          
             
          
          box(width = 10, 
              plotlyOutput(outputId = "plot1"))
        
          
        )
      ,
        
        # ROW 2
        
        fluidRow(
          
          box(width = 2,
              sliderInput(inputId = "bins1",
                          label ="Number of rows:",
                          min = 1,
                          max = 39,
                          value = 10),
                          step = 1),
          
          box(width = 10, 
              plotlyOutput(outputId = "plot2"))
          
          
        )
      
      ),
        
        tabItem(tabName = "page2",

          ## --- ROW 1: INPUT
          fluidRow(
          box(width = 12,
                selectInput(inputId = "input_team",
                          label = "Choose Team",
                          choices = unique(results$away_team)  )
                )
            ),                
                
        
          # ROW 2 PLOT
          
          fluidRow(

          box(width = 2,
              htmlOutput(outputId = "text")),
            
            
          box(width = 10,
              plotlyOutput(outputId = "plot3"))
          
        


          ),
          
          # ROW 3

          fluidRow(
            
       
            box(width = 12,
                plotlyOutput(outputId = "plot4"))
          )
          
        ),
      
      
        tabItem(tabName = "page3",
                
                ## --- ROW 1: INFO BOX
                fluidRow(
                  valueBoxOutput("box"),
                  valueBoxOutput("box1"),
                  valueBoxOutput("box2")
                  
                  # infoBox(width = 6,
                  #         color = "black",
                  #         title = "Total Trending Channel",
                  #         icon = icon("headset"),
                  #         value = length(unique(vids_clean$channel_title)))
                ),
                
                ## --- ROW 2: INPUT
                fluidRow(
                  box(width = 12,
                      selectInput(inputId = "input_year",
                                  label = "Choose Season",
                                  choices = unique(results$season))
                  )
                ),
                
                ## --- ROW 3: PLOT
                
                fluidRow(
                  
                  
                  box(width = 12,
                      plotlyOutput(outputId = "plot5"))
                )
                
                
                
                
                
                
                
                )
      
    
      
      
      
      
    )
    
    
  )
  
  
)


