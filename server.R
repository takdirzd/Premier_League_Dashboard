shinyServer(function(input, output) {
  
  # ----------------------------------- PLOT 1 ---------------------------------
  
  output$plot1 <- renderPlotly({
    
  
    
    top10_home <- results %>% 
                    group_by(home_team) %>% 
                    summarise(home_goals = sum(home_goals)) %>% 
                    ungroup() %>% 
                    arrange(-home_goals) 
    
                  
    
    top10_home_order <- top10_home %>% 
      mutate(
        label = glue(
          "Team: {home_team}
          Goal Count: {home_goals}"
        )
      ) 
  
    
    x <- as.numeric(rownames(top10_home_order))
    
    top10_home_order <- top10_home_order[1:which(input$bins==x),]
    
    plot1 <-  ggplot(data = top10_home_order, mapping = aes(x = home_goals , 
                                                            y = reorder(home_team, home_goals),
                                                            text = label)) +
      geom_col( aes(fill=home_goals), show.legend = F)  +
      scale_fill_continuous(low = "#037ffc", high = "#183654") +    #The color we use HEX Code of color
      geom_label(aes(label=home_goals)) + 

   
      labs(
        title = "Top 10 Home Team with Most Home Goal over 12 season (2006-2007 to 2017-2018)",
        subtitle = "From 12 season (2006-2007 to 2017-2018)",
        y = "Team",
        x = "Goal"
      ) +
      theme_minimal()
    
    ggplotly(plot1, tooltip = "text")


  })
  
  
  
  
  
  # 
  # 
  # 
  # --------------------------------------- PLOT 2 ---------------------------------

  output$plot2 <- renderPlotly({
    
    top10_away_order <- results %>% 
      group_by(away_team) %>% 
      summarise(away_goals = sum(away_goals)) %>% 
      ungroup() %>% 
      arrange(-away_goals)

    
    top10_away_order <- top10_away_order %>% 
      mutate(
        label = glue(
          "Team: {away_team}
          Goal Count: {away_goals}"
        )
      )
    
    x1 <- as.numeric(rownames(top10_away_order))
    
    top10_away_order <- top10_away_order[1:which(input$bins1==x1),]
    
    plot2 <-ggplot(data = top10_away_order, mapping = aes(x = away_goals ,
                                                          y = reorder(away_team, away_goals),
                                                          text = label),) +
      geom_col(aes(fill=away_goals), show.legend = F) +
      scale_fill_continuous(low = "#ff0000", high = "#592222") +     #The color we use HEX Code of color
      geom_label(aes(label=away_goals)) +
      labs(
        title = "Top 10 Away Team with Most Away Goal over 12 season (2006-2007 to 2017-2018)",
        subtitle = "From 12 season (2006-2007 to 2017-2018)",
        y = "Team",
        x = "Goal"
      ) +
      theme_minimal()

    ggplotly(plot2, tooltip = "text")

  })
  # 
  #  
  # --------------------------- PLOT 3 ------------------------------------

  output$plot3 <- renderPlotly({
    
    mu_h <- results %>% 
      filter(home_team == input$input_team) 
      
    
    muh_result <-  as.data.frame(table(mu_h$result))
    
    mu_a <- results %>% 
      filter(away_team == input$input_team) 
    
    mua_result <- as.data.frame(table(mu_a$result))
    
    
    colnames(muh_result)[colnames(muh_result)=="Freq"] = "Home Total"
    colnames(mua_result)[colnames(mua_result)=="Freq"] = "Away Total"
    
    
    muha_join <- left_join(muh_result, mua_result)
    muha_join <- pivot_longer(data = muha_join,
                              cols = c("Home Total", "Away Total"))
    

    muha_join <- muha_join %>% 
      mutate(
        label = glue(
          "Team as: {name}
          Result Count: {value}"
        )
      )
    
    
    plot3 <-  ggplot(data = muha_join, mapping = aes(x = Var1, y = value, fill = Var1, text = label)) + 
      geom_col()  +
      facet_wrap(~ name) +
      labs(
        title=glue('{input$input_team} Match Results over 12 season (2006-2007 to 2017-2018)'),
        subtitle='From 12 season (2006-2007 to 2017-2018)',
        x = 'Result',
        y = 'Home Goal'
      ) + 
      theme_minimal()
    
    ggplotly(plot3, tooltip = "text")
    
  })
  
  # TEXT NOTE
  
  output$text <- renderText({
    
    paste(strong("🟥 IMPORTANT NOTE 🟥 :"), 
      "",
      strong(" as AWAY TEAM") ,
      strong("** A = WIN **"),
      "D = DRAW ",
      "H = LOSS",
      "",
      strong(" as HOME TEAM") ,
      strong("** H = WIN **"),
      "D = DRAW",
      "A = LOSS ", sep = "<br/>")
   
    })
    
  # output$image <- renderImage({
  #   pfad <- "E:\DATA\.ALGORITMA\..2) DATA VISUALIZATION CLASS\LBB DATA VISUALIZATION\Premier_League_Dashboard\image.png"
  #   list(src = pfad,
  #        contentType = 'image/png',
  #        width = 400,
  #        height = 300,
  #        alt = "This is alternate text")
  # }, deleteFile = F)
  
  
  # ---------------------- PLOT 4-----------------------------------
  
  output$plot4 <- renderPlotly({


    mu_home <- results[results$home_team == input$input_team & results$result== "H",]
    mu_home <- aggregate(data = mu_home, x = home_goals ~ season, FUN = sum)


    # mu_home <- results %>%
    #   filter(home_team == input$input_team & result== "H") %>%
    #   group_by(season) %>%
    #   summarise(home_goals = sum(home_goals)) %>%
    #   mutate(
    #       label = glue(
    #         "Home Goals: {home_goals}"
    #       )
    #     ) %>% 
    #   ungroup()


    # mu_away <- results %>%
    #   filter(away_team == input$input_team & result== "A") %>%
    #   group_by(season) %>%
    #   summarise(away_goals = sum(away_goals)) %>%
    #   mutate(
    #     label = glue(
    #       "Away Goals: {away_goals}"
    #     )
    #   ) %>% 
    #   ungroup()

    mu_away <- results[results$away_team == input$input_team & results$result== "A",]
    mu_away <- aggregate(data = mu_away, x = away_goals ~ season , FUN = sum)

    
    
    join_mu <- left_join(mu_home, mu_away)

    join_mu <- pivot_longer(data = join_mu,
                            cols = c("home_goals", "away_goals"))
    
    join_mu <- join_mu %>% 
      mutate(
        name <- as.factor(name),
        labels = glue(
          
          "{name} : {value}"
        )
      )

    plot4 <- ggplot(data = join_mu, mapping = aes(x = season, y = value, group = name, col = name, text = labels)) +
      geom_line() +
      geom_point() +
      labs(
        title=glue('{input$input_team} Goal Performance over 12 season (2006-2007 to 2017-2018)'),
        subtitle='From 12 season (2006-2007 to 2017-2018)',
        x = 'Season',
        y = 'Goal',
        col = 'Result'
      ) +

      theme_minimal()
    
    
    ggplotly(plot4, tooltip = "text")



  })
  
  ## ------------------ PLOT 5 ----------------------
  
  output$plot5 <- renderPlotly({
    
  # HOME
  
  clas_h <- results[results$season == input$input_year,]
  clas_h <- clas_h[,c("home_team","home_goals","result","season")]
  clas_h <- as.data.frame.matrix(table(clas_h$home_team,clas_h$result))
  
  clas_h$point <-  (clas_h$D * 1) + (clas_h$H * 3)
  clas_h <- clas_h[order(clas_h$point, decreasing = TRUE),]
  
  
  clas_h <- cbind(team = rownames(clas_h), clas_h)
  rownames(clas_h) <- NULL
  
  # AWAY
  
  clas_a <- results[results$season == input$input_year,]
  clas_a <- clas_a[,c("away_team","away_goals","result","season")]
  clas_a <- as.data.frame.matrix(table(clas_a$away_team,clas_a$result))
  
  clas_a$point <-  (clas_a$D * 1) + (clas_a$A * 3)
  clas_a <- clas_a[order(clas_a$point, decreasing = TRUE),]
  
  
  
  clas_a <- cbind(team = rownames(clas_a), clas_a)
  rownames(clas_a) <- NULL
  
  clas_h
  clas_a
  
  
  stands <- full_join(clas_h,clas_a, "team")
  stands$total_points <- stands$point.x + stands$point.y
  
  stands <- stands[order(stands$total_points, decreasing = T),]
  row.names(stands) <-  NULL
  
  stands <- stands[,-c(2:9)]
  
  stands <- stands[apply(stands!=0, 1, all),]
  stands
  
  stands <- stands %>% 
    mutate(
      labels = glue(
        
        "Total Points : {total_points}"
      )
    )
  
  plot5 <- ggplot(data = stands, aes(x = total_points, y = reorder(team , total_points), text = labels)) +
    geom_col( position = "dodge") +
    geom_col(aes(fill = total_points), show.legend = FALSE) +
    geom_col(data = stands[1:4,] , fill = "#78d2db") +
    geom_col(data = stands[18:20,], fill = "#800000") +
    scale_fill_continuous(low = "#f94249", high = "#6042e2") +
    
    geom_label(aes(label=total_points)) +
    labs(title = "Premier League Standings",
         subtitle = "Season 2017-2018",
         caption = "Top 4 team eligible for Champions League",
         x = "Total Points Standings",
         y = NULL) +
    theme(legend.position = "top", # untuk mengubah posisi legend
          plot.title.position = "plot") + # untuk mengubah posisi judul plot
    theme_minimal()
  
  ggplotly(plot5, tooltip = "text")
  
  })
  
  # var_maxDate <- reactive({
  #   val <- as.integer(format(input$input_year[2], "%Y"))
  #   data %>% filter(year == val) %>% slice_max(n = 5, order_by = lifeExp)
  # })
  
  # -------------------- VALUE BOX 1 ------------------
  
  output$box <- renderValueBox({
    
    # HOME
    
    clas_h <- results[results$season == input$input_year,]
    clas_h <- clas_h[,c("home_team","home_goals","result","season")]
    clas_h <- as.data.frame.matrix(table(clas_h$home_team,clas_h$result))
    
    clas_h$point <-  (clas_h$D * 1) + (clas_h$H * 3)
    clas_h <- clas_h[order(clas_h$point, decreasing = TRUE),]
    
    
    clas_h <- cbind(team = rownames(clas_h), clas_h)
    rownames(clas_h) <- NULL
    
    # AWAY
    
    clas_a <- results[results$season == input$input_year,]
    clas_a <- clas_a[,c("away_team","away_goals","result","season")]
    clas_a <- as.data.frame.matrix(table(clas_a$away_team,clas_a$result))
    
    clas_a$point <-  (clas_a$D * 1) + (clas_a$A * 3)
    clas_a <- clas_a[order(clas_a$point, decreasing = TRUE),]
    
    
    
    clas_a <- cbind(team = rownames(clas_a), clas_a)
    rownames(clas_a) <- NULL
    
    clas_h
    clas_a
    
    
    stands <- full_join(clas_h,clas_a, "team")
    stands$total_points <- stands$point.x + stands$point.y
    
    stands <- stands[order(stands$total_points, decreasing = T),]
    row.names(stands) <-  NULL
    
    stands <- stands[,-c(2:9)]
    
    stands <- stands[apply(stands!=0, 1, all),]
    stands
    
    stands <- stands %>% 
      mutate(
        labels = glue(
          
          "Total Points : {total_points}"
        )
      )
    
    valueBox(
      paste0(stands[1:1,1]), "Premier League Winner", icon = icon("crown"),
      color = "purple"
    )
    
    
  })
  
  p <- "Champions League "
  output$box1 <- renderValueBox({
    
    valueBox(
      paste0(p),"Top 4 Qualified to Champions League", icon = icon("star", lib = "glyphicon"),
      color = "light-blue"
    )
    
  })
  
  # -------------------- VALUE BOX 2 ------------------
  
  output$box2 <- renderValueBox({
    
    # HOME
    
    clas_h <- results[results$season == input$input_year,]
    clas_h <- clas_h[,c("home_team","home_goals","result","season")]
    clas_h <- as.data.frame.matrix(table(clas_h$home_team,clas_h$result))
    
    clas_h$point <-  (clas_h$D * 1) + (clas_h$H * 3)
    clas_h <- clas_h[order(clas_h$point, decreasing = TRUE),]
    
    
    clas_h <- cbind(team = rownames(clas_h), clas_h)
    rownames(clas_h) <- NULL
    
    # AWAY
    
    clas_a <- results[results$season == input$input_year,]
    clas_a <- clas_a[,c("away_team","away_goals","result","season")]
    clas_a <- as.data.frame.matrix(table(clas_a$away_team,clas_a$result))
    
    clas_a$point <-  (clas_a$D * 1) + (clas_a$A * 3)
    clas_a <- clas_a[order(clas_a$point, decreasing = TRUE),]
    
    
    
    clas_a <- cbind(team = rownames(clas_a), clas_a)
    rownames(clas_a) <- NULL
    
    clas_h
    clas_a
    
    
    stands <- full_join(clas_h,clas_a, "team")
    stands$total_points <- stands$point.x + stands$point.y
    
    stands <- stands[order(stands$total_points, decreasing = T),]
    row.names(stands) <-  NULL
    
    stands <- stands[,-c(2:9)]
    
    stands <- stands[apply(stands!=0, 1, all),]
    stands
    
    stands <- stands %>% 
      mutate(
        labels = glue(
          
          "Total Points : {total_points}"
        )
      )
    
    valueBox(
      paste0(stands[20:20,1]), "Bottom 3 Degradation", icon = icon("arrow-down"),
      color = "red"
    )
    
    
  })
  
  
})