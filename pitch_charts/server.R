# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    gv <- reactive({
        # If no player is selected, return NULL so that it doesn't load all players at once.  Slows down the app.
        if (input$batters == "")
            return(NULL)
        
        batter_df <- batter_df[which(batter_df$matchup.batter.fullName==input$batters),]
        batter_df <- subset(batter_df, game_date >= input$dateRange[1] & game_date <= input$dateRange[2])
        #batter_df <- subset(batter_df, matchup.pitchHand.code %in% input$pitcher_throw)
        #batter_df <- subset(batter_df, pitch %in% input$pitch_type)
        #batter_df <- subset(batter_df, event %in% input$event)

    })


    output$img <- renderUI({
        batter_df <- batter_df[which(batter_df$matchup.batter.fullName==input$batters),]
        tags$img(src = paste0("https://securea.mlb.com/mlb/images/players/head_shot/", first(batter_df$matchup.batter.id), ".jpg"))
    })
    
    output$img2 <- renderUI({
        batter_df <- batter_df[which(batter_df$matchup.batter.fullName==input$batters),]
        tags$img(src = paste0("https://securea.mlb.com/mlb/images/players/head_shot/", first(batter_df$matchup.batter.id), ".jpg"))
    })
    
    output$img3 <- renderUI({
        batter_df <- batter_df[which(batter_df$matchup.batter.fullName==input$batters),]
        tags$img(src = paste0("https://securea.mlb.com/mlb/images/players/head_shot/", first(batter_df$matchup.batter.id), ".jpg"))
    })
    
    output$batterPlot <- renderPlot({

    if (input$chart_pitch_scatter == 'Pitch Chart') {    
      
        # make the first 2 universal
        batter_df <- batter_df[which(batter_df$matchup.batter.fullName==input$batters),]
        batter_df <- subset(batter_df, game_date >= input$dateRange[1] & game_date <= input$dateRange[2])
        
        pitch_chart_df <- batter_df
        pitch_chart_df <- subset(pitch_chart_df, matchup.pitchHand.code %in% input$pitcher_throw_batter_view)
        pitch_chart_df <- subset(pitch_chart_df, pitch %in% input$pitch_type_batter_view)
        pitch_chart_df <- subset(pitch_chart_df, event %in% input$event_batter_view)
        
            batter_gg <-  ggplot() +
                    geom_path(data = kZone, aes(x=x, y=y)) +
                    geom_path(data = homeplate, aes(x=x, y=y))+
                    xlab("") +
                    ylab("") +
                    geom_point(data = pitch_chart_df, aes(x=pitchData.coordinates.pX, y=pitchData.coordinates.pZ, color=event)) +
                    scale_size(range = c(0.01,3)) + 
                    ylim(-.5, 6.2) +
                    xlim(-4, 3) +
                    coord_equal() +
                    ggtitle(paste(first(pitch_chart_df$matchup.batter.fullName), "'s Pitch Chart (Catcher's Perspective)", sep='')) +
                    labs(color='Pitch Type') + 
                    theme_void() +
                    theme(plot.title = element_text(size = 10, hjust = 0.5, family = "Tahoma", face = "bold"),
                            text=element_text(family="Tahoma"),
                            legend.key=element_rect(fill="white", colour="white"))
            
            if (pitch_chart_df$matchup.batSide.code == 'R') {
                rh_w <- matrix(rgb(rh[,,1],rh[,,2],rh[,,3], rh[,,4] * 0.25), nrow=dim(rh)[1])
                batter_gg +
                    annotation_custom(xmin=-8, ymin=-Inf, xmax=Inf, ymax=6.2,    
                                rasterGrob(rh_w)) + geom_point() 
                } else {
                lh_w <- matrix(rgb(lh[,,1],lh[,,2],lh[,,3], lh[,,4] * 0.25), nrow=dim(lh)[1])
                batter_gg +
                annotation_custom(xmin=5.5, ymin=-Inf, xmax=-1.5, ymax=Inf,    
                                rasterGrob(lh_w)) + geom_point()
            }
    
    }else {
            
        batter_density_gg <- ggplot(data = pitch_chart_df, aes(x=pitchData.coordinates.pX, y=pitchData.coordinates.pZ)) +
                    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
                    scale_fill_distiller(palette=1, direction=1) +
                    geom_path(data = kZone, aes(x=x, y=y)) +
                    geom_path(data = homeplate, aes(x=x, y=y))+
                    xlab("")+
                    ylab("")+
                    scale_size(range = c(0.01,3)) + 
                    ylim(-.5, 6.2) +
                    xlim(-4, 3) +
                    coord_equal() +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                    ggtitle("Density Pitch Chart from Catcher's Point of View") + 
                    theme_void() +
                    theme(plot.title = element_text(size = 10, hjust = 0.5, family = "Tahoma", face = "bold"),
                            text=element_text(family="Tahoma"),
                            legend.key=element_rect(fill="white", colour="white"))

        if (pitch_chart_df$matchup.batSide.code == 'R') {
                rh_w <- matrix(rgb(rh[,,1],rh[,,2],rh[,,3], rh[,,4] * 0.25), nrow=dim(rh)[1])
                batter_density_gg +
                    annotation_custom(xmin=-7.5, ymin=-Inf, xmax=Inf, ymax=Inf,    
                                rasterGrob(rh_w))
                } else {
                lh_w <- matrix(rgb(lh[,,1],lh[,,2],lh[,,3], lh[,,4] * 0.25), nrow=dim(lh)[1])
                batter_density_gg +
                annotation_custom(xmin=5.5, ymin=-Inf, xmax=-1.5, ymax=Inf,    
                                rasterGrob(lh_w))
                }

    
        }
    
    })
    
    
    output$batterTypePlot <- renderPlot({
      
        # make the first 2 universal
        batter_df <- batter_df[which(batter_df$matchup.batter.fullName==input$batters),]
        batter_df <- subset(batter_df, game_date >= input$dateRange[1] & game_date <= input$dateRange[2])
        
        batter_type_df <- batter_df
        batter_type_df <- subset(batter_type_df, matchup.pitchHand.code %in% input$pitcher_throw)
        batter_type_df <- subset(batter_type_df, pitch %in% input$pitch_type)
        batter_type_df <- subset(batter_type_df, event %in% input$event)
        
        ggplot() + 
              geom_point(data = subset(batter_type_df, event != 'null'), aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, color=pitch)) +
              geom_path(aes(x, y), data=kZone) +
              xlab("")+
              ylab("")+
              ylim(0, 5) +
              xlim(-2, 2) +
              facet_wrap(~get(input$chart_type), ncol=2) +
              scale_color_discrete(name="Pitch Type") +
              ggtitle(paste(first(batter_type_df$matchup.batter.fullName), "'s Pitches by ", input$chart_type, " (Catcher's Perspective)", sep='')) +
              theme(plot.title = element_text(size = 10, hjust = 0.5, family = "Tahoma", face = "bold"),
                        text=element_text(family="Tahoma"),
                        legend.key=element_rect(fill="white", colour="white")) + 
              theme(axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  panel.background=element_blank())
    })
    
    
    # output$spraychart <- renderPlot({
    #     
    #      ggplot() +  
    #          geom_point(data = gv(), aes(x=hc_x_, y=hc_y_, color = event)) +
    #          geom_spraychart(stadium_ids = input$stadium,
    #                          stadium_transform_coords = TRUE, 
    #                          stadium_segments = "all") + 
    #          theme_void() + 
    #          coord_fixed() + 
    #          facet_wrap(~batter_name) + 
    #          theme(legend.position = "bottom")
    #     
    # })
    
    output$spraychart <- renderPlotly({
        
        p <- gv() %>%
            ggplot(aes(x = hc_x_, y = hc_y_,
                          text = paste('Pitcher: ', matchup.pitcher.fullName,
                                       '<br>Pitch Name: ', pitch,
                                       '<br>Pitch Count: ', pitch_count,
                                       '<br>Event: ', event,
                                       '<br>Batting Team: ', batting_team,
                                       '<br>Fielding Team: ', fielding_team))) +
              geom_spraychart(stadium_ids = input$stadiums,
                                    stadium_transform_coords = TRUE, 
                                    stadium_segments = "all") + 
              geom_point(aes(color = event), size = .5, alpha = 1, stroke = .5) +
              labs(x = NULL, y = NULL) +
              labs(color='Events') +
              annotate("text", x = 2.5, y = 550, size = 4.75, label = paste(gv()$matchup.batter.fullName, "- Spraychart", sep=" ")) +
              annotate("text", x = 2.5, y = 500, label = paste(min(gv()$game_date), 'to', max(gv()$game_date), sep=' ')) + 
              theme_void() + 
              coord_fixed()
        
        ggplotly(p, tooltip="text")
    })
    
    # Generate a table summarizing each players stats
  output$summaryTable <- renderDataTable({
      gv() %>%
            filter(event != 'NA') %>%
            select(game_pk, game_date, event, matchup.batter.fullName) %>%
            group_by(event) %>%
            summarize(count = n()) %>%
            arrange(desc(count))
  })

})
