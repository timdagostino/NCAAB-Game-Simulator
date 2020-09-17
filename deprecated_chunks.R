#Render Radar Plot for Selected Teams

#Radar Plot Dataset
radar_data <- ncaa_team_stats_2020 %>%
  select(steal_pct, block_pct, tov_pct, orb_pct) %>%
  filter(team_name %in% c(input$home, input$away))

#Transpose DF
radar_data <- t(radar_data)

#Generate Plot
output$team_radar <- renderPlotly({
  team_radar <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  ) %>%
    
    add_trace(
      r = radar_data[,1],
      theta = c('Steal %','Block %','TO %', 'ORB %'),
      name = input$home,
      fillcolor = "#63B8FFB3",
      marker = list(
        color = '#63B8FFB3')) %>%
    
    add_trace(
      r = radar_data[,2],
      theta = c('Steal %','Block %','TO %', 'ORB %'),
      name = input$away,
      fillcolor = "#E0FFFFB3",
      marker = list(
        color = '#E0FFFFB3'))
})

#----------------------------------------------