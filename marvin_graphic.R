library(ggplot2)
library(dplyr)
library(plotly)

marvin <- read.csv("drake_marvin_room.csv", row.names = NULL)
colnames(marvin) <- c(
  "YYYY_MM",
  "google_trends_marvinsroom",
  "google_trends_drakeUS",
  "marvin_babycenter_ranking"
)

marvin$`YYYY_MM` <- as.Date(paste0(marvin$YYYY_MM, "-01"))
marvins_room_release <- marvin[which.max(marvin$google_trends_marvinsroom), ]

# This forces the rankings (345 -> 1,155) to follow a 100 - 0 scale where higher rank (345) = 100.
marvin$marvin_name_score <- (-marvin$marvin_babycenter_ranking + min(marvin$marvin_babycenter_ranking, na.rm = TRUE) + 1000)/10
marvin_score <- marvin[!is.na(marvin$marvin_name_score) , c("YYYY_MM", "marvin_babycenter_ranking", "marvin_name_score")]

mline <- list(
  type = "line",
  y0 = 0,
  y1 = 110,
  x0 = marvins_room_release$YYYY_MM,
  x1 = marvins_room_release$YYYY_MM,
  line = list(color = 'black', dash="dot")
)

plot_ly(data = marvin, x = ~YYYY_MM, y = ~google_trends_drakeUS,
        name = "Google Trends: Drake (US)",
        type = "scatter", mode = "lines+markers") %>% 
  add_trace(data = marvin_score, x = ~YYYY_MM, y = ~marvin_name_score,
            name = "'Marvin' Baby Prevelance",
            hoverinfo = 'text', 
            text = paste0("Year: ", 2004:2022,
                          "\nMarvin Baby Center Rank: #", 
                          marvin_score$marvin_babycenter_ranking),
            type = "scatter", mode = "lines+markers") %>% 
  layout(shapes = list(mline),  
         plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)",
         title = list(text = "7-Years after Drake releases Marvin's Room \n no more babies named Marvin", y = 0.975),
         xaxis = list(title = "Date", range = c('2004-01', '2023-02'), showgrid = FALSE),
         yaxis = list(title = "Relative Scale", range = c(0, 120), showgrid = FALSE)) 
  
# Image saved transparently and edited in Figma