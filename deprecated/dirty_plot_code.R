# Pull the 30-min fluxes
FC <- m30.list$F_co2
FC$timeBgn <- as.POSIXct(strptime(x=FC$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
FC$timeEnd <- as.POSIXct(strptime(x=FC$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
FC$turbFilt <- FC$turb
FC$turbFilt[FC$turb.qfFinl==1] <- NA
library(plotly)
plot <- plotly::plot_ly(data=FC, x=~timeBgn, y=~turbFilt,  type='scatter', mode='lines') %>%
  plotly::layout(margin = list(b = 50, t = 50, r=50),
                 title = 'FC',
                 xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                     rep("&nbsp;", 20),
                                                     paste0("Date-time"),
                                                     rep("&nbsp;", 20)),
                                                   collapse = ""),
                              nticks=6,
                              #range = c(1,48),
                              zeroline=TRUE
                 ),
                 yaxis = list(title = 'FC'),
                 showlegend=TRUE)

print(plot)

F <- m30.list$F_H
F$turb <- F$turb/10 # Divide H by 10 to put in same ballpark as FC
F$turbFilt <- F$turb
F$turbFilt[F$turb.qfFinl==1] <- NA
F$type <- 'H'
F$timeBgn <- as.POSIXct(strptime(x=F$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
F$timeEnd <- as.POSIXct(strptime(x=F$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
FC$type <- 'FC'
F <- rbind(F,FC)

plot <- plotly::plot_ly(data=F, x=~timeBgn, y=~turbFilt, split = ~type,  type='scatter', mode='lines') %>%
  plotly::layout(margin = list(b = 50, t = 50, r=50),
                 title = 'Fluxes',
                 xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                     rep("&nbsp;", 20),
                                                     paste0("Date-time"),
                                                     rep("&nbsp;", 20)),
                                                   collapse = ""),
                              nticks=6,
                              #range = c(1,48),
                              zeroline=TRUE
                 ),
                 yaxis = list(title = ''),
                 showlegend=TRUE)

print(plot)