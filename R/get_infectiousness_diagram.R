get_infectiousness_diagram <- function() {
  
  infectious.end.time <- 12
  step.size <- 0.001  
    
  infectiousness<-function(t){
      sapply(1:dim(t)[1],function(j){
        mean(1-pweibull(qnorm(seq(0,1,step.size),t[j,1],t[j,2]),shape=3.2862,scale=6.1244))
      })
  }  
    
  max.proportion.who.know.infected <- 0.8
  other_quarantine_reduction <- 0.4

  proportion.who.know <- function(t, max.proportion.who.know.infected) {
    max.proportion.who.know.infected/(exp(-t+infectious.end.time/2) + 1)
  }
  
    
  plot_ly() %>%
      add_lines(x=seq(0,infectious.end.time,step.size) ,
            y=~dweibull(seq(0,infectious.end.time,step.size), shape = 3.2862, scale = 6.1244), 
            name="basic infectiousness\nin un-notified individuals",
            line=list(width=5)) %>%
      add_lines(x=seq(0,infectious.end.time,step.size) ,
            y=~(dweibull(seq(0,infectious.end.time,step.size), shape = 3.2862, scale = 6.1244) *
                  (1 - other_quarantine_reduction * 
                     proportion.who.know(seq(0,infectious.end.time,step.size), 
                                         max.proportion.who.know.infected))),
            name="infectiousness as reduced\nby other PHSMs",
            line=list(width=5)
  )  %>%
  add_lines(x=seq(0,infectious.end.time,step.size) ,
            y=~c(
              (dweibull(seq(0,5,step.size), shape = 3.2862, scale = 6.1244) *
                 (1 - other_quarantine_reduction * 
                    proportion.who.know(seq(0,5,step.size), 
                                        max.proportion.who.know.infected))) * 1,
              (dweibull(seq(5+step.size,infectious.end.time,step.size), shape = 3.2862, scale = 6.1244) * 0.6)
              ),
            name="infectiousness as reduced\nby app notification\nafter 5 days",
            line=list(width=5)
  ) %>%
  add_ribbons(x=seq(0,infectious.end.time,step.size) ,
            ymin=~c(
              (dweibull(seq(0,5,step.size), shape = 3.2862, scale = 6.1244) *
                 (1 - other_quarantine_reduction * 
                    proportion.who.know(seq(0,5,step.size), 
                                        max.proportion.who.know.infected))) * 1,
              (dweibull(seq(5+step.size,infectious.end.time,step.size), shape = 3.2862, scale = 6.1244) * 0.6)
            ),
            ymax=~(dweibull(seq(0,infectious.end.time,step.size), shape = 3.2862, scale = 6.1244) *
                     (1 - other_quarantine_reduction * 
                        proportion.who.know(seq(0,infectious.end.time,step.size), 
                                            max.proportion.who.know.infected))),
            name="app-averted infectiousness", opacity=0.3, color=I("#2ca02c")
  ) %>%
  layout(    
    xaxis=list(
      title="Time since infection (days)",
      titlefont=f1,
      tickfont=f1
    ),
    yaxis=list(
      title="Infectiousness",
      titlefont=f1,
      tickfont=f1
    ),
    legend=list(
      font=f1
    )
  )
}