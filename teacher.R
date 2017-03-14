rm(list = ls(all=TRUE))
library(plotly)

teachers = read.csv('teacher.csv', header = TRUE)
positions = c("Prof.", "Associate Prof.", "Assistant Prof.",
              "Insturctor", "TA")
years = length(teachers[,1])

allStaffs = data.frame(teachers[,1],
                       rowSums(teachers[,c(2,7)]),
                       rep(positions[1], years))
names(allStaffs) = c("years","people","position")
for(tid in seq(3,6))
{
  if(tid < 5)
  {
    temp = data.frame(teachers[,1],
                      rowSums(teachers[,c(tid,tid+5)]),
                      rep(positions[tid-1],years))
  }
  else
  {
    temp = data.frame(teachers[,1],
                      teachers[,tid],
                      rep(positions[tid-1],years))
  }
  names(temp) = c("years","people","position")
  allStaffs = rbind(allStaffs,temp)
}

p<-plot_ly(allStaffs, x=~years, y=~people, color=~position) %>%
  add_lines() %>%
  layout(title = "Number of NTU Teachers from 1996 to 2015",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Number of people"))
p

htmlwidgets::saveWidget(p, "index.html")
