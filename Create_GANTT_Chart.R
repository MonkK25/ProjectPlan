# R file to create a GANTT chart
# Load library

library(lubridate)
library(ggplot2)
library(dplyr)
library(timeDate)

# Function to create a week start date by passing in a date
 date_fun <- function(aDate) {
    
    return floor_date(aDate, unit = "week", week_start = getOption("lubridate.week.start", 1))
    
  }


# Function to determine the date for plotting actual % complete
  pCompleteDate <- function(start, end, aComplete){
    temp1 <- round((d$actualComplete/100) * interval(d$start,d$end)/days(1),0)
    temp2 <- datefun((start + temp1))
    temp3 <- datefun(temp2)
    return (temp3)
  }
  
# load CSV file with project plan information
# See CSV file format included
  d <- read.csv(csvFile)
  
  # convert to date
  d$start <- mdy(d$start) 
  
  # determine start date of series
  startDate <- min(d$start) 
  
  # determine what day of the week
  startDay <- weekdays(startDate)  
  
  # convert to date
  d$end <- mdy(d$end) 
  
  # determine end of series
  endDate <- max(d$end) 
  
  # convert to dataframe
  d <- as.data.frame(d) 
  
  
  # Bin the data to the week day starting monday 
  d$nStart <- datefun(d$start)
  d$nEnd  <- datefun(d$end) 
  myaxis <- seq.Date(startDate,endDate,'week')
  
  # determine duration between start and end date of milestones
  d$duration <- interval(d$start, d$end)/days(1)
  
  # calculate planned % Complete 
  d$pComplete <- mapply(taskPlanComplete,d$start,d$end)
  
  # Get the relative date for plotting
  d$dateComplete <- pCompleteDate( d$start,d$end,d$actualComplete) 
  
  # Order the data
  d$content <- factor(d$content, levels = (unique(d$content)), order = TRUE) 
  
  # Plot the GANTT chart
  ggplot(d, aes( x = d$nStart, y = d$content)) +
    geom_segment(aes(xend = d$nEnd, yend = d$content, color = 'blue'), size = 4) + # put size outside of aes so that it does not show up in legend
    geom_text(aes(x = d$nEnd, y = d$content, label = d$content), vjust = 0.3,hjust=-0.6, size = 2, color = "red") + 
    geom_segment(aes(xend = d$dateComplete, yend = d$content,  color = 'red'), size = 1) + # line for actual % complete
    geom_vline(aes(xintercept=today()), color="red") +
    geom_point(data = d, aes(x= d$dateComplete, y=d$content), size=2, shape=21, fill="white") + # add a point to the end 
    #scale_y_reverse()
    scale_y_discrete(d$id, limits = rev(levels(d$content)))+
    geom_text(aes(x = d$dateComplete, y = d$content, label = d$actualComplete), vjust = 0.3,hjust=-0.6, size = 2, color = "red") + 
    xlim((startDate - 7),(endDate + 7)) + # Sets the start and end of x axis
    geom_text(aes(x=today(), y=0, label=as.character(today())), size=2, angle=90, vjust=-0.4, hjust=-0.3) +
    scale_color_manual(name = "",values = c("blue","red"),labels=c("Plan","Actual"))+ 
    scale_linetype_manual() + 
    theme_minimal() +
    theme(axis.text.x=element_text(face="bold.italic", color="#993333", size=5,angle = 45)) +
    xlab("Time Scale") +
    ylab("Tasks") +
    scale_x_date(name="Time Scale", breaks = myaxis) +
    labs(title = "Project Shields") +  #theme(legend.title=element_blank()) +
    theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid")    )
  
}
  
  
