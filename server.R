
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(markdown)


shinyServer(function(input, output) {
  
  output$p1 <- renderPlot({
    
    
    # filter data
#     select.measure <- filter(mdesc, measure == input$measure) %>% select(measure)
#     #select.measures <- filter(mdesc, domain == measure.domain[1]) %>% select(measure)
#     select.measures <- select.measures$measure # convert to vector
    qmpd <- filter(qm, measure == input$measure)
    mdescpd <- filter(mdesc, measure == input$measure)
    
    ggplot(qmpd, aes(ACO_Name, value, colour = value, label = round(value, 2))) +
      geom_point(size = 10) +
      geom_text(colour = "black", size = 4) +
      scale_colour_gradient(low = mdescpd$low, high = mdescpd$high) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            plot.title = element_text(size = 10)) +
      geom_hline(yintercept = median(qmpd$value), linetype = "dashed") +
      coord_flip()
      #ggtitle(mdescpd$description)
    
    
    # create list of measures
    
#     # initialize plotlist
#     plotlist <- list()
#     
#     for (i in 1:length(select.measures)){
#       p.x <- ggplot(filter(qmpd, measure == select.measures[i]), aes(ACO_Name, value, colour = value, label = round(value, 2))) +
#           geom_point(size = 10) +
#           geom_text(colour = "black", size = 4) +
#           scale_colour_gradient(low = "red", high = "green") +
#           theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none") +
#           #geom_hline(yintercept = median(d1$ACO.1), linetype = "dashed") +
#           coord_flip()
#       assign(paste0("p",i), p.x) # assign each plot
#       plotlist[[i]] <- assign(paste0("p",i), p.x)
#     }
#     
#     rm(p.x)
#     # if more than 3 measures then two columns
#     #col.num <- ifelse(length(mgroup.m) > 3, 2, 1)
#     multiplot(plotlist = plotlist, cols = 1)
#     #plotlist

  })
  
#   output$p2 <- renderPlot({
#     # filter data
#     d <- filter(hcahps.hosp, mgroup == input$mgroup)
#     star <- filter(star, mgroup == input$mgroup)
#     nonstar <- filter(nonstar, mgroup == input$mgroup)
#     
#     if (input$mgroup == "Summary"){
#       
#       starplot <- ggplot(star, aes(short_name, as.numeric(patient_survey_star_rating))) +
#         geom_point(size = 4, shape = 8) +
#         ylab("Star Rating") +
#         coord_flip() +
#         theme(axis.title.y = element_blank()) +
#         ylim(1,5)
#       
#       starplot
#       
#     }
#     
#     else {
#       
#       nonstarplot <- ggplot(nonstar, aes(short_name, as.numeric(hcahps_answer_percent))) +
#         geom_point(size = 4) +
#         ylab("Percent Recommended") +
#         ylim(min(as.numeric(nonstar$hcahps_answer_percent)) - 5, max(as.numeric(nonstar$hcahps_answer_percent)) + 5) +
#         coord_flip()
#       #multiplot(starplot, nonstarplot, cols = 1)
#       
#       nonstarplot
#       
#     }
#     
#   })
  
  # output a table of values
  output$table <- renderTable({
    
    d.table <- select(d, ACO_Name, Total.Assigned.Beneficiaries, Generated.Savings.Losses1.2)
    
    d.table
  })
  
  output$txtdesc <- renderText({
    
    txt <- filter(mdesc, measure == input$measure)
    txt <- txt$description
    txt
  })

})
