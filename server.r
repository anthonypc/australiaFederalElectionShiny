## Australian House of Reps voting partterns.
# 1990 - 2013 vote totals of first preference votes by state, party and year.
# Source: http://www.aph.gov.au/About_Parliament/Parliamentary_Departments/Parliamentary_Library/pubs/rp/rp1415/FedElect/FedElecte
# Preprocessing for the data was handled outside of R and the information used in the following was restricted to just the totals.
# This is not intended as a discussion of seats won, but as an examination of the underlying voting trends by party.

## Notes on how the data is treated:
# When the analysis is focused on the national level, LP, LP/NP, LNP and NP are all treated as being the coalition.

options(scipen=5)
library(shiny)
library(ggplot2)
library(dplyr)
library(markdown)
library(data.table)
library(RColorBrewer)

## Function for transforming a number to a percentage.
percentage <- function(x) {
  paste0(sub("0\\.([0-9]{2})([0-9]{2,})", "\\1.\\2", x), "%")
}

## Collection and processing of the data.
voteData.df <- read.csv("https://dl.dropboxusercontent.com/u/1850581/aust-1990-2013.tsv", header = TRUE, sep="\t", quote="\"", row.names = NULL)
austVoteFile.df <- voteData.df
austVoteFile.df$type <- as.character(austVoteFile.df$type)

wip.df <- austVoteFile.df[with(austVoteFile.df, type %in% c("ALP", "NP", "LP", "LNP", "LP(a)", "CLP")),]
wip.df$type <- "Major"
suppressWarnings(wip.df[is.na(wip.df)] <- 0)
wip.df <- aggregate(. ~ year + type, wip.df, sum)
austVoteFile.df <- rbind(austVoteFile.df, wip.df)

austVoteFile.df$type <- as.factor(austVoteFile.df$type)
levels(austVoteFile.df$type) <- list(ALP = c("ALP"), COA = c("NP", "LP", "LNP", "LP(a)", "CLP"), Others = c("GRN", "CTA", "DLP", "ON", "PUP", "DEM", "FFP", "Others"), Total = c("Total"), Formal = c("Formal"), Major = c("Major"))

##
suppressWarnings(austVoteFile.df[is.na(austVoteFile.df)] <- 0)
austVoteFile.df <- aggregate(. ~ year + type, austVoteFile.df, sum)

## Percentage set.
voteDataPer.df <- read.csv("https://dl.dropboxusercontent.com/u/1850581/aust-1990-2013-per.tsv", header = TRUE, sep="\t", quote="\"", row.names = NULL)
austVoteFilePer.df <- voteDataPer.df[,1:11]
austVoteFilePer.df$type <- as.character(austVoteFilePer.df$type)

wipPer.df <- austVoteFilePer.df[with(austVoteFilePer.df, type %in% c("ALP", "NP", "LP", "LNP", "LP(a)", "CLP")),]
wipPer.df$type <- "Major"
suppressWarnings(wipPer.df[is.na(wipPer.df)] <- 0)
wipPer.df <- aggregate(. ~ year + type, wipPer.df, sum)
austVoteFilePer.df <- rbind(austVoteFilePer.df, wipPer.df)

austVoteFilePer.df$type <- as.factor(austVoteFilePer.df$type)
levels(austVoteFilePer.df$type) <- list(ALP = c("ALP"), COA = c("NP", "LP", "LNP", "LP(a)", "CLP"), Others = c("GRN", "CTA", "DLP", "ON", "PUP", "DEM", "FFP", "Others"), Total = c("Total"), Formal = c("Formal"), Major = c("Major"))

##
suppressWarnings(austVoteFilePer.df[is.na(austVoteFilePer.df)] <- 0)
austVoteFilePer.df <- aggregate(. ~ year + type, austVoteFilePer.df, sum)


## Functional elements of the file.
function(input, output){

  
  dataset <- reactive({
    ## Create connection to the data set.
    f <- austVoteFile.df

    ## Create the filters to be used here.
    f <- f %>% filter(type %in% input$type)
    
    })
  
  datasetPer <- reactive({
    ## Create connection to the data set.
    d <- austVoteFilePer.df
  
  ## Create the filters to be used here.
  d <- d %>% filter(type %in% input$type)
  
})

  ## Rendering the plot.
  output$percent <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    p <- ggplot(datasetPer(), aes_string( x = "as.factor(year)", y = input$names, fill = "type", label = input$names)) + geom_bar(position = "dodge", stat = "identity") + ggtitle("Federal Primary Vote over time (Percent of Enrolled).") + scale_colour_brewer("Colors in Paired", palette="Paired") + ylab("Percentage of Enrolled voters.") + xlab("Election Year") + geom_text(colour = "white", vjust = 1.5, position = position_dodge(.9), size = 4)
    
    print(p)
    
  }, height=700)
  
    output$plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    p <- ggplot(dataset(), aes_string( x = "as.factor(year)", y = input$names, fill = "type", label = input$names)) + geom_bar(position = "dodge", stat = "identity") + ggtitle("Federal Primary Vote over time.") + scale_colour_brewer("Colors in Paired", palette="Paired") + ylab("Total votes for selected region.") + xlab("Election Year") + geom_text(colour = "white", vjust = 1.5, position = position_dodge(.9), size = 4)
    
    print(p)
    
  }, height=700)

  output$plotDist <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    p <- ggplot(dataset(), aes_string( x = "year", y = input$names, colour = "type")) + geom_line() + ggtitle("Federal Primary Vote over time.") + scale_colour_brewer("Colors in Paired", palette="Paired")
    
    print(p)
    
  }, height=700)

  output$plotState <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    p <- ggplot(dataset(), aes( x = year)) + geom_line(aes(y = NSW, colour = "NSW")) + geom_line(aes(y = Vic, colour = "VIC")) + geom_line(aes(y = Qld, colour = "QLD")) + geom_line(aes(y = SA, colour = "SA")) + geom_line(aes(y = WA, colour = "WA")) + geom_line(aes(y = NT, colour = "NT")) + geom_line(aes(y = Tas, colour = "TAS")) + geom_line(aes(y = ACT, colour = "ACT")) + facet_wrap(~type, ncol = 2) + ggtitle("Federal Primary Vote by state over time.") + ylab("Sum of votes") + scale_colour_brewer("Colors in Paired", palette="Paired")
    
    print(p)
    
  }, height=700)

  output$plotStateNorm <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    
    p <- ggplot(dataset(), aes( x = year)) + geom_line(aes(y = (Aust-mean(Aust))/sd(Aust), colour = "Aust")) + geom_line(aes(y = (NSW-mean(NSW))/sd(NSW), colour = "NSW")) + geom_line(aes(y = (Vic-mean(Vic))/sd(Vic), colour = "VIC")) + geom_line(aes(y = (Qld-mean(Qld))/sd(Qld), colour = "QLD")) + geom_line(aes(y = (SA-mean(SA))/sd(SA), colour = "SA")) + geom_line(aes(y = (WA-mean(WA))/sd(WA), colour = "WA")) + geom_line(aes(y = (NT-mean(NT))/sd(NT), colour = "NT")) + geom_line(aes(y = (Tas-mean(Tas))/sd(Tas), colour = "Tas")) + geom_line(aes(y = (ACT-mean(ACT))/sd(ACT), colour = "ACT")) + facet_wrap(~type, ncol = 2) + ggtitle("Federal Primary Vote by state over time.") + ylab("Normalised votes") + scale_colour_brewer("Colors in Paired", palette="Paired")
    
    print(p)
    
  }, height=700)
  
}
