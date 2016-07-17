options(scipen=5)
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(RColorBrewer)

dataset <- read.csv("https://dl.dropboxusercontent.com/u/1850581/aust-1990-2013.tsv", header = TRUE, sep="\t", quote="\"", row.names = NULL)
dataset$type <- as.character(dataset$type)

wip <- dataset[with(dataset, type %in% c("ALP", "NP", "LP", "LNP", "LP(a)", "CLP")),]
wip$type <- "Major"
suppressWarnings(wip[is.na(wip)] <- 0)
wip <- aggregate(. ~ year + type, wip, sum)
dataset <- rbind(dataset, wip)

dataset$type <- as.factor(dataset$type)
levels(dataset$type) <- list(ALP = c("ALP"), COA = c("NP", "LP", "LNP", "LP(a)", "CLP"), Others = c("GRN", "CTA", "DLP", "ON", "PUP", "DEM", "FFP", "Others"), Total = c("Total"), Formal = c("Formal"), Major = c("Major"))

##
suppressWarnings(dataset[is.na(dataset)] <- 0)
dataset <- aggregate(. ~ year + type, dataset, sum)

fluidPage(
  titlePanel("1990 - 2013 Election Primary Vote Trends"),
  
  sidebarPanel(
    
    selectInput("type", "Total number of votes by group:",
                levels(unique(dataset$type)), selected = c("Others", "Major"), multiple = TRUE
    ),
    
    selectInput("names", "Region:",
                c("Aust", "NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT")
    ),
    
    includeMarkdown("context.md")
    
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Share of Total", plotOutput("percent")),
      tabPanel("Bar Plot (Totals)", plotOutput("plot")),
      tabPanel("Trend (Line) Plot", plotOutput("plotDist")),
      tabPanel("Votes by State", plotOutput("plotState")),
      tabPanel("Normalised Votes by State", plotOutput("plotStateNorm"))
      )
  )
)