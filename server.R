#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
install.packages("Lahman");
library(Lahman)
install.packages("dplyr");
library(dplyr)
install.packages("tidyverse");
library(tidyverse)
install.packages("ggplot2");
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
        

        
        
        #step 1: get season and aggregate by season: needed: G, AB, R, H, HR, RBI, SB, season number
        df <- Batting
        keepcols <- c("playerID", "yearID", "stint", "G", "AB", "H", "HR", "RBI", "SB")
        df <- df[keepcols]
        
        df <- df%>% group_by(playerID, yearID) %>% summarise(G = sum(G), AB = sum(AB), H = sum(H), HR = sum(HR), RBI = sum(RBI), SB = sum(SB))
        myr <- df%>% group_by(playerID) %>% summarise(minyr = min(yearID))
        
        df1 <- (merge(df, myr))
        df1$season <- df1$yearID-df1$minyr + 1
        
        #filter out players with less than 100 games 
        df2 <- df1 %>%  filter(G > 100 & yearID >= 1990)
        df1 <- df1[df1$playerID %in% unique(df2$playerID),]
        df1 %>%  filter(G>100) -> df1
        df1$HPG <- df1$H/df1$G
        df1$HRPG <- df1$HR/df1$G
        df1$RBIPG <- df1$RBI/df1$G
        df1$SBPG <- df1$SB/df1$G
        df1$AVG <- df1$H/df1$AB
        
        
        #now get overall means for batting average by season
        avgVec <- df1 %>%  group_by(season) %>% summarise(AVG = sum(H)/sum(AB), n= n(), HPG = mean(HPG), HRPG = mean(HRPG), RBIPG = mean(RBIPG), SBPG = mean(SBPG))
        #remove averages that have less than 100 individuals in the time period
        avgVec <- avgVec[avgVec$n >=100,]
        avgVec$playerID <- "OVERALL"
        
        #random sample for slider input
 #       if( input$slider > 0){
 #*       s <-df1[df1$playerID %in%(sample(unique(df1$playerID), input$slider)),]
 #*       s <- merge(s, People, all.y = FALSE)
 #       s$name <-(paste(s$nameFirst, s$nameLast))
 #      }
        
        df1 <-df1[intersect(colnames(df1), colnames(avgVec))]
        avgVec <- avgVec[intersect(colnames(df1), colnames(avgVec))]
        #if( input$slider > 0){s <- s[intersect(colnames(s, avgVec))]}
        x <- data.frame()
        
        x <- df1
        #if( input$slider > 0){x <- rbind(df1, s)}
        peep <- People
        peep$name <- paste(peep$nameFirst, peep$nameLast)
        keepcols <- c("playerID", "name", "season", "HPG", "HRPG", "RBIPG", "SBPG", "AVG")
        
        #filter based on names 
        names <- character()
        #if( input$slider > 0){names <- snames}
        if(input$bh == "TRUE") {names <-c(names, "Bryce Harper")}
        if(input$js == "TRUE") {names <-c(names, "Juan Soto")}
        if(input$ja == "TRUE") {names <-c(names, "Jose Altuve")}
        if(input$aj == "TRUE") {names <-c(names, "Aaron Judge")}
        if(input$tt == "TRUE") {names <-c(names, "Trea Turner")}
        if(input$pg == "TRUE") {names <-c(names, "Paul Goldschmidt")}
        if(input$mt == "TRUE") {names <-c(names, "Mike Trout")}
        if(input$mb == "TRUE") {names <-c(names, "Mookie Betts")}
        peep <-peep %>% filter(name %in% names)
        x <-merge(x, peep, all.y =TRUE)
        x <- x[colnames(x) %in% keepcols]
        
        avgVec$name <- "OVERALL AVERAGE"
        if (sum(input$bh,input$js,input$ja,input$aj,input$tt,input$mt,input$pg)==0){
            x <- avgVec
        }else {x <- rbind(x, avgVec)}
        #if (input$slider >0){x <- rbind(x, s)}
        
        
        x$name <- as.factor(x$name)
        ba <- ggplot(data = x, aes(x = season, y = AVG, color = name)) + geom_point() + geom_line() + xlab("Season") + ylab("Batting Average") + ggtitle("Mean Batting Average by Season, 1990-2021") + theme_gray()
        hpg <-ggplot(data = x, aes(x = season, y = HPG, color = name)) + geom_point() + geom_line() + xlab("Season") + ylab("Hits Per Game") + ggtitle("Mean Hits Per Game by Season, 1990-2021")+ theme_gray()
        hrpg <- ggplot(data = x, aes(x = season, y = HRPG, color = name)) + geom_point() + geom_line() + xlab("Season") + ylab("Home Runs Per Game") + ggtitle("Mean Home Runs Per Game by Season, 1990-2021")+ theme_gray()
        rbipg <-ggplot(data = x, aes(x = season, y = RBIPG, color = name)) + geom_point() + geom_line() + xlab("Season") + ylab("RBI Per Game") + ggtitle("Mean RBI Per Game by Season, 1990-2021")+theme_gray()
        sbpg <-ggplot(data = x, aes(x = season, y = SBPG, color = name)) + geom_point() + geom_line() + xlab("Season") + ylab("Stolen Bases Per Game") + ggtitle("Mean Stolen Bases Per Game by Season, 1990-2021")+ theme_gray()
        if(input$stat =="avg"){print(ba)}
        if(input$stat == "rbi"){print(rbipg)}
        if(input$stat == "hpg"){print(hpg)}
        if(input$stat == "hr"){print(hrpg)}
        if(input$stat == "sb"){print(sbpg)}
        
    })

})

