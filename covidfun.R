require(ggplot2)
require(gganimate)
require(dplyr)
require(gifski)

dat <- read.csv(url("https://covid.ourworldindata.org/data/owid-covid-data.csv"))
dat2 <- read.csv(url("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2Fnytimes%2Fcovid-19-data%2Fmaster%2Fus-states.csv&filename=us-states.csv"))


###Working data visualization functions
covid.total <- function(x, dat=dat){
  x <- x
  colors <- c("cases"="red", "deaths"="black")
  sdate <- as.Date(dat$date)
  dat <- cbind(dat, sdate)
  sdat <- data.frame(dat$location, dat$sdate, dat$total_cases, dat$total_deaths)
  sdat <- sdat %>% filter(sdat$dat.location == x)
  names(sdat)[names(sdat) == "dat.location"] <- "location"
  names(sdat)[names(sdat) == "dat.sdate"] <- "date"
  names(sdat)[names(sdat) == "dat.total_cases"] <- "total_cases"
  names(sdat)[names(sdat) == "dat.total_deaths"] <- "total_deaths"
  p <-  ggplot(sdat, aes(x=date))+
          geom_point(aes(y=total_cases, group = seq_along(date), color="cases"))+
          geom_point(aes(y=total_deaths, group = seq_along(date), color="deaths"))+
          xlab("Date") + ylab("Total Number")+
          theme(axis.text.x = element_text(size=10))+
          scale_color_manual(name = x,
                           values = c( "cases" = "red", "deaths" = "black"),
                           labels = c("Cases", "Deaths"))+
          transition_reveal(date)
  animate(p, renderer = gifski_renderer(loop=F))
}

covid.new <- function(x, dat=dat){
  x <- x
  colors <- c("cases"="red", "deaths"="black")
  sdate <- as.Date(dat$date)
  dat <- cbind(dat, sdate)
  sdat <- data.frame(dat$location, dat$sdate, dat$new_cases, dat$new_deaths)
  sdat <- sdat %>% filter(sdat$dat.location == x)
  names(sdat)[names(sdat) == "dat.location"] <- "location"
  names(sdat)[names(sdat) == "dat.sdate"] <- "date"
  names(sdat)[names(sdat) == "dat.new_cases"] <- "new_cases"
  names(sdat)[names(sdat) == "dat.new_deaths"] <- "new_deaths"
  p <-  ggplot(sdat, aes(x=date))+
          geom_point(aes(y=new_cases, group = seq_along(date), color="cases"))+
          geom_point(aes(y=new_deaths, group = seq_along(date), color="deaths"))+
          xlab("Date") + ylab("New Number")+
          theme(axis.text.x = element_text(size=10))+
          scale_color_manual(name = x,
                            values = c( "cases" = "red", "deaths" = "black"),
                            labels = c("New Cases", "New Deaths"))+
          transition_reveal(date)
  animate(p, renderer = gifski_renderer(loop=F))
}

covid.domestic.total <- function(x, dat=dat2){
  x <- x
  colors <- c("cases"="red", "deaths"="black")
  ssdate <- as.Date(dat2$date)
  dat2 <- cbind(dat2, ssdate)
  sdat <- data.frame(dat2$state, dat2$ssdate, dat2$cases, dat2$deaths)
  sdat <- sdat %>% filter(sdat$dat2.state == x)
  names(sdat)[names(sdat) == "dat2.state"] <- "state"
  names(sdat)[names(sdat) == "dat2.ssdate"] <- "date"
  names(sdat)[names(sdat) == "dat2.cases"] <- "cases"
  names(sdat)[names(sdat) == "dat2.deaths"] <- "deaths"
  p <-  ggplot(sdat, aes(x=date))+
    geom_point(aes(y=cases, group = seq_along(date), color="cases"))+
    geom_point(aes(y=deaths, group = seq_along(date), color="deaths"))+
    xlab("Date") + ylab("Total Number")+
    theme(axis.text.x = element_text(size=10))+
    scale_color_manual(name = x,
                       values = c("cases" = "red", "deaths" = "black"),
                       labels = c("Cases", "Deaths"))+
    transition_reveal(date)
  animate(p, renderer = gifski_renderer(loop=F))
}


covid.domestic.new <- function(x, dat=dat2){
  x<-x
  colors <- c("cases"="red", "deaths"="black")
  ssdate <- as.Date(dat2$date)
  dat2 <- cbind(dat2, ssdate)
  cdat <- data.frame(dat2$state, dat2$ssdate, dat2$cases, dat2$deaths)
  cdat <- cdat %>% filter(cdat$dat2.state == x)
  cdat$new_cases <- with(cdat, c(dat2.cases[1], dat2.cases[-1]-dat2.cases[-nrow(cdat)]))
  cdat$new_deaths <- with(cdat, c(dat2.deaths[1], dat2.deaths[-1]-dat2.deaths[-nrow(cdat)]))
  names(cdat)[names(cdat) == "dat2.state"] <- "state"
  names(cdat)[names(cdat) == "dat2.ssdate"] <- "date"
  names(cdat)[names(cdat) == "new_cases"] <- "cases"
  names(cdat)[names(cdat) == "new_deaths"] <- "deaths"
  p <-  ggplot(cdat, aes(x=date))+
    geom_point(aes(y=cases, group = seq_along(date), color="cases"))+
    geom_point(aes(y=deaths, group = seq_along(date), color="deaths"))+
    xlab("Date") + ylab("Total Number of New")+
    theme(axis.text.x = element_text(size=10))+
    scale_color_manual(name = x,
                       values = c("cases" = "red", "deaths" = "black"),
                       labels = c("Cases", "Deaths"))+
    transition_reveal(date)
  animate(p, renderer = gifski_renderer(loop=F))
}

cat("Input a country/state into one of the commands listed below.\n\n1) covid.total(\"Country\")\n2) covid.new(\"Country\")\n3) covid.domestic.total(\"State\")\n4) covid.domestic.new(\"State\")\n\nFor more help: commands()\n\n")


###Help functions
commands <- function(){
  cat("Data Visualization:\n1) covid.total(\"Country\")\n2) covid.new(\"Country\")\n3) covid.domestic.total(\"State\")\n4) covid.domestic.new(\"State\")\n\nView Input List:\n1) countries()\n2) states()\n\nCommands:\n1)commands()")
}

countries <- function(){
  dat <- read.csv(url("https://covid.ourworldindata.org/data/owid-covid-data.csv"))
  country <- dat$location
  country <- country[!duplicated(country)]
  country <- as.character(country)
  print(country)
}

states <- function(){
  dat2 <- read.csv(url("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2Fnytimes%2Fcovid-19-data%2Fmaster%2Fus-states.csv&filename=us-states.csv"))
  state <- dat2$state[order(dat2$state)]
  state <- state[!duplicated(state)]
  state <- as.character(state)
  print(state)
}