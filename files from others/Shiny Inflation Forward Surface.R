# To do:
# Check functions are working across the whole set of maturities
# Check forward curve caulcation is correct
# Spot rates needed - ie. 0 years forward
# Surface chart restricting x axis to range of y axis - set maximum so can see full list of x axis?

setwd("N:/328890/Inflation markets")


# Inflation surface shiny app

library(shiny)
library(plotly)
library(ggplot2)
library(readxl)
source("N:/328890/Inflation markets/Inflation Function Library.R")

# Define UI for surface app ----
ui <- pageWithSidebar(

  # App title ----
  headerPanel("Forward Inflation Surface"),
  # Load in inflation data for the date

  # Sidebar panel for inputs ----
  sidebarPanel(
    # Input: Selector for variable to plot against mpg ----
    selectInput("variable", "Date:",
                Inflation_data[ ,1] )

  ),

  # Main panel for displaying outputs ----
  mainPanel(


    # Output: Plot of the requested variable  ----
    plotlyOutput("surface")

  )
)

server <- function(input, output) {

  #### Import data

  Inflation_data <- read_excel("N:/328890/Inflation markets/Inflation forward matrix.xlsx",sheet = "R import")
  forwards <- c(0:10)
  mats <- c(1,2,3,4,5,6,7,8,9,10,12,15,20,25,30,40,50)

  #### Functions

  #1) Interpolate (Pat's)

  #' Helper function that fits smoothing spline to all observed values of x and y and then predicts values for gaps in y
  #'
  #' @param x Predictor variable
  #' @param y Dependent variable
  #' @param outer logical: if TRUE, will predict values left (right) of minimum (maximum) observed value of y
  #'
  #' @return Vector of interpolated series of y
  #'
  #' @author Patrick Altmeyer (4517)

  interpol = function(x,y,outer=F) {

    dat = na.omit(data.frame(x,y))
    margins = c(head(which(!(is.na(y))),1), tail(which(!(is.na(y))),1)) # index of minimum (maximum) observed value of y
    fm = try(smooth.spline(dat$x,dat$y), silent=T) # fit model on observed values

    if (class(fm) == "try-error") {

      return(y)

    } else {

      idx.mar = ifelse(outer,1,margins[1]):ifelse(outer,length(y),margins[2])
      idx.na = which(is.na(y))
      fitted = predict(fm, x[intersect(idx.mar,idx.na)]) # predict values of y where missing
      y.star = y
      idx = sapply(fitted$x, function(i) which(x == i))

      if (length(fitted$x)!=0 & class(idx)!="list") y.star[idx] = fitted$y

      return(y.star) # return y with fitted values

    }

  }

  #2) Implied Forward Rates (Pat's)

  #' Compute implied forward rates
  #'
  #' @param mats Set of maturities
  #' @param yields Set of spot yields corresponding to the maturities
  #' @param delta Forward horizon
  #' @param plot logical: if TRUE, forward curve is plotted against spot curve
  #' @param shift logical: if TRUE, forward curve is shifted to right by delta
  #'
  #' @return
  #'
  #' @author Patrick Altmeyer (4517)

  impFwd = function(mats, yields, delta, plot=F, shift=F) {

    # 1.) Interpolate given horizon

    # 1.1) Expand vectors to include all interim maturities
    # mats.comp = unique(c(sapply(round(min(mats)):round(max(mats)), function(i) seq(i, i+1, by=ifelse(delta%%1<=0.5 & delta<1,delta,1-delta%%1)))))
    mats.comp =  c(delta ,sapply(mats, function(i) c(i,i+delta)))
    yields.comp.na = sapply(mats.comp, function(i) {

      ifelse(length(which(mats==i))==0,NA,yields[which(mats==i)])

    })

    # 1.2) Interpolate
    yields.comp = interpol(x=mats.comp, y=yields.comp.na, outer=T)

    # 2.) Compute forwards
    fwds = sapply(1:(length(mats)), function(i) {

      M = mats[i]
      M.delta = M+delta
      y.delta = yields.comp[which.min(abs(mats.comp-delta))]
      y.M.delta = yields.comp[which.min(abs(mats.comp-M.delta))] # use min here due to rounding issues when sequence is built above

      fwd.M = (1/M) * ( (y.M.delta * M.delta) - (y.delta * delta) )

    })

    # 3.) Plot
    if (plot) {

      par(mar=c(4,4,1,1))

      fwds.comp.na = sapply(mats.comp, function(i) {

        ifelse(length(which(mats==i))==0,NA,fwds[which(mats==i)])

      })

      fwds.comp = interpol(mats.comp, fwds.comp.na)

      # Spot rates and curve:
      plot(x=mats.comp, y=yields.comp.na, col="red", pch=16,
           xlab = "Maturity",
           ylab = "Yields",
           ylim=c(min(c(yields.comp,fwds.comp),na.rm=T), max(c(yields.comp,fwds.comp), na.rm=T)))
      points(x=mats.comp, y=yields.comp, t="l")

      # Forward rates and curve:
      points(x=mats.comp+ifelse(shift,delta,0), y=fwds.comp.na, col="blue", pch=16)
      points(x=mats.comp+ifelse(shift,delta,0), y=fwds.comp, col="blue", t="l", lty=2)

      legend("bottomright",
             legend=c("Interpolated spot curve",
                      "Spot rate",
                      "Interpolated fwd curve",
                      sprintf("Implied fwd rates (delta=%0.2f)",delta)),
             col=c("black",
                   "red",
                   "blue",
                   "blue"),
             lty=c(1,NA,2,NA),
             pch=c(NA,16,NA,16),
             bty="n",
             cex=0.75)

    }

    return(fwds)

  }

  #3) Create forward rates matrix

  MatrixFunc <-function(x) {

    # x = row number in Inflation Data

    length <- length(mats)+1

    yields <- unlist((Inflation_data[x,c(2:length)]), use.names = F)

    # sapply over list of deltas

    data <- sapply(forwards, function(i) impFwd(mats, yields, delta=i))

    return(data)

  }

  #4) Plot surface chart

  PlotSurface <- function(i) {

    # mats = set of maturites
    # forwards = set of forwards
    # i = row number from Inflation Data

    plot_ly(x = forwards, y = mats, z = MatrixFunc(i)) %>% add_surface(
      contours = list(
        z = list(
          show=TRUE,
          usecolormap=TRUE,
          highlightcolor="#ff0000",
          project=list(z=TRUE)
        )
      )
    ) %>%

      layout(title = "Inflation swaps forward surface",
             # xaxis = list(range = c(0:4),
             #yaxis = list(range = c(0, forwards[length(forwards)]),
             zaxis = list(range = c(2,4)),

             scene = list(
               xaxis = list(title = "Forwards (x)") ,
               yaxis = list(title = "Maturity (y)") ,
               zaxis = list(title = "Price (z)"))
      )

  }


  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("Date:", input$variable)
  })

  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })

  # Generate a plot of the requested variable ----

  output$surface <- renderPlotly({

    PlotSurface(which(Inflation_data[ ,1] == input$variable))


  })

}

shinyApp(ui, server)

