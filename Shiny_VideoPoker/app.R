library(shiny)
library(shinyjs)
library(stringr)
library(dplyr)
library(forcats)
library(ggplot2)
library(plotly)



cardToSourceString <- function(card) {
  return(paste("www/", card, ".png", sep = ""))
}

extractFaceValue <- function(card) {
  if (nchar(card) == 2) {
    return(substr(card, 1, 1))
  } else {
    return(substr(card, 1, 2))
  }
}

extractSuit <- function(card) {
  return(substr(card, nchar(card), nchar(card)))
}

# function to determine winning hand combinations
determine <- function(hand) {
  
  hand <- sort(hand)
  
  card1 <- hand[1]
  card2 <- hand[2]
  card3 <- hand[3]
  card4 <- hand[4]
  card5 <- hand[5]
  
  card1Face <- extractFaceValue(card1)
  card2Face <- extractFaceValue(card2)
  card3Face <- extractFaceValue(card3)
  card4Face <- extractFaceValue(card4)
  card5Face <- extractFaceValue(card5)
  
  card1Suit <- extractSuit(card1)
  card2Suit <- extractSuit(card2)
  card3Suit <- extractSuit(card3)
  card4Suit <- extractSuit(card4)
  card5Suit <- extractSuit(card5)
  
  faces <- c(card1Face, card2Face, card3Face, card4Face, card5Face)
  
  
  
  # functions for determining winning hand combinations by breaking them down
  isJacksOrBetter <- function(hand) {
    # identify a pair of Jacks of better
    # first, convert the vector of faces to a string
    strFaces <- paste(faces, collapse = "")
    
    return(
      xor(
        str_detect(strFaces, "JJ"),
        xor(
          str_detect(strFaces, "QQ"),
          xor(
            str_detect(strFaces, "KK"),
            str_detect(strFaces, "AA")
          )
        )
      )
    )
  }
  
  isTwoPairs <- function(hand) {
    # now after sorting, each pair should be right next to each other
    # There are three ways to arrange 2 consecutive pairs
    return(
      xor(
        card1Face == card2Face & card3Face == card4Face,
        xor(
          card2Face == card3Face & card4Face == card5Face,
          card1Face == card2Face & card4Face == card5Face
        )
      )
    )
  }
  
  isThreeOfAKind <- function(hand) {
    # three ways to arrange three of a kind after sorting
    return(
      xor(
        card1Face == card2Face & card2Face == card3Face,
        xor(
          card2Face == card3Face & card3Face == card4Face,
          card3Face == card4Face & card4Face == card5Face
        )
      )
    )
  }
  
  isStraight <- function(hand) {
    # five cards in a sequence
    # 9 combinations
    return(
      xor(
        sum(2:6 %in% faces) == 5,
        xor(
          sum(3:7 %in% faces) == 5,
          xor(
            sum(4:8 %in% faces) == 5,
            xor(
              sum(5:9 %in% faces) == 5,
              xor(
                sum(6:10 %in% faces) == 5,
                xor(
                  sum(c("7", "8", "9", "10", "J") %in% faces) == 5,
                  xor(
                    sum(c("8", "9", "10", "J", "Q") %in% faces) == 5,
                    xor(
                      sum(c("9", "10", "J", "Q", "K") %in% faces) == 5,
                      sum(c("10", "J", "Q", "K", "A") %in% faces) == 5
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }
  
  isFlush <- function(hand) {
    # 5 cards of the same suit
    return(
      card1Suit == card2Suit &
      card2Suit == card3Suit &
      card3Suit == card4Suit &
      card4Suit == card5Suit
    )
  }
  
  isFullHouse <- function(hand) {
    # two ways to arrange a full house after sorting
    return(
      xor(
        card1Face == card2Face & card2Face == card3Face & card4Face == card5Face,
        card1Face == card2Face & card3Face == card4Face & card4Face == card5Face
      )
    )
  }
  
  isFourOfAKind <- function(hand) {
    # after sorting, cards with the same face should be right next to each other
    # so there are 2 ways to arrange four of a kind now
    return(
      xor(
        card1Face == card2Face & card2Face == card3Face & card3Face == card4Face,
        card2Face == card3Face & card3Face == card4Face & card4Face == card5Face
      )
    )
  }
  
  isStraightFlush <- function(hand) {
    # has to be a straight and a flush
    return(isStraight(hand) & isFlush(hand))
  }
  
  isRoyalFlush <- function(hand) {
    # a special kind of straight flush
    return(
      isStraightFlush(hand) &
      sum(c("10", "J", "Q", "K", "A") %in% faces) == 5
    )
  }
  
  
  
  # connecting the pieces together
  # start checking from the highest rank
  if (isRoyalFlush(hand)) {
    return ("Royal Flush!");
  } else {
    if (isStraightFlush(hand)) {
      return ("Straight Flush!");
    } else {
      if (isFourOfAKind(hand)) {
        return ("Four of a Kind!");
      } else {
        if (isFullHouse(hand)) {
          return ("Full House!");
        } else {
          if (isFlush(hand)) {
            return ("Flush!");
          } else {
            if (isStraight(hand)) {
              return ("Straight!");
            } else {
              if (isThreeOfAKind(hand)) {
                return ("Three of a Kind!");
              } else {
                if (isTwoPairs(hand)) {
                  return ("Two Pairs!");
                } else {
                  if (isJacksOrBetter(hand)) {
                    return ("Jacks or Better!");
                  } else {
                    return ("You Lose!");
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
}



ui <- fluidPage(
  useShinyjs(),
  
  fluidRow(column(12, tags$h2("Video Poker - The Shiny App"), align = "center")),
  fluidRow(column(12, tags$h4("Tom Zhang, SLU '22", style = "color:navy"), align = "center")),
  
  tabsetPanel(
    
    selected = "The Game",
    
    
    
    tabPanel(
      "How to Play",
      
      br(),
      fluidRow(column(11, tags$h3("Game Play", style = "color:firebrick"), offset = 1)),
      fluidRow(column(11, tags$h4('(1) Press "Deal" to get five random cards from the deck.'), offset = 1)),
      fluidRow(column(11, tags$h4('(2) Hold any number of cards by clicking on the corresponding buttons. To cancel a hold, hit the button again.'), offset = 1)),
      fluidRow(column(11, tags$h4('(3) Click on "Draw" to replace the cards you did not hold.'), offset = 1)),
      fluidRow(column(11, tags$h4("(4) The final combination of your cards (called a player's hand) determines the outcome of the game."), offset = 1)),
      br(),
      fluidRow(column(11, tags$h3("Winning Hands", style = "color:firebrick"), offset = 1)),
      fluidRow(column(11, tags$h4("There are nine types of winning hands, which rank as follows:"), offset = 1)),
      fluidRow(column(10, tags$h4("(1) Royal Flush: 10 to Ace of a single suit"), offset = 2)),
      fluidRow(column(10, tags$h4("(2) Straight Flush: Five consecutive cards of the same suit"), offset = 2)),
      fluidRow(column(10, tags$h4("(3) Four of a Kind: Four cards of the same face value"), offset = 2)),
      fluidRow(column(10, tags$h4("(4) Full House: Three cards of one face value, along with two cards of another face value"), offset = 2)),
      fluidRow(column(10, tags$h4("(5) Flush: Five cards of the same suit"), offset = 2)),
      fluidRow(column(10, tags$h4("(6) Straight: Five consecutive cards of any suit"), offset = 2)),
      fluidRow(column(10, tags$h4("(7) Three of a Kind: Three cards of the same face value"), offset = 2)),
      fluidRow(column(10, tags$h4("(8) Two Pairs: Two cards of the same face value, along with two cards of another face value"), offset = 2)),
      fluidRow(column(10, tags$h4("(9) Jacks or Better: A pair of Jacks, Queens, Kings, or Aces"), offset = 2))
      #fluidRow(column(11, tags$h3("Note", style = "color:firebrick"), offset = 1)),
      #fluidRow(column(11, tags$h5(tags$em(tagList('Card images downloaded from:', tags$a("American Contract Bridge League", href = "http://acbl.mybigcommerce.com/52-playing-cards/")))), align = "center", offset = 1))
      
    ),
    
    
    
    tabPanel(
      "The Game",
      
      br(),
      fluidRow(
        column(3, uiOutput("dealUI"), offset = 1),
        column(4, uiOutput("resultUI"), align = "center"),
        column(3, actionButton("reset", "Reset", width = "100%", class = "btn-danger"))
      ),
      tags$br(),
      
      #style = "color: #FF3333"; class = "btn-warning"
      fluidRow(
        column(2, uiOutput("hold1UI"), offset = 1),
        column(2, uiOutput("hold2UI")),
        column(2, uiOutput("hold3UI")),
        column(2, uiOutput("hold4UI")),
        column(2, uiOutput("hold5UI"))
      ),
      tags$br(),
      
      fluidRow(
        column(2, imageOutput("icon1"), offset = 1),
        column(2, imageOutput("icon2")),
        column(2, imageOutput("icon3")),
        column(2, imageOutput("icon4")),
        column(2, imageOutput("icon5"))
      ),
      
      fluidRow(column(12, tags$h5(tags$em(tagList('Card images downloaded from:', tags$a("American Contract Bridge League", href = "http://acbl.mybigcommerce.com/52-playing-cards/")))), align = "center"))
      
    ),
    
    
    
    tabPanel(
      "Statistics",
      
      br(),
      fluidRow(
        column(
          width = 5, offset = 0,
          fluidRow(column(11, tags$h3("Summary", style = "color:firebrick"), offset = 1)),
          br(),
          fluidRow(column(12, plotOutput("bar")))
        ),
        column(
          width = 7, offset = 0,
          fluidRow(column(11, tags$h3("Records", style = "color:firebrick"), offset = 1)),
          br(),
          fluidRow(column(12, (plotlyOutput("line"))))
        )
      )

    )
    
  )
  
)



server <- function(input, output, session) {
  
  cards <- c(
    "AC", "AD", "AH", "AS",
    "2C", "2D", "2H", "2S",
    "3C", "3D", "3H", "3S",
    "4C", "4D", "4H", "4S",
    "5C", "5D", "5H", "5S",
    "6C", "6D", "6H", "6S",
    "7C", "7D", "7H", "7S",
    "8C", "8D", "8H", "8S",
    "9C", "9D", "9H", "9S",
    "10C", "10D", "10H", "10S",
    "JC", "JD", "JH", "JS",
    "QC", "QD", "QH", "QS",
    "KC", "KD", "KH", "KS"
  )
  
  hand <- "foo"
  
  holdStatus <- c(F, F, F, F, F)
  
  dealCounter <- 0
  
  hold1Counter <- 0
  hold2Counter <- 0
  hold3Counter <- 0
  hold4Counter <- 0
  hold5Counter <- 0
  
  values <- reactiveValues()
  
  values$icon1Source <- cardToSourceString("purple_back")
  values$icon2Source <- cardToSourceString("purple_back")
  values$icon3Source <- cardToSourceString("purple_back")
  values$icon4Source <- cardToSourceString("purple_back")
  values$icon5Source <- cardToSourceString("purple_back")
  
  
  
  output$dealUI <- renderUI({
    actionButton("deal", "Deal", width = "100%", class = "btn-success")
  })
  
  output$resultUI <- renderUI({
    tags$h4(textOutput("result"), style = "color:white")
  })
  
  output$result <- renderText({
    "."
  })
  
  
  
  output$hold1UI <- renderUI({
    actionButton("hold1", "Hold", width = "100%")
  })
  
  output$hold2UI <- renderUI({
    actionButton("hold2", "Hold", width = "100%")
  })
  
  output$hold3UI <- renderUI({
    actionButton("hold3", "Hold", width = "100%")
  })
  
  output$hold4UI <- renderUI({
    actionButton("hold4", "Hold", width = "100%")
  })
  
  output$hold5UI <- renderUI({
    actionButton("hold5", "Hold", width = "100%")
  })
  
  
  
  output$icon1 <- renderImage({
    list(src = values$icon1Source,
         contentType = 'png',
         width = "100%",
         height = "auto")
  }, deleteFile = F)
  
  output$icon2 <- renderImage({
    list(src = values$icon2Source,
         contentType = 'png',
         width = "100%",
         height = "auto")
  }, deleteFile = F)
  
  output$icon3 <- renderImage({
    list(src = values$icon3Source,
         contentType = 'png',
         width = "100%",
         height = "auto")
  }, deleteFile = F)
  
  output$icon4 <- renderImage({
    list(src = values$icon4Source,
         contentType = 'png',
         width = "100%",
         height = "auto")
  }, deleteFile = F)
  
  output$icon5 <- renderImage({
    list(src = values$icon5Source,
         contentType = 'png',
         width = "100%",
         height = "auto")
  }, deleteFile = F)
  
  
  
  output$bar <- renderPlot({
    if (length(values$gameRecords) == 0) {
      return(NULL)
    } else {
      df <- tibble(Outcome = values$gameRecords) %>%
        group_by(Outcome) %>%
        summarise(
          Count = n()
        ) %>% 
        ungroup() %>%
        arrange(desc(Count)) %>%
        mutate(Outcome = fct_rev(fct_inorder(factor(Outcome))))
      df %>%
        ggplot(., aes(x = Outcome, y = Count, fill = Outcome)) +
        geom_col(width = 0.7) +
        scale_y_continuous(breaks = c(0, seq(1:max(df$Count)))) +
        coord_flip() +
        scale_fill_viridis_d() +
        labs(
          title ="Bar Plot of Count for Each Outcome"
        ) +
        theme_classic() +
        theme(
          axis.title.y = element_blank(),
          axis.text = element_text(size = rel(1.5)),
          axis.text.x = element_text(angle = 30),
          axis.title.x = element_text(size = rel(1.5)),
          plot.title = element_text(size = rel(1.5)),
          legend.position = "none"
        )
    }
  })
  
  output$line <- renderPlotly({
    if (length(values$gameRecords) == 0) {
      return(NULL)
    } else {
      df <- tibble(Outcome = values$gameRecords) %>%
        mutate(
          Game = 1:length(Outcome),
          Outcome = factor(
            Outcome,
            levels = c(
              "You Lose!",
              "Jacks or Better!",
              "Two Pairs!",
              "Three of a Kind!",
              "Straight!",
              "Flush!",
              "Full House!",
              "Four of a Kind!",
              "Straight Flush!",
              "Royal Flush!"
            )
          ),
          Group = 1
        )
      g <-
      df %>%
        ggplot(., aes(x = Game, y = Outcome, group = Group)) +
        geom_line(size = 1.15) +
        geom_point(
          aes(
            text = paste(
              "Game: ", Game,
              "\nOutcome: ", Outcome
            )
          )
        ) +
        scale_x_continuous(breaks = c(0, seq(1:max(df$Game)))) +
        labs(
          title = "Line Plot of Game Records"
        ) +
        theme_classic() +
        theme(
          axis.title.y = element_blank(),
          axis.text = element_text(size = rel(1.1)),
          axis.text.x = element_text(angle = 30),
          axis.title.x = element_text(size = rel(1.1)),
          legend.position = "none"
        )
      ggplotly(g, tooltip = "text")
    }
  })
  
  
  
  observeEvent(input$deal, {
    
    dealCounter <<- dealCounter + 1
    
    if (dealCounter == 1) {
      cards <<- sample(cards) # shuffle deck of cards
      hand <<- cards[1:5] # pick 5 to add to player's hand
      cards <<- cards[-1:-5] # remove those 5 from the deck
      
      # display player's hand visually
      values$icon1Source <- cardToSourceString(hand[1])
      delay(250, values$icon2Source <- cardToSourceString(hand[2]))
      delay(500, values$icon3Source <- cardToSourceString(hand[3]))
      delay(750, values$icon4Source <- cardToSourceString(hand[4]))
      delay(1000, values$icon5Source <- cardToSourceString(hand[5]))
      
      delay(
        1000,
        output$dealUI <- renderUI({
          actionButton("deal", "Draw", width = "100%", class = "btn-success")
        })
      )
    }
    
    if (dealCounter == 2) {
      # loop through the hold status vector, if false, then add those corresponding cards
      # (which the player did not hold) to the deck of cards
      for (i in 1:5) {
        if (holdStatus[i] == F) {
          cards <<- c(cards, hand[i])
        }
      }
      
      # shuffle the deck and replace the "unholded slots" in player's hand with
      # randomly selected cards from the deck
      cards <<- sample(cards)
      for (j in 1:5) {
        if (holdStatus[j] == F) {
          hand[j] <<- cards[j]
        }
      }
      
      # update the visualization
      for (k in 1:5) {
        if (holdStatus[k] == F) {
          values[[paste("icon", k, "Source", sep = "")]] <- cardToSourceString("purple_back")
        }
      }
      
      delayTimer <- 500
      
      if (holdStatus[1] == F) {
        delay(delayTimer, values$icon1Source <- cardToSourceString(hand[1]))
        delayTimer <- delayTimer + 250
      }
      
      if (holdStatus[2] == F) {
        delay(delayTimer, values$icon2Source <- cardToSourceString(hand[2]))
        delayTimer <- delayTimer + 250
      }
      
      if (holdStatus[3] == F) {
        delay(delayTimer, values$icon3Source <- cardToSourceString(hand[3]))
        delayTimer <- delayTimer + 250
      }
      
      if (holdStatus[4] == F) {
        delay(delayTimer, values$icon4Source <- cardToSourceString(hand[4]))
        delayTimer <- delayTimer + 250
      }
      
      if (holdStatus[5] == F) {
        delay(delayTimer, values$icon5Source <- cardToSourceString(hand[5]))
        delayTimer <- delayTimer + 250
      }
      
      # determine if player's hand is a winning combination
      delay(
        delayTimer <- delayTimer + 250,
        
        output$resultUI <- renderUI({
          tags$h4(textOutput("result"), style = "color:red")
        })
      )
      
      delay(delayTimer,
            output$result <- renderText({
              determine(hand)
            }))
      
      delay(delayTimer, values$gameRecords <- c(values$gameRecords, determine(hand)))
    }
    
  })
  
  
  
  observeEvent(input$reset, {
    
    hold1Counter <<- 0
    hold2Counter <<- 0
    hold3Counter <<- 0
    hold4Counter <<- 0
    hold5Counter <<- 0
    
    dealCounter <<- 0
    
    cardBack <- sample(c("red_back", "purple_back", "blue_back",
                         "gray_back", "yellow_back", "green_back"), 1)
    values$icon1Source <- cardToSourceString(cardBack)
    values$icon2Source <- cardToSourceString(cardBack)
    values$icon3Source <- cardToSourceString(cardBack)
    values$icon4Source <- cardToSourceString(cardBack)
    values$icon5Source <- cardToSourceString(cardBack)
    
    output$resultUI <- renderUI({
      tags$h4(textOutput("result"), style = "color:white")
    })
    
    output$result <- renderText({
      "."
    })
    
    cards <<- c(
      "AC", "AD", "AH", "AS",
      "2C", "2D", "2H", "2S",
      "3C", "3D", "3H", "3S",
      "4C", "4D", "4H", "4S",
      "5C", "5D", "5H", "5S",
      "6C", "6D", "6H", "6S",
      "7C", "7D", "7H", "7S",
      "8C", "8D", "8H", "8S",
      "9C", "9D", "9H", "9S",
      "10C", "10D", "10H", "10S",
      "JC", "JD", "JH", "JS",
      "QC", "QD", "QH", "QS",
      "KC", "KD", "KH", "KS"
    )
    
    hand <<- "foo"
    
    holdStatus <<- c(F, F, F, F, F)
    
    output$hold1UI <- renderUI({
      actionButton("hold1", "Hold", width = "100%")
    })
    
    output$hold2UI <- renderUI({
      actionButton("hold2", "Hold", width = "100%")
    })
    
    output$hold3UI <- renderUI({
      actionButton("hold3", "Hold", width = "100%")
    })
    
    output$hold4UI <- renderUI({
      actionButton("hold4", "Hold", width = "100%")
    })
    
    output$hold5UI <- renderUI({
      actionButton("hold5", "Hold", width = "100%")
    })
    
    output$dealUI <- renderUI({
      actionButton("deal", "Deal", width = "100%", class = "btn-success")
    })
    
  })
  
  
  
  observeEvent(input$hold1, {
    if (dealCounter == 1) {
      hold1Counter <<- hold1Counter + 1
      
      if (hold1Counter %% 2 == 1) {
        holdStatus[1] <<- T
        output$hold1UI <- renderUI({
          actionButton("hold1", "Hold", width = "100%", class = "btn-warning")
        })
      } else {
        holdStatus[1] <<- F
        output$hold1UI <- renderUI({
          actionButton("hold1", "Hold", width = "100%")
        })
      }
    }
  })
  
  observeEvent(input$hold2, {
    if (dealCounter == 1) {
      hold2Counter <<- hold2Counter + 1
      
      if (hold2Counter %% 2 == 1) {
        holdStatus[2] <<- T
        output$hold2UI <- renderUI({
          actionButton("hold2", "Hold", width = "100%", class = "btn-warning")
        })
      } else {
        holdStatus[2] <<- F
        output$hold2UI <- renderUI({
          actionButton("hold2", "Hold", width = "100%")
        })
      }
    }
  })
  
  observeEvent(input$hold3, {
    if (dealCounter == 1) {
      hold3Counter <<- hold3Counter + 1
      
      if (hold3Counter %% 2 == 1) {
        holdStatus[3] <<- T
        output$hold3UI <- renderUI({
          actionButton("hold3", "Hold", width = "100%", class = "btn-warning")
        })
      } else {
        holdStatus[3] <<- F
        output$hold3UI <- renderUI({
          actionButton("hold3", "Hold", width = "100%")
        })
      }
    }
  })
  
  observeEvent(input$hold4, {
    if (dealCounter == 1) {
      hold4Counter <<- hold4Counter + 1
      
      if (hold4Counter %% 2 == 1) {
        holdStatus[4] <<- T
        output$hold4UI <- renderUI({
          actionButton("hold4", "Hold", width = "100%", class = "btn-warning")
        })
      } else {
        holdStatus[4] <<- F
        output$hold4UI <- renderUI({
          actionButton("hold4", "Hold", width = "100%")
        })
      }
    }
  })
  
  observeEvent(input$hold5, {
    if (dealCounter == 1) {
      hold5Counter <<- hold5Counter + 1
      
      if (hold5Counter %% 2 == 1) {
        holdStatus[5] <<- T
        output$hold5UI <- renderUI({
          actionButton("hold5", "Hold", width = "100%", class = "btn-warning")
        })
      } else {
        holdStatus[5] <<- F
        output$hold5UI <- renderUI({
          actionButton("hold5", "Hold", width = "100%")
        })
      }
    }
  })
  
}



shinyApp(ui, server)


