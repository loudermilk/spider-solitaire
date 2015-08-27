#ss-game.R

SEED <- 1

setClass("Game",
         slots = list(piles = "list", stack = "list"))


Game <- function(difficulty) {
  
  difficulty_levels <- c("easy", "medium", "hard")
  if (!(difficulty %in% difficulty_levels)) stop("Unsupported Difficulty Level")
  
  if (difficulty == "easy") {
    d <- Deck(suits = c("S"), num_suits = 8)
  } else if (difficulty == "medium") {
    d <- Deck(suits = c("S", "H"), num_suits = 4)
  } else if (difficulty == "hard"){
    d <- Deck(suits = c("S", "H", "C", "D"), num_suits = 2)
  } else {
    # Should never get here
  }
  
  d <- shuffle(deck = d, seed = SEED)
  piles <- list()
  piles[[1]] <- Pile(cards = d@cards[1:6])    # <- 6 cards
  piles[[2]] <- Pile(cards = d@cards[7:12])   # <- 6 cards
  piles[[3]] <- Pile(cards = d@cards[13:18])  # <- 6 cards
  piles[[4]] <- Pile(cards = d@cards[19:24])  # <- 6 cards
  piles[[5]] <- Pile(cards = d@cards[25:29])  # <- 5 cards
  piles[[6]] <- Pile(cards = d@cards[30:34])  # <- 5 cards
  piles[[7]] <- Pile(cards = d@cards[35:39])  # <- 5 cards
  piles[[8]] <- Pile(cards = d@cards[40:44])  # <- 5 cards
  piles[[9]] <- Pile(cards = d@cards[45:49])  # <- 5 cards
  piles[[10]] <- Pile(cards = d@cards[50:54]) # <- 5 cards
  
  stack <- d@cards[55:104]
  
  g <- new(Class = "Game", piles = piles, stack = stack)
  return(g)
}