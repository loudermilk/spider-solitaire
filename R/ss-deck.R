# Define the 'Deck' class
setClass("Deck",
         slots = list(cards = "list"))

# Let's create a constructor function for 'Deck'

Deck <- function(suits = SUITS, ranks = RANKS, num_suits = 1) {
  cards <- list()
  i <- 1
  for (n in 1:num_suits) {
    for (s in suits) {
      for (r in ranks) {
        card <- Card(suit = s, rank = r)
        cards[[i]] <- card
        i <- i + 1
      }
    }
  }
  new(Class = "Deck", cards = cards)
}

# During 'Deck' creation create unique ids for each card
Deck <- function(suits = SUITS, ranks = RANKS, num_suits = 1) {
  cards <- list()
  id <- 1
  for (n in 1:num_suits) {
    for (s in suits) {
      for (r in ranks) {
        card <- Card(
          suit = s, rank = r, face_up = FALSE, id = id
        )
        cards[[id]] <- card
        id <- id + 1
      }
    }
  }
  new(Class = "Deck", cards = cards)
}


setGeneric("getSuits", function(x) {
  standardGeneric("getSuits")
})

setMethod("getSuits", "Deck", function(x) {
  last <- length(x@cards)
  suits <- c()
  for (i in 1:last) {
    s <- x@cards[[i]]@suit
    if (!(s %in% suits)) {
      suits <- c(suits, s)
    }
  }
  return(suits)
})

setGeneric("getIDs", function(x) {
  standardGeneric("getIDs")
})

setMethod("getIDs", "Deck", function(x) {
  ids <- c()
  for (i in 1:length(x@cards)) {
    id <- x@cards[[i]]@id
    ids <- c(ids, id)
  }
  return(ids)
})

setGeneric(
  name = "shuffle",
  def = function(deck,seed)
  {
    standardGeneric("shuffle")
  }
)

# Given a seed, return the deck back in a new random card order
setMethod(
  f = "shuffle",
  signature = "Deck",
  definition = function(deck, seed)
  {
    deck_size <- length(deck@cards)
    set.seed(seed)
    smpl <- sample(1:deck_size, deck_size)
    
    shuffled_cards <- deck@cards[smpl]
    deck@cards <- shuffled_cards
    return (deck)
  }
)