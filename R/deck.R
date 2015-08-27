SUITS <- c("S","H","C","D")
RANKS <- 1:13

setClass("Card",
         slots = list(suit = "character", rank = "numeric", face_up = "logical"))


setClass("Deck",
         slots = list(cards = "list"))

c1 <- new(Class = "Card", suit = "H", rank = 1)



Card <- function(suit, rank, face_up = FALSE) {
  
  
  if(!(suit %in% SUITS)) stop(paste0("Invalid suit : ", suit))
  if (!(rank %in% RANKS)) stop(paste0("Invalid rank : ", rank))  
  
  new(Class = "Card", suit = suit, rank = rank, face_up = face_up)
}


Deck <- function(suits = c("S","H","C","D"), ranks = 1:13, num_suits = 1) {
  cards <- list()
  id <- 1
  for (n in 1:num_suits) {
    for (s in suits) {
      for (r in ranks) {
        card <- Card(suit = s, rank = r, id = id)
        cards[[id]] <- card
        id <- id + 1
      }
    }
  }
  new(Class = "Deck", cards = cards)
}

setGeneric(name="shuffle",
           def=function(deck,seed)
           {
             standardGeneric("shuffle")
           }
)

setMethod(f="shuffle",
          signature="Deck",
          definition=function(deck, seed)
          {
            deck_size <- length(deck@cards)
            set.seed(seed)
            smpl <- sample(1:deck_size, deck_size)
            
            shuffled_cards <- deck@cards[smpl]
            deck@cards <- shuffled_cards
            return (deck)
          }
)
