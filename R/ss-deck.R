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