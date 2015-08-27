
setClass("Pile", 
         slots = list(cards = "list"))

pile <- new(Class = "Pile")


Pile <- function(cards = NULL) {
  p <- new(Class="Pile", cards = cards)
  return(p)
}