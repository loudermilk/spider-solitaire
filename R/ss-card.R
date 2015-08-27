# ss-card.R

library(methods) # Where S4-stuff lives

# Define some constants
SUITS <- c("S", "H", "C", "D")
RANKS <- 1:13

Card <- setClass("Card",
                 slots = list(suit = "character", 
                              rank = "numeric", 
                              face_up = "logical",
                              id = "numeric"))



# Card constructor
Card <- function(suit, rank, face_up = FALSE, id = 0) {
  
  # Basic validation
  if(!(suit %in% SUITS)) stop(paste0("Invalid suit : ", suit))
  if (!(rank %in% RANKS)) stop(paste0("Invalid rank : ", rank))  
  
  new(Class = "Card", suit = suit, rank = rank, face_up = face_up, id = id)
}

setMethod(f = "show", 
          signature = "Card", 
          definition = function(object){
            if (object@face_up == TRUE) {
              rank <- object@rank
              if (rank < 10){
                rank <- paste0("0",rank)
              }
              show(paste0(object@suit,rank))
            } else {
              show("---")
            }
          })

setGeneric("flip", 
           function(x){
             standardGeneric("flip")
           })

setMethod("flip", 
          "Card", 
          function(x){x@face_up <- !(x@face_up)
          return(x)
          })