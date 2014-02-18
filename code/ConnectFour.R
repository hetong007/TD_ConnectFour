setClass("ConnectFour",
         slots = c(board = "matrix",
                   player = "numeric"))

ConnectFour = function(board=mat.or.vec(6,7),player=1)
{
    new("ConnectFour",board=board,player=player)
}

setGeneric("player", function(object, ...) standardGeneric("player"))

setMethod("player",
          signature = "ConnectFour",
          definition = function(object, ...)
              bd = object@player
)

setGeneric("board", function(object, ...) standardGeneric("board"))

setMethod("board",
          signature = "ConnectFour",
          definition = function(object, ...)
              bd = object@board
)

setGeneric("validStep", function(object, ...) standardGeneric("validStep"))

setMethod("validStep",
          signature = "ConnectFour",
          definition = function(object, ...)
          {
              bd = object@board
              ans = which(bd[1,]==0)
              ans
          }
)

setGeneric("play", function(object, ...) standardGeneric("play"))

setMethod("play",
          signature = "ConnectFour",
          definition = function(object, i, ...)
          {
              bd = object@board
              plyr = object@player
              if (i<1 || i>ncol(bd) || bd[1,i]!=0)
                  stop('Invalid move.')
              ind = which(bd[,i]>0)[1]-1
              if (is.na(ind))
                  ind = nrow(bd)
              bd[ind,i] = plyr
              ans = ConnectFour(bd,-plyr)
              ans
          }
)

setGeneric("win", function(object, ...) standardGeneric("win"))

setMethod("win",
          signature = "ConnectFour",
          definition = function(object, ...)
          {
              bd = object@board
              n = nrow(bd)
              m = ncol(bd)
              for (i in 1:(n-3))
                  for (j in 1:(m-3))
                  {
                      tmp = bd[i,j]
                      
                      cnt = 1
                      while (cnt<4 && bd[i+cnt,j]==tmp)
                          cnt = cnt+1
                      if (cnt>=4)
                          return(object@player)
                      
                      cnt = 1
                      while (cnt<4 && bd[i,j+cnt]==tmp)
                          cnt = cnt+1
                      if (cnt>=4)
                          return(object@player)
                      
                      cnt = 1
                      while (cnt<4 && bd[i+cnt,j+cnt]==tmp)
                          cnt = cnt+1
                      if (cnt>=4)
                          return(object@player)
                  }
              return(0)
          }
)
