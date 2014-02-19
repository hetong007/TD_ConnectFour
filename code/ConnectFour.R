setClass("ConnectFour",
         slots = c(board = "matrix",
                   player = "numeric"))

ConnectFour = function(board=mat.or.vec(6,7),player=1)
{
    new("ConnectFour",board=board,player=player)
}

setMethod("show",
          signature = "ConnectFour",
          definition = function(object)
          {
              cat('Waiting for player',object@player,'\n')
              bd = object@board
              cbd = as.character(bd)
              cbd[which(cbd=='0')] = '.'
              cbd[which(cbd=='1')] = 'O'
              cbd[which(cbd=='-1')] = 'I'
              cbd = matrix(cbd,nrow=nrow(bd))
              cbd = data.frame(cbd)
              names(cbd) = as.character(1:7)
              show(cbd)
          }
)

setGeneric("player", function(object, ...) standardGeneric("player"))

setMethod("player",
          signature = "ConnectFour",
          definition = function(object, ...)
              object@player
)

setGeneric("board", function(object, ...) standardGeneric("board"))

setMethod("board",
          signature = "ConnectFour",
          definition = function(object, ...)
              object@board
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

setGeneric("changePlayer", function(object, ...) standardGeneric("changePlayer"))

setMethod("changePlayer",
          signature = "ConnectFour",
          definition = function(object, ...)
          {
              object@board = -object@board
              object
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
              ind = which(bd[,i]!=0)[1]-1
              if (is.na(ind))
                  ind = nrow(bd)
              bd[ind,i] = 1
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
                  for (j in 1:m)
                  {
                      tmp = bd[i,j]
                      if (tmp!=0)
                      {
                          cnt = 1
                          while (cnt<4 && bd[i+cnt,j]==tmp)
                              cnt = cnt+1
                          if (cnt>=4)
                              return(tmp)
                      }
                  }
              
              for (i in 1:n)
                  for (j in 1:(m-3))
                  {
                      tmp = bd[i,j]
                      if (tmp!=0)
                      {
                          cnt = 1
                          while (cnt<4 && bd[i,j+cnt]==tmp)
                              cnt = cnt+1
                          if (cnt>=4)
                              return(tmp)
                      }
                  }
              
              for (i in 1:(n-3))
                  for (j in 1:(m-3))
                  {
                      tmp = bd[i,j]
                      if (tmp!=0)
                      {
                          cnt = 1
                          while (cnt<4 && bd[i+cnt,j+cnt]==tmp)
                              cnt = cnt+1
                          if (cnt>=4)
                              return(tmp)
                      }
                  }
              
              for (i in 4:n)
                  for (j in 1:(m-3))
                  {
                      tmp = bd[i,j]
                      if (tmp!=0)
                      {
                          cnt = 1
                          while (cnt<4 && bd[i-cnt,j+cnt]==tmp)
                              cnt = cnt+1
                          if (cnt>=4)
                              return(tmp)
                      }
                  }
              
              return(0)
          }
)
