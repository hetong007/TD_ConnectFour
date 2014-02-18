#source('ConnectFour.R')

ParameterInitializer = function(layers)
{
    n = length(layers)
    W = vector(n-1,mode='list')
    for (i in 1:(n-1))
        W[[i]] = matrix(rnorm(layers[i]*layers[i+1],mean=0,sd=0.1),
                        ncol=layers[i])
    W
}

ForwardPropagation = function(cf,W)
{
    f = function(z) 1/(1+exp(-z))
    
    if (is.null(W))
        stop('Not enough input.')
    n = length(W)
    
    x = as.vector(board(cf))
    plyr = player(cf)

    a = vector(n+1,mode='list')
    a[[1]] = x
    z = a

    for (i in 1:n)
    {
        z[[i+1]] = W[[i]]%*%a[[i]]
        a[[i+1]] = f(z[[i+1]])
    }
    
    res = a[[n+1]][2-plyr]
    return(res)
}

updateW = function()
    
    
    
    
    
    