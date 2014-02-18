#source both files

playConnectFour = function(layers=c(42,100,3),W=NULL,
                           alpha=0.1,beta=alpha,lambda=0.7,
                           learning=TRUE)
{
    if (is.null(W))
        W = ParameterInitializer(layers)
    cf = ConnectFour()
    ind = ceiling(runif(1)*7)
    cf = play(cf,ind)
    record = list()
    record[[1]] = cf
    counter = 1
    i = layers[1]
    j = layers[2]
    k = layers[3]
    if (k!=3)
        stop("Wrong layers")
    
    e2 = mat.or.vec(k,j)
    e3 = rep(list(mat.or.vec(j,i)),k)
    x = rep(0,i)
    h = rep(0,j)
    y = c(0.5,0,0.5)
    prev = list(e2=e2,e3=e3,h=h,x=x,y=y)
    
    Ending = FALSE
    result = 0
    
    while(!Ending)
    {
        moves = validStep(cf)
        mx = -1
        mxi = 0
        for (i in moves)
        {
            tbd = play(cf,i)
            #pred = predict(tbd,W)
            pred = ForwardPropagation(tbd,W)
            pred = pred[2-player(cf)]
            #cat(pred,counter,'\n')
            if (pred>mx)
            {
                mx = pred
                mxi = i
            }
        }
        tbd = play(cf,mxi)
        result = win(tbd)
        if (result!=0 || all(board(tbd)!=0))
            Ending = TRUE
            
        counter = counter+1
        record[[counter]] = tbd
        cf = tbd
        if (learning)
        {
            if (Ending)
            {
                if (result==1)
                {
                    prev$y = c(1,0,0)
                }
                else if (result==-1)
                {
                    prev$y = c(0,0,-1)
                }
                else
                    prev$y = c(0,1,0)
            }
            
            upd = updateW(tbd,W,alpha,beta,lambda,prev)
            W = upd[[1]]
            prev = upd[[2]]
        }
    }
    list(record,W)
}

trainWeight = function(W=NULL, layers = c(42,100,3), time = 100)    
{
    if (is.null(W))
        W = ParameterInitializer(layers)
    for (i in 1:time)
    {
        cat(i,'\n')
        res = playConnectFour(layers=layers,W=W)
        W = res[[2]]
    }
    return(W)
}

#W = trainWeight()
#W = trainWeight(W=W)

compete = function(W1,W2,rounds=10)
{
    
}

