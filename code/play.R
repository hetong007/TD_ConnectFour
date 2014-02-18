#source both files

playConnectFour = function()
{
    W = ParameterInitializer(c(42,100,3))
    cf = ConnectFour()
    record = list()
    record[[1]] = cf
    counter = 1
    
    Ending = FALSE
    
    while(!Ending)
    {
        moves = validStep(cf)
        mx = 0
        for (i in moves)
        {
            tbd = play(cf,i)
            #pred = predict(tbd,W)
            pred = ForwardPropagation(tbd,W)
            if (pred>mx)
            {
                mx = pred
                mxi = i
            }
        }
        tbd = play(cf,mxi)
        result = win(tbd)
        if (result!=0 || all(tbd!=0))
            Ending = TRUE
            
        if (!learning)
        {
            counter = counter+1
            record[[counter]] = tbd
        }
        else
        {
            #update w by
        }
    }
    list(record,W)
}
