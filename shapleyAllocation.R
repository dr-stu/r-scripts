library(permute)
library(data.table)

# P&L vectors in matrix
generate_portfolio = function(trades, scenarios){
  tradeId = paste("t", sprintf("%002d", seq_len(trades)), sep ="")
  sensi = runif(trades, -100,100)
  pnl = sensi * matrix(rnorm(trades*scenarios,0,1), trades, scenarios)
  rownames(pnl) = tradeId      
  return(pnl)
}

# portfolio = generate_portfolio(5,10)

var = function(portfolio, tradepos){
  if(length(tradepos) == 1){
    pnlTot = portfolio[tradepos,]
  } else{
  pnlTot = colSums(portfolio[tradepos,])
  }
  return(quantile(pnlTot, 0.025))
}

# var(portfolio, c(1,3,5))

marginalVar = function(portfolio, v){
  result <- vector("numeric", length(v))
  for (i in seq_len(length(v))){
    print[i]
    print(v[1:i])
    result[i] = var(portfolio, v[1:i])
  }
  marginal = c(result[1], diff(result))
  names(marginal) = v
  return(marginal)
}

calculateShapelyAllocation = function(portfolio){
  v = seq_len(nrow(portfolio))
  tradeOrder = rbind(v, allPerms(v))
  # Generate marginal results for each trade for each permutation
  result = NULL
  for (i in seq_len(nrow(tradeOrder))){
    out = marginalVar(portfolio, tradeOrder[i,])
    print(out)
    result = rbind(result, data.table(tradepos = names(out), marginalVar = out))
  }
  return(result[,list(allocation = mean(marginalVar)), by = tradepos])
}

portfolio = generate_portfolio(5,250)
allocation = calculateShapelyAllocation(portfolio)

# Test allocation adds back up to the portfolio value
abs(var(portfolio, seq_len(nrow(portfolio))) - sum(allocation$allocation)) < 0.01
