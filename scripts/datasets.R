# New England Journal of Medicine
new_england = function(data)
{
  data[,1] = data[,1] - data[,2]
  results = data.frame()
  o = matrix(nrow = 2, ncol = 2)
  counter = 1

  for(i in 1:nrow(data))
  {
    if(data[i, 3] == 1) o[1,] = as.matrix(data[i,c(1,2)])

    else
    {
      o[2,] = as.matrix(data[i,c(1,2)])
      results[counter, c("o00", "o10", "o01", "o11")] = c(o)

      counter = counter + 1
    }
  }

  return(results)
}
