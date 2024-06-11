# Calculate the bounds from l_{xy} and u_{xy}
minimize = function(l,u)
{
  a = matrix(0,nrow = 2, ncol = 2,
             dimnames = list(rownames(l), colnames(l)))

  # Case 1:
  if(l[1,1]+l[2,2]+u[1,2]+u[2,1] <= 1)
  {
    # values we know in this case
    a[1,2] = u[1,2]; a[2,1] = u[2,1]

    # lower bound for a_{11}
    a[2,2] = max(l[2,2], 1-u[1,2]-u[2,1]-u[1,1])
    a[1,1] = 1 - (a[2,2] + a[1,2] + a[2,1])

    or = list(a = a, value = odds.ratio(a))

    # Upper bound for a_{11}
    a[2,2] = min(u[2,2], 1-u[1,2]-u[2,1]-l[1,1])
    a[1,1] = 1 - (a[2,2] + a[1,2] + a[2,1])

    if(odds.ratio(a) < or$value)
    {or = list(a = a, value = odds.ratio(a))}
  }

  # Case 2:
  else
  {
    # values we know in this case
    a[1,1] = l[1,1]; a[2,2] = l[2,2]

    # Calculating the value of a_{1,0}
    i = max(l[2,1], 1-l[1,1]-l[2,2]-u[1,2])
    j = min(u[2,1], 1-l[1,1]-l[2,2]-l[1,2])
    a[2,1] = sort(c(i, (1-l[1,1]-l[2,2])/2, j))[2]

    a[1,2] = 1 - a[2,2] - a[1,1] - a[2,1]
    or = list(a = a, value = odds.ratio(a))

  }
  return(or)
}
