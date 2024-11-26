#' @import gurobi
#' @import Matrix

get_gurobi_model = function(o, random, Sigma)
{
  model <- list()
  model$o.hat = c(o)
  if(!missing(Sigma)) model$Sigma = Sigma

  o = model$o.hat / sum(model$o.hat)


  # Variable names
  model$varnames <- c('w',
                      `if`(random, c('t00', 't10', 't01', 't11'), NULL),   # t_xy = (o.hat_xy - o_xy)
                      'a00', 'a10', 'a01', 'a11',   # 1-4
                      'b00', 'b10', 'b01', 'b11',   # 5-8
                      'r00', 'r10', 'r01', 'r11',   # 9-12                 # r_xy = a_xy/b_xy
                      'p1', 'p2', 'p3', 'p4',       # 13-16
                      'q1', 'q2', 'q3')             # 17-19

  n.var = length(model$varnames)
  var = 1:n.var
  names(var) = model$varnames
  model$quadcon = list()

  # Objective function max q1 * q3
  model$Q <- spMatrix(n.var, n.var, i = c(var['q1']), j = var['q3'], x = c(1.0))

  ############################# Linear constraints ###########################################
  if (random)
  {

    model$A <- spMatrix(3, n.var, i = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
                        j = c( var['t00']:(var['t00']+ 3), var['a00']:(var['a00']+ 3) , var['b00']:(var['b00']+ 3)),
                        x = rep(1, 12))
    model$rhs = c(0, 1, 1)
    model$sense <- c('=', '=', '=')
  }

  else
  {
    model$A <- spMatrix(2, n.var, i = c(1, 1, 1, 1, 2, 2, 2, 2),
                        j = c( var['a00']:(var['a00']+ 3) , var['b00']:(var['b00']+ 3) ),
                        x = rep(1, 8))
    model$rhs <- c(1, 1)
    model$sense <- c('=', '=')
  }


  ############################# Quadratic Constraints #########################################

  if(random)
  {
    ## oxy = axy(1 - w) + bxy w
    for(index in 0:3)
    {
      qc = list()
      qc$Qc = spMatrix(n.var, n.var, i = c(var['a00'] + index, var['b00'] + index), j = c(var['w'], var['w']), x = c(-1, 1))
      qc$q = sparseVector(n.var, i = c(var['t00'] + index, var['a00'] + index), x = c(1, 1))
      qc$rhs = o[index + 1]
      qc$sense = '='


      model$quadcon = append(model$quadcon, list(qc))
    }
  }

  else
  {
    ## oxy = axy(1 - w) + bxy w
    for(index in 0:3)
    {
      qc = list()
      qc$Qc = spMatrix(n.var, n.var, i = c(var['a00'] + index, var['b00'] + index), j = c(var['w'], var['w']), x = c(-1, 1))
      qc$q = sparseVector(n.var, i = c(var['a00'] + index), x = c(1))
      qc$rhs = o[index + 1]
      qc$sense = '='


      model$quadcon = append(model$quadcon, list(qc))
    }
  }

  ## rxy bxy = axy
  for(index in 0:3)
  {
    qc = list()
    qc$Qc = spMatrix(n.var, n.var, i = c(var['b00'] + index), j = c(var['r00'] + index), x = c(1))
    qc$q = sparseVector(n.var, i = c(var['a00'] + index), x = c(-1))
    qc$rhs = 0
    qc$sense = '='


    model$quadcon = append(model$quadcon, list(qc))
  }


  ## r11 * r00 = p1
  qc = list()
  qc$Qc = spMatrix(n.var, n.var, i = var['r00'], j = var['r11'], x = c(1))
  qc$q = sparseVector(n.var, i = var['p1'], x = c(-1))
  qc$rhs = 0
  qc$sense = '='

  model$quadcon = append(model$quadcon, list(qc))

  ## r10 * r01 = p2
  qc = list()
  qc$Qc = spMatrix(n.var, n.var, i = var['r10'], j = var['r01'], x = c(1))
  qc$q = sparseVector(n.var, i = var['p2'], x = c(-1))
  qc$rhs = 0
  qc$sense = '='

  model$quadcon = append(model$quadcon, list(qc))

  ## p2p3 = 1
  qc = list()
  qc$Qc = spMatrix(n.var, n.var, i = var['p2'], j = var['p3'], x = c(1))
  qc$rhs = 1
  qc$sense = '='


  model$quadcon = append(model$quadcon, list(qc))

  ## p1p3 = p4
  qc = list()
  qc$Qc = spMatrix(n.var, n.var, i = var['p1'], j = var['p3'], x = c(1))
  qc$q = sparseVector(n.var, i = var['p4'], x = c(-1))
  qc$rhs = 0
  qc$sense = '='


  model$quadcon = append(model$quadcon, list(qc))



  # a11 * a00 = q1
  qc = list()
  qc$Qc = spMatrix(n.var, n.var, i = var['a00'], j = var['a11'], x = c(1))
  qc$q = sparseVector(n.var, i = var['q1'], x = c(-1))
  qc$rhs = 0
  qc$sense = '='

  model$quadcon = append(model$quadcon, list(qc))

  # a10 * a01 = q2
  qc = list()
  qc$Qc = spMatrix(n.var, n.var, i = var['a10'], j =var['a01'], x = c(1))
  qc$q = sparseVector(n.var, i = var['q2'], x = c(-1))
  qc$rhs = 0
  qc$sense = '='

  model$quadcon = append(model$quadcon, list(qc))

  # q2q3 = 1
  qc = list()
  qc$Qc = spMatrix(n.var, n.var, i = var['q2'], j = var['q3'], x = c(1))
  qc$rhs = 1
  qc$sense = '='


  model$quadcon = append(model$quadcon, list(qc))

  return(model)


}
