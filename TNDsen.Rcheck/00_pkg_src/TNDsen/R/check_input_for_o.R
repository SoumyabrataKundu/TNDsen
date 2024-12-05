check_input_for_o = function(o)
{
  if(length(o) != 4){stop("Input should be 2X2 matrix or an array of length 4")}
  if(any(o<0)){stop("The entries must be non-negative")}
  if(sum(o) == 0){stop("The sum of entries must be positive")}
}
