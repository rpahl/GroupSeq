"asPocock" <-
function( alpha, tk, OneOrTwoSidedBounds )
{
  # additonalParameters are not used here
  return( (alpha/OneOrTwoSidedBounds) * log( 1+ ( exp(1)-1 ) * tk ) )
}
