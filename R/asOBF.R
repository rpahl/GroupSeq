"asOBF" <-
function( alpha, tk, OneOrTwoSidedBounds )
{
  # additonalParameters are not used here
  return( 2 * ( 1-pnorm ( ( qnorm(1-(alpha/OneOrTwoSidedBounds)/2 ) ) / sqrt(tk) ) ) )
}
