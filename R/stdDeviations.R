"stdDeviations" <-
function(n, t2)
{
    stdDev.inc <- c(sqrt(t2[1]), sqrt(diff(t2)))
    stdDev.proc <- sqrt(t2)
    list(standardDeviation=stdDev.inc, stdvOfTheProcess=stdDev.proc)
}
