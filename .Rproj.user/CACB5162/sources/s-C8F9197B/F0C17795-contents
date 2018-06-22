
# Function to summarize imbalance across factors (possibly with weights)
#' @param summary_fn Either "sum" or "avg", both function the same way
#' @param d_vec Imbalances across all factors
#' @param weights Vector to weight the factors differently
#'
#' @return Numeric summary of imbalance across all factors
#' @export
#' @examples
#' summarize_imbalance(summary_fn="sum", d_vec=c(3,5,1,2,0))
summarize_imbalance <- function(summary_fn, d_vec, weights=NULL) {
    # no weights given, just equal for all
    if (is.null(weights)) {weights <- rep(1, length(d_vec))}

    # functionally the same
    if (summary_fn == 'sum') {
        temp_G <- sum(d_vec * weights)
    } else if (summary_fn == 'avg') {
        temp_G <- mean(d_vec * weights)
    }

    return(temp_G)
}
