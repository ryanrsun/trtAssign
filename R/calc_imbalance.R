#' Function to calculate the measure of (im)balance for one factor
#' AFTER adding a potential treatment (not current imbalance).
#'
#' @param strata_trt_vec Vector containing the treatment assignment of each patient
#' in this particular strata
#' @param num_trts Total number of treatments
#' @param imb_fn Imbalance function, either "range" or "diff" or "var"
#' @param pot_trt The potential treatment assigned to this patient
#'
#' @return Numeric measure of imbalance
#' @export
#'
#' @examples
#' calc_imbalance(strata_trt_vec=c(1,2,1), num_trts=2, imb_fn="range", pot_trt=1)
calc_imbalance <- function(strata_trt_vec, num_trts, imb_fn, pot_trt) {
    # if nothing yet in this strata
    if (length(strata_trt_vec) == 0) {
        if (imb_fn == 'range' | imb_fn == "diff") {return(1)}
        if (imb_fn == "var") {return(0)}
    }

    # append the potential treatment value
    strata_trt_vec <- c(strata_trt_vec, pot_trt)

    # range
    if (imb_fn == 'range') {
        # sometimes haven't seen all the treatments yet
        if (length(unique(strata_trt_vec)) != num_trts) {
            # range = max - 0
            temp_d <- max(table(strata_trt_vec))
        } else {
            # range = max - min
            temp_d <- max(table(strata_trt_vec)) - min(table(strata_trt_vec))
        }
    } else if (imb_fn == 'diff') {      # diff only for use with two groups
        if (length(unique(strata_trt_vec) > 2)) {stop('Error you specified diff with >2 levels')}
        if (length(unique(strata_trt_vec) == 1)) {return(length(strata_trt_vec))}
        if (length(unique(strata_trt_vec)) == 2) {
            temp_d <- abs(table(strata_trt_vec)[1] - table(strata_trt_vec[2]))
        }
    } else if (imb_fn == 'var') {       # variance of the counts in each trt
        if (length(unique(strata_trt_vec)) == num_trts) {
            temp_d <- var(table(strata_trt_vec))
        } else if (length(unique(strata_trt_vec)) < num_trts) {
            # need to append 0s if not all treatments seen
            zero_trts <- num_trts - length(unique(strata_trt_vec))
            temp_d <- var(c(table(strata_trt_vec), rep(0, zero_trts)))
        }
    } else {
        stop("Bad imb_fn!")
    }

    return(temp_d)
}
