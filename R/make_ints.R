#' Function to assign intervals on [0,1] to trts
#' Always assign the biggest interval to start from 0
#' unless all imbalances equal across treatments, then the intervals
#' are just all equal-sized in the order of the treat_vec.
#'
#' @param prob_fn Only "default" for now, only up to two possible different
#' interval lengths (one possibly big one and then the rest equal)
#' @param p1 Probability for "best" treatment
#' @param imb_vec Vector of imbalances across all treatments
#'
#' @return list with lower=lower bounds of intervals and upper=upper bounds
#' @export
#' @examples
#' make_ints(imb_vec=c(4,9))
make_ints <- function(prob_fn="default", p1=0.8, imb_vec) {
    # construct intervals
    num_trts <- length(imb_vec)
    lower_ints <- rep(NA, num_trts)
    upper_ints <- rep(NA, num_trts)

    # if all imbalances the same, make all intervals the same size and return
    if (sd(imb_vec) == 0) {
        lower_ints <- seq(from=0, to=1, length.out=num_trts+1)[1:num_trts]
        upper_ints <- seq(from=0, to=1, length.out=num_trts+1)[2:(num_trts+1)]
        return(list(lower=lower_ints, upper=upper_ints))
    }

    # p1 to the best, other intervals have equal size (1-p1)/(num_trts-1)
    if (prob_fn == "default") {
        # find the worst one, give it p1
        best_idx <- which(imb_vec == min(imb_vec))
        #best_idx <- best_idx[ceiling(runif(n=1, min=0, max=length(best_idx)))]
        best_idx <- best_idx[1]         # if tie for best, best is the first listed, dangerous?
        lower_ints[best_idx] <- 0
        upper_ints[best_idx] <- p1

        # other indices are assumed to be equal size
        other_idx <- (1:num_trts)[-best_idx]
        other_lengths <- (1-p1) / length(other_idx)
        for (trt_it in 1:length(other_idx)) {
            lower_ints[other_idx[trt_it]] <- p1 + (trt_it-1) * other_lengths
            upper_ints[other_idx[trt_it]] <- p1 + trt_it * other_lengths
        }
    }

    # return
    return(list(lower=lower_ints, upper=upper_ints))
}
