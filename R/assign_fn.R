#' Function to do the assignment, includes calc_imbalance and summarize_inbalance
#' @param assigned_dat All subjects that have been assigned so far
#' @param indiv_dat One row of data for current patient
#' @param trt_vec Vector of treatment names
#' @param factor_vec Vector of factor names
#' @param imb_fn Either "range" or "diff" or "var"
#' @param summary_fn Either "sum" or "avg"
#' @param flip A 2*1 vector to match the client, first one used if no ties and second
#' used if ties for best trt
#'
#' @return list with lower=lower bounds of intervals and upper=upper bounds and trt=
#' assigned treatment
#' @export
assign_fn <- function(assigned_dat, indiv_dat, trt_vec, factor_vec, imb_fn,
                      summary_fn, flip) {

    # for each treatment, get the imbalance vector and then summary of
    # imbalance if assigned to that treatment
    imb_vec <- rep(NA, length(trt_vec))
    factor_dvec <- rep(NA, length(factor_vec))
    for (trt_it in 1:length(trt_vec)) {         # loop through treatments
        temp_trt <- trt_vec[trt_it]
        for (factor_it in 1:length(factor_vec)) {   # loop through factors
            temp_factor <- factor_vec[factor_it]
            temp_level <- as.numeric(indiv_dat[[temp_factor]])
            temp_dat <- filter(assigned_dat, (!!as.name(temp_factor))==temp_level)
            factor_dvec[factor_it] <- calc_imbalance(strata_trt_vec=temp_dat$trt,
                                                     num_trts=length(trt_vec), imb_fn=imb_fn,
                                                     pot_trt=trt_vec[trt_it])
        }
        imb_vec[trt_it] <- summarize_imbalance(summary_fn=summary_fn, d_vec=factor_dvec)
    }

    # get the assigned intervals
    int_list <- make_ints(prob_fn="default", p1=0.8, imb_vec=imb_vec)

    # assign
    if (sd(imb_vec) == 0) {
        assignment <- which(int_list$lower <= flip[2] & int_list$upper > flip[2])
    } else {
        assignment <- which(int_list$lower <= flip[1] & int_list$upper > flip[1])
    }

    return(list(trt=trt_vec[assignment], int_list=int_list, imb_vec=imb_vec))
}
