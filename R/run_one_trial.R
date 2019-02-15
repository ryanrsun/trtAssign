#' Function to perform one trial
#' @param temp_covar All data with no treatment column
#' @param trt_vec Vector of treatment names
#' @param factor_vec Vector of factor names
#' @param imb_fn Either "range" or "diff" or "var"
#' @param summary_fn Either "sum" or "avg"
#' @param flip_tab A num_subs*2 table of coin flip values, first one used if no ties and second
#' used if ties for best trt
#'
#' @return data frame with all covariates and assigned treatments
#' @export

run_one_trial <- function(temp_covar, trt_vec, factor_vec, imb_fn,
                          summary_fn, flip_tab) {
    # holds treatment
    dat_with_trt <- temp_covar %>% mutate(trt=NA)
    # empty to start
    assigned_dat <- dat_with_trt %>% filter(ID == -1)

    # make up flips?
    if (is.null(flip_tab)) {
        flip_tab <- matrix(data=runif(n=2*nrow(temp_covar)), ncol=2)
    }

    # recreate
    for (sub_it in 1:nrow(temp_covar)) {
        # next subject's data
        indiv_dat <- temp_covar[sub_it, ]
        # assign
        assign_output <- assign_fn(assigned_dat=assigned_dat, indiv_dat=indiv_dat,
                                   trt_vec=trt_vec, factor_vec=factor_vec, imb_fn=imb_fn,
                                   summary_fn=summary_fn, flip=flip_tab[sub_it, ])
        dat_with_trt$trt[sub_it] <- assign_output$trt
        # update all known assignments
        assigned_dat <- dat_with_trt[1:sub_it, ]
    }
    return(assigned_dat)
}
