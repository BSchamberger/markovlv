#' Discrete time Markov model class
#'
#' Class for the discrete time Markov model, calculating reserves
#' (Deckungskapital (DK) in German \code{reserve}) and cashflows (CF,
#' \code{cashflow})given a transition matrix (P, \code{transition_matrix}),
#' prenumerando (pre, \code{prenumarando}) and postnumerando (post,
#' \code{postnumerando}) benefits for each time point. You can also specify a
#' discount rate (v, \code{discount_rate})
#' @param n_states [integer](1) The number of states in the Markov model
#' @param rate [numeric](time) Set discount rates to use for calculating
#'   discount rates under discrete annual compounding convention. Scalar values
#'   will be recycled for all times \code{t}
#' @param verbose [logical](1) If \code{TRUE} print the resulting discount
#'   factor, if \code{FALSE} set discount factor silently
#' @param p [array](n_states, n_states, max_time) Transition matrix of the
#'   Markov model for each time point \code{t} in the first dimension and states
#'   \code{i} and \code{j} in the second and third dimensions. If
#'   [matrix](n_states, n_states) it will be recycled for all time steps
#' @param pre [matrix](max_time, n_states) Prenumerando benefits for each time
#'   \code{t} and state \code{i}. If [numeric](n_states) it will be recycled for
#'   all time steps
#' @param post [array](n_states, n_states, max_time) Postnumerando benefits for
#'   each time \code{t} and states \code{i}, \code{j}. If [matrix](n_states,
#'   n_states) it will be recycled for all time steps
#' @param start [integer](1) Start time for calculation
#' @param stop [integer](1) Stop time for calculation
#' @param verbose [logical](1) If \code{TRUE} show verbose output for each
#'   iteration in the calculation. If \code{FALSE} do the calculation silently
#' @param values_to [character](1) Name of the value column, passed to
#'   \code{\link[tidyr]{tidyr}}
#' @param names_to [character](1) Name of the state column, passed to
#'   \code{\link[tidyr]{tidyr}}
#' @param x [data.frame] Data set to transform
#' @param output [matrix](max_time, n_states) Output to be formatted
#' @importFrom R6 R6Class
#' @importFrom ggplot2 ggplot geom_line geom_col aes
#' @importFrom tidyr pivot_longer
#' @export
Markov1 <- R6Class(
  "Markov1",

  public = list(
    #' @field n_states [integer](1) Number of distinct states in the Markov
    #'   model
    n_states = NULL,

    #' @field transition_matrix [array](n_states, n_states, max_time) Transition
    #'   matrix of the Markov model for each time point \code{t} in the first
    #'   dimension and states \code{i} and \code{j} in the second and third
    #'   dimensions
    transition_matrix = NULL,

    #' @field prenumerando [matrix](max_time, n_states) Prenumerando benefits
    #'   for each time \code{t} and state \code{i}
    prenumerando = NULL,

    #' @field postnumerando [array](n_states, n_states, max_time) Postnumerando
    #'   benefits for each time \code{t} and states \code{i} and \code{j}
    postnumerando = NULL,

    #' @field discount_factor [numeric](time) Discount factors to apply to
    #'   cashflows to calculate present values
    discount_factor = NULL,

    #' @field reserve [matrix](max_time, n_states) Mathematical reserve (or
    #'   Deckungskapital (DK) in German) for each time \code{t} and state
    #' \code{i}
    reserve = NULL,

    #' @field cashflow [matrix](max_time, n_states) Cashflows for each time
    #'   \code{t} and state \code{i}
    cashflow = NULL,

    #' @description Create a new object of the \code{Markov} class
    #' @return [Markov1] An object of the \code{Markov} class
    initialize = function(n_states) {
      self$n_states <- n_states
      self$state_names <- paste("state", seq_len(n_states), sep = "_")
    },

    #' @description Set the discount rate \code{discount_rate} for the model
    #' @return Sets the \code{discount_factor} field and returns the model
    #'   invisibly
    set_discount_factor = function(rate, verbose = TRUE) {
      discount_factor <- 1 / (1 +  rate)
      print(
        paste(
          "Discount factor:", format(discount_factor, digits = 4, nsmall = 4)
        )
      )
      if (length(rate) == 1) {
        self$discount_factor <- rep(discount_factor, times = self$max_time)
      } else {
        self$discount_factor <- discount_factor
      }
      self$reset_calculation()
    },

    #' @description Set the transition matrix \code{transition_matrix} for the
    #' model
    #' @return Sets the \code{transition_matrix} field and returns the model
    #'   invisibly
    set_transition_matrix = function(p) {
      if (length(dim(p)) == 3) {
        self$transition_matrix <- p
      } else {
        self$transition_matrix <- array(
          rep(p, times = self$max_time),
          dim = c(dim(p), self$max_time)
        )
      }
      self$reset_calculation()
    },

    #' @description Set the prenumerando benefits \code{prenumerando} for the
    #' model
    #' @return Sets the \code{prenumerando} field and returns the model
    #'   invisibly
    set_prenumerando = function(pre) {
      if (length(dim(pre)) == 2) {
        self$prenumerando <- pre
      } else {
        self$prenumerando <- matrix(
          rep(pre, times = self$max_time),
          ncol  = length(pre),
          byrow = TRUE
        )
      }
      self$reset_calculation()
    },

    #' @description Set the postnumerando benefits \code{postnumerando} for the
    #' model
    #' @return Sets the \code{postnumerando} field and returns the model
    #'   invisibly
    set_postnumerando = function(post) {
      if (length(dim(post)) == 3) {
        self$postnumerando <- post
      } else {
        self$postnumerando <- array(
          rep(post, times = self$max_time),
          dim = c(dim(post), self$max_time)
        )
      }
      self$reset_calculation()
    },

    #' @description Calculate the mathematical reserve (Deckungskapital (DK) in
    #'   German)
    #' @return Calculates the mathematical reserves, stores them in the
    #'   \code{reserve} field and returns the model invisibly
    calculate_reserve = function(start, stop) {
      self$start <- start
      self$stop  <- stop
      self$reserve  <- matrix(0, nrow = self$max_time, ncol = self$n_states)
      for (i in (start - 1L):stop) {
        for (j in seq_len(self$n_states)) {
          self$reserve[i, j] <- self$prenumerando[i, j]
          for (k in seq_len(self$n_states)) {
            self$reserve[i, j] <- self$reserve[i, j] +
              self$discount_factor[i] * self$transition_matrix[j, k, i] *
              (self$postnumerando[j, k, i] + self$reserve[i + 1L, k])
          }
        }
      }
      self$is_calculated_reserve <- TRUE
      invisible(self)
    },

    #' @description Calculate the model cashflows
    #' @return Calculates the model cashflows, stores them in the
    #'   \code{cashflow} field and returns the model invisibly
    calculate_cashflow = function(start, stop, verbose = FALSE) {
      self$start <- start
      self$stop  <- stop
      self$cashflow <- matrix(0, nrow = self$max_time, ncol = self$n_states)
      p_current <- diag(1, nrow = self$n_states)
      for (i in stop:(start - 1L)) {
        for (k in seq_len(self$n_states)) {
          for (l in seq_len(self$n_states)) {
            self$cashflow[i, k] <- self$cashflow[i, k] +
              p_current[k, l] * self$prenumerando[i, l]
          }
        }

        p_next <- self$transition_matrix[, , i]
        for (k in seq_len(self$n_states)) {
          for (l in seq_len(self$n_states)) {
            for (m in seq_len(self$n_states)) {
              self$cashflow[i + 1L, k] <- self$cashflow[i + 1L, k] +
                p_current[k, l] * p_next[l, m] * self$postnumerando[l, m, i]
            }
          }
        }

        p_current <- p_current %*% p_next
      }
      self$is_calculated_cashflow <- TRUE
      invisible(self)
    },

    #' @description Get mathematical reserves between two times
    #' @return [matrix](start - stop, n_states) Mathematical reserve from
    #'   times \code{start} to \code{stop}
    get_reserve = function(start, stop) {
      if (self$start != start || self$stop != stop ||
          !self$is_calculated_reserve) {
        self$calculate_reserve(start, stop)
      }
      self$print_output(self$reserve, start = start, stop = stop)
    },

    #' @description Get model cashflows between two times
    #' @return [matrix](start - stop, n_states) Model cashflows from times
    #'   \code{start} to \code{stop}
    get_cashflow = function(start, stop) {
      if (self$start != start || self$stop != stop ||
          !self$is_calculated_cashflow) {
        self$calculate_cashflow(start, stop)
      }
      self$print_output(self$cashflow, start = start, stop = stop)
    },

    #' @description Plot a line graph of reserves
    #' @return [ggplot2::ggplot] [ggplot2] plot of reserves
    plot_reserve = function(start, stop) {
      plot_data <- self$transform_for_plot(
        self$get_reserve(start, stop = stop),
        values_to = "reserve"
      )
      plot_data |>
        ggplot() +
        geom_line(aes(x = time, y = reserve, colour = state))
    },

    #' @description Plot a column graph of cashflows
    #' @return [ggplot2::ggplot] [ggplot2] plot of cashflows
    plot_cashflow = function(start, stop) {
      plot_data <- self$transform_for_plot(
        self$get_cashflow(start, stop = stop),
        values_to = "cashflow"
      )
      plot_data |>
        ggplot() +
        geom_col(aes(x = time, y = cashflow, fill = state), position = "dodge")
    },

    #' @field start [integer](1) Start time \code{t} for calculations
    start = NULL,

    #' @field stop [integer](1) End time \code{t} for calculations
    stop = NULL,

    #' @field max_time [integer](1) Maximum amount of time steps \code{t} in the
    #'   model
    max_time = 120,

    #' @field state_names [character](n_states) Names for the states in the
    #'   model
    state_names = NULL,

    #' @field is_calculated_reserve [logical](1) Indicator whether model
    #'   technical results have been calculated with most recent configuration
    is_calculated_reserve = FALSE,

    #' @field is_calculated_cashflow [logical](1) Indicator whether model
    #'   cashflows have been calculated with the most recent configuration
    is_calculated_cashflow = FALSE,

    #' @description Reset the state of the model as non-calculated
    #' @return Sets \code{is_calculated_reserve} and
    #'   \code{is_calculated_cashflow} fields to \code{FALSE} and returns the
    #'   model invisibly
    reset_calculation = function() {
      self$is_calculated_reserve <- FALSE
      self$is_calculated_cashflow <- FALSE
      invisible(self)
    },

    #' @description Transform data for plots
    #' @return [\link[tibble]{tibble}] Data set in appropriate format for
    #'   [ggplot2] plots
    transform_for_plot = function(x, values_to, names_to = "state") {
      x |>
        pivot_longer(-time, names_to = names_to, values_to = values_to)
    },

    #' @description Format output in a user-friendly [data.frame]
    #' @return [data.frame] Model output with \code{time} and values for each
    #'   state
    print_output = function(output, start = self$max_time, stop = 1L) {
      out <- as.data.frame(output[stop:start, ])
      names(out) <- self$state_names
      cbind(time = stop:start, out)
    }
  )
)

