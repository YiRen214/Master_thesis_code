###################################################
###################################################
####class RegDesc (originally from: irdpackage)####
###################################################

RegDesc = R6::R6Class("RegDesc",
                      public = list(
                        #' Creates a new Regional Descriptor (`RegDesc`) object.
                        #' This method should only be called by the `$find_box` methods of \link{RevPrim}.
                        #' @param box (`ParamSet`)
                        #' @param x_interest (`data.table(1)` | `data.frame(1)`) \cr
                        #'   A single row with the observation of interest.
                        #' @param predictor (\link[iml]{Predictor})\cr
                        #'   The object (created with `iml::Predictor$new()`) holding the machine learning model and the data.
                        #' @param desired_range (NULL | `numeric(2)`) \cr
                        #' The desired predicted outcome. If NULL (default), the current predicted value of `predictor` for `x_interest` is used as desired prediction.
                        #' Alternatively, a vector with two numeric values that specify an outcome interval. This outcome interval needs to include the predicted value of `x_interest`.
                        initialize = function(box, predictor, predictor_2,x_interest, desired_range, fixed_features = NULL,
                                              desired_class = NULL, method = NULL) {
                          # input checks
                          assert_class(box, "ParamSet")
                          assert_class(predictor, "Predictor")
                          assert_class(predictor_2,"Predictor")
                          assert_data_frame(x_interest, nrows = 1L)
                          setDT(x_interest)
                          assert_numeric(desired_range, len = 2L)
                          assert_character(desired_class, null.ok = TRUE)
                          if (!is.null(fixed_features)) {
                            assert_names(fixed_features, subset.of = predictor$data$features.names)
                          }
                          assert_character(method)
                          assert_names(names(box$params), subset.of = predictor$data$feature.names)
                          
                          # assign
                          private$.box = box
                          private$predictor = predictor
                          private$predictor_2 = predictor_2
                          private$.x_interest = x_interest
                          private$.desired_range = desired_range
                          private$.desired_class = desired_class
                          private$.method = method
                          if (predictor$task == "unknown") {
                            if (is.numeric(predictor$data$y[[1]])) {
                              predictor$task = "regression"
                            } else {
                              predictor$task = "classification"
                            }
                          }
                          private$param_set = make_param_set(predictor$data$X)
                          
                          private$.box_single = get_max_box(x_interest = private$.x_interest,
                                                            fixed_features = private$.fixed_features, predictor = private$predictor,
                                                            predictor_2=private$predictor_2,desired_range = private$.desired_range, param_set = private$param_set,
                                                            resolution = 50L)
                        },
                        
                        #' @description Prints the `RegDesc` object.
                        print = function(digits = 2L) {
                          desired = private$.desired_range
                          cat(nrow(private$.data), "Regional Descriptors \n \n")
                          if (private$predictor$task == "classification") {
                            cat("Desired class:", private$predictor$class, "\n")
                          }
                          if (desired[1L] != desired[2L]) {
                            cat(
                              "Desired range: [", desired[1L], ", ",
                              desired[2L],  "] \n \n", sep = ""
                            )
                          } else {
                            cat("Desired range:", desired[1L], "\n \n")
                          }
                          cat("Descriptor: \n")
                          box = private$.box
                          results = describe_box(box, digits = digits)
                          full = describe_box(private$param_set, digits = digits)
                          results_single = describe_box(private$.box_single, digits = digits)
                          x_interest_print = copy(private$.x_interest)
                          x_interest_print = as.character((x_interest_print[, (1:ncol(x_interest_print)) := lapply(.SD, as.character), .SDcols= 1:ncol(x_interest_print)]))
                          results = data.frame(cbind(feature = results[["id"]], x_interest = x_interest_print,
                                                     box = results[["range"]], box_single = results_single[["range"]], range = full[["range"]]))
                          names(results) = c("feature", "x_interest", "regional descriptor", "1-dim descriptor", "range")
                          # res = xtable::xtable(results)
                          # print(res, include.rownames = FALSE)
                          print(results)
                        },
                        
                        plot_surface = function(feature_names, grid_size = 250L, surface = "prediction") {
                          assert_names(surface, subset.of = c("prediction", "range"))
                          assert_names(feature_names, subset.of = names(private$.box$params))
                          if (!requireNamespace("ggplot2", quietly = TRUE)) {
                            stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
                          }
                          make_surface_plot(box = private$.box, param_set = private$param_set, grid_size = grid_size, predictor = private$predictor,predictor_2=private$predictor_2,
                                            x_interest = private$.x_interest, feature_names = feature_names, surface = surface, desired_range = private$.desired_range)
                        },
                        evaluate = function(n_samples = 100) {
                          evaluate_box(box = private$.box, x_interest = private$.x_interest,
                                       predictor = private$predictor, predictor_2=private$predictor_2,n_samples = n_samples,
                                       desired_range = private$.desired_range)
                        },
                        evaluate_train = function() {
                          evaldt = rbind(private$.x_interest, private$predictor$data$X)
                          inbox = identify_in_box(private$.box, evaldt)
                          evaldt = evaldt[inbox,]
                          precision = sum(predict_range(private$predictor, private$predictor_2,newdata = evaldt, range = private$.desired_range))/sum(inbox)
                          coverage = sum(inbox)/nrow(private$predictor$data$X)
                          return(c(precision = precision, coverage = coverage))
                        }),
                      active = list(
                        desired_range = function(value) {
                          if (missing(value)) {
                            private$.desired_range
                          } else {
                            stop("`$desired_range` is read only", call. = FALSE)
                          }
                        },
                        #' @field box (`data.table`)\cr
                        #'  The regional descriptor for `x_interest`.
                        box = function(value) {
                          if (missing(value)) {
                            private$.box
                          } else {
                            stop("`$box` is read only", call. = FALSE)
                          }
                        },
                        #' @field box_single (`data.table`)\cr
                        #'  The regional descriptors only mutating a single feature for `x_interest`.
                        box_single = function(value) {
                          if (missing(value)) {
                            private$.box_single
                          } else {
                            stop("`$box` is read only", call. = FALSE)
                          }
                        },
                        #' @field x_interest (`data.table(1)`) \cr
                        #'   A single row with the observation of interest.
                        x_interest = function(value) {
                          if (missing(value)) {
                            private$.x_interest
                          } else {
                            stop("`$x_interest` is read only", call. = FALSE)
                          }
                        },
                        #' @field method (`character(1)`) \cr
                        #'   Method with which regional descriptors were generated.
                        method = function(value) {
                          if (missing(value)) {
                            private$.method
                          } else {
                            stop("`$method` is read only", call. = FALSE)
                          }
                        },
                        #' @field fixed_features (`character(1)`) \cr
                        #'   Method with which regional descriptors were generated.
                        fixed_features = function(value) {
                          if (missing(value)) {
                            private$.fixed_features
                          } else {
                            stop("`$fixed_features` is read only", call. = FALSE)
                          }
                        },
                        #' @field desired_class (`character(1)`) \cr
                        #'   Desired class. NULL for regression models
                        desired_class = function(value) {
                          if (missing(value)) {
                            private$.desired_class
                          } else {
                            stop("`$desired_class` is read only", call. = FALSE)
                          }
                        },
                        ##############add one more method for ratio calculation later
                        full_range=function(value){
                          if(missing(value)){
                            as.data.table(private$param_set)
                            
                          } else{
                            stop("'full_range' is read only",call.=FALSE)
                          }
                        }
                      ),
                      
                      private = list(
                        .box = NULL,
                        .box_single = NULL,
                        predictor = NULL,
                        predictor_2 = NULL,
                        .x_interest = NULL,
                        .desired_range = NULL,
                        .fixed_features = NULL,
                        .desired_class = NULL,
                        .method = NULL,
                        param_set = NULL
                      )
)



###########################################################
###########################################################
#####class RegDescMethod (originally from :irdpackages)####
###########################################################

RegDescMethod = R6::R6Class("RegDescMethod",
                            
                            public = list(
                              
                              #' @description Creates a new `RegDescMethod` object.
                              #' @param predictor (\link[iml]{Predictor})\cr
                              #'   The object (created with `iml::Predictor$new()`) holding the machine
                              #'   learning model and the data.
                              #' @param quiet (`logical(1)`)\cr Should information about the optimization status be hidden? Default is FALSE.
                              initialize = function(predictor,predictor_2, quiet = FALSE) {
                                checkmate::assert_class(predictor, "Predictor")
                                checkmate::assert_class(predictor_2,"Predictor")
                                if (predictor$task == "unknown") {
                                  predictor$task = NULL
                                  predictor$predict(predictor$data$X[1:2, ])
                                }
                                private$predictor_2 = predictor_2$clone()
                                private$predictor = predictor$clone()
                                private$quiet = quiet
                                # maximum box from available data
                                private$param_set = make_param_set(predictor$data$X)
                              },
                              
                              #' @description
                              #' Prints a `RegDescMethod` object.
                              #' The method calls a (private) `$print_parameters()` method which should be implemented by the leaf classes.
                              print = function() {
                                cat("Regional descriptor method: ", class(self)[1], "\n")
                                cat("Parameters:\n")
                                private$print_parameters()
                              },
                              #'
                              #' Runs the hyperrectangle searching algorithm and returns the hyperrectangles interval ranges.
                              #' All observations in the hyperrectangle should have a predicted probability in the interval `desired_prob`
                              #' (for classification for a `desired_class`).
                              #'
                              #' @param x_interest (`data.table(1) | data.frame(1)`) \cr A single row with the observation of interest.
                              #' @param desired_range (`numeric(2)`) \cr
                              #'   The desired predicted outcome range - a vector with two numeric values
                              #'   that specify an outcome interval.
                              #'   For regression the interval operates on the numeric outcome,
                              #'   while for classification it reflects either a hard label or
                              #'   the probability for the class with the largest probability among all
                              #'   possible outcome classes.
                              #' @param desired_class (`character(1)` | `NULL`) \cr The desired class. Ignored if predictor$task = "regression".
                              #' If NULL (default) for a classification task then predictor$class is taken.
                              #' @param obsdata (`data.table` | `data.frame`) Data set used to find the box. If NULL (default) either
                              #' predictor$data or newly sampled data according to the specified `strategy` is used.
                              #' @param fixed_features (`character()` | `NULL`) \cr
                              #' Names of features that are not allowed to be changed. NULL (default) allows all features to be changed.
                              #' @param box_largest (`ParamSet`  | `NULL`) \cr
                              #' Largest initial box. If NULL, largest box is generated.
                              #' @param box_init (`ParamSet` | `NULL`) \cr Initial box to process. Ignored if method is not `PostProcessing`.
                              #
                              find_box = function(x_interest, desired_range = NULL, obsdata = NULL, fixed_features = NULL, desired_class = NULL, box_largest = NULL, box_init = NULL) {
                                if (!is.null(fixed_features)) {
                                  assert_names(fixed_features, subset.of = private$predictor$data$feature.names)
                                }
                                
                                # Checks x_interest
                                assert_data_frame(x_interest, nrows = 1L)
                                assert_names(names(x_interest), must.include = names(private$predictor$data$X))
                                x_interest = setDT(x_interest)[, names(private$predictor$data$X), with = FALSE]
                                # x_interest = data.table::data.table(x_interest)[, names(private$predictor$data$X), with = FALSE]
                                if (any(sapply(x_interest, typeof) != sapply(private$predictor$data$X, typeof))) {
                                  stop("Columns that appear in `x_interest` and `predictor$data$X` must have the same types.")
                                }
                                
                                f_hat_interest = abs(private$predictor$predict(x_interest)[[1]]-private$predictor_2$predict(x_interest)[[1]])
                                
                                # Check desired_class
                                if (private$predictor$task == "classification") {
                                  if (is.null(desired_class)) {
                                    if (is.null(private$predictor$class)) {
                                      stop("For classification models `desired_class` must be specified when calling $find_box().")
                                    } else {
                                      desired_class = private$predictor$class
                                      message(sprintf("The `desired_class` was set to `predictor$class` which is %s.", desired_class))
                                    }
                                  }
                                  assert_character(desired_class, len = 1L, any.missing = FALSE)
                                  
                                  private$predictor$class = desired_class
                                  f_hat_interest = abs(private$predictor$predict(x_interest)[[1]]-private$predictor_2$predict(x_interest)[[1]])
                                } else {
                                  if (!is.null(desired_class)) {
                                    message(sprintf("For regression models `desired_class = %s` is ignored.", desired_class))
                                  }
                                }
                                
                                # Check desired_range
                                
                                checkmate::assert_numeric(desired_range, min.len = 1L,  max.len = 2L,
                                                          null.ok = TRUE)
                                if (is.null(desired_range)) {
                                  f_hat_data = abs(private$predictor$predict(private$predictor$data$get.x())[[1]]-private$predictor_2$predict(private$predictor_2$data$get.x())[[1]])
                                  sdf_hat = 1/4*sd(f_hat_data)
                                  desired_range = c(f_hat_interest - sdf_hat, f_hat_interest + sdf_hat)
                                  if (private$predictor$task == "classification") {
                                    # cap between 0 and 1
                                    desired_range = c(max(min(desired_range), 0), min(max(desired_range), 1))
                                  }
                                  message(sprintf("'desired_range' is NULL. Using 1/2 standard deviation of predictions of observed data (predictor$data), it was set to 'c(%f, %f)'.",
                                                  desired_range[1], desired_range[2]))
                                } else {
                                  if (private$predictor$task == "classification") {
                                    checkmate::assert_numeric(desired_range, min.len = 1L,  max.len = 2L,
                                                              lower = 0, upper = 1)
                                  }
                                  if (length(desired_range) == 1L) {
                                    desired_range = c(desired_range, desired_range)
                                  }
                                  if (desired_range[2L] < desired_range[1L]) {
                                    stop("The lower bound of `desired_range` cannot be greater than the upper bound.")
                                  }
                                  if (f_hat_interest < desired_range[1] | f_hat_interest > desired_range[2]) {
                                    stop(sprintf("`desired_range` must cover the prediction of `x_interest` of %s", round(f_hat_interest, 3)))
                                  }
                                }
                                
                                
                                # Check fixed_features
                                if (!is.null(fixed_features)) {
                                  assert_names(fixed_features, subset.of = private$predictor$data$feature.names)
                                }
                                
                                # Check box_init
                                assert_class(box_init, "ParamSet", null.ok = TRUE)
                                if (!is.null(box_init)) {
                                  
                                  assert_names(names(box_init$params), subset.of = private$predictor$data$feature.names)
                                  # <FIXME:> Take update fixed_features into account!
                                }
                                
                                assert_class(box_largest, "ParamSet", null.ok = TRUE)
                                if (!is.null(box_largest)) {
                                  assert_names(names(box_largest$params), subset.of = private$predictor$data$feature.names)
                                }
                                
                                # Update private$param_set
                                private$param_set = make_param_set(rbind(x_interest, private$predictor$data$X))
                                
                                # Check obsdata
                                if (!is.null(obsdata)) {
                                  obsdata = as.data.table(obsdata)[, names(private$predictor$data$X), with = FALSE]
                                  assert_true(all(identify_in_box(box = private$param_set, data = obsdata)))
                                }
                                
                                # Set number of calls to fhat to 0
                                private$.calls_fhat = 0
                                
                                private$x_interest = x_interest
                                private$f_hat_interest = f_hat_interest
                                private$desired_range = desired_range
                                private$fixed_features = fixed_features
                                private$desired_class = desired_class
                                private$obsdata = obsdata
                                private$box_largest = box_largest
                                private$box_init = box_init
                                box = private$run()
                                box = private$sanitize_box(box)
                                
                                RegDesc$new(box = box, predictor = private$predictor,predictor_2 =private$predictor_2,
                                            x_interest = x_interest, desired_range = desired_range,
                                            fixed_features = fixed_features, desired_class = desired_class,
                                            method = class(self)[1])
                                
                              }),
                            active = list(
                              #' @field history (`data.table`) \cr
                              #'  stores for each iteration of the method the chosen variable, boundary
                              #'  value and evaluation measures.
                              history = function(value) {
                                if (missing(value)) {
                                  private$.history
                                } else {
                                  stop("`$history` is read only", call. = FALSE)
                                }
                              },
                              calls_fhat = function(value) {
                                if (missing(value)) {
                                  private$.calls_fhat
                                } else {
                                  stop("`$calls_fhat` is read only", call. = FALSE)
                                }
                              }
                            ),
                            
                            private = list(
                              predictor = NULL,
                              predictor_2=NULL,
                              quiet = NULL,
                              param_set = NULL,
                              x_interest = NULL,
                              f_hat_interest = NULL,
                              desired_range = NULL,
                              obsdata = NULL,
                              fixed_features = NULL,
                              desired_class = NULL,
                              box_largest = NULL,
                              box_init = NULL,
                              .history = NULL,
                              .calls_fhat = NULL,
                              sanitize_box = function(box) {
                                for (j in names(box$params)) {
                                  if (class(box$params[[j]])[1] == "ParamInt") {
                                    b = box$params[[j]]
                                    box$params[[j]]$lower = ceiling(b$lower)
                                    box$params[[j]]$upper = floor(b$upper)
                                  }
                                }
                                return(box)
                              },
                              run = function() {
                                stop("Abstract base class")
                              },
                              print_parameters = function() {}
                            )
)

