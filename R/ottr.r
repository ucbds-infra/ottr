#===================================================================================================
# Otter-Grader Script for Grading R and Rmd files
#===================================================================================================

#---------------------------------------------------------------------------------------------------
# Helpful Global Variables
#---------------------------------------------------------------------------------------------------

#' A string containing characters that can be made into a valid variable name. Does not include any
#' digits because randomly sampling with them included could result in an invalid variable name.
VALID_EXPR_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJLKMNOPQRSTUVWXYZ._"


#---------------------------------------------------------------------------------------------------
# Helpful Classes for Storing Suite and Case Results
#---------------------------------------------------------------------------------------------------

#' A test case for Ottr. Contains configurations and code to be executed for the test.
#'
#' @param name The name of the test case
#' @param code The code to be executed as part of the test case
#' @param points The point value of the test case
#' @param hidden Whether the test case is hidden
#' @param success_message A message to show to students if the test passes
#' @param failure_message A message to show to students if the test fails
#' @export
#' @examples
#' tc = TestCase$new("q1", {
#'   testthat::assert_true(q1.ans)
#' })
#' env = new.env()
#' env$q1.ans = TRUE
#' tc$run(env)
TestCase = R6::R6Class(
  "TestCase",
  public = list(
    name = NA,
    code = NA,
    points = NA,
    hidden = NA,
    success_message = NA,
    failure_message = NA,
    initialize = function(name, code, points=1, hidden=FALSE, success_message=NA, failure_message=NA) {
      self$name = name
      self$code = substitute(code)
      self$points = points
      self$hidden = hidden
      self$success_message = success_message
      self$failure_message = failure_message
    },
    run = function(env) {
      error = NULL
      tryCatch(
        eval(self$code, envir=env, enclos=baseenv()),
        error = function(e) error <<- e
      )
      return(error)
    },
    to_list = function() {
      return(list(
        name = self$name,
        code = paste(deparse(self$code), collapse="\n"),
        points = self$points,
        hidden = self$hidden,
        success_message = self$success_message,
        failure_message = self$failure_message
      ))
    }
  )
)


#' A utility class for tracking the results of a `TestCase`
#'
#' @param passed Whether the test passed
#' @param error An error raised by executing the test, if any
#' @param test_case The `TestCase` that this result tracks
TestCaseResult = R6::R6Class(
  "TestCaseResult",
  public = list(
    passed = NA,
    error = NA,
    test_case = NA,
    initialize = function(passed, error, test_case) {
      self$passed = passed
      self$error = error
      self$test_case = test_case
    },
    get_score = function() {
      if (self$passed) {
        return(self$test_case$points)
      } else {
        return(0)
      }
    },
    repr = function() {
      message = self$get_message()
      if (is.na(message)) message = ""
      if (self$passed) return(paste0("Test ", self$test_case$name, " passed\n", message))
      indented_message = paste(strsplit(self$error$message, "\n")[[1]], collapse="\n  ")
      if (length(message) > 0) message = paste0(message, "\n")
      output = paste0("Test ", self$test_case$name, " failed:\n", message, indented_message)
      return(output)
    },
    to_list = function() {
      return(list(
        passed = self$passed,
        # don't put NULL because jsonlite turns it into {}
        error = ifelse(is.null(self$error$message), "", self$error$message),
        test_case = self$test_case$to_list()
      ))
    },
    get_message = function() {
      if (self$passed) return(self$test_case$success_message)
      return(self$test_case$failure_message)
    }
  )
)


#' A collection of test case results that correspond to a single test file
#'
#' @param test_case_results The `TestCaseResult` objects that make up this test file
#' @param filename The name of the test file
TestFileResult = R6::R6Class(
  "TestFileResult",
  public = list(
    test_case_results = NA,
    filename = NA,
    initialize = function(test_case_results, filename) {
      self$test_case_results = test_case_results
      self$filename = filename
    },
    get_basename = function() basename(self$filename),
    get_score = function() {
      earned = 0; possible = 0;
      for (tcr in self$test_case_results) {
        earned = earned + tcr$get_score()
        possible = possible + tcr$test_case$points
      }
      return(ifelse(possible == 0, 0, earned / possible))
    },
    repr = function() {
      # if all tests passed, just return that
      messages = c()
      for (tcr in self$test_case_results) {
        if (!is.na(tcr$get_message())) messages = c(messages, tcr$get_message())
      }
      messages = paste(messages, collapse="\n")
      if (length(messages) > 0) messages = paste0(messages, "\n")
      if (self$get_score() == 1) {
        return(paste0(messages, "All tests passed!"))
      }

      # otherwise, iterate through results and put hints together
      output = c()
      for (tcr in self$test_case_results) {
        output = c(output, tcr$repr())
      }
      return(paste0(output, collapse="\n\n"))
    },
    failed_hidden_cases = function() {
      tcrs = c()
      for (tcr in self$test_case_results) {
        if (tcr$test_case$hidden && !tcr$passed) {
          tcrs = c(tcrs, tcr)
        }
      }
      return(tcrs)
    },
    failed_public_cases = function() {
      tcrs = c()
      for (tcr in self$test_case_results) {
        if (!tcr$test_case$hidden && !tcr$passed) {
          tcrs = c(tcrs, tcr)
        }
      }
      return(tcrs)
    },
    get_points = function() {
      return(sum(sapply(sapply(self$test_case_results, getElement, "test_case"), getElement, "points")))
    },
    failed_any_public = function() {
      for (tcr in self$test_case_results) {
        if (!tcr$test_case$hidden && !tcr$passed) {
          return(TRUE)
        }
      }
      return(FALSE);
    },
    to_list = function() {
      tcr_lists = list()
      for (i in seq_along(self$test_case_results)) {
        tcr_lists[[i]] = self$test_case_results[[i]]$to_list()
      }
      return(list(
        filename = self$filename,
        test_case_results = tcr_lists
      ))
    }
  )
)


#---------------------------------------------------------------------------------------------------
# Test Metadata and Result Parsers and Getters
#---------------------------------------------------------------------------------------------------

#' Loads test case data from a test file. Executes the file and grabs the global `test` variable,
#' which should be a `list`.
#'
#' @param test_file The path to the test file
#' @return The test cases
load_test_cases = function(test_file) {
  env = new.env()

  exps = parse(file=test_file)

  for (i in seq_along(exps)) {
    exp = exps[i]
    eval(exp, envir=env)
  }

  if (!("test" %in% names(env))) {
    stop(paste0("Test file does not declare a global test variable: ", test_file))
  }

  # add names to any test cases missing them
  test_suite = env$test
  if (is.na(test_suite$name)) {
    test_suite$name = basename(test_file)
  }

  for (i in seq_along(test_suite$cases)) {
    tc = test_suite$cases[[i]]
    if (is.na(tc$name)) {
      tc$name = paste(test_suite$name, "-", i)
    }
  }

  return(test_suite)
}


#---------------------------------------------------------------------------------------------------
# Executors and Graders
#---------------------------------------------------------------------------------------------------

#' Execute checks in a test suite and return the `TestFileResult` object from executing the test.
#' Optionally prints results of the test to console.
#'
#' @param test_file Path to a test file
#' @param test_env An environment against which to run tests
#' @param show_results Whether to print the results to stdout
#' @return The parsed test results for the suite
#' @export
#' @examples
#' \dontrun{
#' check("tests/q1.R")
#' }
check = function(test_file, test_env, show_results) {

  # need to specify a test file
  if (missing(test_file)) {
    stop("must have a test file")
  }

  # if show_results is not passed, default to TRUE
  if (missing(show_results)) {
    show_results = TRUE
  }

  # grab the calling frame
  if (missing(test_env)) {
    test_env = parent.frame(1)
  }

  test_case_results = c()

  # redirect stdout so that testthat doesn't print
  testthat::capture_output({
    # read the test cases from the test file
    test_cases = load_test_cases(test_file)$cases

    # run the tests
    for (tc in test_cases) {
      err = tc$run(test_env)
      test_case_results = c(test_case_results, TestCaseResult$new(is.null(err), err, tc))
    }
  })

  file_result = TestFileResult$new(test_case_results, test_file)

  # print out suite_results if show_results is TRUE
  if (show_results) {
    cat(file_result$repr())
  }

  # return the test suite results
  return(file_result)
}


#' Execute a string as an R script and return the environment from that execution.
#'
#' Converts a string to an AST and executes that script in a dummy environment for running test cases
#' against. Transforms all expressions of the form `. = ottr::check(...)` by replacing the `.` with
#' an index into a list in the environment with name `check_results_{SECRET}` to collect the
#' `TestFileResult` objects generated from those checks. (This helps to handle variable name collisions
#' in tests when grading a script.)
#'
#' @param script The string to be executed
#' @param secret The string to be appended to the name `check_results_` as the list name to collect
#' results
#' @param ignore_errors Whether to ignore errors thrown while executing the script
#' @return The global environment after executing the script
execute_script = function(script, secret, ignore_errors) {

  if (missing(ignore_errors)) {
    ignore_errors = TRUE
  }

  # convert script to a list of expressions
  tree = as.list(parse(text=script))

  # create check result collection list name as expression
  list_name = parse(text=paste0("check_results_", secret))[[1]]

  # wrap calls of form `. = ottr::check(...)` to append to list and convert back to string
  tree = update_ast_check_calls(tree, list_name)

  # create dummy env for execution and add check_results_XX list
  test_env = new.env()
  test_env[[as.character(list_name)]] = list()

  # run the script, capturing stdout, and return the environment
  testthat::capture_output({
    for (expr in tree) {
      tryCatch(
        eval(expr, envir=test_env),
        error = function(e){
          if (!ignore_errors) {
            stop(e)
          }
        }
      )
    }
  })
  return(test_env)
}


#' Execute a script, parse check outputs, and run additional tests specified by the glob pattern
#' `tests_glob` on the test environment.
#'
#' @param script_path The path to the script
#' @param tests_glob The pattern to search for extra tests
#' @param secret The string to be appended to the name `check_results_` as the list name to collect
#' results (optional)
#' @param ignore_errors Whether to ignore errors thrown while executing the script
#' @return The list of `TestFileResult` objects after executing tests referenced in the script
#' and those specified by `tests_glob`
grade_script = function(script_path, tests_glob, secret, ignore_errors) {
  # convert script to a string
  script = paste(readLines(script_path), collapse="\n")

  # create a secret with make_secret if unspecified
  if (missing(secret)) {
    secret = make_secret()
  }

  if (missing(ignore_errors)) {
    ignore_errors = TRUE
  }

  # run the script and extract results from env, capturing stdout
  testthat::capture_output({
    test_env = execute_script(script, secret, ignore_errors)
    test_file_results = test_env[[paste0("check_results_", secret)]]
  })

  # run the tests in tests_glob on the env, collect in test_file_results
  num_embedded_tests = length(test_file_results)
  tests_glob = Sys.glob(tests_glob)
  i = 1
  for (test_file in tests_glob) {
    already_tested = sapply(test_file_results, function(tfr) tfr$get_basename())
    if (!(basename(test_file) %in% already_tested)) {
      test_file_results[[i + num_embedded_tests]] = check(test_file, test_env, FALSE)
      i = i + 1
    }
  }
  return(test_file_results)
}


#' Run autograder in a Gradescope container and return the results as a properly-formatted JSON
#' string
#'
#' @param script_path The path to the script
#' @param secret The string to be appended to the name `check_results_` as the list name to collect
#' results (optional)
#' @param ignore_errors Whether to ignore errors thrown while executing the script
#' @param test_dir A directory of tests to glob from
#' @return The JSON string
#' @export
#' \dontrun{
#' run_autograder("hw01.R", "ABC123", TRUE, "tests")
#' }
run_autograder = function(script_path, secret, ignore_errors, test_dir) {
  if (missing(secret)) {
    secret = make_secret()
  }

  if (missing(ignore_errors)) {
    ignore_errors = TRUE
  }

  if (missing(test_dir)) {
    test_dir = "/autograder/source/tests"
  }

  test_file_results = grade_script(script_path, paste0(test_dir, "/*.[Rr]"), secret, ignore_errors)
  test_file_results = results_to_json(test_file_results)
  return(test_file_results)
}


#---------------------------------------------------------------------------------------------------
# Utilities
#---------------------------------------------------------------------------------------------------

# TODO: convert update_ast_check_calls to also work for calls that aren't in assignment statements
# (i.e.. `ottr::check(...)`, not `. = ottr::check(...)`)

#' Traverse an AST (a list of expressions) and change calls of the form `. = ottr::check(...)` so
#' that they are appended to a list with name `list_name`.
#'
#' If `list_name` is `check_results_XX`, then `. = ottr::check(...)` becomes
#' `check_results_XX[[<int>]] = ottr::check(...)`, where `<int>` is an integer
#'
#' @param tree The tree to traverse
#' @param list_name The quoted name of the list
#' @return The tree with substitutions made
update_ast_check_calls = function(tree, list_name) {
  list_idx = 1
  for (i in seq_along(tree)) {
    expr = tree[[i]]
    if (class(expr) == "=") {
      right_expr = expr[[3]]
      call = right_expr[[1]]
      if (length(call) >= 3) {
        pkg = call[[2]]
        fn = call[[3]]
        if (pkg == "ottr" && fn == "check") {
          env = new.env()
          env$list_name = list_name
          env$list_idx = list_idx
          new_left_expr = substitute(list_name[[list_idx]], env)
          expr[[2]] = new_left_expr
          list_idx = list_idx + 1
        }
      }
    }
    tree[[i]] = expr
  }
  return(tree)
}


#' Randomly generate a string of `n_chars` sampled at random from `valid_chars`.
#'
#' @param n_chars The number of characters in the string; defaults to 6
#' @param valid_chars A string of characters to choose from; defaults to all alphanumerals, `.`, and
#' `_`
#' @return The generated string
make_secret = function(n_chars, valid_chars) {
  if (missing(n_chars)) {
    n_chars = 6
  }
  if (missing(valid_chars)) {
    valid_chars = strsplit(VALID_EXPR_CHARS, "")[[1]]
  }

  chars = sample(valid_chars, n_chars, replace=TRUE)
  return(paste(chars, collapse=""))
}


#' Convert a list of `TestFileResult` objects to a JSON-like object of the correct form for writing
#' results for Gradescope.
#'
#' The returned list has the JSON format
#'
#' ```
#' {
#'   "test_file_results": [
#'     {
#'       // output of TestFileResults$to_list
#'     }
#'   ]
#' }
#' ```
#'
#' @param results The list of `TestFileResult`s
#' @return The generated list
results_to_list = function(results) {
  out = list(
    test_file_results = list()
  )
  for (i in seq_along(results)) {
    out$test_file_results[[i]] = results[[i]]$to_list()
  }
  return(out)
}


#' Export a list of `TestFileResult` objects to a JSON string
#'
#' @param results The list of result objects
#' @return The JSON string
results_to_json = function(results) {
  results = results_to_list(results)
  return(jsonlite::toJSON(results, auto_unbox = TRUE, pretty = TRUE))
}


#' Export a Jupyter Notebook to a zip file for submission.
#'
#' @param notebook_path The path to the notebook
#' @param export_path The path at which to write the zip file (optional)
#' @param display_link Whether to display a download link with `IRdisplay`
#' @export
#' \dontrun{
#' export("hw01.ipynb")
#' }
export = function(notebook_path, export_path=NULL, display_link=TRUE) {
  timestamp = format(Sys.time(), "%Y_%m_%dT%H_%M_%S")

  if (is.null(export_path)) {
    notebook_name = tools::file_path_sans_ext(basename(notebook_path))
    export_path = paste0(notebook_name, "_", timestamp, ".zip")
  }

  zip_filename_file_name = "__zip_filename__"
  writeLines(c(export_path), zip_filename_file_name, sep="")

  zip_files = c(zip_filename_file_name, notebook_path)
  zip::zip(export_path, zip_files)
  file.remove(zip_filename_file_name)

  if (display_link) {
    IRdisplay::display_html(sprintf("
    <p>Your submission has been exported. Click <a href='%s' download='%s'
    target='_blank'>here</a> to download the zip file.</p>
    ", export_path, export_path))
  }
}


#' Determine whether a code snippet has any syntax errors.
#'
#' @param script The code snippet
#' @return Whether the code snippet is valid (can be parsed with `parse`)
#' @export
#' @examples
#' s = "
#' a = TRUE
#' b = c(1, 2, 3)
#' d = function(x) x ^ 2
#' f = d(b)
#' "
#' valid_syntax(s)  # returns TRUE
#'
#' s = "
#' if (TRUE) {
#'   a = c(1, 2)
#' "
#' valid_syntax(s)  # returns FALSE
valid_syntax = function(script) {
  error = FALSE
  tryCatch(
    parse(text = script),
    error = function(e) error <<- TRUE
  )
  return(!error)
}
