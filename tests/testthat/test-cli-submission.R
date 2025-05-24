test_that("create_exit_status creates right code and message", {
  true_unnamed <- TRUE
  false_unnamed <- FALSE
  true_named <- TRUE |> purrr::set_names("true")
  false_named <- FALSE |> purrr::set_names("false")
  target_variable_name <- "test_variable"
  named_all_trues <- c(true_named, true_named, true_named)
  unnamed_all_trues <- c(true_unnamed, true_unnamed, true_unnamed)
  named_all_falses <- c(false_named, false_named, false_named)
  unnamed_all_falses <- c(false_unnamed, false_unnamed, false_unnamed)
  named_mixed <- c(true_named, false_named, true_named)
  unnamed_mixed <- c(true_unnamed, false_unnamed, true_unnamed)
  mixed_all_true <- c(true_named, true_named, true_unnamed)
  mixed_all_false <- c(false_named, false_named, false_unnamed)

  # danger vars
  expect_equal(
    create_exit_status(target_variable_name, danger_variables = named_all_trues),
    list(code = 0, message = "Success: test_variable")
  )
  expect_equal(
    create_exit_status(target_variable_name, danger_variables = unnamed_all_trues),
    list(code = 0, message = "Success: test_variable")
  )
  expect_equal(
    create_exit_status(target_variable_name, danger_variables = mixed_all_true),
    list(code = 0, message = "Success: test_variable")
  )
  expect_equal(
    create_exit_status(target_variable_name, danger_variables = named_all_falses),
    list(code = 1, message = "Danger: test_variable does not have allowable value/s\nTroublemakers: false, false, false")
  )
  expect_equal(
    create_exit_status(target_variable_name, danger_variables = unnamed_all_falses),
    list(code = 1, message = "Danger: test_variable does not have allowable value/s")
  )
  expect_equal(
    create_exit_status(target_variable_name, danger_variables = named_mixed),
    list(code = 1, message = "Danger: test_variable does not have allowable value/s\nTroublemakers: false")
  )
  expect_equal(
    create_exit_status(target_variable_name, danger_variables = unnamed_mixed),
    list(code = 1, message = "Danger: test_variable does not have allowable value/s")
  )
  expect_equal(
    create_exit_status(target_variable_name, danger_variables = mixed_all_false),
    list(code = 1, message = "Danger: test_variable does not have allowable value/s")
  )
  # warn vars
  expect_equal(
    create_exit_status(target_variable_name, warn_variables = named_all_trues),
    list(code = 0, message = "Success: test_variable")
  )
  expect_equal(
    create_exit_status(target_variable_name, warn_variables = unnamed_all_trues),
    list(code = 0, message = "Success: test_variable")
  )
  expect_equal(
    create_exit_status(target_variable_name, warn_variables = mixed_all_true),
    list(code = 0, message = "Success: test_variable")
  )
  expect_equal(
    create_exit_status(target_variable_name, warn_variables = named_all_falses),
    list(code = 2, message = "Warning: test_variable may not have correct format\nTroublemakers: false, false, false")
  )
  expect_equal(
    create_exit_status(target_variable_name, warn_variables = unnamed_all_falses),
    list(code = 2, message = "Warning: test_variable may not have correct format")
  )
  expect_equal(
    create_exit_status(target_variable_name, warn_variables = named_mixed),
    list(code = 2, message = "Warning: test_variable may not have correct format\nTroublemakers: false")
  )
  expect_equal(
    create_exit_status(target_variable_name, warn_variables = unnamed_mixed),
    list(code = 2, message = "Warning: test_variable may not have correct format")
  )
  expect_equal(
    create_exit_status(target_variable_name, warn_variables = mixed_all_false),
    list(code = 2, message = "Warning: test_variable may not have correct format")
  )
})

test_that("message_cli runs without error", {
  status_list_success <- list(code = 0, message = "Success: test_variable")
  status_list_danger <- list(code = 1, message = "Danger: test_variable does not have allowable value/s")
  status_list_warn <- list(code = 2, message = "Warning: test_variable may not have correct format")
  expect_no_error(message_cli(list(code = 3, message = "unknown")))
  expect_no_error(message_cli(status_list_success))
  expect_no_error(message_cli(status_list_danger))
  expect_no_error(message_cli(status_list_warn))
})

test_that("list_troublemakers returns right string", {
  mixed <- c(TRUE, FALSE, TRUE) |> purrr::set_names(c("true", "false", "true"))
  all_true <- c(TRUE, TRUE, TRUE) |> purrr::set_names(c("true", "true", "true"))
  all_false <- c(FALSE, FALSE, FALSE) |> purrr::set_names(c("false", "false", "false"))

  expect_equal(list_troublemakers(mixed), "false")
  expect_equal(list_troublemakers(all_true), "")
  expect_equal(list_troublemakers(all_false), "false, false, false")
})
