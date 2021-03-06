context('Task')

task <- Task$new(function() {
  print('Running task')
  1 + 1
}, autorun = FALSE)

test_that('creating the task object is correct', {
  expect_is(task, 'environment')
  expect_equal(task$Status, 'IDLE')
  expect_false(task$IsRunning)
  expect_false(task$IsFinished)
  expect_false(task$IsCancelled)
  expect_equal(task$HTMLRunLog, '')
  expect_equal(task$RunLog, '')
  expect_null(task$TaskHandle)
  expect_null(task$Result)
})

test_that('running the task is correct', {
  capture.output(task$Run())
  expect_is(task, 'environment')
  expect_equal(task$Status, 'SUCCESS')
  expect_false(task$IsRunning)
  expect_true(task$IsFinished)
  expect_false(task$IsCancelled)
  expect_equal(task$HTMLRunLog, '[1] \"Running task\"\r\n')
  expect_equal(task$RunLog, '[1] \"Running task\"\r\n')
  expect_is(task$TaskHandle, 'r_process')
  expect_equal(task$Result, 2)
})

session <- shiny::MockShinySession$new()
taskShiny <- Task$new(
  function() {
    print('Running task')
    Sys.sleep(5)
    print('Ending task')
    return(1 + 1)
  },
  autorun = FALSE,
  session = session
)
taskShiny$Run()
isolate(taskShiny$HTMLRunLog)
session$flushReact()
isolate(taskShiny$HTMLRunLog)
isolate(taskShiny$IsCancelled)
isolate(taskShiny$IsFinished)
isolate(taskShiny$Result)

session <- shiny::MockShinySession$new()
taskShiny <- Task$new(
  function() {
    stop('Problem')
    return(1)
  },
  autorun = FALSE,
  session = session,
  failCallback = function(msg = NULL) {
    if (!is.null(msg)) {
      PrintAlert(msg, type = 'danger')
    }
  }
)
taskShiny$Run()
isolate(taskShiny$HTMLRunLog)
session$flushReact()
isolate(taskShiny$HTMLRunLog)
isolate(taskShiny$IsCancelled)
isolate(taskShiny$IsFinished)
isolate(taskShiny$Status)
isolate(taskShiny$Result)
