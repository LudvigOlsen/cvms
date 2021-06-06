run_quietly <- function(fn, ..., msg_context_fn){

    call_process <- tryCatch(
      {
        purrr::map(.x = 1, .f = purrr::quietly(function(.x) {
          fn(...)
        }))
      },
      error = function(e) {
        stop(msg_context_fn(m=e))
      }
    )

    call_process[[1]]
}

throw_messages <- function(messages, msg_context_fn){
  # Message the caught messages to the user
  for (m in messages) {

    # purrr::quietly adds \n to end of messages, which we're not interested in here
    m <- gsub("\\\n$", "", m)

    message(
      simpleMessage(
        paste0(msg_context_fn(m = m), "\n"),
        call = if (p <- sys.parent(1)) sys.call(p)
      )
    )
  }
}

throw_warnings <- function(warnings, msg_context_fn){
  # Throw the caught warnings
  for (w in warnings) {
    warning(
      simpleWarning(
        msg_context_fn(m = w),
        call = if (p <- sys.parent(1)) sys.call(p)
      )
    )
  }
}
