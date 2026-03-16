# chat helper sanitization tests
#
# Run from project root with:
# Rscript tests/test_chat_helpers.R

library(testthat)

source(file.path("R", "chat_helpers.R"))

if (!methods::isClass("MockChatContent")) {
  methods::setClass("MockChatContent", slots = c(text = "character"))
}

if (!methods::isClass("MockChatTurn")) {
  methods::setClass(
    "MockChatTurn",
    slots = c(role = "character", contents = "list")
  )
}

describe("sanitize_chat_text_scalar", {
  test_that("drops null-like tokens and blanks", {
    inputs <- c("", "   ", "NA", "user NA", "null", "N/A", "none")
    out <- vapply(inputs, sanitize_chat_text_scalar, character(1))

    expect_true(all(is.na(out)))
  })

  test_that("keeps meaningful text", {
    expect_equal(sanitize_chat_text_scalar("hello"), "hello")
    expect_equal(
      sanitize_chat_text_scalar("  Wheat yield trend  "),
      "Wheat yield trend"
    )
  })
})

describe("turn replay sanitization", {
  test_that("keeps only user/assistant turns with meaningful text", {
    turns <- list(
      methods::new(
        "MockChatTurn",
        role = "user",
        contents = list(methods::new(
          "MockChatContent",
          text = "Show wheat yield"
        ))
      ),
      methods::new(
        "MockChatTurn",
        role = "assistant",
        contents = list(methods::new("MockChatContent", text = "NA"))
      ),
      methods::new(
        "MockChatTurn",
        role = "tool",
        contents = list(methods::new(
          "MockChatContent",
          text = "tool call payload"
        ))
      ),
      methods::new(
        "MockChatTurn",
        role = "assistant",
        contents = list(methods::new(
          "MockChatContent",
          text = "Here are the trends"
        ))
      )
    )

    kept <- sanitize_turns_for_replay(turns)

    expect_length(kept, 2)
    expect_equal(normalize_turn_role(kept[[1]]), "user")
    expect_equal(normalize_turn_role(kept[[2]]), "assistant")
    expect_equal(get_turn_text(kept[[2]]), "Here are the trends")
  })
})
