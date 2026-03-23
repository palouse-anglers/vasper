# chat helper sanitization tests
#
# Run from project root with:
# Rscript tests/test_chat_helpers.R

library(testthat)

source(file.path("R", "chat_helpers.R"))

if (!methods::isClass("MockChatContent")) {
  methods::setClass("MockChatContent", slots = c(text = "character"))
}

if (!methods::isClass("MockToolResultContent")) {
  methods::setClass("MockToolResultContent", slots = c(value = "list"))
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

describe("parse_tool_result_plot_artifact", {
  test_that("extracts png path and labels for plot artifact tool results", {
    result <- list(
      artifact_type = "plot",
      artifact_id = "plot_soil_trend",
      artifact_label = "Soil trend",
      files = list(
        png = "C:/tmp/plot_soil_trend.png",
        svg = "C:/tmp/plot_soil_trend.svg"
      )
    )

    parsed <- parse_tool_result_plot_artifact(result)

    expect_true(parsed$has_plot)
    expect_equal(parsed$png_path, "C:/tmp/plot_soil_trend.png")
    expect_equal(parsed$artifact_id, "plot_soil_trend")
    expect_equal(parsed$artifact_label, "Soil trend")
  })

  test_that("ignores non-plot or missing-image results", {
    expect_false(parse_tool_result_plot_artifact(list())$has_plot)

    expect_false(
      parse_tool_result_plot_artifact(list(
        artifact_type = "table",
        files = list(png = "C:/tmp/x.png")
      ))$has_plot
    )
  })
})

describe("extract_turn_tool_results", {
  test_that("collects tool result payloads from turn contents", {
    turn <- methods::new(
      "MockChatTurn",
      role = "assistant",
      contents = list(
        methods::new("MockChatContent", text = "Here is your chart."),
        methods::new(
          "MockToolResultContent",
          value = list(
            artifact_type = "plot",
            files = list(png = "C:/tmp/replay.png")
          )
        )
      )
    )

    results <- extract_turn_tool_results(turn)

    expect_length(results, 1)
    expect_equal(results[[1]]$artifact_type, "plot")
    expect_equal(results[[1]]$files$png, "C:/tmp/replay.png")
  })
})
