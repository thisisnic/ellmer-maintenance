library(ellmer)
library(httr2)
library(rvest)
library(readr)
library(dplyr, warn.conflicts = FALSE)

# --- Config ---
target_repo <- "tidyverse/ellmer"
sources <- read_csv("sources.csv", show_col_types = FALSE)

reported_path <- "reported.csv"
if (file.exists(reported_path)) {
  reported <- read_csv(reported_path, col_types = cols(.default = "c"))
} else {
  reported <- tibble(
    provider = character(),
    title = character(),
    description = character(),
    change_date = character(),
    link = character(),
    detected = character(),
    ignore = character()
  )
}

# --- Structured output type ---
update_type <- type_object(
  "A single relevant update",
  title = type_string("Short title for a GitHub issue, e.g. 'GPT-5.6 pricing reduced 80%'"),
  description = type_string("2-3 sentences: what changed and what ellmer might need to do"),
  date = type_string("Date of the change (YYYY-MM-DD), or 'unknown'"),
  link = type_string("URL to the specific announcement page, or 'unknown'")
)

result_type <- type_object(
  updates = type_array(items = update_type)
)

system_prompt <- paste(
  "You identify LLM provider changelog entries relevant to the ellmer R package.",
  "ellmer wraps LLM APIs (OpenAI, Google Gemini, Anthropic Claude, AWS Bedrock,",
  "Perplexity, Ollama, Groq, DeepSeek, etc.).",
  "",
  "An update is only relevant if it plausibly requires a change to ellmer's",
  "code:",
  "- New flagship model versions that are candidates for bumping ellmer's",
  "  default models",
  "- Model deprecations or retirements (ellmer may reference them in defaults",
  "  or docs)",
  "- API changes (new parameters, changed endpoints, changed behavior)",
  "- New capabilities relevant to chat/tool-use/structured-output",
  "",
  "ellmer's pricing data is regenerated automatically from LiteLLM's data, so",
  "pricing changes on their own are NOT relevant and should not be mentioned",
  "as something ellmer needs to act on.",
  "",
  "A model merely becoming *available* is NOT relevant on its own. ellmer does",
  "not maintain a model catalogue: users pass whatever model name they like.",
  "This especially applies to local runners like Ollama, where users pull",
  "their own models. Only report a new model if ellmer would need to act",
  "(update defaults or handle a new API surface).",
  "",
  "Ignore: UI changes, dashboard features, non-chat products (image/video",
  "generation, speech, robotics), enterprise account features, things with",
  "no API impact on a chat/tool-calling/structured-output client.",
  "Ignored product categories stay ignored even for deprecations or",
  "retirements: a robotics or video model being deprecated is still not",
  "relevant. Never emit vague catch-all items like 'changelog update'.",
  "",
  paste0(
    "Today's date is ", Sys.Date(), ". Only include updates dated on or ",
    "after ", Sys.Date() - 30, ". Undated items are only allowed if the ",
    "page clearly implies they are recent."
  ),
  "If nothing is relevant, return an empty updates array.",
  sep = "\n"
)

# --- Main loop ---
new_reports <- list()

for (i in seq_len(nrow(sources))) {
  provider <- sources$provider[i]
  url <- sources$url[i]
  message("Checking ", provider, "...")

  page_text <- tryCatch(
    {
      resp <- request(url) |>
        req_headers("User-Agent" = "ellmer-maintenance/1.0") |>
        req_perform()
      html <- read_html(resp_body_string(resp))
      html_text2(html)
    },
    error = function(e) {
      message("  Failed to fetch: ", e$message)
      NULL
    }
  )
  if (is.null(page_text)) next

  # Truncate to keep costs down
  page_text <- substr(page_text, 1, 20000)

  chat <- chat_anthropic(
    model = "claude-sonnet-4-6",
    system_prompt = system_prompt
  )

  already_reported <- reported$title[reported$provider == provider]
  reported_context <- if (length(already_reported) > 0) {
    paste0(
      "\n\nAlready reported (do NOT include these again, even reworded):\n",
      paste("-", already_reported, collapse = "\n")
    )
  } else {
    ""
  }

  result <- tryCatch(
    chat$chat_structured(
      paste0(
        "Provider: ", provider, "\n\nChangelog:\n\n", page_text,
        reported_context
      ),
      type = result_type
    ),
    error = function(e) {
      message("  Error from Claude: ", e$message)
      NULL
    }
  )

  updates <- result$updates
  if (is.null(updates) || nrow(updates) == 0) {
    message("  No relevant updates")
    next
  }

  for (j in seq_len(nrow(updates))) {
    title <- updates$title[j]
    description <- updates$description[j]
    date <- updates$date[j]
    link <- updates$link[j]

    if (title %in% reported$title) {
      message("  Already reported: ", title)
      next
    }

    new_reports <- c(new_reports, list(tibble(
      provider = provider,
      title = title,
      description = description,
      change_date = date,
      link = if (link != "unknown") link else url,
      detected = as.character(Sys.Date()),
      ignore = "FALSE"
    )))
  }
}

if (length(new_reports) > 0) {
  updated <- bind_rows(reported, bind_rows(new_reports))
  write_csv(updated, reported_path)
  message("Added ", length(new_reports), " new updates to reported.csv")
} else {
  message("No new updates to report")
}
