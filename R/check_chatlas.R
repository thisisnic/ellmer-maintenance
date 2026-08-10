library(ellmer)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tibble)

# --- Config ---
parity_path <- "chatlas_parity.csv"
if (file.exists(parity_path)) {
  parity <- read_csv(parity_path, col_types = cols(.default = "c"))
} else {
  parity <- tibble(
    title = character(),
    description = character(),
    chatlas_pr = character(),
    merged_date = character(),
    link = character(),
    detected = character(),
    ignore = character()
  )
}

# --- Fetch chatlas merged PRs from past 2 weeks ---
message("Fetching recently merged chatlas PRs...")

cutoff <- Sys.Date() - 30

# gh pr list sorts by creation date, so search by merge date instead to avoid
# missing older PRs that were merged recently
chatlas_json <- system2(
  "gh", c(
    "pr", "list",
    "--repo", "posit-dev/chatlas",
    "--state", "merged",
    "--search", shQuote(paste0("merged:>=", cutoff)),
    "--limit", "100",
    "--json", "number,title,mergedAt,body,url"
  ),
  stdout = TRUE
) |> paste(collapse = "\n")

chatlas_prs <- jsonlite::fromJSON(chatlas_json)

if (nrow(chatlas_prs) == 0) {
  message("No recent chatlas PRs")
  quit(save = "no", status = 0)
}

message("Found ", nrow(chatlas_prs), " chatlas PRs from past 2 weeks")

# --- Fetch ellmer context: open issues + recent merged PRs ---
message("Fetching ellmer context...")

ellmer_issues_json <- system2(
  "gh", c(
    "issue", "list",
    "--repo", "tidyverse/ellmer",
    "--state", "open",
    "--limit", "100",
    "--json", "number,title"
  ),
  stdout = TRUE
) |> paste(collapse = "\n")

ellmer_prs_json <- system2(
  "gh", c(
    "pr", "list",
    "--repo", "tidyverse/ellmer",
    "--state", "all",
    "--limit", "50",
    "--json", "number,title,state,body"
  ),
  stdout = TRUE
) |> paste(collapse = "\n")

ellmer_prs <- jsonlite::fromJSON(ellmer_prs_json)
ellmer_prs_text <- paste(
  sprintf(
    "PR #%s (%s): %s\n%s",
    ellmer_prs$number,
    ellmer_prs$state,
    ellmer_prs$title,
    substr(ifelse(is.na(ellmer_prs$body), "", ellmer_prs$body), 1, 500)
  ),
  collapse = "\n\n---\n\n"
)

# --- Build prompt ---
chatlas_text <- paste(
  sprintf(
    "PR #%s (merged %s): %s\n%s",
    chatlas_prs$number,
    substr(chatlas_prs$mergedAt, 1, 10),
    chatlas_prs$title,
    substr(ifelse(is.na(chatlas_prs$body), "", chatlas_prs$body), 1, 500)
  ),
  collapse = "\n\n---\n\n"
)

system_prompt <- paste(
  "You compare recently merged PRs from chatlas (Python LLM package) against",
  "ellmer (its R equivalent) to find features or fixes that ellmer should also have.",
  "",
  "chatlas and ellmer are sister packages with similar APIs.",
  "Many changes will already exist in ellmer or be R-irrelevant (Python-specific",
  "packaging, typing, async patterns, etc). Only flag things where:",
  "- ellmer is missing an equivalent feature or fix",
  "- No ellmer issue or PR (open or merged) already covers it",
  "",
  "Read the ellmer PR bodies carefully: they may reveal that ellmer already",
  "had a capability chatlas only added recently, or that work is in flight.",
  "",
  "Skip: documentation-only changes, Python-specific changes, CI/tooling,",
  "things that are clearly already in ellmer based on the issue/PR list.",
  "",
  "If nothing is relevant, return an empty updates array.",
  sep = "\n"
)

update_type <- type_object(
  "A chatlas PR that ellmer should also implement",
  title = type_string("Short description of what ellmer needs, e.g. 'Add price refusal fallback'"),
  description = type_string("2-3 sentences: what chatlas did and what the equivalent ellmer change would be"),
  chatlas_pr = type_string("chatlas PR number, e.g. '#380'"),
  date = type_string("Date the chatlas PR was merged (YYYY-MM-DD)")
)

result_type <- type_object(
  updates = type_array(items = update_type)
)

chat <- chat_anthropic(
  model = "claude-sonnet-4-6",
  system_prompt = system_prompt
)

prompt <- paste0(
  "## Recently merged chatlas PRs\n\n",
  chatlas_text,
  "\n\n## Open ellmer issues\n\n",
  ellmer_issues_json,
  "\n\n## Recent ellmer PRs (open and merged)\n\n",
  ellmer_prs_text
)

result <- tryCatch(
  chat$chat_structured(prompt, type = result_type),
  error = function(e) {
    message("Error from Claude: ", e$message)
    NULL
  }
)

updates <- result$updates
if (is.null(updates) || nrow(updates) == 0) {
  message("No new chatlas features missing from ellmer")
  quit(save = "no", status = 0)
}

# --- Record new updates ---
new_reports <- list()

for (j in seq_len(nrow(updates))) {
  title <- updates$title[j]
  pr_num <- updates$chatlas_pr[j]
  pr_url <- paste0("https://github.com/posit-dev/chatlas/pull/", gsub("#", "", pr_num))

  if (pr_url %in% parity$link) {
    message("  Already reported: ", title)
    next
  }

  message("  New: ", title)

  new_reports <- c(new_reports, list(tibble(
    title = title,
    description = updates$description[j],
    chatlas_pr = pr_num,
    merged_date = updates$date[j],
    link = pr_url,
    detected = as.character(Sys.Date()),
    ignore = "FALSE"
  )))
}

if (length(new_reports) > 0) {
  updated <- bind_rows(parity, bind_rows(new_reports))
  write_csv(updated, parity_path)
  message("Added ", length(new_reports), " items to chatlas_parity.csv")
} else {
  message("No new items to add")
}
