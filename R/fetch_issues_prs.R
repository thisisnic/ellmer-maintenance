library(gh)
library(purrr)
library(dplyr, warn.conflicts = FALSE)
library(readr)

dir.create("data", showWarnings = FALSE)

# --- Fetch open issues ---
message("Fetching open issues...")
issues <- list()
page <- 1

repeat {
  resp <- gh(
    "GET /repos/{owner}/{repo}/issues",
    owner = "tidyverse",
    repo = "ellmer",
    state = "open",
    per_page = 100,
    page = page
  )
  if (length(resp) == 0) break
  issues <- c(issues, resp)
  page <- page + 1
}

# gh returns PRs mixed in with issues
issues <- keep(issues, ~ is.null(.x$pull_request))

issues_df <- tibble(
  number = map_int(issues, "number"),
  title = map_chr(issues, "title"),
  body = map_chr(issues, ~ .x$body %||% NA_character_),
  user = map_chr(issues, ~ .x$user$login),
  labels = map_chr(issues, ~ paste(.x$labels |> map_chr("name"), collapse = "; ")),
  state = map_chr(issues, "state"),
  created_at = map_chr(issues, "created_at"),
  updated_at = map_chr(issues, "updated_at"),
  comments = map_int(issues, "comments"),
  url = map_chr(issues, "html_url")
)

# --- Fetch linked PRs (GitHub "Development" links) via GraphQL ---
message("Fetching linked PRs...")
gql <- '
query {
  repository(owner: "tidyverse", name: "ellmer") {
    issues(states: OPEN, first: 100) {
      nodes {
        number
        closedByPullRequestsReferences(first: 10, includeClosedPrs: false) {
          nodes { number }
        }
      }
    }
  }
}'
gql_resp <- gh("POST /graphql", query = gql)
linked_df <- tibble(
  number = map_int(gql_resp$data$repository$issues$nodes, "number"),
  linked_prs = map_chr(
    gql_resp$data$repository$issues$nodes,
    ~ paste(
      map_int(.x$closedByPullRequestsReferences$nodes, "number"),
      collapse = "; "
    )
  )
)
issues_df <- left_join(issues_df, linked_df, by = "number")

write_csv(issues_df, "data/ellmer_issues.csv")
message("Saved ", nrow(issues_df), " open issues")

# --- Fetch open PRs ---
message("Fetching open PRs...")
prs <- list()
page <- 1

repeat {
  resp <- gh(
    "GET /repos/{owner}/{repo}/pulls",
    owner = "tidyverse",
    repo = "ellmer",
    state = "open",
    per_page = 100,
    page = page
  )
  if (length(resp) == 0) break
  prs <- c(prs, resp)
  page <- page + 1
}

prs_df <- tibble(
  number = map_int(prs, "number"),
  title = map_chr(prs, "title"),
  body = map_chr(prs, ~ .x$body %||% NA_character_),
  user = map_chr(prs, ~ .x$user$login),
  labels = map_chr(prs, ~ paste(.x$labels |> map_chr("name"), collapse = "; ")),
  state = map_chr(prs, "state"),
  draft = map_lgl(prs, "draft"),
  created_at = map_chr(prs, "created_at"),
  updated_at = map_chr(prs, "updated_at"),
  url = map_chr(prs, "html_url")
)

write_csv(prs_df, "data/ellmer_prs.csv")
message("Saved ", nrow(prs_df), " open PRs")
