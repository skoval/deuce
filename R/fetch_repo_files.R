#' Scrapes CSV files from GITHUB repository
#' 
#' @param. repo The repo user/repo_name
#' 
#' @export
fetch_repo_files <- function(repo, branch = "master") {
  # repo format: "user/repo"
  api_url <- paste0("https://api.github.com/repos/", repo, "/git/trees/", branch, "?recursive=1")
  res <- GET(api_url)

  if (status_code(res) != 200) {
    stop("Failed to retrieve data from GitHub API.")
  }

  content <- fromJSON(content(res, "text", encoding = "UTF-8"))
  files <- content$tree$path
  files <- grep("csv", files, val = TRUE)
  full_files <- sapply(files, function(x){
	glue::glue("/{repo}/blob/{branch}/{x}")
  })

return(full_files)
}
