qmd_files <- list.files(
  path = ".",
  pattern = "\\.qmd$",
  recursive = TRUE,
  full.names = FALSE
)

# Ignore rendered files and files that start with an underscore
qmd_files <- qmd_files[
  !grepl("^(docs|scratch)/", qmd_files) &
  !grepl("^_", basename(qmd_files)) &
  qmd_files != "index.qmd"
]

dirs <- sort(unique(dirname(qmd_files)))

repo_remote <- system2("git", c("remote", "get-url", "origin"), stdout = TRUE, stderr = FALSE)
if (!is.null(attr(repo_remote, "status")) && attr(repo_remote, "status") != 0L) {
  stop("Could not retrieve the origin remote URL.", call. = FALSE)
}
if (!length(repo_remote) || !nzchar(repo_remote[1])) {
  stop("The origin remote URL is empty.", call. = FALSE)
}

repo_base_url <- sub("\\.git$", "", repo_remote[1])
if (grepl("^git@github\\.com:", repo_base_url)) {
  repo_base_url <- sub("^git@github\\.com:", "https://github.com/", repo_base_url)
}
if (grepl("^ssh://git@github\\.com/", repo_base_url)) {
  repo_base_url <- sub("^ssh://git@github\\.com/", "https://github.com/", repo_base_url)
}

# create static yml contents
cat(
'project:
  type: website
  output-dir: scratch/site

filters:
  - output-styling

website:
  title: "The Pacific Northwest Tribal Coding Group"
  search: true
  navbar:
    left:
      - href: index.qmd
        text: Home
  sidebar:
    style: docked
    search: true
    contents:
', file = "_quarto.yml"
)

# Build every directory level represented by the Quarto files.
all_dirs <- unique(unlist(lapply(dirs, function(d) {
  if (d == ".") return(character())
  parts <- strsplit(d, "/", fixed = TRUE)[[1]]
  vapply(seq_along(parts), function(i) paste(parts[seq_len(i)], collapse = "/"), character(1))
})))
top_dirs <- all_dirs[!grepl("/", all_dirs)]

yaml_quote <- function(x) encodeString(x, quote = '"')

render_sidebar_dir <- function(d, indent) {
  pad <- strrep(" ", indent)
  lines <- c(
    sprintf('%s- section: %s\n', pad, yaml_quote(basename(d))),
    sprintf('%s  contents:\n', pad)
  )

  these <- sort(qmd_files[dirname(qmd_files) == d])
  lines <- c(lines, sprintf('%s    - %s\n', pad, vapply(these, yaml_quote, character(1))))

  children <- sort(all_dirs[dirname(all_dirs) == d])
  for (child in children) {
    lines <- c(lines, render_sidebar_dir(child, indent + 4L))
  }
  lines
}

root_files <- sort(qmd_files[dirname(qmd_files) == "."])
cat(sprintf('      - %s\n', vapply(root_files, yaml_quote, character(1))),
    file = "_quarto.yml", append = TRUE, sep = "")
for (d in sort(top_dirs)) {
  cat(render_sidebar_dir(d, 6L), file = "_quarto.yml", append = TRUE, sep = "")
}

# append more static contents
cat(
  sprintf(
    paste0(
      '\n  repo-url: "%s"\n',
      '  repo-actions: [edit, issue]\n\n',
      'format:\n',
      '  html:\n',
      '    theme: cosmo\n',
      '    css:\n',
      '      - checklist.css\n',
      '    include-in-header:\n',
      '      - checklist-sort-head.html\n'
    ),
    if (!is.na(repo_base_url) && nzchar(repo_base_url)) repo_base_url else ""
  ),
  file = "_quarto.yml",
  append = TRUE
)

quarto_status <- system2("quarto", "render")
if (!is.null(quarto_status) && quarto_status != 0L) {
  stop("Quarto render failed with status ", quarto_status, call. = FALSE)
}
