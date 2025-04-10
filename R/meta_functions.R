.badge_repostatus <- function(status) {
  status <- tolower(status)
  svg <- paste0("https://www.repostatus.org/badges/latest/", 
                status, ".svg")
  url <- paste0("https://www.repostatus.org/#", status)
  alt <- switch(
    status,
    concept = "Concept - Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.",
    wip = "WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.",
    suspended = "Suspended - Initial development has started, but there has not yet been a stable, usable release; work has been stopped for the time being but the author(s) intend on resuming work.", 
    abandoned = "Abandoned - Initial development has started, but there has not yet been a stable, usable release; the project has been abandoned and the author(s) do not intend on continuing development.", 
    active = "Active - The project has reached a stable, usable state and is being actively developed.", 
    inactive = "Inactive - The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.", 
    unsupported = "Unsupported - The project has reached a stable, usable state but the author(s) have ceased all work on it. A new maintainer may be desired.", 
    moved = "Moved to http://example.com - The project has been moved to a new location, and the version at that location should be considered authoritative.",
    stop("invalid status: ", status)
  )
  alt <- paste("Project Status:", alt)
  badge_out <- paste0("<a href=\"", url, "\"><img src=\"", svg, "\" alt=\"", alt, "\"></a>")
  badge_out <- paste0("[![", alt, "](", svg, ")](", url, ")")
  paste(badge_out)
}
