source("renv/activate.R")
Sys.setenv(TERM_PROGRAM = "vscode")
fpath <- file.path(
    Sys.getenv(
        if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"
    ),
    ".vscode-R", "init.R"
)
if (file.exists(fpath)) {
    source(fpath)
}
options(rdeck.mapbox_access_token = "pk.eyJ1IjoibWlrZWx5ZGVhbW9yZSIsImEiOiJjbGQyZmVwM2MwODM0M3ZxaDNscTFubTF3In0.mUGkYsAz7lQi33zRrXnCyg")
