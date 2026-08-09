# Presence of this file stops Shiny from auto-sourcing R/ alphabetically.
# app.R sources global.R first (packages) and then everything else; letting
# Shiny load the folder in name order would run data_sources.R before any
# library() call and fail with "could not find function 'tribble'".
