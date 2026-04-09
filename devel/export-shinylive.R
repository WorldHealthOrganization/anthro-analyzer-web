# This script exports the current shiny app to wasm/js/html
# in stores everything in "dist"
shinylive_assets_version <- "0.10.8"
paths_to_copy <- readLines(fs::path("devel", "shinylive_manifest.txt"))
dest_dir <- file.path(tempdir(), "dist")
if (fs::dir_exists(dest_dir)) {
  fs::dir_delete(dest_dir)
}
dirs_to_copy <- Filter(fs::is_dir, paths_to_copy)
files_to_copy <- Filter(fs::is_file, paths_to_copy)
stopifnot(setequal(c(dirs_to_copy, files_to_copy), paths_to_copy))
fs::dir_copy(dirs_to_copy, file.path(dest_dir, dirs_to_copy))
fs::file_copy(files_to_copy, file.path(dest_dir, files_to_copy))

export_dir <- "dist"
if (fs::dir_exists(export_dir)) {
  fs::dir_delete(export_dir)
}
fs::dir_create(export_dir)
fs::file_create(file.path(export_dir, ".gitignore"))
shinylive::export(
  dest_dir,
  export_dir,
  assets_version = shinylive_assets_version
)
fs::file_copy(
  fs::path("devel", "index.html"),
  fs::path(export_dir, "index.html"),
  overwrite = TRUE
)
fs::dir_delete(fs::path(export_dir, "edit"))
fs::dir_delete(dest_dir)

# The function below is currently intended to create a somewhat simple sbom
# of all packages dependencies
generate_sbom <- function() {
  rds <- readRDS(file.path(
    export_dir,
    "shinylive",
    "webr",
    "packages",
    "metadata.rds"
  ))
  pkgs <- lapply(rds, function(pkg) {
    name <- pkg$name
    pkg_path <- file.path(export_dir, "shinylive", "webr", pkg$path)
    desc <- read.dcf(archive::archive_read(
      pkg_path,
      file = file.path(name, "DESCRIPTION")
    ))
    license <- desc[, "License"]
    file_license <- ""
    if (grepl("file LICENSE", license, fixed = TRUE)) {
      file_license <- readr::read_file(
        archive::archive_read(
          pkg_path,
          file = file.path(name, "LICENSE")
        )
      )
    }
    if (grepl("file LICENCE", license, fixed = TRUE)) {
      file_license <- readr::read_file(
        archive::archive_read(
          pkg_path,
          file = file.path(name, "LICENCE")
        )
      )
    }
    data.frame(
      name = desc[, "Package"],
      type = "r-package",
      version = desc[, "Version"],
      license = desc[, "License"],
      source = pkg$assets[[1]]$url,
      file_license = stringr::str_trunc(file_license, 1000)
    )
  })
  pkgs <- dplyr::bind_rows(pkgs)
  pkgs <- dplyr::bind_rows(
    pkgs,
    data.frame(
      name = "Shinylive assets",
      type = "shinylive-assets",
      version = shinylive_assets_version,
      license = "multiple, see assets bundle",
      source = paste0(
        "https://github.com/posit-dev/shinylive/releases/tag/v",
        shinylive_assets_version
      ),
      file_license = ""
    )
  )
  pkgs$retrieved_at <- Sys.time()
  writexl::write_xlsx(pkgs, "sbom_shinylive.xlsx")
}
generate_sbom()
