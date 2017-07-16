build_site1 <- function (local = FALSE, method = c("html", "html_encoded", "custom")) {
    if (missing(method)) 
        method = getOption("blogdown.method", method)
    method = match.arg(method)
    config = blogdown:::load_config()
    files = list.files("content", "[.][Rr]md$", recursive = TRUE, 
                       full.names = TRUE)
    files = files[!grepl("^_", basename(files)) | grepl("^_index[.]", 
                                                        basename(files))]
    bookdown:::check_special_chars(files)
    if (getOption("knitr.use.cwd", FALSE)) 
        knitr::opts_knit$set(root.dir = getwd())
    blogdown:::run_script("R/build.R", as.character(local))
    if (method != "custom") {
        build_rmds1(files, config, local, method == "html")
    }
    invisible()
}

build_rmds1 <- function (files, config, local, raw = FALSE) {
    if (length(files) == 0) 
        return(blogdown:::hugo_build(local, config))
    lib1 = blogdown:::by_products(files, c("_files", "_cache", if (!raw) ".html"))
    lib2 = gsub("^content", "blogdown", lib1)
    if (raw) {
        i = grep("_files$", lib2)
        lib2[i] = gsub("^blogdown", "static", lib2[i])
    }
    for (i in seq_along(lib2)) {
        if (blogdown:::file_exists(lib2[i])) {
            file.rename(lib2[i], lib1[i])
        } else {
            blogdown:::dir_copy(lib2[i], lib1[i])
        }
    }
        
    root = getwd()
    base = blogdown:::site_base_dir()
    shared_yml = file.path(root, "_output.yml")
    if (!file.exists(shared_yml)) 
        shared_yml = NA
    copied_yaml = character()
    on.exit(unlink(copied_yaml), add = TRUE)
    for (f in files) blogdown:::in_dir(d <- dirname(f), {
        f = basename(f)
        html = blogdown:::with_ext(f, "html")
        if (local && !require_rebuild(html, f)) 
            next
        if (!is.na(shared_yml) && !file.exists("_output.yml")) {
            file.copy(shared_yml, "./")
            copied_yaml = c(copied_yaml, normalizePath("_output.yml"))
        }
        knitr::knit(f, quiet = TRUE, encoding = 'UTF-8', envir = .GlobalEnv)
        x = blogdown:::readUTF8(html)
        x = blogdown:::encode_paths(x, blogdown:::by_products(f, "_files"), d, raw,  
                         root, base)
        x1 = gsub(pattern = file.path(blogdown:::by_products(f, "_files"), "figure-html"),
            replacement = paste(config$baseurl, gsub("content/", "", d), sep = .Platform$file.sep),
            x)
        x = blogdown:::encode_paths(x, "figure", d, raw, root, base)    
        blogdown:::dirs_copy(file.path("figure"), 
                             file.path(paste(rep("..", length(strsplit(d, "/")[[1]])), collapse = .Platform$file.sep),
                                       "content", config$blogdir, blogdown:::by_products(f, "_files"), "figure-html"))
        if (getOption("blogdown.widgetsID", TRUE)) 
            x = blogdown:::clean_widget_html(x)
        if (raw) 
            x = blogdown:::split_html_tokens(x, FALSE)$body
        blogdown:::prepend_yaml(f, html, x)
    })
    blogdown:::dirs_copy(lib1, lib2)
    blogdown:::hugo_build(local, config)
    if (!raw) {
        blogdown:::in_dir(blogdown:::publish_dir(config), blogdown:::process_pages(local, root))
        i = blogdown:::file_exists(lib1)
        lapply(unique(dirname(lib2[i])), blogdown:::dir_create)
        file.rename(lib1[i], lib2[i])
    }
    unlink(lib1, recursive = TRUE)
}

build_site1()