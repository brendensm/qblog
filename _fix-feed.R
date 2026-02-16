library(xml2)

site_dir <- "_site"
feed_file <- file.path(site_dir, "blog-r.xml")

if (!file.exists(feed_file)) {
  message("No blog-r.xml found, skipping feed fix.")
  quit(save = "no")
}

feed <- read_xml(feed_file)
items <- xml_find_all(feed, "//item")
content_ns <- "http://purl.org/rss/1.0/modules/content/"

for (item in items) {
  link <- xml_text(xml_find_first(item, "link"))
  path <- sub("https://brendenmsmith\\.com/", "", link)
  path <- sub("/$", "", path)
  html_file <- file.path(site_dir, path, "index.html")

  if (!file.exists(html_file)) {
    message("Skipping (not found): ", html_file)
    next
  }

  # Get the YAML description from the post's meta tag
  html_raw <- readLines(html_file, warn = FALSE)
  html_text <- paste(html_raw, collapse = "\n")

  desc_match <- regmatches(
    html_text,
    regexpr('name="description" content="([^"]*)"', html_text, perl = TRUE)
  )
  yaml_desc <- if (length(desc_match) > 0) {
    sub('name="description" content="', "", desc_match)
    sub('"$', "", sub('name="description" content="', "", desc_match))
  } else {
    ""
  }

  # Set description to YAML description
  desc_node <- xml_find_first(item, "description")
  xml_set_text(desc_node, yaml_desc)

  # Extract body content for content:encoded
  content <- sub(".*</header>\\s*", "", html_text)
  content <- sub("^\\s*<nav id=\"TOC\".*?</nav>\\s*", "", content)
  content <- sub("\\s*<script>\\s*\ndocument\\.addEventListener.*", "", content)
  content <- sub("\\s*<hr>.*", "", content)
  content <- sub("\\s*<script src=\"https://utteranc\\.es.*", "", content)
  content <- sub("\\s*<script id=\"quarto-html.*", "", content)
  content <- sub("\\s*</body>.*", "", content)
  content <- trimws(content)

  if (nchar(content) > 0) {
    # Add content:encoded element
    content_node <- xml_add_child(item, "content:encoded", .where = "after")
    xml_set_text(content_node, content)
  }
}

write_xml(feed, feed_file)

# Post-process: unescape HTML in content:encoded and wrap in CDATA
feed_text <- readLines(feed_file, warn = FALSE)
feed_text <- paste(feed_text, collapse = "\n")

unescape_html <- function(x) {
  x <- gsub("&lt;", "<", x)
  x <- gsub("&gt;", ">", x)
  x <- gsub("&amp;", "&", x)
  x <- gsub("&quot;", '"', x)
  x <- gsub("&#39;", "'", x)
  x
}

# Wrap content:encoded in CDATA with unescaped HTML (content spans multiple lines)
matches <- gregexpr("<content:encoded>[\\s\\S]*?</content:encoded>", feed_text, perl = TRUE)
m <- regmatches(feed_text, matches)[[1]]

for (orig in m) {
  inner <- sub("^<content:encoded>", "", orig)
  inner <- sub("</content:encoded>$", "", inner)
  inner <- unescape_html(inner)
  replacement <- paste0("<content:encoded><![CDATA[", inner, "]]></content:encoded>")
  feed_text <- sub(orig, replacement, feed_text, fixed = TRUE)
}

writeLines(feed_text, feed_file)
message("Fixed feed: ", feed_file)
