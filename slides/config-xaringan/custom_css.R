u <- '
.author-footnote:after {
    z-index: 10;
    bottom: 0.3em;
    left: 20px;
    font-size: 14px;
    content: \'%s\';
    color: white;
    position: absolute;
    height: 1em;
    width: 50%%;
    background-repeat: no-repeat;
    background-size: contain;
}'

if (exists("params") && !is.null(params$footnote) && is.character(params$footnote[1])) {
    cat(sprintf(u, params$footnote[1]),
        file = "config-xaringan/custom.css")
} else {
    cat("", file = "config-xaringan/custom.css")
}

u <- '
:not(.title-slide).remark-slide-content {
    background-image: url(%s);
    background-position: center;
    background-size: cover;
    background-repeat: no-repeat;
}
.title-slide.remark-slide-content {
    background-image: linear-gradient(rgba(255, 255, 255, 0.35), rgba(0, 0, 0, 0.31)), url(%s);
    background-position: center;
    background-size: cover;
    background-repeat: no-repeat;
}'

if (exists("params") && !is.null(params$background) && is.character(params$background[1])) {
    cat(sprintf(u, params$background[1], params$background[1]),
        file = "config-xaringan/custom.css",
        append = TRUE)
}

u <- '
.title-slide.remark-slide-content:after {
    content: "";
    position: absolute;
    top: 100px;
    left: 125px;
    height: 150px;
    width: 280px;
    background-repeat: no-repeat;
    background-size: contain;
    background-image: url(%s);
}'

if (exists("params") && !is.null(params$logo) && is.character(params$logo[1])) {
    cat(sprintf(u, params$logo[1]),
        file = "config-xaringan/custom.css",
        append = TRUE)
}
