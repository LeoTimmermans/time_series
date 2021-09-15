panel_card <- function(title, ..., footer = NULL) {
    
    ftr <- NULL
    if (!is.null(footer)) ftr <- div(class = "panel-footer", footer)
    
    div(
        class = "panel", 
        div(
            class = "panel-header",
            h4(title)
        ),
        div(
            class = "panel-body",
            ...
        ),
        ftr
    )
    
}

load_message <- function() {
    require(shiny)
    tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 60px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
            "))
}