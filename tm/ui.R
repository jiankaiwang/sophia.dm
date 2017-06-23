
# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # css
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "description", content = "I am Jian-Kai Wang."),
    tags$meta(name = "author", content = "JKW, Jian-Kai Wang"),
    tags$title("Single Document | Text Mining | Sophia.dm"),
    tags$link(rel = "stylesheet", type = "text/css", href = "font-awesome/4.6.3/css/font-awesome.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "__customized/1.0/general.css")
  ),
  
  h2("Single Document | Text Mining"),
  h4(
    tags$a(
      href = "https://github.com/jiankaiwang/sophia.dm", 
      span(
        tags$i(class="fa fa-github", `aria-hidden` = "true", class="text-gray"),
        span("Sophia.dm", class="text-gray")
      ),
      target = "_blank"
    ),
    span(" | "),
    tags$a(
      href = "http://jkw.cloudapp.net:3838/sophia.dm", 
      span(
        tags$i(class="fa fa-power-off", `aria-hidden` = "true", class="text-gray"),
        span("Entry", class="text-gray")
      ),
      target = "_blank"
    )
  ),
  
  sidebarPanel(
    
    radioButtons(
      "dataImport", 
      label = h3("Data Import Types"),
      choices = list("Plain Text" = 1, "Web Url" = 2), 
      selected = 1
    ),
    hr(),
    
    selectInput(
      "dataLang", 
      label = h3("Data Language"),
      choices = list(
        "English" = "eng", 
        "Chinese" = "zho",
        "German" = "deu",
        "French" = "fra",
        "Italian" = "ita"
      ), 
      selected = "eng"
    ),
    
    span("*", class="text-red"),
    span(" Notice the data content must be in UTF-8 encoding."),
    hr(),
    
    checkboxGroupInput(
      "dataTrans", 
      label = h3("Data Transformation"), 
      choices = list(
        "Remove Punctuation" = 1, 
        "Remove Numbers" = 2, 
        "Strip Whitespace" = 3
      ),
      selected = 1
    ),
    hr(),
    
    textInput(
      "dataFilter", 
      label = h3("Data Filter by Words"), 
      value = ""
    ),
    span("*", class="text-red"),
    span(" Seperated by comma ',' ..."),
    hr(),
    
    h3("Data Filter by its Length"),
    sliderInput(
      "wordLength", 
      label = h5("Word Length"), 
      min = 1, 
      max = 99, 
      value = c(2, 10)
    ),
    hr(),
    
    h3("Term-Document Weighting"),
    fluidRow(
      column(12,
         radioButtons(
          "weightMethod", 
          label = h5(""),
          choices = list(
            "No Weighting (Default)" = 1, 
            "TF" = 2, 
            "TFIdf" = 3,
            "Bin" = 4,
            "SMART" = 5
          ), 
          selected = 1)
      )
    ),
    h5(
      span("*", class="text-red"),
      span(" Method Description")
    ),
    h5("TF : Term Frequency"),
    h5("TFIdf : Term Frequency - Inverse Document Frequency"),
    h5("Bin : Binary weight"),
    h5("TF : Term Frequency"),
    hr(class = "dashed"),
    h4("Advanced Weighting (only for SMART)"),
    h5("SMART : Combination of Weighting with the following"),
    fluidRow(
      column(12,
        selectInput(
          "smartTerm", 
          label = h5("Weighting for Term Frequency"),
          choices = list(
            "natural" = "n", 
            "logarithm" = "l",
            "augmented" = "a",
            "boolean" = "b",
            "log average" = "L"
          ), 
          selected = "n"
        )
      )
    ),
    fluidRow(
      column(12,
        selectInput(
          "smartDoc", 
          label = h5("Weighting for Document Frequency"),
          choices = list(
            "no" = "n", 
            "idf" = "t",
            "prob idf" = "p"
          ), 
          selected = "n"
        )
      )
    ),
    fluidRow(
      column(12,
       selectInput(
         "smartNormalization", 
         label = h5("Normalization"),
         choices = list(
           "none" = "n", 
           "cosine" = "c",
           "pivoted unique" = "u",
           "byte size" = "b"
         ), 
         selected = "n"
       )
      )
    ),
    hr(),
    
    h3("Data Analysis"),
    sliderInput(
      "findFreqTerms", 
      label = h5("Analysis.1 : Find Terms by Frequency."), 
      min = 0, 
      max = 30, 
      value = 2
    ),
    hr(),
    
    textInput(
      "termAssoc", 
      label = h5("Analysis.2-1 : Term association"), 
      value = "amazon"
    ),
    h5(
      span("*", class="text-red"),
      span(" Seperated by comma ',' ...")
    ),
    sliderInput(
      "findAssocsRatio", 
      label = h5("Analysis.2-2 : Association ratio"), 
      min = 0, 
      max = 1, 
      value = 0.2
    ),
    hr(),
    
    textInput(
      "dictList", 
      label = h5("Analysis.3 : Dictionary Listing"), 
      value = "amazon"
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    
    # web-url or data-content
    fluidRow(
      column(12, 
         textAreaInput(
           "dataImportField", 
           label = h3("Data Import in Text Field"), 
           value = plainDataImportEN,
           width = "100%",
           height = "200px"
         )       
      )
    ),
    h5(
      span("*", class="text-red"),
      span(paste(" Text field could be one of plain-text data or web url path.", sep = ""))
    ),
    h5(
      span("*", class="text-red"),
      span(" English example of web-url type: "),
      span(webUrlImportEN, class="text-bold"),
      span(".")
    ),
    h5(
      span("*", class="text-red"),
      span(" Chinese example of web-url type: "),
      span(webUrlImportCN, class="text-bold"),
      span(".")
    ),
    hr(),
    
    # most frequency
    fluidRow(
      column(12,
        h3("Find terms by frequency")
      ),
      column(12, 
        dataTableOutput("mostFreqTermsTable")
      )
    ),
    hr(),
    
    # association
    fluidRow(
      column(12,
         h3("Term association upon the ratio")
      ),
      column(12, 
        dataTableOutput("assocTermsTable")
      )
    ),
    hr(),
    
    # Dictionary Listing
    fluidRow(
      column(12,
        h3("Listing documents related to terms")
      ),
      column(12, 
        dataTableOutput("dictListing")
      )
    ),
    hr()
  ),
  
  # javascript
  tags$script(type = "text/javascript", src = "seed/1.0/Common.js"),
  tags$script(type = "text/javascript", src = "seed/1.0/JqueryExtends.js"),
  tags$script(type = "text/javascript", src = "seed/1.0/TimeCounter.js"),
  tags$script(type = "text/javascript", src = "__customized/1.0/general.js"),
  tags$script(HTML(""))
  
))










