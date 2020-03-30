# UI

# Just like the code for the server, we base this code heavily on the great outline provided by "The Semantic Librarian".
# Again, our deepest and biggest gratitude goes to [Randall Jamieson](https://umcognitivesciencelaboratory.weebly.com) (Model Development), 
# Matt Cook, [Harinder Aujla](http://ion.uwinnipeg.ca/~haujla/) (Semantic Vectors) and [Matthew Crump](https://crumplab.github.io)  (Shiny Code)
# for their fantastic template on [semantic search engines](https://github.com/CrumpLab/SemanticLibrarian). 
# We build heavily on their source code to set up the graphs, tables and general appearance of the ShinyApp 
# and would like to express our deepest appreciation for their hard work, for making the code publicly 
# available and for their fantastic [SemanticLibrarian](https://semanticlibrarian.shinyapps.io/SemanticLibrarian/) 
# search engine!

##################
# Load libraries
##################
## Save package names as a vector of strings
pkgs <-
  c(
    "shinyjs",
    "shinythemes",
    "plotly"
  )

## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

## Load all packages to library and adjust options
lapply(pkgs, library, character.only = TRUE)

jscode <- "shinyjs.goauthor = function(x){
      Shiny.onInputChange('show_author', x);
Shiny.onInputChange('change_author',x);
tabs = $('.tabbable .nav.nav-tabs li a');
$(tabs[1]).click();
};

shinyjs.goarticle = function(x){
      Shiny.onInputChange('show_article', x);
Shiny.onInputChange('change_article',x);
tabs = $('.tabbable .nav.nav-tabs li a');
$(tabs[0]).click();
};

shinyjs.goarticle = function(x){
      Shiny.onInputChange('show_article_SS', x);
Shiny.onInputChange('change_article_SS',x);
tabs = $('.tabbable .nav.nav-tabs li a');
$(tabs[0]).click();
};

"

ui <- tagList(tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}")),
                        tags$style(HTML(".fa { font-size: 20px; }"))
                        #tags$style(HTML(".hovertext text { font-size: 12px !important;}"))
),
useShinyjs(),
extendShinyjs(text = jscode),
navbarPage("Coro2vid-19",
           theme = shinytheme("yeti"),
           tabPanel("Keyword search",
                    sidebarLayout(
                      sidebarPanel(h3("Search for scientific articles"),
                                   textInput("search_terms", label="Search terms",
                                             value="virus"),
                                   actionButton("vectorize_search_terms", label = "Search"),
                                   actionButton("SShelp", "Help"),
                                   #uiOutput("dictionary"),
                                   #selectizeInput("server_dictionary",label="dictionary",choices=NULL,multiple=TRUE),
                                   br(),
                                   br(),
                                   selectInput("select_query_type", label = "Query Style",
                                               choices = list("Compound terms" = 1, "AND" = 2, "OR" = 3),
                                               selected = 1),
                                   hidden(
                                     sliderInput("slider_BEAGLE_SS", label = "# of BEAGLE dimensions", min = 2,
                                                 max = 1024, value = 1024),
                                     actionButton("redo_beagle_vecs_SS", label = "Recompute")
                                   ),
                                   hr(),
                                   #h5("Selected Abstract"),
                                   htmlOutput("article_plotly")),
                      mainPanel(h3("Find the most semantically similar articles"),
                                textOutput("articleformatted_SS"),
                                h4("Article space"),
                                plotlyOutput("sim_articles_plot_SS", height='100%'),
                                actionButton("SShelp2", "More info"),
                                fluidRow(
                                  column(4,
                                         sliderInput("slider_num_articles", label = "Number of articles", min = 10,
                                                     max = 500, value = 100, step=1)
                                  ),
                                  column(4,
                                         sliderInput("slider_num_k", label = "Number of clusters", min = 1,
                                                     max = 10, value = 3, step=1)
                                  ),
                                  column(4,
                                         sliderInput("slider_num_year_SS", label = "years", min = 1955,
                                                     max = 2020, value = c(1955,2020), step=1)
                                  )
                                ),
                                #h4("Search Term Space"),
                                #plotOutput("sim_terms_plot_SS"),
                                h4("Top 100 Articles"),
                                DT::dataTableOutput("articledataset_SS"),
                                br(),
                                h4("Author space"),
                                plotOutput("sim_authors_plot_SS"),
                                fluidRow(
                                  column(4,
                                         sliderInput("slider_num_authors", label = "Number of authors", min = 10,
                                                     max = 100, value = 25, step=1)
                                  ),
                                  column(4,
                                         sliderInput("slider_num_k_author", label = "Number of clusters", min = 1,
                                                     max = 10, value = 3, step=1)
                                  )
                                )
                                #h4("Mean Similarity by Year"),
                                #plotOutput("sim_hist_SS"),
                                #sliderInput("slider_sim_hist_year", label = "year", min = 1890,
                                #           max = 2015, value = c(1890,2015), step=1)
                      )
                    ),
                    hr(),
                    print("Based on the SemanticLibrarian")
           ),
           
           tabPanel("Find similar articles",
                    sidebarLayout(
                      sidebarPanel(#uiOutput("article"),
                        h3("Search for a scientific article"),
                        selectizeInput("server_article",
                                       label="article",
                                       choices=NULL),
                        sliderInput("slider_BEAGLE_article", label = "# of BEAGLE dimensions", min = 2, 
                                    max = 100, value = 100),
                        actionButton("redo_beagle_vecs", label = "Recompute"),
                        hr(),
                        #h5("Selected Abstract"),
                        htmlOutput("article_plotly2")),
                      mainPanel(h3("Find similar articles"),
                                h4("Article space"),
                                plotlyOutput("sim_articles_plot_AS", height='100%'),
                                fluidRow(
                                  column(4,
                                         sliderInput("slider_num_articles_AS", label = "Number of articles", min = 10, 
                                                     max = 500, value = 100, step=1)
                                  ),
                                  column(4,
                                         sliderInput("slider_num_k_AS", label = "Number of clusters", min = 1, 
                                                     max = 10, value = 3, step=1)
                                  )
                                ),
                                h4("Top articles"),
                                DT::dataTableOutput("articledataset"),
                                br()
                                # h4("Mean Similarity by Year"),
                                # plotOutput("sim_hist")
                      )
                    )
           ),
           
           tabPanel("Find similar authors",
                    sidebarLayout(
                      sidebarPanel(#uiOutput("author"),
                        h3("Search for authors"),
                        selectizeInput("server_author",
                                       label="author",
                                       choices=NULL)),
                      #sliderInput("slider_BEAGLE", label = "# of BEAGLE dimensions", min = 2,
                      #                          max = 1024, value = 1024),
                      # checkboxInput("check_svd", label = "Use SVD", value = FALSE),
                      # sliderInput("slider_svd", label = "choose SVDs", min = 1,
                      #             max = 500, value = c(1,500)),
                      # downloadButton("downloadAuthorData", "Download")
                      # ),
                      mainPanel(h3("Find similar authors"),
                                h4("Author space"),
                                plotlyOutput("sim_articles_plot_AuS", height='100%'),
                                fluidRow(
                                  column(4,
                                         sliderInput("slider_num_articles_AuS", label = "Number of authors", min = 10,
                                                     max = 100, value = 100, step=1)
                                  ),
                                  column(4,
                                         sliderInput("slider_num_k_AuS", label = "Number of clusters", min = 1,
                                                     max = 10, value = 3, step=1)
                                  )
                                ),
                                h4("Top authors"),
                                DT::dataTableOutput("authordataset"))
                    )
           ),
           
           #tabPanel("Database",
           #         verticalLayout(
           #           mainPanel(DT::dataTableOutput("alldataset"))
           #         )
           #),
           
           #tabPanel("MultipleAuthor",
           #         sidebarLayout(
           #           sidebarPanel(uiOutput("multiple_author"),
           #                        actionButton("compute_msim",label="compute")),
           #           mainPanel(plotlyOutput("m_author_sim_plot"))
           #         )
           #),
           
           # tabPanel("ArticleAuthor",
           #          sidebarLayout(
           #            sidebarPanel(
           #              h3("Search an scientific article"),
           #              uiOutput("article_author")),
           #            mainPanel(
           #              h3("Most similar authors to the article"),
           #              h4("Takes a couple of seconds to load..."),
           #              DT::dataTableOutput("article_authordataset"))
           #                       )
           #          ),
           
           tabPanel("About",
                    mainPanel(
                      div(includeMarkdown("markdown/about.md"),
                          style = "margin-left:10%;margin-right:10%;")
                    )
           ),
           tabPanel("Github", icon = icon("fab fa-github"),
                    mainPanel(
                      div(includeMarkdown("markdown/git.md"),
                          style = "margin-left:10%;margin-right:10%;")
                    ))
)
)


shinyApp(ui, server)

