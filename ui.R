ui <- fluidPage(
    title = 'Look Around You',
    tags$head(includeHTML(path = 'google-analytics.html')),
    titlePanel('Look Around You'),
    fluidPage(
        # column(
        #     width = 2L,
        #     fluidRow(
        #         uiOutput(outputId = 'investigate_vidxs', inline = TRUE),
        #         actionButton(inputId = 'investigate', label = 'Investigate', width = '40%', inline = TRUE)
        #     )
        # ),
        column(
            width = 1L,
            br(), br(),
            uiOutput(outputId = 'investigate_vidxs'),
            br(), br(),
            uiOutput(outputId = 'move_vidxs'),
            br(), br(),
            'Points remaining:'
        ),
        column(
            width = 1L,
            br(), br(),
            actionButton(inputId = 'investigate', label = 'Investigate'),
            br(), br(), br(), br(),
            actionButton(inputId = 'move', label = 'Move'),
            br(), br(), br(), br(),
            textOutput(outputId = 'points_remaining')
        ),
        column(
            width = 9L,
            plotOutput(outputId = 'the_plot',
                       width = '100%',
                       height = '800px')
        )
    )    
)
