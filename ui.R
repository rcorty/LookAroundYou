fluidPage(
    title = 'Look Around You',
    tags$head(includeHTML('www/google-analytics.html')),
    titlePanel('Look Around You'),
    fluidRow(
        column(
            width = 1,
            brs(2),
            selectInput('investigate_vidx', NULL, choices = NULL),
            brs(2),
            selectInput('move_vidx', NULL, choices = NULL),
            brs(2),
            'Points remaining:'
        ),
        column(
            width = 1,
            brs(2),
            actionButton('investigate', 'Investigate'),
            brs(4),
            actionButton('move', 'Move'),
            brs(4),
            textOutput('points_remaining')
        ),
        column(
            width = 9,
            plotOutput(
                'plot',
                width = '100%',
                height = '800px'
            )
        )
    )
)
