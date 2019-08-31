function(input, output, session) {

    # initialization
    showModal(
        modalDialog(
            title = 'Look Around You',
            size = 'l',
            HTML('You start at spot 1. Your goal is to get to the <b>GOAL</b>.<br><br>',
                 'You can INVESTIGATE and MOVE.<br><br>',
                 'MOVE changes where you are.',
                 'You must move foreward (never backward) and each MOVE costs one point.<br><br>',
                 'INVESTIGATE shows you where you can go from a given spot.',
                 'You can INVESTIGATE the spot where you are or any spot ahead of you.',
                 'The cost to INVESTIGATE a spot is your distance to that spot + 1.'),
            easyClose = TRUE,
            footer = NULL
        )
    )

    initial_graph <-
        make_empty_graph(directed = TRUE) %>%
        add_vertices(
            nv = 1, depth = 0L, x = 0, y = 0,
            current = TRUE, investigated = FALSE
        )
    graph <- reactiveVal(initial_graph)
    points <- reactiveVal(100L)

    # MOVE
    # reactive UI
    observe({
        children <- neighbors(
            graph = graph(),
            v = current_vidx(graph()),
            mode = 'out'
        )
        updateSelectInput(session, 'move_vidx', choices = children)
    })

    # change the graph in response to move
    observeEvent(input$move, {
        new_graph <- move(graph = graph(), to_vidx = input$move_vidx)
        graph(new_graph)
    })

    # update point total in response to move
    observeEvent(input$move, {
        points(points() - 1L)
    })

    # check for win
    observeEvent(input$move, ignoreInit = TRUE, {
        if (has_won(graph())) {
            showModal(modalDialog(
                title = 'You won!', size = 'l',
                paste('You had', points(), 'points remaining.'),
                easyClose = TRUE, footer = NULL
            ))
        }
    })

    # INVESTIGATE
    # reactive UI
    observe({
        n <- graph() %>%
            neighborhood(order = 10, nodes = current_vidx(graph()), mode = 'out') %>%
            first() %>%
            as.integer()

        available <- graph() %>%
            vertex_df() %>%
            slice(n) %>%
            filter(!investigated) %>%
            pull(vidx)

        updateSelectInput(session, 'investigate_vidx', choices = available)
    })

    # based on investigation, add nodes to graph
    observeEvent(input$investigate, {
        new_graph <- investigate(graph = graph(), vidx = input$investigate_vidx)
        graph(new_graph)
    })

    # based on investigation, deduct points
    observeEvent(input$investigate, {
        deduct <- cost_to_investigate(graph = graph(), vidx = input$investigate_vidx)
        points(points() - deduct)
    })

    # render outputs
    output$plot <- renderPlot({
        plot_graph(graph())
    })
    output$points_remaining <- renderText({
        points()
    })
}
