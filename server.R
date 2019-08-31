library(shiny)
library(tidyverse)
library(igraph)
library(shinyalert)

vertex_df <- function(g) {
    as_data_frame(x = g, what = 'vertices') %>%
        mutate(vidx = 1:n())
}

edge_df <- function(g) {
    as_data_frame(x = g, what = 'edges')
}

current_vidx <- function(g) {
    r <- which(vertex_df(g)$current)
    if (length(r) != 1L)
        stop('More than one vertex is labelled current.')
    return(r)
}

has_won <- function(g) {
    # browser()
    all(between(x = vertex_df(g)$x[current_vidx(g)], left = 10, right = 12),
        between(x = vertex_df(g)$y[current_vidx(g)], left = -5, right = 5))
}

investigate <- function(g, vidx) {

    if (!(vidx %in% V(g)))
        stop('Tried to add children to absent parent.')

    x <- vertex_attr(graph = g, name = 'x', index = vidx)
    y <- vertex_attr(graph = g, name = 'y', index = vidx)
    depth <- vertex_attr(graph = g, name = 'depth', index = vidx)

    num_children <- sample(x = 1:5, size = 1)
    new_spots <- simulate_spots(n = num_children, x = x, y = y)
    g %>%
        set_vertex_attr(name = 'investigated', index = vidx, value = TRUE) %>%
        add_vertices(
            nv = num_children,
            depth = depth + 1,
            x = new_spots$x,
            y = new_spots$y,
            current = FALSE,
            investigated = FALSE
        ) %>%
        add_edges(
            # slightly hacky way to interleave ("riffle") two vectors
            c(rbind(vidx, tail(V(.), num_children)))
        )
}

simulate_spots <- function(n, x, y) {
    repeat{
        angle <- sort(runif(n = n, min = 0, max = 2*pi))
        if (all(diff(angle) > 0.3)) break
    }
    r <- runif(n = n, min = 0.5, max = 3)
    list(x = x + r*sin(angle),
         y = y + r*cos(angle))
}

cost_to_investigate <- function(g, vidx) {
    vdf <- vertex_df(g)
    vdf$depth[as.integer(vidx)] - vdf$depth[vdf$current] + 1L
}

move <- function(g, to_vidx) {

    if (!(to_vidx %in% V(g)))
        stop('Tried to move to a non-existent node.')

    from_vidx <- current_vidx(g)

    # check for edge between from_vertex and to_vertex
    if (nrow(filter(edge_df(g = g), from == from_vidx, to == to_vidx)) != 1L)
        stop('Error -- no edge between from_vertex and to_vertex')

    # if we got this far, go ahead and make the 'move'
    g %>%
        set_vertex_attr(name = 'current', index = from_vidx, value = FALSE) %>%
        set_vertex_attr(name = 'current', index = to_vidx, value = TRUE)
}

plot_graph <- function(g) {

    vs <- as_data_frame(x = g, what = 'vertices') %>%
        mutate(vidx = as.integer(V(g)))

    current_depth <- vs$depth[current_vidx(g)]

    es <- as_data_frame(x = g, what = 'edges') %>%
        mutate(from_x = vs$x[from],
               to_x = vs$x[to],
               from_y = vs$y[from],
               to_y = vs$y[to])

    ggplot(data = vs) +
        annotate(geom = 'rect', xmin = 10, xmax = 12, ymin = -5, ymax = 5,
                 color = 'black', fill = 'gray') +
        annotate(geom = 'text', x = 11, y = 0, label = 'GOAL', angle = -90,
                 size = 20, color = 'red') +
        geom_segment(data = es,
                     mapping = aes(x = from_x, xend = to_x,
                                   y = from_y, yend = to_y)) +
        geom_label(mapping = aes(x = x, y = y, label = vidx, fill = depth),
                   alpha = 1) +
        annotate(geom = 'segment',
                 x = vs$x[current_vidx(g)],
                 y = vs$y[current_vidx(g)] + 0.6,
                 xend = vs$x[current_vidx(g)],
                 yend = vs$y[current_vidx(g)] + 0.2,
                 size = 3,
                 lineend = 'butt',
                 linejoin = 'mitre',
                 arrow = arrow(length = unit(12, 'pt'), type = 'closed')) +
        scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                             midpoint = current_depth) +
        # scale_fill_manual(breaks = c(FALSE, TRUE), values = c('darkgray', 'lightgray')) +
        # scale_size_manual(breaks = c(FALSE, TRUE), values = c(10, 18)) +
        guides(fill = FALSE, size = FALSE) +
        theme_void() +
        theme(plot.margin = margin(0,0,0,0, 'pt'))
}

server <- function(input, output) {

    # initialization
    showModal(modalDialog(title = 'Look Around You', size = 'l',
                          HTML('You start at spot 1. Your goal is to get to the GOAL.<br><br>',
                               'You can INVESTIATE and MOVE.<br><br>',
                               'MOVE changes where you are.',
                               'You must move foreward (never backward) and each MOVE costs one point<br><br>',
                               'INVESTIGATE shows you where you can go from a given spot.',
                               'You can INVESTIGATE the spot where you are or any spot ahead of you.',
                               'The cost to INVESTIGATE a spot is your distance to that spot + 1'),
                          easyClose = TRUE, footer = NULL))

    g <- reactiveVal(
        make_empty_graph(directed = TRUE) %>%
            add_vertices(nv = 1, depth = 0L, x = 0, y = 0,
                         current = TRUE, investigated = FALSE)
    )
    points <- reactiveVal(100L)

    # MOVE
    # reactive UI
    output$move_vidxs <- renderUI(expr = {

        children <- neighbors(graph = g(),
                              v = current_vidx(g()),
                              mode = 'out')

        selectInput(inputId = 'move_vidx',
                    label = NULL,
                    choices = children,
                    multiple = FALSE)
    })

    # change the graph in response to move
    observeEvent(
        eventExpr = input$move,
        handlerExpr = g(move(g = g(), to_vidx = input$move_vidx))
    )

    # update point total in response to move
    observeEvent(
        eventExpr = input$move,
        handlerExpr = points(points() - 1L)
    )

    # check for win
    observeEvent(
        eventExpr = input$move,
        handlerExpr = {
            if (has_won(g()))
                showModal(modalDialog(title = 'You won!', size = 'l',
                                      paste('You had', points(), 'points remaining.'),
                                      easyClose = TRUE, footer = NULL))
        },
        ignoreInit = TRUE
    )

    # INVESTIGATE
    # reactive UI
    output$investigate_vidxs <- renderUI(expr = {

        n <- as.integer(neighborhood(graph = g(),
                                     order = 10,
                                     nodes = current_vidx(g()),
                                     mode = 'out')[[1]])

        vertex_df(g = g()) %>%
            slice(n) %>%
            filter(!investigated) %>%
            pull(vidx) ->
            available

        selectInput(inputId = 'investigate_vidx',
                    label = NULL,
                    choices = available,
                    multiple = FALSE)
    })

    # based on investigation, add nodes to g
    observeEvent(
        eventExpr = input$investigate,
        handlerExpr = g(investigate(g = g(), vidx = input$investigate_vidx))
    )

    # based on investigation, deduct points
    observeEvent(
        eventExpr = input$investigate,
        handlerExpr = points(points() - cost_to_investigate(g = g(), vidx = input$investigate_vidx))
    )

    # render output
    output$the_plot <- renderPlot(plot_graph(g()))
    output$points_remaining <- renderText(points())
}

