library(shiny)
library(tidyverse)
library(igraph)

GOAL_X1 <- 6
GOAL_X2 <- 12
GOAL_Y1 <- -5
GOAL_Y2 <- 5

# Generate `n` consecutive line breaks in the UI
brs <- function(n = 1) {
    lapply(seq(n), function(i) shiny::tags$br())
}

vertex_df <- function(graph) {
    as_data_frame(x = graph, what = 'vertices') %>%
        mutate(vidx = seq(n()))
}

edge_df <- function(graph) {
    as_data_frame(x = graph, what = 'edges')
}

current_vidx <- function(graph) {
    res <- which(vertex_df(graph)$current)
    if (length(res) != 1L) {
        stop('More than one vertex is labelled current.')
    }
    return(res)
}

has_won <- function(graph) {
    all(
        between(x = vertex_df(graph)$x[current_vidx(graph)], left = GOAL_X1, right = GOAL_X2),
        between(x = vertex_df(graph)$y[current_vidx(graph)], left = GOAL_Y1, right = GOAL_Y2)
    )
}

investigate <- function(graph, vidx) {

    if (!(vidx %in% V(graph))) {
        stop('Tried to add children to absent parent.')
    }

    x <- vertex_attr(graph = graph, name = 'x', index = vidx)
    y <- vertex_attr(graph = graph, name = 'y', index = vidx)
    depth <- vertex_attr(graph = graph, name = 'depth', index = vidx)

    num_children <- sample(x = 1:5, size = 1)
    new_spots <- simulate_spots(n = num_children, x = x, y = y)
    graph %>%
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
    repeat {
        angle <- sort(runif(n = n, min = 0, max = 2*pi))
        if (all(diff(angle) > 0.3)) break
    }
    r <- runif(n = n, min = 0.5, max = 3)
    list(
        x = x + r*sin(angle),
        y = y + r*cos(angle)
    )
}

cost_to_investigate <- function(graph, vidx) {
    vdf <- vertex_df(graph)
    cost <- vdf$depth[as.integer(vidx)] - vdf$depth[vdf$current] + 1L
    cost
}

move <- function(graph, to_vidx) {

    if (!(to_vidx %in% V(graph))) {
        stop('Tried to move to a non-existent node.')
    }

    from_vidx <- current_vidx(graph)

    # check for edge between from_vertex and to_vertex
    if (nrow(filter(edge_df(graph = graph), from == from_vidx, to == to_vidx)) != 1L) {
        stop('Error -- no edge between from_vertex and to_vertex')
    }

    # if we got this far, go ahead and make the 'move'
    graph %>%
        set_vertex_attr(name = 'current', index = from_vidx, value = FALSE) %>%
        set_vertex_attr(name = 'current', index = to_vidx, value = TRUE)
}

plot_graph <- function(graph) {

    vs <- as_data_frame(x = graph, what = 'vertices') %>%
        mutate(vidx = as.integer(V(graph)))

    current_depth <- vs$depth[current_vidx(graph)]

    es <- as_data_frame(x = graph, what = 'edges') %>%
        mutate(
            from_x = vs$x[from],
            to_x = vs$x[to],
            from_y = vs$y[from],
            to_y = vs$y[to]
        )

    ggplot(data = vs) +
        annotate(
            geom = 'rect',
            xmin = GOAL_X1, xmax = GOAL_X2, ymin = GOAL_Y1, ymax = GOAL_Y2,
            color = 'black', fill = 'gray'
        ) +
        annotate(
            geom = 'text', x = 11, y = 0, label = 'GOAL', angle = -90,
            size = 20, color = 'red'
        ) +
        geom_segment(
            data = es,
            mapping = aes(x = from_x, xend = to_x, y = from_y, yend = to_y)
        ) +
        geom_label(
            mapping = aes(x = x, y = y, label = vidx, fill = depth), alpha = 1
        ) +
        annotate(
            geom = 'segment',
            x = vs$x[current_vidx(graph)],
            y = vs$y[current_vidx(graph)] + 0.6,
            xend = vs$x[current_vidx(graph)],
            yend = vs$y[current_vidx(graph)] + 0.2,
            size = 3,
            lineend = 'butt',
            linejoin = 'mitre',
            arrow = arrow(length = unit(12, 'pt'), type = 'closed')
        ) +
        scale_fill_gradient2(
            low = 'blue', mid = 'white', high = 'red', midpoint = current_depth
        ) +
        guides(fill = FALSE, size = FALSE) +
        theme_void() +
        theme(plot.margin = margin(0, 0, 0, 0, 'pt'))
}
