## =====================================================================
## Noronha project functions ===========================================
## authors: henrique laureano
## authors: ...
## date: 2020-3-17
## =====================================================================

## =====================================================================
## libraries

pack <- c("tidyverse", "DT", "INLA", "viridis", "patchwork", "tictoc",
          "kableExtra", "scales")

pacman::p_load(pack, character.only = TRUE)

## =====================================================================
## homeperformance: latent effects posterior mean plots with credibility
##                  intervals
## args:
## -- teams: character, vector with all team names;
## -- ploteam: character, vector with the teams to be plotted;
## -- n_rounds: scalar, number of time points;
## -- mod: inla object, model.
## return:
## -- ggplot2 graphic.

homeperformance <- function(teams, ploteam, n_rounds, mod) {
    f_mean  <- data.frame(sapply(
        ploteam,
        function(i) mod$summary.random$round_home$
                    mean[inla.idx(seq(n_rounds),
                                  replicate = which(teams == i),
                                  nrep = length(teams)
                                  )]))
    f_mean$type <- rep("mean", n_rounds)
    f_lower  <- data.frame(sapply(
        ploteam,
        function(i) mod$summary.random$round_home$
                    `0.025quant`[inla.idx(1:n_rounds,
                                          replicate = which(teams == i),
                                          nrep = length(teams)
                                          )]))
    f_lower$type <- rep("lower", n_rounds)
    f_upper  <- data.frame(sapply(
        ploteam,
        function(i) mod$summary.random$round_home$
                    `0.975quant`[inla.idx(1:n_rounds,
                                          replicate = which(teams == i),
                                          nrep = length(teams)
                                          )]))
    f_upper$type <- rep("upper", n_rounds)
    dat_plot <- rbind(f_mean, f_lower, f_upper)
    dat_plot$round <- rep(seq(n_rounds), 3)
    dat_plot <- dat_plot %>%
        gather(key = "team", value = "value", - round, - type)
    dat_plot <- spread(dat_plot, type, value)
    dat_plot$team <- factor(dat_plot$team, levels = ploteam)
    ggplot(dat_plot, aes(x = round, group = team)) +
        geom_line(aes(y = mean, color = team), size = 1.5) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = team),
                    alpha = .15) +
        labs(x = "rounds", y = "Posterior mean") +
        scale_color_viridis(discrete = TRUE) +
        theme_minimal()
}

## =====================================================================
## plot_home.adv: plot of the marginal distribution of the home
##                advantage fixed effect
## args:
## -- model: inla object, model.
## return:
## -- ggplot2 graphic.

plot_home.adv <- function(model) {
    df = data.frame(model$marginals.fixed$home.adv)
    ggplot(df, aes(x = x)) +
        geom_density(adjust = 3, size = 1) +
        scale_x_continuous(name = "Home Advantage") +
        scale_y_continuous(name = "Density") +
        theme_minimal()
}

## =====================================================================
## ranks: league final rank, that compares the observed final rank with
##        the one predictd by the model
## args:
## -- teams: character, vector with all team names;
## -- mod: inla object, model;
## -- n_rounds: scalar, number of time points;
## -- data: dataset.
## return:
## -- list of three slots, a matrix with the ranks and two vector with
##    the team indexes - observed and predicted.

ranks <- function(teams, mod, n_rounds, data) {
    teams_mean <- sapply(
        teams,
        function(i)
            mean(mod$summary.random$round_home$
                 mean[inla.idx(1:n_rounds,
                               replicate = which(teams == i),
                               nrep = length(teams))]))
    performance <- data.frame(cbind(teams, teams_mean))
    names(performance) <- c("Team", "Performance")
    performance$Performance <-
        as.numeric(as.character(performance$Performance))
    index_order <- order(performance$Performance, decreasing = TRUE)
    performance <- performance[index_order, ]
    rownames(performance) <- NULL
    team_points <- numeric(n_teams)
    names(team_points) <- teams
    for (i in 1:nrow(data)) {
        home = data[i, "home"]; x = data[i, "x"]
        away = data[i, "away"]; y = data[i, "y"]
        team_points[c(home, away)] <- team_points[c(home, away)] +
            if (x > y) c(3, 0) else
            if (y > x) c(0, 3) else c(1, 1)
    }
    table_points <- cbind(sort(team_points, decreasing = TRUE))
    index_true <- order(team_points, decreasing = TRUE)
    compare <- cbind(as.character(performance$Team),
                     names(team_points)[index_true],
                     team_points[index_true])
    colnames(compare) <- c("Rank by performance", "Real rank", "Points")
    rownames(compare) <- 1:20
    return(list("rank" = compare,
                "index_order" = index_order,
                "index_true" = index_true))
}

## =====================================================================
## plot_teams: mean or median kindoff barplot with a credibility
##             interval for the team performances
## args:
## -- mod: inla object, model;
## -- teams: character, vector with all team names;
## -- index_order: character, vector with ranked teams names;
## -- n_rounds: scalar, number of time points;
## -- graph type: 1, graphics; 2, ggplot2.
## return:
## -- graph, a plot or a ggplot2.

plot_teams <- function(mod, teams, index_order, n_rounds, type) {
    teams <- factor(teams)
    orderedteams <- factor(teams, levels(teams)[index_order])
    teams_0.025 <- sapply(
        teams,
        function(i) mean(mod$summary.random$round_home$
                         `0.025quant`[inla.idx(
                             1:n_rounds,
                             replicate = which(teams == i),
                             nrep = length(teams))]))
    teams_0.5 <- sapply(
        teams,
        function(i) mean(mod$summary.random$round_home$
                         `0.5quant`[inla.idx(
                             1:n_rounds,
                             replicate = which(teams == i),
                             nrep = length(teams))]))
    teams_0.975 <- sapply(
        teams,
        function(i) mean(mod$summary.random$round_home$
                         `0.975quant`[inla.idx(
                             1:n_rounds,
                             replicate = which(teams == i),
                             nrep = length(teams))]))
    if (type == 1) {
        plot(orderedteams[index_order], teams_0.5[index_order],
             las = 2, xlab = NULL, ylab = "Performance",
             ylim = c(-.8, 1), main = "Team performances")
        points(orderedteams[index_order],
               teams_0.025[index_order], pch = 2, cex = .75)
        points(orderedteams[index_order],
               teams_0.975[index_order], pch = 6, cex = .75)
    } else {
        df <- data.frame(variable = teams[index_order],
                         color = seq(n_teams),
                         median = teams_0.5[index_order],
                         start = teams_0.025[index_order],
                         end = teams_0.975[index_order])
        ggplot(df, aes(ymin = start, ymax = end, x = variable)) +
            geom_linerange(aes(color = color),
                           position = position_dodge(width = .2),
                           size = 3,
                           show.legend = FALSE) +
            coord_flip() +
            geom_point(mapping = aes(x = variable, y = median)) +
            labs(x = NULL, y = "Posterior") +
            scale_color_viridis() +
            theme_minimal()
    }
}

## =====================================================================
## criteria_eval: goodness-of-fit and comparison criterions for model
##                selection
## args:
## -- mod: inla object, model.
## return:
## -- vector with three measures: CPO, DIC and WAIC.

criteria_eval <- function(mod) {
    stopifnot(class(mod) == 'inla')
    return(c(-sum(log(mod$cpo$cpo)), mod$dic$dic, mod$waic$waic))
}

## =====================================================================
## pit_plot: probability integral transform plot
## args:
## -- mod: inla object, model;
## -- name: character, response type.
## return:
## -- ggplot2 graphic.

pit_plot <- function(mod, name){
    stopifnot(class(mod) == 'inla')
    if (name == 'Poisson') pit <- mod$cpo$pit - .5*mod$cpo$cpo
    else pit <- mod$cpo$pit
    ggplot() +
        geom_histogram(aes(x = pit, y = ..density..), bins = 20,
                       color = "black", fill = "white") +
        labs(x = "PIT values", y = "Density") +
        theme_minimal()
}

## =====================================================================
## table_producer: performing N season simulations and returning it
##                 final ranks
## args:
## -- teams: character, vector with all team names;
## -- mod: inla object, model;
## -- N: number of seasons to be simulated;
## -- data: dataset.
## results:
## -- n_teams by n_teams matrix with the number of times that each team
## finished at each position

table_producer <- function(teams, mod, N, data) {
    n_teams <- length(teams)
    out <- matrix(0, nrow = n_teams, ncol = n_teams)
    rownames(out) <- teams
    colnames(out) <- seq(n_teams)
    SAMPLE <- inla.posterior.sample(n = N, mod)
    for (i in seq(N)) {
        sample <- SAMPLE[[i]]$latent[seq(nrow(data) * 2)]
        lambda <- exp(sample)
        goals <- rpois(length(lambda), lambda = lambda)
        new_season <- data.frame("home" = dat$home, "away" = dat$away,
                                 "x" = goals[2 * seq(nrow(data)) - 1],
                                 "y" = goals[2 * seq(nrow(data))])
        homescore <-
            (new_season$x > new_season$y) * 3 +
            (new_season$x == new_season$y)
        awayscore <-
            (new_season$x < new_season$y) * 3 +
            (new_season$x == new_season$y)
        homescore <- aggregate(homescore, by = list(new_season$home),
                               FUN = sum)
        awayscore <- aggregate(awayscore, by = list(new_season$away),
                               FUN = sum)
        num_points = homescore$x + awayscore$x
        rankorder = rank(-num_points, ties.method = "random")
        for (j in seq(n_teams)) {
            out[j, rankorder[j]] <- out[j, rankorder[j]] + 1
        }
  }
    return(out)
}

## =====================================================================
## table_plot: kable version of the matrix output from `table_producer`
## args:
## -- out: the matrix output from `table_producer`;
## -- colindex: logical vector indicating the cell color.
## results:
## -- kable table.

table_plot <- function(out, colindex){
    out %>%
        mutate(
            "Team" = row.names(.),
            "1" = cell_spec(out[ , 1], "html",
                            color = ifelse(colindex[ , 1], "red", "blue")),
            "2" = cell_spec(out[ , 2], "html",
                            color = ifelse(colindex[ , 2], "red", "blue")),
            "3" = cell_spec(out[ , 3], "html",
                            color = ifelse(colindex[ , 3], "red", "blue")),
            "4" = cell_spec(out[ , 4], "html",
                            color = ifelse(colindex[ , 4], "red", "blue")),
            "5" = cell_spec(out[ , 5], "html",
                            color = ifelse(colindex[ , 5], "red", "blue")),
            "6" = cell_spec(out[ , 6], "html",
                            color = ifelse(colindex[ , 6], "red", "blue")),
            "7" = cell_spec(out[ , 7], "html",
                            color = ifelse(colindex[ , 7], "red", "blue")),
            "8" = cell_spec(out[ , 8], "html",
                            color = ifelse(colindex[ , 8], "red", "blue")),
            "9" = cell_spec(out[ , 9], "html",
                            color = ifelse(colindex[ , 9], "red", "blue")),
            "10" = cell_spec(out[ , 10], "html",
                             color = ifelse(colindex[ , 10], "red", "blue")),
            "11" = cell_spec(out[ , 11], "html",
                             color = ifelse(colindex[ , 11], "red", "blue")),
            "12" = cell_spec(out[ , 12], "html",
                             color = ifelse(colindex[ , 12], "red", "blue")),
            "13" = cell_spec(out[ , 13], "html",
                             color = ifelse(colindex[ , 13], "red", "blue")),
            "14" = cell_spec(out[ , 14], "html",
                             color = ifelse(colindex[ , 14], "red", "blue")),
            "15" = cell_spec(out[ , 15], "html",
                             color = ifelse(colindex[ , 15], "red", "blue")),
            "16" = cell_spec(out[ , 16], "html",
                             color = ifelse(colindex[ , 16], "red", "blue")),
            "17" = cell_spec(out[ , 17], "html",
                             color = ifelse(colindex[ , 17], "red", "blue")),
            "18" = cell_spec(out[ , 18], "html",
                             color = ifelse(colindex[ , 18], "red", "blue")),
            "19" = cell_spec(out[ , 19], "html",
                             color = ifelse(colindex[ , 19], "red", "blue")),
            "20" = cell_spec(out[ , 20], "html",
                             color = ifelse(colindex[ , 20], "red", "blue"))
        ) %>%
        select("Team", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
               "11", "12", "13", "14", "15",
               "16", "17", "18", "19", "20") %>%
        kable(format = "html", escape = FALSE) %>%
        kable_styling("striped", full_width = FALSE)
}
