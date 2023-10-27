set.seed(45345)

fake_dat <- tibble::tibble(
  x = seq(-5, 5, length.out = 1000 ),
  y = x^3 + rnorm(1000, mean= 0, sd = 15)
)

point_plot <- fake_dat |> 
  ggplot2::ggplot(ggplot2::aes(x,y))+
  ggplot2::geom_point(col = "#0072B2", alpha = 0.15, size = 3)

point_plot

point_plot +
  ggplot2::stat_function(
    fun = \(x) x^3, #Put formula from model into function
    col = "#009E73",
    linewidth = 2.5,
    lineend = 'round'
  ) +
  ggplot2::labs(title = "any function can be plotted with stat_function()")

penguins <- palmerpenguins::penguins |>
  dplyr::filter(!is.na(sex))

grouped_penguins <- penguins|>
  dplyr::mutate(species = dplyr::if_else(species == 'Gentoo', species, 'Other'))

penguin_colors <- c("#E69F00", "#009E73", "#0072B2", "grey80")

names(penguin_colors) <- c(
  unique(penguins$species |> as.character()),
  'Others'
)

density_chart <- grouped_penguins|>
  ggplot2::ggplot()+
  ggplot2::geom_density(
    ggplot2::aes(x= body_mass_g, fill = species),
    alpha = 0.8,
    col = NA
  )+
  ggplot2::labs(
    x='Body weight (in g)',
    y=ggplot2::element_blank(),
    title = 'Penguin weights are somewhat normally distributed',
    fill = 'Species'
  )+
  ggplot2::scale_fill_manual(values = penguin_colors)+
  ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult = c(0,0.025)))
density_chart

params_grouped_penguins <- grouped_penguins|>
  dplyr::summarize(
    mean_body_mass_g = mean(body_mass_g),
    sd_body_mass = sd(body_mass_g),
    .by=species
  )

dnorm(seq(3500,3800,100), mean =3715, sd =436)

fun_layers <- params_grouped_penguins |>
  purrr::pmap(
    ~{
      #pass current parameters to new list
      l <- list(...)
      #use ist to put parameters into correct place in dnorm()
      ggplot2::stat_function(
        fun = \(x)  dnorm(
          x,
          mean = l$mean_body_mass_g,
          sd = l$sd_body_mass
        ),
        #add some aesthetics as well
        linewidth = 1,
        linetype =  if(l$species == 'Gentoo') 2 else 3
      )
    }
    
  )

density_chart+
  fun_layers


facet_plot <- penguins|>
  ggplot2::ggplot()+
  ggplot2::geom_density(
    ggplot2::aes(x = body_mass_g, fill = species),
    col =NA
  )+
  ggplot2::facet_wrap(ggplot2::vars(species), ncol = 1)+
  ggplot2::labs(
    x = 'Bdy weight (in g)',
    y = ggplot2::element_blank(),
    title = 'Penguin weights are somwhat normally distributed'
  )+
  ggplot2::scale_fill_manual(values = penguin_colors)+
  ggplot2::scale_y_continuous(
    breaks = c(1, 6e-4),
    labels = scales::label_comma(),
    expand = ggplot2::expansion(mult = c(0, 0.005))
  )+
  ggplot2::theme(legend.position = 'none')
facet_plot

params_penguins <- penguins |>
  dplyr::summarize(
    mean_body_mass_g = mean(body_mass_g),
    sd_body_mass_g = sd(body_mass_g),
    .by = species
  )

range_body_mass <- range(penguins$body_mass_g)

computed_vals <- params_penguins |>
  dplyr::mutate(
    x = list( #wrap vector in this list()) to save the whole vector into the cell
      seq(range_body_mass[1], range_body_mass[2],1)
    ),
    y = purrr::pmap(
      list(x = x , mean = mean_body_mass_g, sd = sd_body_mass_g),
      ~{
        l <- list(...)
        #compute manually here with dnorm()
        dnorm(l$x, mean = l$mean, sd = l$sd)
      }
    )
  ) |>
  dplyr::select(species, x, y)


coords_penguins <- computed_vals |>
  tidyr::unnest(cols = c(x,y))

facet_plot +
  ggplot2::geom_line(
    data = coords_penguins,
    ggplot2::aes( x = x, y = y),
    linetype = 2,
    linewidth = 1
  )
