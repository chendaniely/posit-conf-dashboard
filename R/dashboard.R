reveal_tmp_image <- function(
	gg,
	width = 1050 * 2,
	height = 550 * 2,
	dpi = 144,
	units = "px",
	dev = "svg",
	...
) {
	tmpf <- tempfile(fileext = paste0(".", dev))
	ggsave(tmpf, gg, width = width, height = height, dpi = dpi, units = units, ...)

	tmpf
	list(src = tmpf, width = "100%", height = "auto", class = "r-stretch")
}

plot_location <- function(responses) {
	world <- map_data("world")

	response_data <- responses |>
		filter(!is.na(attending)) |>
		mutate(across(c(lat, lon), \(x) round(x, 2))) |>
		count(lat, lon, attending = factor(attending)) |>
		arrange(n)

	gg <- response_data |>
		ggplot() +
		geom_map(
			data = world |> filter(region != "Antarctica"),
			map = world,
			aes(long, lat, map_id = region),
			color = "white",
			fill = "lightgray",
			linewidth = 0.1
		) +
		geom_jitter(
			aes(x = lon, y = lat, size = n, color = attending),
			alpha = 0.8
		) +
		coord_quickmap() +
		xlim(-155, 170) +
		ylim(-50, 80) +
		scale_size(range = c(2, 7), trans = scales::log2_trans()) +
		scale_color_manual(
			values = c("#72994D", "#419599")
		) +
		guides(
			size = FALSE,
			color = guide_legend(title = NULL, override.aes = list(size = 8))
		) +
		theme_void(getOption("base_font_size", 24)) +
		theme(
			plot.margin = margin(0, 0, 0, 0),
			legend.position = "bottom",
		)

	gg
}



plot_person <- function(responses){
	person_types <- c("Dog", "Cat", "Plant", "All", "None", "Other")

	gg <-
		responses |>
		tidyr::replace_na(list(type_of_person = "None")) |>
		mutate(type_of_person = factor(type_of_person, person_types)) |>
		count(type_of_person) |>
		mutate(n = n / sum(n)) |>
		ggplot() +
		aes(x = 1, y = n, fill = type_of_person) +
		geom_col(color = "white", size = 3) +
		coord_radial(
			theta = "y",
			start = 0.5 * pi,
			inner.radius = 0.1,
			direction = -1
		) +
		scale_y_continuous(expand = c(0, 0)) +
		scale_fill_manual(
			values = c(
				"#447098FF",
				"#419498FF",
				"#72984EFF",
				"#ED6331FF",
				"#994665FF",
				"#C1C1C3FF"
			)
		) +
		labs(fill = NULL) +
		theme_void(getOption("base_font_size", 24)) +
		theme(
			legend.key.size = unit(1.5, "cm")
		)

	gg
}

plot_careers <- function(responses) {
	role_types <- c(
		"Student",
		"Teacher",
		"Data Scientist",
		"Data Analyst",
		"Software Engineer",
		"Manager",
		"Other"
	)

	gg <-
		responses |>
		filter(!is.na(role)) |>
		mutate(role = factor(role, role_types)) |>
		ggplot() +
		aes(x = years_posit, y = years_industry, color = role) +
		geom_point(size = 5, show.legend = TRUE) +
		scale_y_continuous(
			name = "Years in data science",
			breaks = seq(0, 40, 10),
			limits = c(0, 40),
			expand = c(0, 0),
			labels = c("0", "10yrs", "20yrs", "30yrs", "40yrs")
		) +
		scale_x_continuous(
			name = "Years using Posit products",
			breaks = seq(2, 14, 4),
			limits = c(0, 14)
		) +
		scale_color_manual(
			drop = FALSE,
			labels = role_types,
			values = c(
				"#447098FF", # student
				"#F4C540FF", # teacher
				"#72984EFF", # data scientist
				"#ED6331FF", # data analyst
				"#994665FF", # software engineer
				"#419498FF", # manager
				"#C1C1C3FF"  # other
			),
			guide = guide_legend("", override.aes = list(size = 8))
		) +
		coord_cartesian(clip = "off") +
		theme(
			panel.grid.minor = element_blank(),
			legend.key.size = unit(1.5, "cm")
		)

	gg
}

plot_conf_over_years <- function(responses) {
	confs <- c(
		"2025 Atlanta",
		"2024 Seattle",
		"2023 Chicago",
		"2022 Washington D.C.",
		"2021 Virtual",
		"2020 San Francisco",
		"2019 Austin",
		"2018 San Diego",
		"2017 Orlando"
	)

	gg <-
		responses |>
		select(conf_attended) |>
		separate_rows(conf_attended, sep = ",\\s*") |>
		mutate(conf_attended = factor(conf_attended, rev(confs))) |>
		ggplot() +
		aes(x = conf_attended) +
		geom_bar(aes(fill = conf_attended), show.legend = FALSE, na.rm = FALSE) +
		scale_x_discrete(
			name = NULL,
			labels = function(x) gsub(" ", "\n", x),
			drop = FALSE,
		) +
		scale_fill_manual(
			values = c(
				"#94BDBFFF",
				"#213D4FFF",
				"#F6A294FF",
				"#72994EFF",
				"#447098FF",
				"#E7B10AFF",
				"#ED6331FF",
				"#994665FF",
				"#72984EFF"
			)
		) +
		scale_y_continuous(NULL, expand = c(0, 0)) +
		theme(
			panel.grid.minor = element_blank(),
			panel.grid.major.x = element_blank(),
			axis.text.x = element_text(size = getOption("base_font_size", 24) * 0.66),
		)

	gg
}


plot_word_cloud <- function(responses, var) {
	word_colors <- c(
		"#A67380FF",
		"#8AA67AFF",
		"#419498FF",
		"#447098FF",
		"#F4C540FF",
		"#ED6331FF",
		"#994665FF",
		"#72984EFF"
	)

	responses |>
		filter(!is.na({{ var }}), nzchar({{ var }})) |>
		count({{ var }}) |>
		mutate(
			color = factor(sample(word_colors, n(), replace = TRUE)),
			angle = 45 * sample(-1:2, n(), replace = TRUE, prob = c(1, 4, 1, 1))
		) |>
		ggplot() +
		aes(
			label = {{ var }},
			size = n,
			color = color
		) +
		geom_text_wordcloud(
			aes(angle = angle),
			#family = "source code pro",
			fontface = "bold",
			rm_outside = TRUE,
			shape = "circle",
			use_richtext = FALSE,
		) +
		scale_size_area(max_size = 48, trans = power_trans(1.5), expand = c(0, 0)) +
		scale_color_identity() +
		theme_void(
			getOption("base_font_size", 24),
			base_family = "source code pro"
		)
}
