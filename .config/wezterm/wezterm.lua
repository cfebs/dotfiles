return {
	-- color_scheme = "Solarized Dark - Patched",
	-- color_scheme = "Catppuccin Mocha",
	-- color_scheme = "Catppuccin Mocha",
	-- color_scheme = "Dracula",
	-- color_scheme = "Dracula+",
	-- color_scheme = "duckbones",
	-- color_scheme = "duckbones",
	-- color_scheme = "Trim Yer Beard (terminal.sexy)",
	-- color_scheme = "Tomorrow Night Bright",
	color_scheme = "Monokai Remastered",

	-- TODO: starship doesn't run in login shell for some reason
	-- default_prog = { "/bin/bash", "-l" }
	default_prog = { "/bin/bash" },

	harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },

	hide_tab_bar_if_only_one_tab = true,

	show_new_tab_button_in_tab_bar = false,

	check_for_updates = false,
}
