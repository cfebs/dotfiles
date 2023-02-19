return {
	-- TODO: starship doesn't run in login shell for some reason
	-- default_prog = { "/bin/bash", "-l" }
	default_prog = { os.getenv("SHELL") },

	harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },

	hide_tab_bar_if_only_one_tab = true,

	show_new_tab_button_in_tab_bar = false,

	check_for_updates = false,

	-- for i3 to keep window size correct
	adjust_window_size_when_changing_font_size = false,

	font_size = 11.0,

	color_scheme = "Builtin Solarized Dark",
	-- color_scheme = "Solarized Dark - Patched",
	-- color_scheme = "Solarized Dark (base16)",
	-- color_scheme = "Catppuccin Mocha",
	-- color_scheme = "Dracula",
	-- color_scheme = "Dracula+",
	-- color_scheme = "duckbones",
	-- color_scheme = "Trim Yer Beard (terminal.sexy)",
	-- color_scheme = "Tomorrow Night Bright",
	-- color_scheme = "Monokai Remastered",
}
