-- Window rules

o.window({ class = "steam" }, {
	float = true,
})

-- Let Steam control its own size
o.window({ class = "steam", title = "^Steam$" }, {
	center = false,
	size = "",
})
