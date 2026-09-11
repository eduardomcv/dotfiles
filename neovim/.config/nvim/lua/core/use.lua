local M = {}
-- By default, use GitHub for plugins
local repository = "https://github.com/"

function M.setup(opts)
	opts = opts or {}
	local source = opts.repository or repository
	assert(type(source) == "string" and source ~= "", "use: repository must be a non-empty string")
	repository = source:gsub("/+$", "") .. "/"
end

local function add(specs, opts)
	local packages = {}
	local visiting = {}

	local function visit(spec)
		if type(spec) == "string" then
			spec = { spec }
		end

		assert(type(spec) == "table", "use: plugin spec must be a string or table")
		assert(not visiting[spec], "use: cyclic dependencies")

		local source = spec.url or spec[1]
		assert(type(source) == "string" and source ~= "", "use: plugin spec requires a name or url")

		visiting[spec] = true

		if spec.dependencies ~= nil then
			assert(type(spec.dependencies) == "table" and vim.islist(spec.dependencies), "use: dependencies must be a list")
			for _, dependency in ipairs(spec.dependencies) do
				visit(dependency)
			end
		end

		local package = {}
		for key, value in pairs(spec) do
			if key ~= 1 and key ~= "url" and key ~= "dependencies" then
				package[key] = value
			end
		end
		package.src = spec.url or (repository .. source)
		packages[#packages + 1] = package

		visiting[spec] = nil
	end

	assert(type(specs) == "table" and vim.islist(specs), "use: plugins must be a list")
	for _, spec in ipairs(specs) do
		visit(spec)
	end

	return vim.pack.add(packages, opts)
end

return setmetatable(M, {
	__call = function(_, specs, opts)
		return add(specs, opts)
	end,
})
