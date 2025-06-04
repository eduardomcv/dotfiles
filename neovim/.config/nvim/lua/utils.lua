local M = {}

M.table_join = function(table_a, table_b)
	local new_table = {}

	for _, value in ipairs(table_a) do
		table.insert(new_table, value)
	end

	for _, value in ipairs(table_b) do
		table.insert(new_table, value)
	end

	return new_table
end

return M
