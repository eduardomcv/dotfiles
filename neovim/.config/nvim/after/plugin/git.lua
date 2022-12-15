local ok, git = pcall(require, 'git')
if not ok then return end

git.setup {}
