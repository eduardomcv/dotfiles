local ok, vscode = pcall(require, 'vscode')
if not ok then return end

vscode.setup {}
