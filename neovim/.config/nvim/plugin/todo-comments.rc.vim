
lua << EOF
local match_pattern = [[.*<(KEYWORDS)\s*]]
require("todo-comments").setup {
    -- keywords recognized as todo comments
    keywords = {
        DELETEME = {
            icon = "",
            color = "#ff0000",
        },
        FIXME = {
            icon = "",
            color = "#fff400",
        },
        HACK = {
            icon = "",
            color = "#00f9ff",
        },
        TODO = {
            icon = "",
            color = "#ed00ff",
        },
        WIP = {
            icon = "",
            color = "#ff7C00",
        }
    },
    merge_keywords = false,
    highlight = {
        before = "",
        keyword = "wide",
        after = "fg",
        pattern = match_pattern,
    },
    search = {
        pattern = match_pattern,
    }
}
EOF
