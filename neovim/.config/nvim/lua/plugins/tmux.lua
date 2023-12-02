return {
  "alexghergh/nvim-tmux-navigation",
  keys = {
    {
      "<c-h>",
      function()
        require("nvim-tmux-navigation").NvimTmuxNavigateLeft()
      end,
      desc = "Go to left window (nvim or tmux)",
    },
    {
      "<c-j>",
      function()
        require("nvim-tmux-navigation").NvimTmuxNavigateDown()
      end,
      desc = "Go to lower window (nvim or tmux)",
    },
    {
      "<c-k>",
      function()
        require("nvim-tmux-navigation").NvimTmuxNavigateUp()
      end,
      desc = "Go to upper window (nvim or tmux)",
    },
    {
      "<c-l>",
      function()
        require("nvim-tmux-navigation").NvimTmuxNavigateRight()
      end,
      desc = "Go to right window (nvim or tmux)",
    },
  },
}
