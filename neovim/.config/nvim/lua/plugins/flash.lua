return {
  "folke/flash.nvim",
  keys = {
    { "s", false },
    { "S", false },
    { "r", false },
    { "R", false },
    { "<c-s>", false },
    {
      "<F2>",
      mode = { "n", "x", "o" },
      function()
        require("flash").jump()
      end,
      desc = "Flash",
    },
    {
      "<F3>",
      mode = { "n", "o", "x" },
      function()
        require("flash").treesitter()
      end,
      desc = "Flash Treesitter",
    },
  },
}
