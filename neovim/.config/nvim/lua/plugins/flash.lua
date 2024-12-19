return {
  "folke/flash.nvim",
  keys = {
    { "s", mode = { "n", "o", "x" }, false },
    { "S", mode = { "n", "o", "x" }, false },
    { "r", mode = "o", false },
    { "R", mode = { "o", "x" }, false },
    { "<c-s>", false },
    {
      "<F2>",
      mode = { "n", "o", "x" },
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
