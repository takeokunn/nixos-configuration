-- basic

vim.opt.encoding = "utf-8"
vim.opt.fileencodings = "utf-8,euc-jp,cp932"
vim.opt.clipboard:append('unnamed')
vim.opt.backspace = "indent,eol,start"
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.laststatus = 3
vim.opt.statusline = "%y"
vim.opt.showmatch = true
vim.opt.wrapscan = true
vim.opt.hlsearch = true
vim.opt.showcmd = true
vim.opt.title = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.foldenable = false
vim.opt.swapfile = false
vim.opt.expandtab = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.cmd.syntax("on")

-- keymap

vim.keymap.set('n', '/', '/\\v', { remap = false })
vim.keymap.set('n', '<Leader><Leader>', 'V', { remap = false })
vim.keymap.set('n', '<Esc><Esc>', '<Cmd>nohlsearch<CR><Esc>', { remap = false })

-- theme

vim.cmd[[colorscheme dracula]]

require('lualine').setup {
  options = {
    icons_enabled = false,
    section_separators = '',
    component_separators = ''
  }
}

-- cursor

require'hop'.setup { }
vim.keymap.set('n', 's', '<Cmd>HopChar2<CR>')