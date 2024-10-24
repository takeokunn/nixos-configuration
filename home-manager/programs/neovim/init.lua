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
vim.opt.termguicolors = true

-- syntax

vim.cmd.syntax("on")

-- basic keymap

vim.g.mapleader = ','

vim.keymap.set('n', '/', '/\\v', { remap = false })
vim.keymap.set('n', 'U', '<C-r>', { remap = false })
vim.keymap.set('n', '<Leader><Leader>', 'V', { remap = false })
vim.keymap.set('n', '<Esc><Esc>', '<Cmd>nohlsearch<CR><Esc>', { remap = false })
vim.keymap.set('i', '<C-j>', '<CR>')

-- window keymap

vim.keymap.set('n', 'sj', '<C-w>j', { noremap = true, silent = true })
vim.keymap.set('n', 'sk', '<C-w>k', { noremap = true, silent = true })
vim.keymap.set('n', 'sl', '<C-w>l', { noremap = true, silent = true })
vim.keymap.set('n', 'sh', '<C-w>h', { noremap = true, silent = true })
vim.keymap.set('n', 'sJ', '<C-w>J', { noremap = true, silent = true })
vim.keymap.set('n', 'sK', '<C-w>K', { noremap = true, silent = true })
vim.keymap.set('n', 'sL', '<C-w>L', { noremap = true, silent = true })
vim.keymap.set('n', 'sH', '<C-w>H', { noremap = true, silent = true })
vim.keymap.set('n', 'sw', '<C-w>w', { noremap = true, silent = true })

-- buffer keymap

vim.keymap.set('n', 'sp', ':<C-u>bp<CR>', { noremap = true, silent = true })
vim.keymap.set('n', 'sn', ':<C-u>bn<CR>', { noremap = true, silent = true })
