vim.opt.encoding = "utf-8"
vim.opt.fileencodings = "utf-8,euc-jp,cp932"
vim.opt.clipboard:append('unnamed')
vim.opt.backspace = "indent,eol,start"
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.laststatus = 2
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

vim.keymap.set('n', '/', '/\\v', { noremap = true })
vim.keymap.set('n', '<Leader><Leader>', 'V', { noremap = true })
vim.keymap.set('n', '<Esc><Esc>', '<Cmd>nohlsearch<CR><Esc>', { noremap = true })

vim.cmd.syntax("on")
