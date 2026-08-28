----------------------------------
---------------- CONFIGURE OPTIONS
----------------------------------

-- turn off intro message
vim.opt.shortmess:append("I")

-- turn off automatic backup
vim.opt.backup = false

-- turn off hidden swap files
vim.opt.swapfile = false

-- when (vertically) splitting, open new window to the right
vim.opt.splitright = true

-- show existing tab with N spaces width
vim.opt.tabstop = 4

-- when backspacing, delete N spaces width
vim.opt.softtabstop = 4

-- when indenting with '<' or '>', use N spaces width
vim.opt.shiftwidth = 4

-- insert spaces instead of tabs
vim.opt.expandtab = true

-- make tab key insert shiftwidth amount of spaces
vim.opt.smarttab = true

-- make join commands insert one space, not two, after '.' '?' or '!'
vim.opt.joinspaces = false

-- when formatting, set text width to N
vim.opt.textwidth = 78

-- show sign column
vim.opt.signcolumn = "yes"

-- turn off line wrapping
vim.opt.wrap = false

-- turn on hybrid mode line numbers
vim.opt.number = true          -- turn on line numbers
vim.opt.relativenumber = true  -- turn on relative line numbers

-- highlight cursor's line
vim.opt.cursorline = true

-- display invisible characters
vim.opt.list = true

-- use these characters to display invisible characters
vim.opt.listchars = { tab = "→ ", trail = "•", precedes = "⟨", extends = "⟩" }

-- enable mouse
vim.opt.mouse = "a"

-- turn off folding for all files by default
vim.opt.foldenable = false

-- turn on case insensitivity unless caps are present
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- turn off completion scanning current/included files
vim.opt.complete:remove("i")

-- make the omni-completion pop-up menu 1) pop up, 2) even on only one match 3) and do not auto-select any item
vim.opt.completeopt = { "menu", "menuone", "noselect" }

-- enhance command completion
vim.opt.wildmode = "full"
vim.opt.wildignorecase = true

-- set mapping delay
vim.opt.timeoutlen = 420

-- allow yanking to system clipboard
-- sudo apt install wl-clipboard # wayland
-- sudo apt install xclip xsel   # x11 / xorg
vim.opt.clipboard = "unnamedplus"

----------------------------------------
---------------- CONFIGURE CORE MAPPINGS
----------------------------------------

-- clear highlights on search when pressing <Esc> in normal mode
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

vim.keymap.set('n', '<Leader>v', function() vim.cmd.tabnew('$MYVIMRC') end, { desc = 'open init.lua in new tab' })
vim.keymap.set('n', '<Leader>t', function() vim.cmd.tabnew() end, { desc = 'open empty new tab' })
vim.keymap.set('n', '<Leader>w', function() vim.cmd.write() end, { desc = 'save buffer' })
vim.keymap.set('n', '<Tab>', function() pcall(vim.cmd.buffer, '#') end, { desc = "switch to the 'alternate file'" }) -- we use pcall to avoid an ignore errors when there is no 'alternate file' buffer
vim.keymap.set('n', '<Leader>q', function() vim.cmd.quit() end, { desc = 'close window or tab' })
vim.keymap.set('n', '<Leader>qq', function() vim.cmd.quitall({ bang = true }) end, { desc = 'close every buffer' })
vim.keymap.set('n', "'", '`', { desc = 'mark position (exact character cursor mark)' })
vim.keymap.set('n', 'Q', 'gqip', { desc = 'format paragraph' })
vim.keymap.set('v', 'Q', 'gq', { desc = 'format visual selection' })
vim.keymap.set('v', '>', '>gv', { desc = 'indent right and keep selection' })
vim.keymap.set('v', '<', '<gv', { desc = 'indent left and keep selection' })
vim.keymap.set('n', '<F5>', function() vim.cmd.source('$MYVIMRC') end, { desc = 'reload vimrc' })
vim.keymap.set('n', '<Leader>r', function() vim.cmd('botright terminal') end, { desc = 'open terminal (bottom right)' })
vim.keymap.set('n', '<Leader>rr', function() vim.cmd('tab terminal') end, { desc = 'open terminal in new tab' })
vim.keymap.set('t', '<Esc><Esc>', [[<C-\><C-n>]], { desc = 'enter normal mode from terminal' })
vim.keymap.set('t', '<C-PageUp>', vim.cmd.tabprev, { desc = 'previous tab from terminal' })
vim.keymap.set('t', '<C-PageDown>', vim.cmd.tabnext, { desc = 'next tab from terminal' })
vim.keymap.set('n', '[t', 'gT', { desc = 'previous tab' })
vim.keymap.set('n', ']t', 'gt', { desc = 'next tab' })
vim.keymap.set('n', '<C-S-PageUp>', function() vim.cmd('tabmove -1') end, { desc = 'Move tab left' })
vim.keymap.set('n', '<C-S-PageDown>', function() vim.cmd('tabmove +1') end, { desc = 'Move tab right' })
vim.keymap.set('i', '<C-S-PageUp>', function() vim.cmd('stopinsert | tabmove -1') end, { desc = 'Move tab left' })
vim.keymap.set('i', '<C-S-PageDown>', function() vim.cmd('stopinsert | tabmove +1') end, { desc = 'Move tab right' })
vim.keymap.set('t', '<C-S-PageUp>', function() vim.cmd('tabmove -1') end, { desc = 'Move tab left from terminal' })
vim.keymap.set('t', '<C-S-PageDown>', function() vim.cmd('tabmove +1') end, { desc = 'Move tab right from terminal' })

-- quickfix list
function wrapping_cnext()
    local success, _ = pcall(vim.cmd.cnext)
    if not success then
        pcall(vim.cmd.cfirst)
    end
end
vim.keymap.set('n', ']q', wrapping_cnext, { desc = 'next quickfix item (wraps around)' })

function wrapping_cprev()
    local success, _ = pcall(vim.cmd.cprev)
    if not success then
        pcall(vim.cmd.clast)
    end
end
vim.keymap.set('n', '[q', wrapping_cprev, { desc = 'previous quickfix item (wraps around)' })

-- function toggle_quickfix_list()
--     local qf_open = false
--     for _, win in ipairs(vim.fn.getwininfo()) do
--         if win.quickfix == 1 then
--             qf_open = true
--             break
--         end
--     end
--     if qf_open then
--         -- count how many windows are open in the current tab
--         local current_tab_wins = vim.api.nvim_tabpage_list_wins(0)
--
--         if #current_tab_wins <= 1 then
--             -- if the quickfix list is the last window, open a standard buffer first to safely close the quickfix
--             vim.cmd('enew')
--         else
--             vim.cmd('cclose')
--         end
--     else
--         -- open quickfix list, but safely catch errors if the list is empty
--         local success, _ = pcall(vim.cmd, 'copen')
--         if not success then
--             vim.notify("quickfix list is empty!", vim.log.levels.INFO)
--         end
--     end
-- end
-- vim.keymap.set('n', '<leader>q', toggle_quickfix_list, { desc = 'toggle quickfix list' })

-- spellchecking
local function toggle_spellcheck()
    vim.opt.spell = not vim.opt.spell:get()
    if vim.opt.spell:get() then
        print('spellcheck on')
        vim.keymap.set('n', 's', '1z=', { buffer = true })
    else
        print('spellcheck off')
        vim.keymap.del('n', 's', { buffer = true })
    end
end
vim.keymap.set('n', '<leader>s', toggle_spellcheck, { desc = 'toggle spellcheck' })

--- super tab completion mechanics

--- determine if a valid keyword is before the cursor
local has_words_before = function()
    unpack = unpack or table.unpack
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

--- map <Tab> to cycle to the next pop up menu item or natively trigger LSP completion
vim.keymap.set("i", "<Tab>", function()
    -- if the pop up menu is visible
    if vim.fn.pumvisible() == 1 then
        -- move to next menu item
        return "<C-n>"
    -- if it's not visible but there are words before
    elseif has_words_before() then
        -- trigger the completion engine
        vim.lsp.completion.trigger()
        -- return empty string (so no extra text leaks into the buffer)
        return ""
    -- else it's not visible and there are not words before
    else
        -- return normal <Tab>
        return "<Tab>"
    end
end, { expr = true, replace_keycodes = true, desc = "Super-Tab Navigation" })

--- map <S-Tab> to cycle to the previous pop up menu item or natively trigger the LSP completion
vim.keymap.set("i", "<S-Tab>", function()
    -- if the pop up menu is visible
    if vim.fn.pumvisible() == 1 then
        -- move to previous menu item
        return "<C-p>"
    -- else it's not visible
    else
        -- return normal <S-Tab>
        return "<S-Tab>"
    end
end, { expr = true, replace_keycodes = true })

--- map <CR> to choose a pop up menu item
vim.keymap.set("i", "<CR>", function()
    -- if the pop up menu is visible
    if vim.fn.pumvisible() == 1 then
        -- choose pop up menu item
        return "<C-y>"
    -- else it's not visible
    else
        -- return a normal <CR>
        return "<CR>"
    end
end, { expr = true, replace_keycodes = true })

---------------------------------------
---------------- CONFIGURE AUTOCOMMANDS
---------------------------------------

-- when markdown is detected
local markdown_group = vim.api.nvim_create_augroup("filetype_markdown", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
    group = markdown_group,
    pattern = "markdown",
    callback = function()
        -- when text is inserted or deleted, automatically format it
        vim.opt_local.formatoptions:append("a")
        -- when formatting, preserve list indentation (autoindent option must be on)
        vim.opt_local.formatoptions:append("n")
        vim.opt_local.autoindent = true
        -- make whitespace continue paragraph
        -- https://stackoverflow.com/a/21610187
        vim.opt_local.formatoptions:append("w")
    end,
})

-- when a language server attaches
vim.api.nvim_create_autocmd('LspAttach', {
    -- add these mappings
    callback = function(args)
        local opts = { buffer = args.buf }
        -- go to definition
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        -- go to implementation
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        -- see docs
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    end,
})

-- activate specific language server processors

-- C/C++
-- installation
--     sudo apt install -y clangd build-essential
-- useful addition to CMakeLists.txt
--     set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
vim.lsp.enable('clangd')

-- Python
-- installation
--     sudo apt install -y nodejs npm
--     sudo npm install -g pyright
vim.lsp.enable('pyright')

----------------------------------
---------------- CONFIGURE PLUGINS
----------------------------------

-- change surrounding delimiters
vim.pack.add({ 'https://github.com/tpope/vim-surround' })

-- repeat actions (only necessary for tpope plugins)
vim.pack.add({ 'https://github.com/tpope/vim-repeat' })

-- wrap git
vim.pack.add({ 'https://github.com/lewis6991/gitsigns.nvim' })
require('gitsigns').setup({
    -- display git actions with these characters
    signs = {
        add = { text = '+' }, ---@diagnostic disable-line: missing-fields
        change = { text = '~' }, ---@diagnostic disable-line: missing-fields
        delete = { text = '_' }, ---@diagnostic disable-line: missing-fields
        topdelete = { text = '‾' }, ---@diagnostic disable-line: missing-fields
        changedelete = { text = '~' }, ---@diagnostic disable-line: missing-fields
    },
    -- create (non-global) mappings for applicable buffers
    on_attach = function(bufnr)
        -- next git diff hunk
        vim.keymap.set(
            'n',
            ']c',
            function() return vim.wo.diff and ']c' or '<cmd>Gitsigns nav_hunk next<CR>' end,
            { expr = true, buffer = bufnr, desc = 'next diff hunk' })

        -- previous git diff hunk
        vim.keymap.set(
            'n',
            '[c',
            function() return vim.wo.diff and '[c' or '<cmd>Gitsigns nav_hunk prev<CR>' end,
            { expr = true, buffer = bufnr, desc = 'Prev diff hunk' })

        -- show git blame
        vim.keymap.set('n', 'gb', require('gitsigns').blame, { buffer = bufnr, desc = 'git blame' })
    end
})

-- remove distractions
vim.pack.add({ 'https://github.com/folke/zen-mode.nvim' })
require('zen-mode').setup({
    window = {
        width = 0.85,
        height = 0.75
    }
})
vim.keymap.set('n', '<leader>y', function() require("zen-mode").toggle() end, { desc = 'toggle zen mode' })

-- enhance statusline
vim.pack.add({ 'https://github.com/nvim-mini/mini.statusline' })
require('mini.statusline').setup({ use_icons = false, })

-- enhance file management
vim.pack.add({ 'https://github.com/stevearc/oil.nvim' })
require('oil').setup({})

-- enhance window management (window moving and resizing)
vim.pack.add({ 'https://github.com/mrjones2014/smart-splits.nvim'})
require('smart-splits').setup({ default_amount = 4, })
-- create mappings for window focus and resizing
local smart_splits = require('smart-splits')
local directions = {
    Up = { split = smart_splits.move_cursor_up, resize = smart_splits.resize_up },
    Down = { split = smart_splits.move_cursor_down, resize = smart_splits.resize_down },
    Left = { split = smart_splits.move_cursor_left, resize = smart_splits.resize_left },
    Right = { split = smart_splits.move_cursor_right, resize = smart_splits.resize_right },
}
for dir, action in pairs(directions) do
    -- move focus: Shift + Arrow keys (Normal, Insert, Terminal)
    vim.keymap.set({ 'n', 't' }, '<S-' .. dir .. '>', action.split, { silent = true, desc = 'move focus ' .. dir:lower() })
    vim.keymap.set('i', '<S-' .. dir .. '>', function()
        -- temporarily escape insert mode to run the split command, then return to insert mode
        action.split()
        vim.cmd('startinsert')
    end, { silent = true, desc = 'move focus ' .. dir:lower() })

    -- resize windows: Ctrl + Shift + Arrow keys (Normal, Insert, Terminal)
    vim.keymap.set({ 'n', 't' }, '<C-S-' .. dir .. '>', action.resize, { silent = true, desc = 'resize window ' .. dir:lower() })
    vim.keymap.set('i', '<C-S-' .. dir .. '>', function()
        action.resize()
    end, { silent = true, desc = 'resize window ' .. dir:lower() })
end

-- fuzzy finding in neovim (mimics FZF)
vim.pack.add({ 'https://github.com/ibhagwan/fzf-lua' })
require('fzf-lua').setup({
    winopts = {
        height     = 1.00,              -- full height
        width      = 1.00,              -- full width
        row        = 0.00,              -- start at the very top
        col        = 0.00,              -- start at the very left
        preview = {
            layout   = 'vertical',      -- 'vertical' splits the window horizontally (stacking them)
            vertical = 'up:50%',        -- places the preview on top, taking up 50% of the screen
        },
    },
})
vim.keymap.set('n', '<leader>p', function() require('fzf-lua').files() end, { desc = 'fzf files' })
vim.keymap.set('n', '<leader>b', function() require('fzf-lua').buffers() end, { desc = 'fzf buffers' })
vim.keymap.set('n', '<leader>g', function() require('fzf-lua').live_grep() end, { desc = 'fzf live grep' })
vim.keymap.set('n', 'gh', function() require('fzf-lua').grep_cword() end, { desc = 'fzf grep current word' })
vim.keymap.set('n', '<leader>h', function() require('fzf-lua').help_tags() end, { desc = 'fzf help tags' })
vim.keymap.set('n', '<leader>c', function() require('fzf-lua').colorschemes() end, { desc = 'fzf color schemes' })

-- LSP server configurations for the neovim LSP client
vim.pack.add({ 'https://github.com/neovim/nvim-lspconfig' })

-- enhance completion
vim.pack.add({ 'https://github.com/nvim-mini/mini.completion' })
require('mini.completion').setup({
    -- delay before popup window appears automatically (in ms)
    delay = {
        completion = 100,
        info = 200,
        signature = 50
    },
})

----------------------------------------
---------------- CONFIGURE COLOR SCHEMES
----------------------------------------

-- vim.cmd.colorscheme("peachpuff")
vim.cmd.colorscheme("retrobox")

