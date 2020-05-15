" vim-plug plugin manager
" put plugins to install (github repos) between begin() and end()
" run :PlugInstall" and restart vim
call plug#begin()
"----graphical improvements------------------------
Plug 'ap/vim-buftabline'
Plug 'tpope/vim-fugitive'                                       "git integration (works with the status bar above)
"----text motion-----------------------------------
Plug 'tpope/vim-surround'                                       "change surrounding delimiters efficiently
Plug 'tpope/vim-repeat'                                         "repeat actions correctly for plugins
Plug 'easymotion/vim-easymotion'                                "jump around text *way* easier
"----comment toggling------------------------------
Plug 'tpope/vim-commentary'
"----completion------------------------------------
" Plug 'vim-scripts/L9'
" Plug 'vim-scripts/AutoComplPop'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --ts-completer', 'for': 'javascript', } " completion for ts/js
"----language specific improvements----------------
Plug 'luisjure/csound',  { 'for': ['csound'] }                  " csound syntax highlighting/completion
Plug 'rust-lang/rust.vim'                                       " rust
Plug 'racer-rust/vim-racer'                                     " rust completion
Plug 'derekwyatt/vim-scala'                                     " scala
Plug 'rhysd/vim-crystal'                                        " crystal
Plug 'fatih/vim-go'                                             " go
Plug 'pangloss/vim-javascript'                                  " js
Plug 'maksimr/vim-jsbeautify'                                   " js formatter
Plug 'maxmellon/vim-jsx-pretty'                                 " jsx highlighting and formatter
Plug 'Quramy/tsuquyomi'                                         " typescript mode
Plug 'HerringtonDarkholme/yats.vim'                             " typescript highlighting
Plug 'tpope/vim-fireplace'
" nota bene: plug-installing the following python plugin isn't enough, you need
" to find where it's installed, and then git submodule the necessary components
" (namely for completion with the rope submodule)
" run these commands
" cd ~/.vim/plugged/python-mode/ # where it's likely installed
" git clone --recurse-submodules https://github.com/python-mode/python-mode
" git submodule update --init --recursive
" completion works with rope via initializing a .ropeproject automatically
Plug 'python-mode/python-mode', { 'for': 'python', 'branch': 'develop' }
"----color themes----------------------------------
" Plug 'flazz/vim-colorschemes'
Plug 'xolox/vim-colorscheme-switcher'
Plug 'xolox/vim-misc' " <--- this is required for the vim-colorscheme-switcher plugin above to work
Plug 'dracula/vim'
" Plug 'MidnaPeach/neonwave.vim'   " <--- colors don't set correctly for some reason
Plug 'cinaeco/neonwave.vim'        " <--- these do however in this forked (modified) version
Plug 'srcery-colors/srcery-vim'
Plug 'Reewr/vim-monokai-phoenix'
Plug 'dikiaap/minimalist'
Plug 'tomasr/molokai'
Plug 'liuchengxu/space-vim-dark'
Plug 'christophermca/meta5'
Plug 'atelierbram/vim-colors_atelier-schemes'
Plug 'atelierbram/Base2Tone-vim'
Plug 'jnurmine/Zenburn/'
Plug 'fxn/vim-monochrome'
Plug 'Lokaltog/vim-monotone'
Plug 'morhetz/gruvbox'
Plug 'romainl/flattened'           " <--- solarized simplified
Plug 'fcpg/vim-orbital'
Plug 'fcpg/vim-fahrenheit'
Plug 'fcpg/vim-farout'
Plug 'Haron-Prime/evening_vim'
Plug 'Haron-Prime/Antares'
Plug 'whatyouhide/vim-gotham'
Plug 'ajh17/Spacegray.vim'
Plug 'romainl/Apprentice'
Plug 'trapd00r/neverland-vim-theme'
Plug 'vim-scripts/reloaded.vim'
Plug 'vim-scripts/revolutions.vim'
Plug 'axvr/photon.vim'
call plug#end()

"-----------------------------------------------
" configure options ----------------------------
"-----------------------------------------------
" turn off intro message
set shortmess+=I
" turn off automatic backup and hidden swap file
set nobackup
set noswapfile
" miscellaneous gui options
if has("gui_running")
  " turn off GUI widgets
  set guioptions-=m  " menu bar
  set guioptions-=T  " toolbar
  set guioptions-=r  " scrollbar
  " set default GUI fonts (cross-platform)
  if has("gui_gtk2")       " linux
    set guifont=Monospace\ 18
  elseif has("gui_win32")  " windows
    set guifont=Luxi_Mono:h18:cANSI
  elseif has("gui_macvim") " mac
    set guifont=Menlo\ Regular:h18
  endif
endif
" turn on filetype detection, plugin, and indentation
" https://vi.stackexchange.com/a/10125
filetype plugin indent on
" show existing tab with N spaces width
set tabstop=4
" when backspacing delete N spaces width
set softtabstop=4
" when indenting with '<' or '>' use N spaces width
set shiftwidth=4
" insert spaces instead of tab characters
set expandtab
" textwidth (what width paragraphs are formatted to)
set textwidth=79
" provide a column indicator of text width
" this is turned off because it doesn't necessarily match the colorscheme colors
" set colorcolumn=80
" show commands (so we can see when timeouts happen)
set showcmd
" allow buffers to be hidden
" (buffers persist when not displayed by a window)
set hidden
" turn off line wrapping
set nowrap
" turn on hybrid mode line numbers
set number         " turn on line numbers
set relativenumber " turn on relative line numbers
" to display (some) invisible characters
" type ':set !list' to toggle
set listchars=tab:→\ ,eol:↲,nbsp:␣,trail:•,extends:⟩,precedes:⟨ " british newline
set listchars=tab:→\ ,eol:↵,nbsp:␣,trail:•,extends:⟩,precedes:⟨ " american newline
set listchars=tab:→\ ,nbsp:␣,trail:•,extends:⟩,precedes:⟨       " no newline
set list
" turn on mouse
set mouse=a
" turn off folding for all files
set nofoldenable
" vim's ACP plugin is painfully slow unless this is used
" solution found on this page:
" http://stackoverflow.com/questions/2169645/vims-autocomplete-is-excruciatingly-slow
" exact link to specific answer (on same page above):
" http://stackoverflow.com/a/4277400
set foldmethod=manual
" turn on case insensitivity unless caps are present
set ignorecase
set smartcase
" move cursor to matched searches
set incsearch
" turn on syntax highlighting
syntax on
" must set this for color to work correctly
set background=dark
" set 256 colors (if not already set)
set t_Co=256
set termguicolors
" set statusline
set laststatus=2                " turn on status line
set ttimeoutlen=1               " Fix the insert mode to normal mode delay
set statusline=
set statusline+=%f              " file name
set statusline+=%m              " is modified?
set statusline+=\ %y            " file type
set statusline+=%=              " switching to right section
set statusline+=\ %16.l         " current line number (min 16 characters)
set statusline+=\ %4.c          " current col number  (min 4 characters)
set statusline+=%8.p%%          " percentage scrolled through (min 8 characters)

"-----------------------------------------------
" configure plugins ----------------------------
"-----------------------------------------------
"--netrw
" disable the banner
let g:netrw_banner=0
" view file listing as a tree
let g:netrw_liststyle=3

"--javascript
" create a function which formats THEN correctly returns the cursor position
function! SmartJsBeautify()
    let saved_view = winsaveview()
    call JsBeautify()
    call winrestview(saved_view)
endfunction
augroup filetype_js
    " clear previous autocommands in this autocommand group
    autocmd!
    " auto format on js filetype buffer write
    autocmd BufWritePost *.js silent :call SmartJsBeautify()
augroup END
"
" correct the object formatting of js-beautify
" objects with one property should be on one line
"
" To configure vim-jsbeautifier, we must locate the .editorconfig file
" conveniently hidden away in the plugin directory which for me is
" ~/.vim/plugged/vim-jsbeautify/plugin/.editorconfig
" In this file, under the header labeled: [**.js],
" write the following:
" brace_style = collapse-preserve-inline
"
" I want to applaud the plugin writer for such an immaculately easy
" customization.  Why bother containing everything in the vimrc really when I
" can burn more of my time hunting down externalities.

"--golang
augroup filetype_go
    " clear previous autocommands in this autocommand group
    autocmd!
    " vim-go requires you to first :GoInstallBinaries to get completion
    " tabs are displayed as 8 spaces (and we use tab characters not spaces)
    autocmd FileType go setlocal ts=8 sw=8 sts=8 noexpandtab
    " vim-go mappings
    autocmd FileType go nnoremap <leader>r :GoRun % <cr>
    autocmd FileType go nnoremap <leader>ie <plug>(go-iferr)
    autocmd FileType go nnoremap <leader>b  <plug>(go-build)
    autocmd FileType go nnoremap <leader>t  <plug>(go-test)
    autocmd FileType go nnoremap <leader>c  <plug>(go-coverage)
augroup END
" toggle automatic type info under the cursor
let g:go_auto_type_info = 1

"--rust
" turn on auto-format  on save
let g:rustfmt_autosave = 1
" turn on racer
let g:racer_cmd = '~/.cargo/bin/racer'
let g:racer_experimental_completer = 1
augroup filetype_rust
    " clear previous autocommands in this autocommand group
    autocmd!
    autocmd FileType rust nmap <leader>r :Crun <cr>
    autocmd FileType rust nmap <leader>b :Cbuild <cr>
    autocmd FileType rust nmap <leader>t :Ctest <cr>
augroup END

"--python
" enable python 3 syntax checking
let g:pymode_python = 'python3'
" turn off errors window focus
let g:pymode_lint_cwindow = 0
" turn on documentation
let g:pymode_doc = 1
" bind key for documentation
let g:pymode_doc_bind = '<leader>d'
" turn on rope mode
let g:pymode_rope = 1
" turn on rope completion
let g:pymode_rope_completion = 1
" turn off auto-completion when a dot is typed
let g:pymode_rope_complete_on_dot = 0
" regenerate rope cache on saves
let g:pymode_rope_regenerate_on_write = 1
augroup filetype_python
    " clear previous autocommands in this autocommand group
    autocmd!
    " autoformat python code
    autocmd BufWritePost *.py silent :PymodeLintAuto
augroup END

"--csound
augroup filetype_csound
    " clear previous autocommands in this autocommand group
    autocmd!
    " fix weird issue with csound files not using foldmethod=manual
    autocmd BufNewFile,BufRead *.orc,*.sco,*.csd,*.udo set foldmethod=manual
    " turn off line wrapping
    autocmd FileType csound setlocal formatoptions-=t
    " turn off relative line numbering which slows it down for some reason
    autocmd FileType csound set norelativenumber
augroup END

"--easymotion
let g:EasyMotion_smartcase=1
" turn off the default bindings
" if on, can't do <leader><leader> below as easymotion wants that
let g:EasyMotion_do_mapping=0


"----completion/searching configuration----------
set omnifunc=syntaxcomplete#Complete
" make omni-completion pop-up menu match longest
" as well as don't select an option initially
set completeopt=longest,menuone
" smart tab completion lovingly stolen from here:
"http://vim.wikia.com/wiki/Smart_mapping_for_tab_completion
function! Smart_TabComplete()
  let line = getline('.')                         " current line
  let substr = strpart(line, -1, col('.')+1)      " from the start of the current
                                                  " line to one character right
                                                  " of the cursor
  let substr = matchstr(substr, "[^ \t]*$")       " word till cursor
  if (strlen(substr)==0)                          " nothing to match on empty string
    return "\<tab>"
  endif
  let has_period = match(substr, '\.') != -1      " position of period, if any
  let has_slash = match(substr, '\/') != -1       " position of slash, if any
  let has_double_colon=match(substr, '::') != -1  " position of double colon, if any (for rust... I added this)
  if (!has_period && !has_slash && !has_double_colon)
    return "\<C-X>\<C-P>"                         " existing text matching (syntax omnicompletion)
  elseif ( has_slash )
    return "\<C-X>\<C-F>"                         " file matching
  else
    return "\<C-X>\<C-O>"                         " plugin matching
  endif
endfunction
" turn on smart tab completion in insert mode
inoremap <tab> <c-r>=Smart_TabComplete()<cr>
" make pop-up menu more friendly
" found here:
" http://vim.wikia.com/wiki/Improve_completion_popup_menu
" http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
" NB. we can't map <esc> in insert mode for some reason (it turns the arrow keys into letters)
inoremap <expr> <cr>       pumvisible() ? "\<c-y>" : "\<cr>"
inoremap <expr> <down>     pumvisible() ? "\<c-n>" : "\<down>"
inoremap <expr> <up>       pumvisible() ? "\<c-p>" : "\<up>"
inoremap <expr> <pagedown> pumvisible() ? "\<pagedown>\<c-p>\<c-n>" : "\<pagedown>"
inoremap <expr> <pageup>   pumvisible() ? "\<pageup>\<c-p>\<c-n>" : "\<pageup>"

"------------------------------------------------
" configure mappings ----------------------------
"------------------------------------------------
" save file
nnoremap <leader>w <esc>:w<cr>
" edit file
nnoremap <leader>e <esc>:e<space>
" close buffer
nnoremap <leader>x <esc>:bd<cr>
" quit
nnoremap <leader>q <esc>:q<cr>
" reload vimrc
noremap  <f5> :source $MYVIMRC<cr>
" edit vimrc
nnoremap <leader>v <esc>:e $MYVIMRC<cr>
" command
nnoremap <leader><leader> <esc>:
" help
nnoremap <leader>h <esc>:h<space>
" indent while keeping visual highlighting
vnoremap > >gv
vnoremap < <gv
" remap C-PageDown and C-PageUp to buffer movement (much like tabs in a web browser)
" NB. for future readers on the distinctions among buffers, windows,
" and tabs (a la vim).  Buffers are just a buffer of text (obviously), windows
" are a view on that text (you seeing the buffer), and tabs are an arrangement of windows.
" It turns out remapping C-Tab and C-S-Tab are tricky, see this:
" http://stackoverflow.com/questions/2686766/mapping-c-tab-in-my-vimrc-fails-in-ubuntu
nnoremap <c-pagedown>           :bnext<cr>
nnoremap <c-pageup>             :bprevious<cr>
inoremap <c-pagedown>      <esc>:bnext<cr>
inoremap <c-pageup>        <esc>:bprevious<cr>
vnoremap <c-pagedown>           :bnext<cr>
vnoremap <c-pageup>             :bprevious<cr>
tnoremap <c-pagedown> <c-\><c-n>:bnext<cr>
tnoremap <c-pageup>   <c-\><c-n>:bprevious<cr>
" map Shift-Arrow to window switching
nnoremap <silent> <s-up>    <c-w>k
nnoremap <silent> <s-down>  <c-w>j
nnoremap <silent> <s-right> <c-w>l
nnoremap <silent> <s-left>  <c-w>h
inoremap <silent> <s-up>    <esc><c-w>ki
inoremap <silent> <s-down>  <esc><c-w>ji
inoremap <silent> <s-right> <esc><c-w>li
inoremap <silent> <s-left>  <esc><c-w>hi
tnoremap <silent> <s-up>    <c-w>k
tnoremap <silent> <s-down>  <c-w>j
tnoremap <silent> <s-right> <c-w>l
tnoremap <silent> <s-left>  <c-w>h
" map Ctrl-Shift-Arrow to window dimensions
nnoremap <silent> <c-s-up>    <c-w>+
nnoremap <silent> <c-s-down>  <c-w>-
nnoremap <silent> <c-s-right> <c-w>>
nnoremap <silent> <c-s-left>  <c-w><
inoremap <silent> <c-s-up>    <esc><c-w>+
inoremap <silent> <c-s-down>  <esc><c-w>-
inoremap <silent> <c-s-right> <esc><c-w>>
inoremap <silent> <c-s-left>  <esc><c-w><
tnoremap <silent> <c-s-up>    <c-w>+
tnoremap <silent> <c-s-down>  <c-w>-
tnoremap <silent> <c-s-right> <c-w>>
tnoremap <silent> <c-s-left>  <c-w><
" easymotion plugin mappings
nnoremap <leader>f <plug>(easymotion-overwin-w)
" colorscheme-switcher plugin mappings
" disable F8=next color, shift-F8=previous color
let g:colorscheme_switcher_define_mappings=0
" new mappings are:
nnoremap <f4>      :NextColorScheme<cr>
inoremap <f4> <esc>:NextColorScheme<cr>i
vnoremap <f4> <esc>:NextColorScheme<cr>v
nnoremap <f3>      :PrevColorScheme<cr>
inoremap <f3> <esc>:PrevColorScheme<cr>i
vnoremap <f3> <esc>:PrevColorScheme<cr>v

"------------------------------------------------
" configure colorscheme -------------------------
"------------------------------------------------
" select colorscheme and fail silently otherwise
" silent! color antares
" silent! color apprentice
" silent! color Atelier_PlateauDark
" silent! color Base2Tone_MotelDark
" silent! color dracula
" silent! color evening
" silent! color fahrenheit
" silent! color farout
" silent! color flattened_dark
" silent! color gotham
" silent! color gruvbox
" silent! color meta5
" silent! color minimalist
" silent! color molokai
" silent! color monochrome
" silent! color monokai-phoenix
" silent! color monotone
" silent! color neonwave
" silent! color neverland
" silent! color orbital
" silent! color photon
" silent! color spacegray
" silent! color space-vim-dark
" silent! color srcery
" silent! color reloaded
" silent! color revolutions
let g:zenburn_high_Contrast=1 " <--- configure zenburn
silent! color zenburn
