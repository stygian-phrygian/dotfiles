" vim-plug plugin manager
" put plugins to install (github repos) between begin() and end()
" run :PlugInstall" and restart vim
call plug#begin()
"----graphical improvements------------------------
Plug 'ap/vim-buftabline'
Plug 'tpope/vim-fugitive'                                 "git integration (works with the status bar above)
Plug 'roman/golden-ratio'                                 "automatic window resizing to golden ratio
"----text motion-----------------------------------
Plug 'tpope/vim-surround'                                 "change surrounding delimiters efficiently
Plug 'tpope/vim-repeat'                                   "repeat actions correctly for plugins
Plug 'easymotion/vim-easymotion'                          "jump around text *way* easier
"----comment toggling------------------------------
Plug 'tpope/vim-commentary'
"----completion------------------------------------
Plug 'vim-scripts/L9'
Plug 'vim-scripts/AutoComplPop'
"----language specific improvements----------------
Plug 'luisjure/csound',  { 'for': ['csound'] }            " csound syntax highlighting/completion
Plug 'rust-lang/rust.vim'                                 " rust
Plug 'racer-rust/vim-racer'                               " rust completion
Plug 'derekwyatt/vim-scala'                               " scala
Plug 'rhysd/vim-crystal'                                  " crystal
Plug 'fatih/vim-go'                                       " go
Plug 'maksimr/vim-jsbeautify'                             " js formatter
Plug 'Quramy/tsuquyomi'                                   " typescript mode
Plug 'HerringtonDarkholme/yats.vim'                       " typescript highlighting
Plug 'dart-lang/dart-vim-plugin'                          " dart
"
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
Plug 'romainl/flattened'           " <--- solarized but without the bullshit
Plug 'fcpg/vim-orbital'
Plug 'fcpg/vim-fahrenheit'
Plug 'fcpg/vim-farout'
Plug 'Haron-Prime/evening_vim'
Plug 'Haron-Prime/Antares'
Plug 'whatyouhide/vim-gotham'
Plug 'ajh17/Spacegray.vim'
Plug 'mgutz/vim-colors'            " <--- cappuccino, chance-of-storm, idle, mudcandy, t256, underwater-mod
Plug 'romainl/Apprentice'
Plug 'trapd00r/neverland-vim-theme'
Plug 'vim-scripts/reloaded.vim'
Plug 'vim-scripts/revolutions.vim'
call plug#end()

"----GUI options---------------------------------
if has("gui_running")
  " turn off GUI widgets (for gvim)
  set guioptions-=m  "menu bar
  set guioptions-=T  "toolbar
  set guioptions-=r  "scrollbar
  " set fonts (cross-platform)
  if has("gui_gtk2")       " linux
    set guifont=Monospace\ 18
  elseif has("gui_win32")  " windows
    set guifont=Luxi_Mono:h18:cANSI
  elseif has("gui_macvim") " mac
    set guifont=Menlo\ Regular:h18
  endif
endif

"----configure plugins----------------------------

"--javascript
" create a function which formats THEN correctly returns the cursor position
function! SmartJsBeautify()
    let saved_view = winsaveview()
    call JsBeautify()
    call winrestview(saved_view)
endfunction
" auto format on js filetype buffer write
autocmd BufWritePost *.js silent :call SmartJsBeautify()

"--golang
" tabs are displayed as 8 spaces (and we use tab characters not spaces)
autocmd FileType go setlocal ts=8 sw=8 sts=8 noexpandtab
" vim-go mappings
au FileType go nmap <leader>r :GoRun % <cr>
au FileType go nmap <leader>ie <plug>(go-iferr)
au FileType go nmap <leader>b  <plug>(go-build)
au FileType go nmap <leader>t  <plug>(go-test)
au FileType go nmap <leader>c  <plug>(go-coverage)
let g:go_auto_type_info=1 "toggle automatic type info under the cursor

"--rust
" turn on auto-format  on save
let g:rustfmt_autosave = 1
" turn on racer
let g:racer_cmd = '~/.cargo/bin/racer'
let g:racer_experimental_completer = 1
au FileType rust nmap <leader>r :Crun <cr>
au FileType rust nmap <leader>b :Cbuild <cr>
au FileType rust nmap <leader>t :Ctest <cr>

"--python
" enable python 3 syntax checking
let g:pymode_python = 'python3'
" turn off errors window focus
let g:pymode_lint_cwindow = 0
" autoformat python code
autocmd BufWritePost *.py silent :PymodeLintAuto

"--dart
" format the buffer on save
let dart_format_on_save = 1
" enable 2 space formatting as specified by dart style guide
let dart_style_guide = 2

"--csound
" fix weird issue with csound files not using foldmethod=manual
autocmd BufNewFile,BufRead *.orc,*.sco,*.csd,*.udo set foldmethod=manual

"--easymotion
let g:EasyMotion_smartcase=1
" turn off the default bindings
" if on, can't do <leader><leader> below as easymotion wants that
let g:EasyMotion_do_mapping=0

"----vim indentation/tab configuration------------
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

"----miscellaneous-------------------------------
" show commands (so we can see when timeouts happen)
set showcmd
" allow buffers to be hidden
set hidden
" turn off line wrapping
set nowrap
" turn on hybrid mode line numbers ----
set number         " turn on line numbers
set relativenumber " turn on relative line numbers
" turn on case insensitivity unless caps are present
set ignorecase
set smartcase
" move cursor to matched searches
set incsearch
" to display (some) invisible characters
" type ':set !list' to toggle
set listchars=tab:→\ ,eol:↲,nbsp:␣,trail:•,extends:⟩,precedes:⟨ " british newline
set listchars=tab:→\ ,eol:↵,nbsp:␣,trail:•,extends:⟩,precedes:⟨ " american newline
set listchars=tab:→\ ,nbsp:␣,trail:•,extends:⟩,precedes:⟨       " no newline
set list
" turn off vim automatic backup
set nobackup
set noswapfile
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

"----completion configuration--------------------
" turn on omnicompletion
filetype plugin on
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

"----mappings-------------------------------------
" save file
nnoremap <leader>w <esc>:w<cr>
" edit file
nnoremap <leader>e <esc>:e<space>
" close buffer
nnoremap <leader>x <esc>:bd<cr>
" quit
inoremap <c-w> <esc>:q<cr>
nnoremap <c-w> <esc>:q<cr>
vnoremap <c-w> <esc>:q<cr>
nnoremap <leader>q <esc>:q<cr>
" reload vimrc
map <f5> :source $MYVIMRC<cr>
" edit vimrc
nnoremap <leader>v <esc>:e $MYVIMRC<cr>
" command
nnoremap <leader><leader> <esc>:
" help
nnoremap <leader>h <esc>:h<space>
" indent while keeping visual highlighting
vmap > >gv
vmap < <gv
" toggle paste mode
" https://stackoverflow.com/questions/13967356/vimrc-addition-to-toggle-set-paste/46768583#46768583
nnoremap <leader>p :set invpaste<cr>
" remap C-PageDown and C-PageUp to buffer movement (much like tabs in a web browser)
" It turns out remapping C-Tab and C-S-Tab are tricky, see this:
" http://stackoverflow.com/questions/2686766/mapping-c-tab-in-my-vimrc-fails-in-ubuntu
nnoremap <c-pagedown>      :bnext<cr>
nnoremap <c-pageup>        :bprevious<cr>
inoremap <c-pagedown> <esc>:bnext<cr>
inoremap <c-pageup>   <esc>:bprevious<cr>
vnoremap <c-pagedown>      :bnext<cr>
vnoremap <c-pageup>        :bprevious<cr>
" map Shift-Arrow to buffer switching
nnoremap <silent> <s-up>    <c-w>k
nnoremap <silent> <s-down>  <c-w>j
nnoremap <silent> <s-right> <c-w>l
nnoremap <silent> <s-left>  <c-w>h
inoremap <silent> <s-up>    <esc><c-w>ki
inoremap <silent> <s-down>  <esc><c-w>ji
inoremap <silent> <s-right> <esc><c-w>li
inoremap <silent> <s-left>  <esc><c-w>hi
" easymotion mappings
nmap     <leader>f <plug>(easymotion-overwin-w)
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

"----statusline-----------------------------------
set laststatus=2                " turn on status line
set ttimeoutlen=1               " Fix the insert mode to normal mode delay
set statusline=
set statusline+=%f              " file name
set statusline+=%m              " is modified?
set statusline+=\ %y            " file type
set statusline+=%=              " switching to right section
set statusline+=%16.l           " current line number (min 16 characters)
set statusline+=%4.c            " current col number  (min 4 characters)
set statusline+=%8.p%%          " percentage scrolled through (min 8 characters)

"----color scheme and syntax highlighting---------
" make background dark (might change color scheme slightly)
syntax on
set background=dark
" set 256 colors (if not already set)
set t_Co=256
set termguicolors " <---only works in vim8
" set colorscheme and fail silently otherwise
" silent! color antares
" silent! color apprentice
" silent! color Atelier_PlateauDark
" silent! color Base2Tone_CaveDark
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
" silent! color mudcandy
" silent! color neonwave
" silent! color neverland
" silent! color orbital
" silent! color spacegray
" silent! color space-vim-dark
" silent! color srcery
" silent! color reloaded
" silent! color revolutions
let g:zenburn_high_Contrast=1 " <--- configure zenburn
silent! color zenburn
