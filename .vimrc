
"vim-plug plugin manager
"put plugins to install (github repos) between begin() and end()
"run :PlugInstall" and restart vim
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
Plug 'luisjure/csound',  { 'for': ['csound'] }            "csound syntax highlighting
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
Plug 'derekwyatt/vim-scala'
Plug 'rhysd/vim-crystal'
Plug 'fatih/vim-go'
Plug 'maksimr/vim-jsbeautify'
Plug 'HerringtonDarkholme/yats.vim' " typescript
Plug 'dart-lang/dart-vim-plugin'
"----color themes----------------------------------
Plug 'flazz/vim-colorschemes'
Plug 'xolox/vim-colorscheme-switcher'
Plug 'xolox/vim-misc' " <--- this is required for the vim-colorscheme-switcher plugin above to work
Plug 'dracula/vim'
Plug 'MidnaPeach/neonwave.vim'
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
Plug 'romainl/flattened' " <--- solarized but without the bullshit
Plug 'fcpg/vim-orbital'
Plug 'fcpg/vim-fahrenheit'
Plug 'fcpg/vim-farout'
Plug 'Haron-Prime/evening_vim'
Plug 'Haron-Prime/Antares'
Plug 'whatyouhide/vim-gotham'
Plug 'ajh17/Spacegray.vim'
Plug 'mgutz/vim-colors' " <--- cappuccino, chance-of-storm, idle, mudcandy, t256, underwater-mod
" Plug 'vim-scripts/southwest-fog' " <--- originally written by mgutz above but can't find a github for him save this mirror
Plug 'romainl/Apprentice'
Plug 'trapd00r/neverland-vim-theme'
call plug#end()

" GUI options
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

" turn off netrw banner
let g:netrw_banner = 0

" change leader key to space
" let mapleader="\<Space>"
" show commands (so we can see when timeouts happen)
set showcmd

"----configure plugins----------------------------
"
"--javascript
" auto formatter (on js filetype buffer write)
"
" create a function which formats THEN correctly returns the cursor position
function! SmartJsBeautify()
    let saved_view = winsaveview()
    call JsBeautify()
    call winrestview(saved_view)
endfunction
autocmd bufwritepost *.js silent :call SmartJsBeautify()

" autocmd bufwritepre *.js silent :normal gg=G``

" --json
" autocmd bufwritepost *.json silent :call JsonBeautify()

" -- css
" autocmd bufwritepost *.css silent :call CSSBeautify()

"--golang
" tabs are displayed as 8 spaces (and we use tab characters not spaces)
autocmd Filetype go setlocal ts=8 sw=8 sts=8 noexpandtab
"vim-go mappings
au FileType go nmap <leader>r :GoRun % <CR>
au FileType go nmap <leader>ie <Plug>(go-iferr)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
let g:go_auto_type_info=1 "toggle automatic type info under the cursor

"--rust
" turn on auto-format  on save
let g:rustfmt_autosave = 1
" turn on racer
let g:racer_cmd = '~/.cargo/bin/racer'
let g:racer_experimental_completer = 1
au FileType rust nmap <leader>r :Crun <CR>
au FileType rust nmap <leader>b :Cbuild <CR>
au FileType rust nmap <leader>t :Ctest <CR>

"--dart
" format the buffer on save
let dart_format_on_save = 1
" enable 2 space formatting as specified by dart style guide
let dart_style_guide = 2

" " fix acp plugin issue when typing '<'
" " found here: 
" " https://github.com/sukima/xmledit/issues/15
" autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags noci
" autocmd FileType html set omnifunc=htmlcomplete#CompleteTags noci
" autocmd FileType css set omnifunc=csscomplete#CompleteCSS

"configure easymotion
let g:EasyMotion_smartcase=1
"turn off the default bindings
"if on, can't do <leader><leader> below as easymotion wants that
let g:EasyMotion_do_mapping=0
"
"configure netrw (vim's file browser)
"configuration gleaned from here:
"https://shapeshed.com/vim-netrw/
"let g:netrw_banner = -1
"let g:netrw_liststyle = 2
"let g:netrw_browse_split = 3
"let g:netrw_altv = 0
"let g:netrw_winsize = 24
"augroup ProjectDrawer
"  autocmd!
"  autocmd VimEnter * :Vexplore
"augroup END


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
"allow buffers to be hidden
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
set listchars=tab:..,trail:_,extends:>,precedes:<,nbsp:~
set listchars=tab:→\ ,eol:◄,nbsp:␣,trail:•,extends:⟩,precedes:⟨
set listchars=tab:→\ ,eol:↲,nbsp:␣,trail:•,extends:⟩,precedes:⟨
set listchars=tab:→\ ,eol:↵,nbsp:␣,trail:•,extends:⟩,precedes:⟨
set list

" turn off vim automatic backup
set nobackup
set noswapfile
"turn on mouse
set mouse=a
" turn off folding for all files
set nofoldenable
" vim's ACP plugin is painfully slow unless this is used
" solution found on this page:
" http://stackoverflow.com/questions/2169645/vims-autocomplete-is-excruciatingly-slow
" exact link to specific answer (on same page above):
" http://stackoverflow.com/a/4277400
set foldmethod=manual


"----csound specific configuration---------------
" configure csound indentation and use of tab characters
" http://stackoverflow.com/questions/158968/changing-vim-indentation-behavior-by-file-type
"autocmd Filetype csound setlocal ts=8 sw=8 sts=8 noexpandtab
" fix weird issue with csound files not using foldmethod=manual
autocmd BufNewFile,BufRead *.orc,*.sco,*.csd,*.udo   set foldmethod=manual


"----completion configuration--------------------
"turn on omnicompletion
filetype plugin on
set omnifunc=syntaxcomplete#Complete
" make omni-completion pop-up menu match longest
" as well as don't select an option initially
set completeopt=longest,menuone
"smart tab completion lovingly stolen from here:
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
    return "\<C-X>\<C-P>"                         " existing text matching
  elseif ( has_slash )
    return "\<C-X>\<C-F>"                         " file matching
  else
    return "\<C-X>\<C-O>"                         " plugin matching
  endif
endfunction


"----mappings------------------------------------
"remap esc (we're experiementing)
" remember, <C-[> is already bound to <Esc>
" """""""""""""""""
inoremap <C-\> <esc>
vnoremap <C-\> <esc>
inoremap ii <esc>
vnoremap ii <esc>
""""""""""""""""""""
"easier (on my keyboard) quick save file
nnoremap <Leader>w <esc>:w<cr>
"easier (on my keyboard) quick quit
inoremap <c-w> <esc>:q<CR>
nnoremap <c-w> <esc>:q<CR>
vnoremap <c-w> <esc>:q<CR>
nnoremap <Leader>q <esc>:q<cr>
"easier (on my keyboard) quick edit
nnoremap <Leader>e <esc>:e<space>
"easier (on my keyboard) quick help
nnoremap <Leader>h <esc>:h<space>
"easier entering of commands
nnoremap <leader><leader>  <esc>:
"easier (on my keyboard) quick buffer delete
nnoremap <Leader>x <esc>:bd<cr>
"easier entering of commands
nnoremap <Space><Leader> :
"indent while keeping visual highlighting
vmap > >gv
vmap < <gv
"toggle paste mode
"https://stackoverflow.com/questions/13967356/vimrc-addition-to-toggle-set-paste/46768583#46768583
nnoremap <leader>p :set invpaste<CR>
"turn on smart tab completion in insert mode
inoremap <tab> <c-r>=Smart_TabComplete()<CR>
"remap C-PageDown and C-PageUp to buffer movement (much like tabs in a web browser)
"It turns out remapping C-Tab and C-S-Tab are tricky, see this:
"http://stackoverflow.com/questions/2686766/mapping-c-tab-in-my-vimrc-fails-in-ubuntu
nnoremap <C-PageDown>      :bnext<CR>
nnoremap <C-PageUp>        :bprevious<CR>
inoremap <C-PageDown> <Esc>:bnext<CR>
inoremap <C-PageUp>   <Esc>:bprevious<CR>
vnoremap <C-PageDown>      :bnext<CR>
vnoremap <C-PageUp>        :bprevious<CR>
" map Shift-Arrow to buffer switching
nnoremap <silent> <s-Up>    <c-w>k
nnoremap <silent> <s-Down>  <c-w>j
nnoremap <silent> <s-Right> <c-w>l
nnoremap <silent> <s-Left>  <c-w>h
inoremap <silent> <s-Up>    <esc><c-w>ki
inoremap <silent> <s-Down>  <esc><c-w>ji
inoremap <silent> <s-Right> <esc><c-w>li
inoremap <silent> <s-Left>  <esc><c-w>hi
"make pop-up menu more friendly
"found here:
"http://vim.wikia.com/wiki/Improve_completion_popup_menu
"http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
" NB. we can't map <Esc> in insert mode for some reason (it turns the arrow keys into letters)
inoremap <expr> <CR>       pumvisible() ? "\<C-y>" : "\<CR>"
inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
inoremap <expr> <PageDown> pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<PageDown>"
inoremap <expr> <PageUp>   pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<PageUp>"
" easymotion mappings
nmap     <Leader>f <Plug>(easymotion-overwin-w)
" search for 1 character
"nmap <Leader>f <Plug>(easymotion-overwin-f)

"colorscheme-switcher plugin mappings
" disable F8=next color, shift-F8=previous color
let g:colorscheme_switcher_define_mappings=0
" new mappings are:
nnoremap <F4>      :NextColorScheme<CR>
inoremap <F4> <ESC>:NextColorScheme<CR>i
vnoremap <F4> <ESC>:NextColorScheme<CR>v
nnoremap <F3>      :PrevColorScheme<CR>
inoremap <F3> <ESC>:PrevColorScheme<CR>i
vnoremap <F3> <ESC>:PrevColorScheme<CR>v
" map <F5> to reload the vimrc
map <F5> :source $MYVIMRC <CR>

set laststatus=2                " turn on status line
set ttimeoutlen=1               " Fix the insert mode to normal mode delay
set statusline=
set statusline+=%f              " file name
set statusline+=%m              " is modified?
set statusline+=%=              " switching to right section
set statusline+=%y\             " file type
set statusline+=%#CursorColumn#
set statusline+=\ %6.l\ \:      " current line number (min 6 characters)
set statusline+=%3.c            " current col number  (min 3 characters)
set statusline+=\ %3.3p%%       " percentage scrolled through (min/max 3 characters)

"----color scheme and syntax highlighting-----------
"make background dark (might change color scheme slightly)
syntax on
set background=dark
"set 256 colors (if not already set)
set t_Co=256
set termguicolors " <---only works in vim8

" set colorscheme and fail silently otherwise
" silent! colorscheme desert
" silent! color elflord
" silent! color evening
" silent! color antares
" silent! color gotham
" silent! color dracula
" silent! color monochrome
" silent! color monotone
" silent! color molokai
let g:zenburn_high_Contrast=1 " <--- configure zenburn
" silent! color zenburn
" silent! color meta5
" silent! color gruvbox
" silent! color orbital
" silent! color fahrenheit
" silent! color farout
" silent! color space-vim-dark
" silent! color flattened_dark
" silent! color mudcandy
" silent! color apprentice
" silent! color southwest-fog
" silent! color spacegray
silent! color neonwave
" silent! color minimalist
" silent! color Atelier_PlateauDark
" silent! color Base2Tone_CaveDark
" silent! color neverland
