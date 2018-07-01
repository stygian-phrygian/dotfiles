
"vim-plug plugin manager
"put plugins to install (github repos) between begin() and end()
"run :PlugInstall" and restart vim
call plug#begin()
"----graphical improvements------------------------
Plug 'vim-airline/vim-airline'                            "vim status bar
Plug 'vim-airline/vim-airline-themes'                     "vim status bar themes
Plug 'tpope/vim-fugitive'                                 "git integration (works with the status bar above)
Plug 'roman/golden-ratio'                                 "automatic window resizing to golden ratio
"----text motion-----------------------------------
Plug 'tpope/vim-surround'                                 "change surrounding delimiters efficiently
Plug 'tpope/vim-repeat'                                   "repeat actions correctly for plugins
Plug 'easymotion/vim-easymotion'                          "jump around text *way* easier
"----comment toggling------------------------------
Plug 'tpope/vim-commentary'
"----completion------------------------------------
"Plug 'Shougo/neocomplete.vim'
"go here for install instruction: https://github.com/Valloric/YouCompleteMe
"Plug 'Valloric/YouCompleteMe', { 'do': './install.py --tern-completer --clang-completer' }
Plug 'vim-scripts/L9'
Plug 'vim-scripts/AutoComplPop'
"----language specific improvements----------------
Plug 'luisjure/csound',  { 'for': ['csound'] }            "csound syntax highlighting
"Plug 'kovisoft/paredit', { 'for': ['clojure', 'scheme'] } "easy lisp structural editing
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
Plug 'derekwyatt/vim-scala'
Plug 'rhysd/vim-crystal'
Plug 'fatih/vim-go'
"----color themes----------------------------------
Plug 'dracula/vim'
Plug 'tomasr/molokai'
Plug 'liuchengxu/space-vim-dark'
Plug 'christophermca/meta5'
Plug 'jnurmine/Zenburn/'
Plug 'fxn/vim-monochrome'
Plug 'morhetz/gruvbox'
Plug 'flazz/vim-colorschemes'
Plug 'fcpg/vim-orbital'
Plug 'fcpg/vim-farout'
Plug 'fcpg/vim-fahrenheit'
call plug#end()
"

"----color scheme and syntax highlighting-----------
"set 256 colors (if not already set)
set t_Co=256
syntax on
"make background dark (might change color scheme slightly)
set bg=dark
" configure space-vim-dark
"   range:   233 (darkest) ~ 238 (lightest)
"   default: 235
let g:space_vim_dark_background = 233
" configure molokai
"let g:rehash256 = 1
"configure zenburn
let g:zenburn_high_Contrast=1
" set colorscheme and fail silently otherwise
silent! color desert
silent! color elflord
silent! color dracula
silent! color space-vim-dark
silent! color monochrome
silent! color molokai
silent! color zenburn
silent! color meta5
silent! color fahrenheit
silent! color gruvbox
silent! color farout

"
"
" change leader key to space
let mapleader="\<Space>"
" show commands (so we can see when timeouts happen)
set showcmd

"----configure plugins----------------------------
"
"--language specific configuration
"--golang
" tabs are displayed as 8 spaces (and we use tab characters not spaces)
autocmd Filetype go setlocal ts=8 sw=8 sts=8 noexpandtab
"vim-go mappings
au FileType go nmap <leader>r :GoRun % <CR>
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
let g:go_auto_type_info=1 "toggle automatic type info under the cursor
"--rust
" turn on auto-format  on save
let g:rustfmt_autosave = 1
" turn on racer
let g:racer_cmd = "$HOME/.cargo/bin/racer"
" experimental completion
"let g:racer_experimental_completer = 1
"
"
"--------------------------------------------------
"" Use neocomplete.
"let g:neocomplete#enable_at_startup = 1
"" Use smartcase.
"let g:neocomplete#enable_smart_case = 1
"" Set minimum syntax keyword length.
"let g:neocomplete#sources#syntax#min_keyword_length = 3
"" AutoComplPop like behavior.
""let g:neocomplete#enable_auto_select = 1
"" Enable omni completion.
""autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
""autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
""autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
""autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
""autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
"
"rust completion configuration
let g:racer_cmd = "/home/j/.cargo/bin/"
"
" Enable and configure vim-airline
set laststatus=2
set ttimeoutlen=1                                " Fix the insert mode to normal mode delay
let g:airline#extensions#tabline#enabled = 1     " Enable the list of buffers
let g:airline#extensions#tabline#fnamemod = ':t' " Show just the filename
"
" fix a acp plugin issue when typing '<'
" found here: 
" https://github.com/sukima/xmledit/issues/15
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags noci
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags noci
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
"
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
set tabstop=2
" when backspacing delete N spaces width
set softtabstop=2
" when indenting with '<' or '>' use N spaces width
set shiftwidth=2
" insert spaces instead of tab characters
set expandtab
" textwidth (what width paragraphs are formatted to)
set textwidth=79
" provide a column indicator of text width
set colorcolumn=80


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
set listchars=tab:→\ ,eol:↲,nbsp:␣,trail:•,extends:⟩,precedes:⟨
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
autocmd BufNewFile,BufRead *.orc,*.sco,*.csd,*.udo   set foldmethod=manual"


"----completion configuration--------------------
"turn on omnicompletion
filetype plugin on
set omnifunc=syntaxcomplete#Complete
"make omni-completion pop-up menu match longest 
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
  if (!has_period && !has_slash)
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
inoremap <c-left> <esc>:w<CR>
nnoremap <c-left> <esc>:w<CR>
vnoremap <c-left> <esc>:w<CR>
nnoremap <Leader>w <esc>:w<cr>
"easier (on my keyboard) quick quit
inoremap <c-w> <esc>:q<CR>
nnoremap <c-w> <esc>:q<CR>
vnoremap <c-w> <esc>:q<CR>
nnoremap <Leader>q <esc>:q<cr>
"easier entering of commands
nnoremap <Space><Leader> :
"indent while keeping visual highlighting
vmap > >gv
vmap < <gv
"move to first and and last characters on a line
nnoremap 0  ^
nnoremap 00 g_
"toggle paste mode
"https://stackoverflow.com/questions/13967356/vimrc-addition-to-toggle-set-paste/46768583#46768583
nnoremap <leader>p :set invpaste<CR>
"easier redo (nb. must press <leader>u THEN release
"can't just hold down space, tapping 'u' for multiple iterations)
nnoremap <Leader>u <C-r>
"turn on smart tab completion in insert mode
inoremap <tab> <c-r>=Smart_TabComplete()<CR>
"remap <Leader>h & <Leader>l to buffer previous/next respectively
nnoremap <Leader>h :bprevious<CR>
nnoremap <Leader>l :bnext<CR>
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
