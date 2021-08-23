" vim-plug plugin manager
" put plugins to install (github repos) between begin() and end()
" run :PlugInstall" and restart vim
call plug#begin()
"----workflow--------------------------------------
Plug 'tpope/vim-fugitive'                                       " git integration
Plug 'tpope/vim-surround'                                       " change surrounding delimiters efficiently
Plug 'tpope/vim-repeat'                                         " repeat actions correctly for plugins
Plug 'tpope/vim-commentary'                                     " toggle comments
Plug 'easymotion/vim-easymotion'                                " jump around text *way* easier
Plug 'vim-airline/vim-airline'                                  " better status bar (works with git fugitive)
"----languages-------------------------------------
Plug 'xavierd/clang_complete'
Plug 'luisjure/csound',  { 'for': ['csound'] }                  " csound syntax highlighting/completion
Plug 'rust-lang/rust.vim'                                       " rust
Plug 'racer-rust/vim-racer'                                     " rust completion
Plug 'fatih/vim-go'                                             " go
Plug 'pangloss/vim-javascript'                                  " js
Plug 'maksimr/vim-jsbeautify'                                   " js formatter
Plug 'tpope/vim-fireplace'                                      " clojure
Plug 'davidhalter/jedi-vim'                                     " python completion
Plug 'psf/black'                                                " python formatter
"----color themes----------------------------------
" Plug 'MidnaPeach/neonwave.vim'   " <--- colors don't set correctly for some reason
Plug 'cinaeco/neonwave.vim'        " <--- these do however in this forked (modified) version
Plug 'jnurmine/Zenburn/'
Plug 'morhetz/gruvbox'
Plug 'fcpg/vim-farout'
Plug 'whatyouhide/vim-gotham'
Plug 'ajh17/Spacegray.vim'
Plug 'axvr/photon.vim'
Plug 'nikolvs/vim-sunbather'
Plug 'kyoz/purify', { 'rtp': 'vim' }
call plug#end()

"-----------------------------------------------
" configure options ----------------------------
"-----------------------------------------------
" turn off intro message
set shortmess+=I
" turn off automatic backup and hidden swap file
set nobackup
set noswapfile
" turn on syntax highlighting
syntax on
" must set this for color to work correctly
set background=dark
" set 256 colors (if not already set)
set t_Co=256
set termguicolors
" turn on filetype detection, plugin, and indentation
" https://vi.stackexchange.com/a/10125
filetype plugin indent on
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
" show existing tab with N spaces width
set tabstop=4
" when backspacing delete N spaces width
set softtabstop=4
" when indenting with '<' or '>' use N spaces width
set shiftwidth=4
" insert spaces instead of tab characters
set expandtab
" tab inserts shiftwidth amount of spaces
set smarttab
" make backspace not insane
set backspace=indent,eol,start
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
" turn off folding for all files by default
set nofoldenable
" turn on case insensitivity unless caps are present
set ignorecase
set smartcase
" move cursor to matched searches
set incsearch
" turn off completion scanning current/included files
set complete-=i
" turn on syntax completion
set omnifunc=syntaxcomplete#Complete
" make omni-completion pop-up menu match longest and don't select an option initially
set completeopt=longest,menuone
" enhance command completion
set wildmode=full
set wildmenu
" turn off interpreting leading zero numbers as octal
set nrformats-=octal
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
"--c++
let g:clang_library_path='/usr/lib/llvm-10/lib/libclang.so.1'

"--airline
let g:airline_symbols_ascii=1

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
" no automatic dot completion
let g:jedi#popup_on_dot = 0
augroup filetype_python
    " clear previous autocommands in this autocommand group
    autocmd!
    " auto format python according to pep8
    autocmd BufWritePre *.py execute ':Black'
    " turn off jedi keybinding (which collides with this binding)
    let g:jedi#rename_command = ""
    autocmd FileType python nnoremap <leader>r :!"%:p"<cr>
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

"------------------------------------------------
" configure mappings ----------------------------
"------------------------------------------------
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
"
" save file
nnoremap <leader>w <esc>:w<cr>
" edit file
nnoremap <leader>e <esc>:e<space>
" close buffer
nnoremap <leader>x <esc>:bd<cr>
" close window or quit if last window
nnoremap <leader>q <esc>:q<cr>
" reload vimrc
noremap  <f5> :source $MYVIMRC<cr>
" edit vimrc
nnoremap <leader>v <esc>:e $MYVIMRC<cr>
" command
nnoremap <leader><leader> <esc>:
" help
nnoremap <leader>h <esc>:h<space>
" terminal open (in vertical right window)
nnoremap <leader>t <esc>:vertical botright terminal<cr>
" make <c-^> (switch to alternate-file (the last buffer)) work in terminal
tnoremap <c-^> <c-w>:b#<cr>
tnoremap <c-6> <c-w>:b#<cr>
" make ctrl-d work as it does in terminal
tnoremap <c-d> <c-d>
tnoremap <c-^> <c-w>:b#<cr>
" terminal enter normal mode https://github.com/vim/vim/issues/2216#issuecomment-337566816
tnoremap <esc><esc> <c-\><c-n>
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
tnoremap <c-pagedown>      <c-w>:bnext<cr>
tnoremap <c-pageup>        <c-w>:bprevious<cr>
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
" must be nmap not nnoremap
nmap <leader>f <plug>(easymotion-overwin-w)

"------------------------------------------------
" configure colorscheme -------------------------
"------------------------------------------------
" select colorscheme and fail silently otherwise
" silent! color farout
" silent! color gotham
" silent! color gruvbox
" silent! color neonwave
" silent! color photon
" silent! color purify
" silent! color spacegray
" silent! color sunbather
let g:zenburn_high_Contrast=1 " <--- configure zenburn
silent! color zenburn
