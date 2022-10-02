" vim-plug plugin manager
" put plugins to install (github repos) between begin() and end()
" run :PlugInstall" and restart vim
call plug#begin()
"----workflow----------------------------------------------------------------
" integrate git
Plug 'tpope/vim-fugitive'
" change surrounding delimiters
Plug 'tpope/vim-surround'
" repeat plugin maps
Plug 'tpope/vim-repeat'
" add pairs of useful maps
Plug 'tpope/vim-unimpaired'
" toggle comments
Plug 'tpope/vim-commentary'
" fix 'path' option
Plug 'tpope/vim-apathy'
" jump around text *way* easier
Plug 'easymotion/vim-easymotion'
" fuzzy find files
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
" lint
Plug 'dense-analysis/ale'
"----languages---------------------------------------------------------------
" c++ completion
Plug 'xavierd/clang_complete'
" csound syntax highlighting/completion
Plug 'luisjure/csound'
" clojure highlighting/repl/completion
Plug 'tpope/vim-fireplace'
" python completion
Plug 'davidhalter/jedi-vim'
" python formatter
Plug 'psf/black'
"----color themes------------------------------------------------------------
Plug 'Haron-Prime/Antares'
Plug 'ajh17/Spacegray.vim'
Plug 'axvr/photon.vim'
Plug 'fcpg/vim-farout'
Plug 'jnurmine/Zenburn/'
Plug 'kyoz/purify', { 'rtp': 'vim' }
Plug 'morhetz/gruvbox'
Plug 'nikolvs/vim-sunbather'
Plug 'whatyouhide/vim-gotham'
call plug#end()

"----------------------------------------------------------------------------
" configure options ---------------------------------------------------------
"----------------------------------------------------------------------------
" turn off intro message
set shortmess+=I
" turn off automatic backup and hidden swap file
set nobackup
set noswapfile
" must set this for color to work correctly
set background=dark
" set 256 colors (if not already set)
set t_Co=256
set termguicolors
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
" make join commands only insert 1 space (not 2) after '.' '?' or '!'
set nojoinspaces
" textwidth (what width paragraphs are formatted to)
set textwidth=78
" show commands (so we can see when timeouts happen)
set showcmd
" allow buffers to be hidden
set hidden
" turn off line wrapping
set nowrap
" turn on hybrid mode line numbers
set number         " turn on line numbers
set relativenumber " turn on relative line numbers
" display invisible characters
set list
" use these characters to display invisible characters
" set listchars=tab:>\ ,trail:.,precedes:<,extends:>
set listchars=tab:→\ ,trail:•,precedes:⟨,extends:⟩
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
set wildignorecase
set wildmenu
" set mapping delay
set timeoutlen=420
" set keycode delay (fixes perceptible latency moving from insert mode to normal mode)
set ttimeoutlen=1
" turn off interpreting leading zero numbers as octal
set nrformats-=octal
" turn on statusline
set laststatus=2
" stop redundant mode printing
set noshowmode
" create function to display mode
let g:currentmode={'n':'NORMAL', 'v':'VISUAL','V':'V-LINE','t':'TERMINAL',"\<C-V>":'V-BLOCK','i':'INSERT','R':'REPLACE','Rv':'V-REPLACE','c':'COMMAND'}
function! GetMode()
return get(g:currentmode, mode(), '       ')
endfunction
" configure status line
set statusline=
" current mode
set statusline+=\ %-{GetMode()}\ 
" file name
set statusline+=%f
" is modified?
set statusline+=%m
" switching to right section
set statusline+=%=
" file type
set statusline+=\ %y
" file encoding
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
" file line ending
set statusline+=\[%{&fileformat}]
" current line number
set statusline+=\ ln:%l/%{line('$')}
" current col number
set statusline+=\ cn:%c
" percentage scrolled through
set statusline+=\ %p%%

"----------------------------------------------------------------------------
" configure plugins ---------------------------------------------------------
"----------------------------------------------------------------------------
" turn on syntax highlighting
syntax on
" turn on filetype detection, plugin, and indentation
filetype plugin indent on

"--netrw
" disable the banner
let g:netrw_banner=0
" view file listing as a tree
let g:netrw_liststyle=3

"--text/markdown
augroup filetype_text
    " clear previous autocommands in this autocommand group
    autocmd!
    " everytime text is inserted or deleted, the paragraph will be formated
    autocmd FileType text,markdown set formatoptions+=a
    " when formatted, preserve list indentation
    autocmd FileType text,markdown set formatoptions+=n
    " whitespace continues paragraph
    " https://stackoverflow.com/a/21610187
    autocmd FileType text,markdown set formatoptions+=w
augroup END

"--c++
augroup filetype_cpp
    " clear previous autocommands in this autocommand group
    autocmd!
    let g:clang_library_path='/usr/lib/llvm-14/lib/libclang.so.1'
    " ale cpp options
    let g:ale_cpp_cc_options='-std=c++17 -Wall -Wextra -Wpedantic'
    " avoid needing to press shift for scope operator
    autocmd FileType cpp inoremap ;; ::
    " set c/c++ comment style to '//'
    autocmd FileType c,cpp setlocal commentstring=//\ %s
augroup END

"--python
augroup filetype_python
    " clear previous autocommands in this autocommand group
    autocmd!
    " auto format python according to pep8
    " autocmd BufWritePre *.py execute ':Black'
    " no automatic dot completion
    let g:jedi#popup_on_dot = 0
    " turn off jedi keybindings
    let g:jedi#rename_command = ""
    let g:jedi#goto_command = "gd"
    let g:jedi#goto_assignments_command = ""
    let g:jedi#goto_stubs_command = ""
    " let g:jedi#usages_command = ""
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

"--fzf
" place preview window thusly
let g:fzf_preview_window = ['up:50%', 'ctrl-/']

"--terminal
augroup terminal_buffer
    autocmd!
    " turn off line numbers in terminal buffers
    autocmd TerminalOpen * setlocal nonumber norelativenumber
    " set empty lines below end of buffer to <space>
    autocmd TerminalOpen * setlocal fillchars+=eob:\ 
augroup END


"----------------------------------------------------------------------------
" configure mappings --------------------------------------------------------
"----------------------------------------------------------------------------
" smart tab completion lovingly stolen from here:
"http://vim.wikia.com/wiki/Smart_mapping_for_tab_completion
function! Smart_TabComplete()

    " get the word before cursor
    let current_line = getline('.')
    let until_cursor = strpart(current_line, -1, col('.'))
    let substr       = matchstr(until_cursor, "[^ \t]*$")

    " nothing to match on empty string
    if (strlen(substr)==0)
        return "\<tab>"
    endif

    let has_period        = match(substr, '\.') != -1
    let has_forward_slash = match(substr, '\/') != -1
    let has_double_colon  = match(substr, '::') != -1
    if (!has_period && !has_forward_slash && !has_double_colon)
       " existing text matching (syntax omnicompletion)
        return "\<c-x>\<c-p>"
    elseif (has_forward_slash)
        " file matching
        return "\<c-x>\<c-f>"
    else
        " plugin matching
        return "\<c-x>\<c-o>"
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

" open vimrc (in new tab)
nnoremap <leader>v <esc>:tabnew $MYVIMRC<cr>
" open buffer (in window)
nnoremap <leader>e <esc>:e<space>
" open buffer (in new tab)
nnoremap <leader>t <esc>:tabnew<space>
" open terminal (in bottom right window)
nnoremap <leader>r <esc>:botright terminal<cr>
" open terminal (in new tab)
nnoremap <leader>rr <esc>:tab terminal<cr>
" save buffer
nnoremap <leader>w <esc>:w<cr>
" reload vimrc
noremap  <f5> :source $MYVIMRC<cr>
" format paragraph
nnoremap Q gqip
vnoremap Q gq
" close buffer
nnoremap <leader>x <esc>:bd<cr>
" close window or tab
nnoremap <leader>q <esc>:q<cr>
" force close
nnoremap <leader>qq <esc>:qa!<cr>
" command
nnoremap <leader><leader> <esc>:
" help
nnoremap <leader>h <esc>:h<space>
" make <tab> switch to alternate file in normal mode
nnoremap <tab> <c-^>
" make <c-^> (switch to alternate-file (the last buffer)) work in terminal
tnoremap <c-^> <c-w>:b#<cr>
tnoremap <c-6> <c-w>:b#<cr>
" make ctrl-d work as it does in terminal
tnoremap <c-d> <c-d>
" terminal enter normal mode https://github.com/vim/vim/issues/2216#issuecomment-337566816
tnoremap <esc><esc> <c-\><c-n>
" make terminal tab movement similar to window tab movement
tnoremap <c-pagedown>      <c-w>gt<cr>
tnoremap <c-pageup>        <c-w>gT<cr>
" indent while keeping visual highlighting
vnoremap > >gv
vnoremap < <gv
" map Shift-Arrow to window switching
nnoremap <silent> <s-up>         <c-w>k
nnoremap <silent> <s-down>       <c-w>j
nnoremap <silent> <s-right>      <c-w>l
nnoremap <silent> <s-left>       <c-w>h
inoremap <silent> <s-up>    <esc><c-w>ki
inoremap <silent> <s-down>  <esc><c-w>ji
inoremap <silent> <s-right> <esc><c-w>li
inoremap <silent> <s-left>  <esc><c-w>hi
tnoremap <silent> <s-up>         <c-w>k
tnoremap <silent> <s-down>       <c-w>j
tnoremap <silent> <s-right>      <c-w>l
tnoremap <silent> <s-left>       <c-w>h
" map Ctrl-Shift-Arrow to window dimensions
nnoremap <silent> <c-s-up>         <c-w>+
nnoremap <silent> <c-s-down>       <c-w>-
nnoremap <silent> <c-s-right>      <c-w>>
nnoremap <silent> <c-s-left>       <c-w><
inoremap <silent> <c-s-up>    <esc><c-w>+
inoremap <silent> <c-s-down>  <esc><c-w>-
inoremap <silent> <c-s-right> <esc><c-w>>
inoremap <silent> <c-s-left>  <esc><c-w><
tnoremap <silent> <c-s-up>         <c-w>+
tnoremap <silent> <c-s-down>       <c-w>-
tnoremap <silent> <c-s-right>      <c-w>>
tnoremap <silent> <c-s-left>       <c-w><
" map Ctrl-Shift-Page{Up,Down} for tab reordering
nnoremap <c-s-pageup>        :tabmove -1<cr>
nnoremap <c-s-pagedown>      :tabmove +1<cr>
inoremap <c-s-pageup>   <esc>:tabmove -1<cr>
inoremap <c-s-pagedown> <esc>:tabmove +1<cr>
tnoremap <c-s-pageup>   <c-w>:tabmove -1<cr>
tnoremap <c-s-pagedown> <c-w>:tabmove +1<cr>
" toggle spellchecking
function! ToggleSpell()
    set spell!
    if &spell
        echo 'spellcheck on'
        nnoremap <buffer> s 1z=
    else
        echo 'spellcheck off'
        nnoremap <buffer> s s
    endif
endfunction
nnoremap <leader>s :call ToggleSpell()<cr>
" easymotion: find (must be nmap not nnoremap)
nmap <leader>f <plug>(easymotion-overwin-w)
" fugitive: git blame
nnoremap gb :Git blame<cr>
" fzf: fuzzy find file
nnoremap <leader>p :Files!<cr>
" fzf: buffers
nnoremap <leader>b :Buffers!<cr>
" fzf: grep
nnoremap <leader>g :Rg!<space>
" fzf: grep <cword>
nnoremap gh :Rg!<space><c-r><c-w><cr>
" ale: cycle warnings
nnoremap ]a :ALENext<cr>
nnoremap [a :ALEPrevious<cr>
" ale: print warning
nnoremap <leader>a :ALEDetail<cr>
" tab movement
nnoremap [t gT<cr>
nnoremap ]t gt<cr>

"----------------------------------------------------------------------------
" configure colorscheme -----------------------------------------------------
"----------------------------------------------------------------------------
" select colorscheme and fail silently otherwise
" silent! color farout
" silent! color gotham
" silent! color gruvbox
" silent! color photon
" silent! color purify
" silent! color spacegray
" silent! color sunbather
let g:zenburn_high_Contrast=1 " <--- configure zenburn
silent! color zenburn

