""""""""""""""""""""""""""""""""""""""""""""""""""
" https://github.com/junegunn/vim-plug
"
""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'craigemery/vim-autotag'
Plug 'kchmck/vim-coffee-script'
Plug 'groenewege/vim-less'
Plug 'altercation/vim-colors-solarized'
Plug 'godlygeek/tabular'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown'
Plug 'mattn/emmet-vim'
Plug 'jpo/vim-railscasts-theme'
Plug 'scrooloose/nerdcommenter'
Plug 'aaronbieber/quicktask'
Plug 'itchyny/lightline.vim'
Plug 'nanotech/jellybeans.vim'
Plug 'gcmt/breeze.vim'
Plug 'gregsexton/gitv'
Plug 'pangloss/vim-javascript'
Plug 'ekalinin/Dockerfile.vim'
Plug 'junegunn/fzf.vim'
Plug 'lvht/mru'
Plug 'digitaltoad/vim-pug'
Plug 'cfebs/vim-gh-line'
Plug 'posva/vim-vue'
Plug 'rust-lang/rust.vim'
Plug 'lambdatoast/elm.vim'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'hashivim/vim-terraform'
Plug 'c9s/helper.vim'
Plug 'c9s/treemenu.vim'
Plug 'c9s/vikube.vim'
Plug 'junegunn/goyo.vim'
Plug 'dense-analysis/ale'
Plug 'martinda/Jenkinsfile-vim-syntax'
Plug 'fgsch/vim-varnish'
Plug 'maksimr/vim-jsbeautify'
Plug 'tpope/vim-vinegar'
Plug 'dart-lang/dart-vim-plugin'
"" Plug 'fatih/vim-go'
Plug 'jvirtanen/vim-hcl'
Plug 'jakwings/vim-pony'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'evanleck/vim-svelte', { 'branch': 'main' }
Plug 'vim-crystal/vim-crystal'

set fileformat=unix
set fileformats=unix,dos

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""
" Fundamentals
""""""""""""""""""""""""""""""""""""""""""""""""""
set nocp
filetype plugin indent on

set guicursor=

set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

set history=10000
"set cursorline
let mapleader = ","
set autoread
set ruler
set backspace=eol,start,indent
set hidden
set scrolloff=3
" Prevent Vim from clobbering the scrollback buffer. See
" http://www.shallowsky.com/linux/noaltscreen.html
set t_ti= t_te=
set shell=bash
"set shellcmdflag=-ic
set nonu
set mouse-=a

set backupcopy=yes
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp

"" Noice
nnoremap ; :

"" Markdown
let g:markdown_syntax_conceal = 0
nmap <leader>1 VypVr=
nmap <leader>2 VypVr-
nmap <leader>3 I### <Esc>
nmap <leader>4 I### <Esc>

"" Pasting
nmap <leader>p :setlocal paste! paste?<cr>

"" Searching
set ignorecase
set smartcase
set hlsearch
set incsearch
set showmatch

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colors
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"" Solarized! This works for solarized + allaritty
let g:solarized_termcolors= 16
let g:solarized_termtrans = 1
set background=dark
"set background=light
colorscheme solarized

"" Dracula
"colorscheme dracula

"let g:dracula_colorterm = 0
"set t_Co=256   " 256 colors
"set t_Co=16
"colorscheme railscasts
"colorscheme desert
"colorscheme lucius
"colorscheme jellybeans
"colorscheme dracula
set lazyredraw
syntax on
" avoid lines > 100 lines
set colorcolumn=100

"" Tabbing
set ai
set expandtab
set shiftwidth=4
set tabstop=4
set smarttab

"" Wilds
set wildmenu
set wildmode=longest,list
set wildignore+=*.so,*.swp,*/.git/*,.gitkeep

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Status
"" Raw status line ditched for lightline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"set statusline=\ %{HasPaste()}\ %<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)
"set statusline+=\ %{fugitive#statusline()}

set laststatus=2

""""""""""""""""""""""""""""""""""""""""""""""""""
"" Experimental Turn OFF
""""""""""""""""""""""""""""""""""""""""""""""""""
"set cpo-=<
"set wcm=<C-Z>

""""""""""""""""""""""""""""""""""""""""""""""""""
"" Auto Commands
""""""""""""""""""""""""""""""""""""""""""""""""""

"" Whitespace
augroup vimrcEx
  autocmd!
  autocmd FileType text setlocal textwidth=78

  " Jump to last cursor position unless it's invalid or in an event handler
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
  autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
  autocmd InsertLeave * match ExtraWhitespace /\s\+$/
  autocmd BufWinLeave * call clearmatches()

  "" 4 spaces
  autocmd FileType python,javascript setlocal tabstop=4 softtabstop=4 shiftwidth=4

  "" 2 spaces
  autocmd FileType crystal,coffee,ruby,haml,eruby,html,sass,cucumber,pug,haskell setlocal ai sw=2 sts=2 et

  autocmd FileType tsv setlocal ai noexpandtab sw=4 sts=4 tabstop=4
  autocmd FileType phtml setlocal ai et sw=4 sts=4 tabstop=4
  autocmd FileType go setlocal noexpandtab ai sw=4 sts=4 tabstop=4
  autocmd! FileType yaml setlocal ts=2 sts=2 sw=2 expandtab indentkeys-=0# indentkeys-=<:>

  "" Custom fts
  autocmd! BufRead,BufNewFile *.jade set ft=pug
  autocmd! BufRead,BufNewFile *.es6 set ft=javascript.jsx
  autocmd! BufRead,BufNewFile *.sass set ft=sass
  autocmd! BufRead,BufNewFile *.phtml,*.firt set ft=phtml
  autocmd! BufRead,BufNewFile *.fir,*.jhtml set ft=html
  autocmd! BufRead,BufNewFile *.jade set ft=javascript.jsx
  autocmd! BufRead,BufNewFile *.tsx,*.jsx set ft=typescript.tsx
  autocmd! BufRead,BufNewFile *.tsv set ft=tsv
  autocmd! BufRead,BufNewFile *.j2 set ft=jinja
  autocmd! BufRead,BufNewFile .gitconfig-* set ft=gitconfig

  "" 2 spaces vimeo frontend
  au BufRead,BufNewFile,BufEnter *vimeo/frontend/*.ts setlocal ts=2 sts=2 sw=2
  au BufRead,BufNewFile,BufEnter *vimeo/frontend/*.tsx setlocal ts=2 sts=2 sw=2

  autocmd! FileType gitcommit setlocal spell

  autocmd FileType json setlocal conceallevel=0

  autocmd BufEnter term://* startinsert
augroup end

function! SetLocalTabSize()
    let size = input('Tab Size: ')
    execute "set ts=" . size . " sts=" . size . " sw=" . size
endfunction

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
nnoremap <silent> <Leader>ws :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"" clipboard
if executable('xclip')
    vmap <Leader>xc :w !xclip -selection clipboard<cr>
endif

"" Quick leader functions
nnoremap <leader><leader> <c-^>
nnoremap <leader><cr> :noh<cr>
nnoremap <leader>o :only<cr>
nnoremap <leader>x :clo<cr>
imap <c-c> <esc>


"" Split/window nav
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

"" Quit
nnoremap <C-q> :qall!<CR>

"" Quick file access
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ev :vsp $MYVIMRC<cr>
nnoremap <leader>eb :vsp ~/.bashrc<cr>
nnoremap <leader>eba :vsp ~/.bash_aliases<cr>

nnoremap <S-t> :tab split<cr>
nnoremap <S-h> gT
nnoremap <S-l> gt
tnoremap <S-h> <C-\><C-n>gT
tnoremap <S-l> <C-\><C-n>gt
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-l> <C-\><C-n><C-w>l
tnoremap <Esc> <C-\><C-n>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" kill arrow keys
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>

"" %% expands to the current directory
cnoremap %% <C-R>=expand('%:h').'/'<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" rename current file
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>n :call RenameFile()<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  "let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  "let g:ctrlp_use_caching = 0
endif

"command! FZFMru call fzf#run({
"\ 'source':  s:all_files(),
"\ 'sink':    'edit',
"\ 'options': '-m -x +s',
"\ 'down':    '40%' })

function! s:all_files()
  return extend(
  \ filter(copy(v:oldfiles),
  \        "v:val !~ 'fugitive:\\|NERD_tree\\|^/tmp/\\|.git/'"),
  \ map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), 'bufname(v:val)'))
endfunction

"" fzf
let g:fzf_preview_window = []
nmap <silent> <Leader>t :Files<CR>
nmap <silent> <Leader>f :GitFiles<CR>
nmap <silent> <Leader>b :Buffers<CR>
nmap <silent> <Leader>m :History<CR>

"" Breeze
let g:breeze_active_filetypes = "*.html,*.htm,*.xhtml,*.xml,*.phtml"

"" Fugitive
nmap <silent> <leader>gs :Git<cr>
nmap <silent> <leader>gf :Gdiff<cr>
nmap <silent> <leader>gd :Git diff<cr>
nmap <silent> <leader>gc :Git diff --cached<cr>
nmap <silent> <leader>go :Git commit<cr>
set diffopt+=vertical

"" Gitv
let g:Gitv_DoNotMapCtrlKey = 1

"" Lightline
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'fugitive', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component': {
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ }
      \ }

" wrap current visual selection in translate calls
function! VisualBlockWrap(left, right)
    if (len(a:left) > 0 && len(a:right) > 0)
        normal `>
        if &selection == 'exclusive'
            exe "normal i".a:right
        else
            exe "normal a".a:right
        endif
        normal `<
        exe "normal i".a:left
        normal `<
    endif
endfunction

function! InsertLogger()
    if (&ft == 'php')
        normal osyslog(LOG_INFO, json_encode([]));
        normal F[
        normal 1l
        startinsert

    elseif (&ft == 'js' || &ft == 'javascript')
        normal oconsole.log();
        normal F(
        normal 1l
        startinsert
    endif
endfunction

function! InsertTemplateTag()
    set nosmarttab
    set noai
    if (&ft == 'phtml')
        normal i<?  ?>
        normal 2h
    endif
    set smarttab
    set ai
endfunction

function! InsertTemplateTagOut()
    set nosmarttab
    set noai
    if (&ft == 'phtml')
        normal i<?=  ?>
        normal 2h
    endif
    set smarttab
    set ai
endfunction

" xmap == only visual mode map, no select mode
xnoremap <leader>vit :call VisualBlockWrap("<?= $this->translate('", "') ?>")<cr>
xnoremap <leader>vir :call VisualBlockWrap("$this->translate('", "')")<cr>

noremap <leader>vt :call InsertTemplateTag()<cr>
noremap <leader>vo :call InsertTemplateTagOut()<cr>
noremap <leader>vl :call InsertLogger()<cr>

set listchars=eol:$,tab:>-,trail:~,extends:>,precedes:<

let g:gh_line_map_default = 0
let g:gh_line_map = '<leader>gh'
let g:gh_open_command = 'xdg-open '
let g:gh_use_canonical = 1

let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 0

"""" Ale

"" highlight SignColumn ctermbg=Black
highlight clear SignColumn
let g:ale_sign_error = '● '
let g:ale_sign_warning = '· '

"" For debugging ale LSP input/output
"" https://github.com/dense-analysis/ale/issues/2137#issuecomment-797468471
"" let g:ale_command_wrapper = '~/bin/ale_wrapper.sh'

"" For launching psalm with an explicit wrapper
"" Ripped from https://github.com/dense-analysis/ale/blob/master/ale_linters/php/psalm.vim
call ale#linter#Define('php', {
\   'name': 'psalm_custom',
\   'lsp': 'stdio',
\   'executable': '/home/collin/bin/psalm_wrapper.sh',
\   'command': function('ale_linters#php#psalm#GetCommand'),
\   'project_root': function('ale_linters#php#psalm#GetProjectRoot'),
\})

let g:ale_completion_enabled = 0
set omnifunc=ale#completion#OmniFunc
inoremap <silent> <C-Space> <C-\><C-O>:ALEComplete<CR>

"let g:ale_python_pylint_executable = "find-pylint.sh"

" govet broken
"\ '\.go$': {'ale_linters': [ 'gopls', 'govet', 'gofmt', 'gobuild' ], 'ale_fixers': ['gofmt', 'goimports']},
"" \ '\.go$': {'ale_linters': [ 'gopls', ], 'ale_fixers': ['gofmt', 'goimports']},
let g:ale_pattern_options = {
\ '\.sql$': {'ale_linters': [ ], 'ale_fixers': []},
\ '\.rb$': {'ale_linters': [ 'ruby', 'solargraph' ], 'ale_fixers': []},
\ '\.erb$': {'ale_linters': [ 'erb' ], 'ale_fixers': []},
\ '\.php$': {'ale_linters': [ 'php', 'psalm_custom' ], 'ale_fixers': [ ]},
\ '\.js$': {'ale_linters': [ 'eslint', 'tsserver' ], 'ale_fixers': []},
\ '\.py$': {'ale_linters': [ 'pylint', 'jedils' ], 'ale_fixers': []},
\ '\.phtml$': {'ale_linters': [ 'prettier' ], 'ale_fixers': []},
\ '\.tf$': {'ale_linters': ['terraform'], 'ale_fixers': ['terraform']},
\ '\.go$': {'ale_linters': [ 'gopls', ], 'ale_fixers': ['gofmt', 'goimports']},
\ '^.*vimeo\/frontend.*\.tsx$': {'ale_linters': [ 'eslint', 'standard', 'tslint', 'tsserver', 'typecheck', 'xo' ], 'ale_fixers': ['prettier']},
\ '^.*vimeo\/frontend.*\.ts$': {'ale_linters': [ 'eslint', 'standard', 'tslint', 'tsserver', 'typecheck', 'xo' ], 'ale_fixers': ['prettier']},
\}

" If you configure g:ale_pattern_options outside of vimrc, you need this.
let g:ale_pattern_options_enabled = 1
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_enter = 0
let g:ale_lint_on_save = 1
let g:ale_fix_on_save = 1

let g:ale_history_enabled = 1
let g:ale_history_log_output = 1

nnoremap <leader>af :ALEFix<cr>
nnoremap <leader>at :ALEGoToTypeDefinition<cr>
nnoremap <leader>ad :ALEGoToDefinition<cr>
nnoremap <leader>ae :ALEDetail<cr>
nnoremap <leader>aa :ALEHover<cr>
nnoremap <leader>ai :ALEInfo<cr>

" let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck']
let g:go_fmt_fail_silently = 1
let g:go_metalinter_enabled = []

let g:vim_json_syntax_conceal = 0

" annoying sql omni complete disable
let g:ftplugin_sql_omni_key = '<Plug>DisableSqlOmni'


""" Completion
"" TODO

""" Rust
let g:rustfmt_autosave = 1

""" Goyo
nnoremap <silent><leader>vv :Goyo<cr>:set linebreak<cr>:set wrap<cr>
