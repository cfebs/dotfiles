" Switch to pathogen
runtime bundle/pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

"set t_Co=256
set t_Co=16
set history=1000
let mapleader = ","
set autoread
set nocp
filetype plugin indent on
set ruler
set backspace=eol,start,indent

" created by the manage script
set backupdir=$HOME/.vim/.vimbackup/
set directory=$HOME/.vim/.vimswp/

"" Noice
nnoremap ; :
map <C-J> <C-W>j
map <C-K> <C-W>k

"" Markdown
nmap <leader>1 VypVr=
nmap <leader>2 VypVr-
nmap <leader>3 I### <Esc>
nmap <leader>4 I### <Esc>

"" Searching
set ignorecase
set smartcase
set hlsearch
set incsearch
set showmatch

"" Colors
syntax enable
colorscheme solarized
set background=dark
set nonu
set nolazyredraw

"" Tabbing
set ai
set expandtab
set shiftwidth=2
set tabstop=2
set smarttab

"" Status
set laststatus=2
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:\ %l/%L:%c
set statusline+=\ %{fugitive#statusline()}

function! CurDir()
    let home = '/home/collin/'
    let curdir = substitute(getcwd(), home, "~/", "g")
    return curdir
endfunction

function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    else
        return ''
    endif
endfunction


set hidden
source $VIMRUNTIME/menu.vim
set wildmenu
set wildmode=list:longest
set wildignore+=*tmp/*,*.so,*.swp,*.zip,*/.git/*,.gitkeep

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|pyc)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }

set cpo-=<
set wcm=<C-Z>

"" whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

"" Python
autocmd FileType python setlocal tabstop=4 softtabstop=4 shiftwidth=4

"" Spell Check
map <leader>ss :setlocal spell!<cr>
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=


"""" Plugins

""" org
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org            call org#SetOrgFileType()

"" ctrlp
nmap <silent> <Leader>t :CtrlP<CR>
nmap <silent> <Leader>d :CtrlPDir<CR>
nmap <silent> <Leader>b :CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = ''

"" tabman
let g:tabman_number = 0

"" Tabularize
if exists(":Tabularize")
 nmap <Leader>a= :Tabularize /=<CR>
 nmap <Leader>a: :Tabularize /:\zs<CR>
 nmap <Leader>a, :Tabularize /,\zs<CR>
endif
