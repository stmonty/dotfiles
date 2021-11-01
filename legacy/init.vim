call plug#begin()
 Plug 'morhetz/gruvbox'
 Plug 'sheerun/vim-polyglot'
 Plug 'vim-airline/vim-airline-themes'
 Plug 'arcticicestudio/nord-vim'
 Plug 'vim-airline/vim-airline'
 Plug 'dracula/vim'
 Plug 'honza/vim-snippets'
 Plug 'scrooloose/nerdtree'
 Plug 'Yggdroot/indentLine'
 Plug 'preservim/nerdcommenter'
 Plug 'mhinz/vim-startify'
 Plug 'neoclide/coc.nvim', {'branch': 'release'}
 Plug 'ryanoasis/vim-devicons'
call plug#end()

"Mappings
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l
map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1
inoremap jk <ESC>
let mapleader = " "
syntax on
set noswapfile
set number
set hlsearch
set ignorecase
set incsearch

colorscheme gruvbox
let g:airline_theme='dark'
let g:gruvbox_enable_italic=1
