" ============================================================="
" Name:        deeperblue
" Description: A port of the Emacs color scheme of the same name
" Author:      Stephen Dickinson
" ============================================================="

hi clear
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "deeperblue"

" ============================================================="
" UI Elements
" ============================================================="

hi Normal         guifg=#cccccc guibg=#181a26
hi LineNr         guifg=#90ee90 guibg=#181a26
hi StatusLine     guifg=#181a26 guibg=#bfbfbf gui=NONE
hi StatusLineNC   guifg=#181a26 guibg=#00bfff gui=NONE
hi Visual         guifg=#cccccc guibg=#103050

hi WildMenu       guifg=#181a26 guibg=#00bfff
hi Pmenu          guifg=#00bfff guibg=#181a26
hi PmenuBorder    guifg=#00bfff guibg=#00bfff
hi PmenuSel       guifg=#181a26 guibg=#00bfff
hi PmenuSbar      guibg=#cccccc
hi PmenuThumb     guifg=#00bfff guibg=#00bfff

" ============================================================="
" Syntax Highlighting
" ============================================================="

hi Comment        guifg=#7f7f7f               gui=italic
hi Constant       guifg=#a2cd5a
hi String         guifg=#deb887
hi Identifier     guifg=#daa520
hi Function       guifg=#daa520

hi Statement      guifg=#00bfff
hi Keyword        guifg=#00bfff
hi! link Conditional Normal
hi! link Repeat Normal
hi! link Label Normal
hi! link Operator Normal
hi! link Exception Normal

hi! link PreProc Normal
hi! link Include Normal
hi! link Define Normal
hi! link Macro Normal
hi! link PreCondit Normal

hi! link Type Normal
" hi StorageClass guifg=#5fafd7 ctermfg=74
" hi Structure    guifg=#5fafd7 ctermfg=74
" hi Typedef      guifg=#5fafd7 ctermfg=74

" hi Special      guifg=#d75f87 ctermfg=168
" hi SpecialChar  guifg=#d75f87 ctermfg=168
" hi Tag          guifg=#d75f87 ctermfg=168
" hi Delimiter    guifg=#d75f87 ctermfg=168
" hi SpecialComment guifg=#d75f87 ctermfg=168
" hi Debug        guifg=#d75f87 ctermfg=168

" hi Underlined   gui=underline cterm=underline
" hi Ignore       guifg=#444444 ctermfg=238
" hi Error        guifg=#ffffff guibg=#d70000 ctermfg=15 ctermbg=160
" hi Todo         guifg=#000000 guibg=#ffff5f ctermfg=0 ctermbg=227

" ============================================================="
" Diff (useful for git)
" ============================================================="

hi DiffAdd        guifg=#cccccc guibg=#556b2f
hi DiffChange     guifg=#cccccc guibg=#104e8b
hi DiffDelete     guifg=#cccccc guibg=#8b3a3a
hi DiffText       guifg=#cccccc

" ============================================================="
" Search
" ============================================================="

hi Search         guifg=#cccccc guibg=#5f9ea0
hi IncSearch      guifg=#cccccc guibg=#ee6a50
hi CurSearch      guifg=#cccccc guibg=#ee6a50

" ============================================================="
" LSP
" ============================================================="

hi ALEErrorSign   guifg=#cccccc guibg=#ff0000
hi ALEWarningSign guifg=#cccccc guibg=#ffff00
hi ALEInfoSign    guifg=#cccccc guibg=#87cefa

" ============================================================="
" End of file
" ============================================================="

