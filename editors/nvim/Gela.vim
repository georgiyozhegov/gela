" @aigenerated

if exists("b:current_syntax")
  finish
endif

syntax match gelaComment "#.*$"
syntax keyword gelaKeyword let var in if then else
syntax match gelaOperator "->\|+\|-\|\$\/\|\*"
syntax match gelaNumber "\<\d\+\>"
syntax region gelaString start=+\"+ skip=+\\\\\|\\"+ end=+\"+
syntax match gelaName "\<[A-Za-z_][A-Za-z0-9_]*\>"
syntax match gelaParenthesis "[()]"
syntax match gelaEqual "="

highlight link gelaComment Comment
highlight link gelaKeyword Keyword
highlight link gelaOperator Operator
highlight link gelaNumber Number
highlight link gelaString String
highlight link gelaName Identifier
highlight link gelaParenthesis Delimiter
highlight link gelaEqual Operator

let b:current_syntax = "gela"
