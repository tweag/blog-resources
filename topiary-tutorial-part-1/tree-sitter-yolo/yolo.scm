; Do not mess with spaces within strtings
(string) @leaf

; Do not remove empty lines between statements, for readability and space
(statement) @allow_blank_line_before

; Always surround keywords with spaces
[
  "="
  ">"
  "<"
  "&"
  "|"
  "_"
  "=>"
  "+"
  "-"
  "*"
  ":="
] @prepend_space @append_space

; Add space after `input` and `output` decl
[
  "input"
  "output"
] @append_space

; Add a space after and remove space before the comma in an identifier list
(
 (identifier)
 .
 "," @prepend_antispace @append_space
 .
 (identifier)
)

; Add a newline between two consecutive statements
(
  (statement) @append_hardline
  .
  (statement)
)

; Lay out the case skeleton
(case
  "{" @prepend_space @append_hardline
  "}" @prepend_hardline
)

; Indent the content of case
(case
  "{" @append_indent_start
  "}" @prepend_indent_end
)

; Put case branches on their own lines
(case
  "," @append_hardline
)
