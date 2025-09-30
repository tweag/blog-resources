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
  "-"
  "*"
  ":="
] @prepend_space @append_space

; Creates a scope for the whole right-hand side of a definition statement
(definition_statement
  (#scope_id! "definition_rhs")
  ":="
  (expression) @prepend_begin_scope @append_end_scope
)

; (Multi-line) spacing around +
(
  (#scope_id! "definition_rhs")
  "+" @prepend_spaced_scoped_softline @append_space
)

; Add spaced softline after `input` and `output` decl
[
  "input"
  "output"
] @append_spaced_softline


; Add a spaced softline after and remove space before the comma in an identifier
; list
(
 (identifier)
 .
 "," @prepend_antispace @append_spaced_softline
 .
 (identifier)
)

; Indent multi-line lists of inputs.
(input_statement
  "input" @append_indent_start
) @append_indent_end

; Indent multi-line lists of outputs.
(output_statement
  "output" @append_indent_start
) @append_indent_end

; Add a newline between two consecutive statements
(
  (statement) @append_hardline
  .
  (statement)
)

; Lay out the case skeleton
(case
  "{" @prepend_space @append_spaced_softline
  "}" @prepend_spaced_softline
)

; Indent the content of case
(case
  "{" @append_indent_start
  "}" @prepend_indent_end
)

; Put case branches on their own lines
(case
  "," @append_spaced_softline
)
