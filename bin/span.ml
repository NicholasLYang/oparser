type span = { start_index : int; end_index : int }
type 'a spanned = { value : 'a; span : span }

let s value ~start_index ~end_index =
  { value; span = { start_index; end_index } }

let value spanned = spanned.value
let span spanned = spanned.span
let start_index span = span.start_index
let end_index span = span.end_index
