Definitions.

Type  = (uint8|int32|float|string|uint8\[\])
Label = ([a-z]+[_0-9a-z]*)
HexNumber = (#[0-9A-F]+)
Version = ([0-9]+(\.[0-9]+)?)

Rules.

{Type} : {token, {type, TokenLine, list_to_atom(TokenChars)}}.

{HexNumber} : {token, {number, TokenLine, parse_hex(TokenChars)}}.

{Version} : {token, {version, TokenLine, parse_version(TokenChars)}}.

{Label} : {token, {label, TokenLine, TokenChars}}.

\(.*\) : {token, {params, TokenLine, parse_params(string:substr(TokenChars, 2, TokenLen - 2))}}.

: : {token, {type_colon, TokenLine}}.

[\n\r]+ : {token, {new_line, TokenLine}}.

\. : {token, {peof, TokenLine}}.

[\s\t]+ : skip_token.

Erlang code.

parse_version(TokenChars) -> {string:sub_word(TokenChars, 1, $.), string:sub_word(TokenChars, 2, $.)}.

parse_hex(TokenChars) -> erlang:list_to_integer(string:substr(TokenChars, 2), 16).

parse_params([]) -> noparams;
parse_params(Params) -> list_to_atom(Params).