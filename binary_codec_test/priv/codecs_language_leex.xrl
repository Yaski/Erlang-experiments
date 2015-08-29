Definitions.

Type  = (uint8|int32|float|string)
Label = ([a-z]+[_0-9a-z]*)
HexNumber = (#[0-9A-F]+)
Version = ([0-9]+(\.[0-9]+)?)

Rules.

{Type} : {token, {type, TokenLine, list_to_atom(TokenChars)}}.

{HexNumber} : {token, {number, TokenLine, parse_hex(TokenChars)}}.

{Version} : {token, {version, TokenLine, parse_version(TokenChars)}}.

client_{Label} : {token, {def, TokenLine, {client, string:substr(TokenChars, 8)}}}.
server_{Label} : {token, {def, TokenLine, {server, string:substr(TokenChars, 8)}}}.

{Label} : {token, {label, TokenLine, TokenChars}}.

\([.]*\) : {token, {params, TokenLine, string:substr(TokenChars, 2, TokenLen - 2)}}.

: : {token, {type_colon, TokenLine}}.

[\n\r]+ : {token, {new_line, TokenLine}}.

[\s\t]+ : skip_token.

Erlang code.

parse_version(TokenChars) -> {string:sub_word(TokenChars, 1, $.), string:sub_word(TokenChars, 2, $.)}.

parse_hex(TokenChars) -> erlang:list_to_integer(string:substr(TokenChars, 2), 16).
