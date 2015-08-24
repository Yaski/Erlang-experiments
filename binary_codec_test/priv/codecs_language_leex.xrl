Definitions.

Type  = (byte|int32|uint32|ubyte|float|string)
Label = ([a-z]+[_0-9a-z]*)

Rules.

{Label} : {token, {label, TokenLine, TokenChars}}.

\(session\) : {token, {params, TokenLine, session}}.

\(nosession\) : {token, {params, TokenLine, nosession}}.

: : {token, {type_colon, TokenLine}}.
{Type} : {token, {type, TokenLine, list_to_atom(TokenChars)}}.

[\n\r]+ : {token, {new_line, TokenLine}}.

\{ : {token, {tuple_start, TokenLine}}.
\} : {token, {tuple_end, TokenLine}}.

, : {token, {comma, TokenLine}}.

[\s\t]+ : skip_token.

Erlang code.
