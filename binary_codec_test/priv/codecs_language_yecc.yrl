Nonterminals PROTOCOL_FILE HEADER MODULE_INFO VERSION_INFO PACKETS PACKET PACKET_SIGNATURE VARIABLES VARIABLE.

Terminals label params type_colon type new_line number version.

Rootsymbol PROTOCOL_FILE.

Left 100 new_line.
%Unary 200 label.

PROTOCOL_FILE -> HEADER : '$1'.
%PROTOCOL_FILE -> HEADER new_line : '$1'.
PROTOCOL_FILE -> PACKETS : {packets, '$1'}.
PROTOCOL_FILE -> HEADER new_line PACKETS : {'$1', {packets, '$3'}}.

HEADER -> MODULE_INFO new_line VERSION_INFO : hd('$1', '$3').

MODULE_INFO  -> label type_colon number  : '$3'.
VERSION_INFO -> label type_colon version : '$3'.

PACKETS -> PACKETS new_line PACKETS : ['$1' | '$2'].
PACKETS -> PACKET : ['$1'].
%PACKETS -> PACKET new_line : ['$1'].

PACKET -> PACKET_SIGNATURE new_line VARIABLES : pk('$1', '$3').
PACKET -> PACKET_SIGNATURE : '$1'.
PACKET_SIGNATURE -> label params : pk('$1', '$2').

VARIABLES -> VARIABLE : [$1].
VARIABLES -> VARIABLES new_line VARIABLE : [$1 | $3].

VARIABLE -> label type_colon type : vr($1, $3).

Erlang code.

-record(header, {module_id, version}).

-record(variable, {label, type}).

-record(packet, {label, params, variables=[]}).

hd({number, _, Number}, {version, _, Version}) -> #header{module_id = Number, version = Version}.

vr({label, _, Label}, {type, _, Type}) -> #variable{label = Label, type = Type}.

pk(#packet{} = Packet, Variables) -> Packet#packet{variables = Variables};
pk({label, _, Label}, {params, _, Params}) -> #packet{label = Label, params = Params}.

pk(Label, Params, Variables) ->
  Packet = pk(Label, Params),
  Packet#packet{variables = Variables}.
