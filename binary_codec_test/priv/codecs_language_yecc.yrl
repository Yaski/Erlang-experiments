Nonterminals PROTOCOL_FILE MODULE_INFO VERSION_INFO PACKETS PACKET PACKET_SIGNATURE VARIABLES VARIABLE.

Terminals label params type_colon type new_line number version peof.

Rootsymbol PROTOCOL_FILE.

PROTOCOL_FILE -> MODULE_INFO new_line VERSION_INFO new_line PACKETS : ['$1', '$3', {packets, '$5'}].

MODULE_INFO  -> label type_colon number  : md('$3').
VERSION_INFO -> label type_colon version : vs('$3').

PACKETS -> PACKET PACKETS : ['$1' | '$2'].
PACKETS -> PACKET : ['$1'].

PACKET -> PACKET_SIGNATURE : '$1'.
PACKET -> PACKET VARIABLES peof new_line : pk('$1', '$2').
PACKET_SIGNATURE -> label params new_line : pk('$1', '$2').

VARIABLES -> VARIABLE : ['$1'].
VARIABLES -> VARIABLE VARIABLES : ['$1' | '$2'].

VARIABLE -> label type_colon type new_line : vr('$1', '$3').

Erlang code.

md({number, _, Number}) -> {module, Number}.
vs({version, _, {V0, V1}}) -> {version, [V0, V1]}.

vr({label, _, Label}, {type, _, Type}) -> [{name, Label}, {type, Type}].

pk({label, _, Label}, {params, _, Params}) -> [{name, Label}, {params, atom_to_list(Params)}];
pk(Packet, Vars) -> [{variables, Vars} | Packet].
