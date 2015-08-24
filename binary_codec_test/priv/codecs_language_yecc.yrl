%Nonterminals protocol_file packet variable variables struct variables_sep.
Nonterminals PROTOCOL_FILE PACKET.

Terminals label params type_colon type new_line tuple_start tuple_end comma.

Rootsymbol protocol_file.


PROTOCOL_FILE -> PACKET : ['$1'].

PROTOCOL_FILE -> PACKET new_line PACKET : [$1 | $3].

PACKET -> label params : 
  pk('$1', '$2').

%packet -> label params new_line variables : 
%  pk('$1', '$2', '$4').


%variables_sep -> comma .

%variables_sep -> comma new_line .

%variable -> type : 
%  vr('$1').

%variable -> label type_colon type : 
%  vr('$1', '$3').

%variables -> variable :
%  vrs('$1').

%variables -> variable variables_sep variables :
%  vrs('$1', '$3').

%variable -> struct :
%  '$1'.

%truct -> tuple_start tuple_end :
%  st().

%struct -> tuple_start variables tuple_end :
%  st('$2').

%struct -> label type_colon struct :
%  st('$1', '$3').



Erlang code.

-record(struct, {label, content}).

-record(variable, {label, type}).

-record(packet, {label, params, variables}).

%st({label, _, Label}, Struct) -> Struct#struct{label = Label};
%st(List) -> #struct{content = List};
%st() -> #struct{}.

%vr({label, _, Label}, {type, _, Type}) -> #variable{label = Label, type = Type};
%vr({type, _, Type}) -> #variable{type = Type}.

%vrs(Var, List) -> [Var | List];
%vrs(Var) -> [Var].

pk({label, _, Label}, {params, _, Params}) -> #packet{label = Label, params = Params}.

pk(Label, Params, Variables) ->
  Packet = pk(Label, Params),
  Packet#packet{variables = Variables}.
