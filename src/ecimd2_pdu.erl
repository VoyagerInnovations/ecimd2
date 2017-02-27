%% @private CIMD2 PDU related functions
-module(ecimd2_pdu).

-export([
  login/3,
  alive/1,
  parse/1
]).

%% ----------------------------------------------------------------------------
%% Exposed API
%% ----------------------------------------------------------------------------

%% @private Creates a login packet
login(PNum, Username, Password) ->
  ParamMap = #{
    <<"010">> => Username,
    <<"011">> => Password
  },
  Params = build_params(ParamMap),
  <<2, "01:", PNum/binary, Params, 3>>.

%% @private Creates an alive packet
alive(PNum) ->
  <<2, "40:", PNum/binary, 9, 3>>.
  

%% @private Parses a PDU in binary format
parse(<<2, _Tail/binary>> = PDU) ->
  [Head | Data]        = binary:split(PDU, [<<9>>], [global]),
  {params, Params}     = get_params(Data),
  <<2, Header/binary>> = Head,
  [OpCode, PacketNum]  = binary:split(Header, [<<58>>], [global]), 
  Operation            = ecimd2_opcode:translate(OpCode),
  Status               = maps:get(<<"900">>, Params, <<"0">>),
  StatusAtom           = ecimd2_status:translate(Status),
  {Operation, StatusAtom, PacketNum, Params};

%% @private Unknown packet
parse(PDU) ->
  {unknown_pdu, PDU}.
  
   
%% ----------------------------------------------------------------------------
%% internal 
%% ----------------------------------------------------------------------------

%% @private
get_params(Data) ->
  get_params(Data, #{}).
get_params([], Params) ->
  {params, Params};
get_params([<<PCode:3/binary, 58, Value/binary>> | Data], Params) ->
  NewParams = maps:put(PCode, Value, Params),
  get_params(Data, NewParams);
get_params([_Foot | Data], Params) ->
  get_params(Data, Params).

%% @private
build_params(Map) ->
  maps:fold(fun(Key, Value, Acc) ->
    <<Acc/binary, Key/binary, 58, Value/binary, 9>>
  end, <<9>>, Map).
