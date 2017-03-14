%% @private CIMD2 PDU related functions
-module(ecimd2_pdu).

-export([
  login/3,
  alive/1,
  deliver_message_response/1,
  deliver_status_report_response/1,
  submit/2,
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
  PDU    = <<2, "01:", PNum/binary, Params/binary, 3>>,
  {pdu, PDU}.

%% @private Creates an alive packet
alive(PNum) ->
  PDU = <<2, "40:", PNum/binary, 9, 3>>,
  {pdu, PDU}.

%% @private Creates a deliver_message_response packet
deliver_message_response(PNum) ->
  PDU = <<2, "70:", PNum/binary, 9, 3>>,
  {pdu, PDU}.

%% @private Creates a deliver_status_report_response packet
deliver_status_report_response(PNum) ->
  PDU = <<2, "73:", PNum/binary, 9, 3>>,
  {pdu, PDU}.

%% @private Creates a submit packet
submit(PNum, Map) ->
  ParamMap = #{
    <<"021">> => maps:get(dst_address,   Map, <<>>),
    <<"023">> => maps:get(src_address,   Map, <<>>),
    <<"027">> => maps:get(src_alpha,     Map, <<>>),
    <<"030">> => maps:get(data_coding,   Map, 0),
    <<"032">> => maps:get(udh,           Map, <<>>),
    <<"033">> => maps:get(message,       Map, <<>>),
    <<"034">> => maps:get(message_bin,   Map, <<>>),
    <<"056">> => maps:get(status_report, Map, 0),
    <<"064">> => maps:get(tariff_class,  Map, <<"00">>),
    <<"065">> => maps:get(service_desc,  Map, <<"00">>)
  },
  Params = build_params(ParamMap),
  PDU    = <<2, "03:", PNum/binary, Params/binary, 3>>,
  {pdu, PDU}.
  

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
    ValueBin = ecimd2_format:ensure_binary(Value),
    build_params(Acc, Key, ValueBin)
  end, <<9>>, Map).

%% @private
build_params(Acc, _Key, <<>>) ->
  <<Acc/binary, 9>>;
build_params(Acc, Key, Val) ->
  <<Acc/binary, Key/binary, 58, Val/binary, 9>>.
  
