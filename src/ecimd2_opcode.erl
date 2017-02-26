%% @private Bin to atom translations of CIMD2 operation codes
-module(ecimd2_opcode).

-export([
  translate/1
]).

%% Client
translate(<<"01">>) -> login_request;
translate(<<"02">>) -> logout_request;
translate(<<"03">>) -> submit_request;
translate(<<"04">>) -> enquire_request;
translate(<<"05">>) -> delivery_request;
translate(<<"06">>) -> cancel_request;
translate(<<"08">>) -> set_request;
translate(<<"09">>) -> get_request;
translate(<<"40">>) -> alive_request;
translate(<<"51">>) -> login_response;
translate(<<"52">>) -> logout_response;
translate(<<"53">>) -> submit_response;
translate(<<"54">>) -> enquire_response;
translate(<<"55">>) -> delivery_response;
translate(<<"56">>) -> cancel_response;
translate(<<"58">>) -> set_response;
translate(<<"59">>) -> get_response;
translate(<<"90">>) -> alive_response;
translate(<<"98">>) -> general_error;
translate(<<"99">>) -> nack;

%% SMSC
translate(<<"20">>) -> deliver_message;
translate(<<"70">>) -> deliver_message_response;
translate(<<"23">>) -> deliver_status;
translate(<<"73">>) -> deliver_status_response;

%% Unknown
translate(_OpCode)  -> unknown.
