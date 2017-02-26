%% @private Bin to atom translations of CIMD2 status codes
-module(ecimd2_status).

-export([
  translate/1
]).

%% Generic
translate(<<"0">>)   -> ok;
translate(<<"1">>)   -> unexpected_operation;
translate(<<"2">>)   -> syntax_error;
translate(<<"3">>)   -> unsupported_param;
translate(<<"4">>)   -> smsc_connection_lost;
translate(<<"5">>)   -> no_response;
translate(<<"6">>)   -> system_error;
translate(<<"7">>)   -> cannot_find_info;
translate(<<"8">>)   -> param_formatting_error;
translate(<<"9">>)   -> request_failed;
translate(<<"10">>)  -> temp_congestion;

%% Login
translate(<<"100">>) -> invalid_login;
translate(<<"101">>) -> incorrect_access_type;
translate(<<"102">>) -> too_many_users;
translate(<<"103">>) -> login_refused;
translate(<<"104">>) -> invalid_window_size;
translate(<<"105">>) -> windowing_disabled;
translate(<<"106">>) -> virual_smsc_barring;
translate(<<"107">>) -> invalid_subaddr;
translate(<<"108">>) -> alias_login_refused;

%% Submit
translate(<<"300">>) -> invalid_dest_addr;
translate(<<"301">>) -> invalid_number_dest_addr;
translate(<<"302">>) -> user_data_syntax_error;
translate(<<"303">>) -> invalid_user_data_combination;
translate(<<"304">>) -> invalid_dcs_param;
translate(<<"305">>) -> invalid_validity_param;
translate(<<"306">>) -> invalid_src_addr;
translate(<<"307">>) -> invalid_pid_param;
translate(<<"308">>) -> invalid_first_delivery_param;
translate(<<"309">>) -> invalid_reply_path;
translate(<<"310">>) -> invalid_status_report_param;
translate(<<"311">>) -> invalid_cancel_enabled_param;
translate(<<"312">>) -> invalid_priority_param;
translate(<<"313">>) -> invalid_tariff_class_param;
translate(<<"314">>) -> invalid_service_desc_param;
translate(<<"315">>) -> invalid_transport_type_param;
translate(<<"316">>) -> invalid_message_type_param;
translate(<<"318">>) -> invalid_mms_param;
translate(<<"319">>) -> invalid_operation_timer_param;
translate(<<"320">>) -> invalid_dlg_id_param;
translate(<<"321">>) -> invalid_alpha_src_addr;
translate(<<"322">>) -> invalid_alpha_src_addr_data;
translate(<<"323">>) -> online_closed_user_group_rejection;
translate(<<"324">>) -> license_exceeded;

%% Enquire
translate(<<"400">>) -> invalid_addr_param;
translate(<<"401">>) -> invalid_scts_param;

%% Delivery
translate(<<"500">>) -> invalid_scts_param;
translate(<<"501">>) -> invalid_mode_param;
translate(<<"502">>) -> invalid_param_combination;

%% Cancel
translate(<<"600">>) -> invalid_scts_param;
translate(<<"601">>) -> invalid_addr_param;
translate(<<"602">>) -> invalid_mode_param;
translate(<<"603">>) -> invalid_param_combination;

%% Deliver
translate(<<"700">>) -> delivery_ok;
translate(<<"710">>) -> generic_failure;
translate(<<"711">>) -> unsupported_dcs;
translate(<<"712">>) -> unsupported_udh;
translate(<<"730">>) -> unknown_subscriber;

%% Set
translate(<<"800">>) -> change_password_failed;
translate(<<"801">>) -> change_password_forbidden;

%% Get
translate(<<"900">>) -> unsupported_item;

%% Unknown
translate(_ErrCode)  -> unknown_error.
