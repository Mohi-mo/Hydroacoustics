-module(mod_test).
-behaviour(fsm_worker).

-include_lib("evins/include/fsm.hrl").

-export([start/4, register_fsms/4]).

start(Mod_ID, Role_IDs, Sup_ID, {M, F, A}) ->
  fsm_worker:start(?MODULE, Mod_ID, Role_IDs, Sup_ID, {M, F, A}).

register_fsms(Mod_ID, Role_IDs, Share, ArgS) ->
  LogONFiles = [],
  FSM = fsm_test,
  Role_names = lists:usort([Role || {Role, _, _, _, _} <- Role_IDs]),
  Roles = fsm_worker:role_info(Role_IDs, Role_names),
  [evins:set_module_level(M,ArgS) || M <- LogONFiles],
  [#sm{roles = Roles, module = fsm_conf},
   #sm{roles = Roles, env = parse_conf(Mod_ID, ArgS, Share), module = FSM}].


parse_conf(_Mod_ID, ArgS, Share) ->
  Value = proplists:get_value(value, ArgS, "Template"),
  ShareID = #sm{share = Share},
  share:put(ShareID, [{value, Value}]).
