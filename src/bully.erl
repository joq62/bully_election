%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to application_org
-module(bully).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 boot/0,
	 start/2,
	 stop/1
        ]).

-export([
	 status/0,
	 election_message/1,
	 election_response/0,
	 election_timeout/1,
	 coordinator_message/1,
	 start_election/0,
	 who_is_leader/0,
	 am_i_leader/1
	]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([

	]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
boot()->
    application:start(?MODULE).
who_is_leader()->
     bully_server:who_is_leader().
am_i_leader(CallingNode)->
     bully_server:am_i_leader(CallingNode).
status()->
    bully_server:status().
election_message(CoordinatorNode)->
    bully_server:election_message(CoordinatorNode).
election_response()->
    bully_server:election_response().
election_timeout(PidTimeout)->
    bully_server:election_timeout(PidTimeout).
coordinator_message(CoordinatorNode)->
    bully_server:coordinator_message(CoordinatorNode).
start_election()->
    bully_server:start_election().

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
    
    {ok,Pid}= bully_sup:start_link(),
    {ok,Pid}.
   
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
