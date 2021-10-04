%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(bully_server).

-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
% -include("").
%% --------------------------------------------------------------------
-define(WAIT_FOR_ELECTION_RESPONSE_TIMEOUT,5*100).
%% External exports
-export([
	 status/0,
	 election_message/1,
	 election_response/0,
	 election_timeout/1,
	 coordinator_message/1,
	 start_election/0
	]).

%% gen_server callbacks

-export([start/0,stop/0]).

-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {nodes, coordinator_node, messageLoopTimeout,pid_timeout}).

%% ====================================================================
%% External functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

status()->
    gen_server:call(?MODULE,{status},infinity).
   
election_message(CoordinatorNode)->
     gen_server:cast(?MODULE,{election_message,CoordinatorNode}).
election_response()->
     gen_server:cast(?MODULE,{election_response}).
election_timeout(PidTimeout)->
    gen_server:cast(?MODULE,{election_timeout,PidTimeout}).
coordinator_message(CoordinatorNode)->
     gen_server:cast(?MODULE,{coordinator_message,CoordinatorNode}).

start_election()->
    gen_server:cast(?MODULE,{start_election}).



%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    bully:start_election(),
    {ok, #state{nodes = [],
		coordinator_node = node(), 
		pid_timeout=no_pid}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({status},_From, State) ->
    Reply = State,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({election_message,CoordinatorNode}, State) ->
    case CoordinatorNode<node() of
	false->% lost election
	    NewState=State;
	true->
	    rpc:cast(CoordinatorNode,bully,election_response,[]),
	    NewState=start_election(State)	   
    end,
    {noreply, NewState};

handle_cast({election_response}, State) ->
    State#state.pid_timeout!kill,
    NewState=State#state{pid_timeout=no_pid},
    {noreply, NewState};

handle_cast({election_timeout,_PidTimeout}, State) ->
    NewState=win_election(State),
    {noreply, NewState};

handle_cast({coordinator_message,CoordinatorNode}, State) ->
    NewState=set_coordinator(State, CoordinatorNode),
    {noreply, NewState};

handle_cast({start_election}, State) ->
    NewState=start_election(State),
    {noreply, NewState};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?FUNCTION_NAME,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info({nodedown, CoordinatorNode},State) -> 
    io:format("nodedown Node ~p~n",[{CoordinatorNode,State#state.coordinator_node,?FUNCTION_NAME,?MODULE,?LINE}]),
    NewState=case State#state.coordinator_node=:=CoordinatorNode of
		 true->
		     start_election(State);
		 false->
		     State
	     end,
    {noreply, NewState};

handle_info(Info, State) ->
    io:format("unmatched match Info ~p~n",[{Info,?FUNCTION_NAME,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Exported functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_election(State) ->
    NodesHigherId=nodes_with_higher_ids(nodes()),
    [rpc:cast(Node,bully,election_message,[node()])||Node<-NodesHigherId],
    PidTimeout=spawn(fun()->election_timeout() end),
    State#state{pid_timeout=PidTimeout}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
win_election( State) ->
    io:format("Node ~s has declared itself a leader.~n", [atom_to_list(node())]),
    NodesLowerId=nodes_with_lower_ids(nodes()),
    [rpc:cast(Node,bully,coordinator_message,[node()])||Node<-NodesLowerId],
    set_coordinator(State, node()).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
set_coordinator(State, CoordinatorNode) ->
    io:format("Node ~p has changed leader from ~p to ~p~n", [node(), State#state.coordinator_node, CoordinatorNode]),
    monitor_node(State#state.coordinator_node, false),
    monitor_node(CoordinatorNode, true),
    if 
	State#state.pid_timeout/=no_pid->
	    State#state.pid_timeout!kill;
	true->
	    ok
    end,
    State#state{coordinator_node = CoordinatorNode, pid_timeout = no_pid}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
election_timeout()->
    receive
	kill->
	    ok
    after ?WAIT_FOR_ELECTION_RESPONSE_TIMEOUT->
	    Pid=self(),
	    rpc:cast(node(),bully,election_timeout,[Pid])
    end.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
nodes_with_higher_ids(Nodes) ->
  [Node || Node <- Nodes, Node > node()].

nodes_with_lower_ids(Nodes) ->
  [Node || Node <- Nodes, Node < node()].
