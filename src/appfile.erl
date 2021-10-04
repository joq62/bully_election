%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(appfile).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================
read(AppFile,Type)->
   Result=case file:consult(code:where_is_file(AppFile)) of
	      {ok,AllInfo}->
		  read_app_file(AllInfo,Type);
	      {error,Reason}->
		  {error,Reason}
	  end,
    Result.

read_app_file(AllInfo,all)->
    AllInfo;
read_app_file([{application,App,_}],application)->
    App;
read_app_file([{application,_,Info}],git_path) ->
    {git_path,GitPath}=lists:keyfind(git_path,1,Info),
    GitPath;
read_app_file([{application,_,Info}],Key) ->
   Result=case lists:keyfind(Key,1,Info) of
	      {Key,Value}->
		  Value;
	      false->
		  {error,[eexists,Key,?FUNCTION_NAME,?MODULE,?LINE]}
	  end,
    Result;
read_app_file([{application,_,_Info}],EExists) ->
    {error,[eexists,EExists,?FUNCTION_NAME,?MODULE,?LINE]};
read_app_file(Error,_) ->
    {error,[unmatched_signal,Error,?FUNCTION_NAME,?MODULE,?LINE]}.




%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
