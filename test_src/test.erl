%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([start/0]). 


%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    io:format("~p~n",[{"Start setup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=setup(),
    io:format("~p~n",[{"Stop setup",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start t1()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=t1(),
    io:format("~p~n",[{"Stop t1()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass1()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass1(),
%    io:format("~p~n",[{"Stop pass1()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start single()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=single(),
 %   io:format("~p~n",[{"Stop single()",?MODULE,?FUNCTION_NAME,?LINE}]),

   % io:format("~p~n",[{"Start cluster()",?MODULE,?FUNCTION_NAME,?LINE}]),
   % ok=cluster(),
   % io:format("~p~n",[{"Stop cluster()",?MODULE,?FUNCTION_NAME,?LINE}]),


   % io:format("~p~n",[{"Start pod_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),
  % ok=pod_spec(),
  %  io:format("~p~n",[{"Stop pod_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start pod()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=pod(),
 %   io:format("~p~n",[{"Stop pod()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start deployment_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=deployment_spec(),
  %  io:format("~p~n",[{"Stop deployment_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start deployment()",?MODULE,?FUNCTION_NAME,?LINE}]),
   % ok=deployment(),
  %  io:format("~p~n",[{"Stop deployment()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start kubelet()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=kubelet(),
  %  io:format("~p~n",[{"Stop kubelet",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start cluster_info()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=cluster_info(),
 %   io:format("~p~n",[{"Stop cluster_info",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_3()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_3(),
%    io:format("~p~n",[{"Stop pass_3()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_4()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_4(),
%    io:format("~p~n",[{"Stop pass_4()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_5()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_5(),
%    io:format("~p~n",[{"Stop pass_5()",?MODULE,?FUNCTION_NAME,?LINE}]),
 
    
   
      %% End application tests
    io:format("~p~n",[{"Start cleanup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=cleanup(),
    io:format("~p~n",[{"Stop cleaup",?MODULE,?FUNCTION_NAME,?LINE}]),
   
    io:format("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
t1()->
    [N1,N2,N3]=nodes(),
    [rpc:cast(Node,bully,boot,[])||Node<-nodes()],

    timer:sleep(1000),
    io:format("Leader = ~p~n",[rpc:call(N1,bully,who_is_leader,[],1000)]),
    % Kill slave
    slave:stop(N3),
    timer:sleep(1000),
    io:format("Leader = ~p~n",[rpc:call(N1,bully,who_is_leader,[],1000)]),
    
    % REstart node
    HostId=net_adm:localhost(),
    Cookie=atom_to_list(erlang:get_cookie()),
    Args="-pa ebin -setcookie "++Cookie,
    [{ok,N3}]=[slave:start(HostId,NodeName,Args)||NodeName<-["c"]],
    rpc:cast(N3,bully,boot,[]),
    timer:sleep(1000),
    io:format("Leader = ~p~n",[rpc:call(N1,bully,who_is_leader,[],1000)]),
    

    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass1()->
    Nodes=nodes(),
    [N1,N2,N3]=Nodes,
   % io:format("Nodes ~p~n",[Nodes]),
    [{'a@joq62-X550CA',{error,[mnesia_not_started]}},
     {'b@joq62-X550CA',{error,[mnesia_not_started]}},
     {'c@joq62-X550CA',{error,[mnesia_not_started]}}]=[{Node,rpc:call(Node,db_lock,check_init,[],2000)}||Node<-Nodes],

    % Start first node
 %   ok=rpc:call(N1,application,start,[dbase_dist],3000),
    ok=rpc:call(N1,dbase_dist,boot,[],3000),
    [{'a@joq62-X550CA',ok},
     {'b@joq62-X550CA',{error,[mnesia_not_started]}},
     {'c@joq62-X550CA',{error,[mnesia_not_started]}}]=[{Node,rpc:call(Node,db_lock,check_init,[],2000)}||Node<-Nodes],
   
    true=rpc:call('a@joq62-X550CA',db_lock,is_leader,[controller_lock,'a@joq62-X550CA'],2000),
    [{controller_lock,_,'a@joq62-X550CA'}]=rpc:call(N1,db_lock,read_all_info,[],5000),
 % Start second node
%    ok=rpc:call(N2,application,start,[dbase_dist],3000),
    ok=rpc:call(N2,dbase_dist,boot,[],3000),
    [{'a@joq62-X550CA',ok},
     {'b@joq62-X550CA',ok},
     {'c@joq62-X550CA',{error,[mnesia_not_started]}}]=[{Node,rpc:call(Node,db_lock,check_init,[],2000)}||Node<-Nodes],
   
    true=rpc:call(N2,db_lock,is_leader,[controller_lock,'a@joq62-X550CA'],2000),
    [{controller_lock,_,'a@joq62-X550CA'}]=rpc:call(N2,db_lock,read_all_info,[],5000),
 % Start third node
  %  ok=rpc:call(N3,application,start,[dbase_dist],3000),
    ok=rpc:call(N3,dbase_dist,boot,[],3000),
    [{'a@joq62-X550CA',ok},
     {'b@joq62-X550CA',ok},
     {'c@joq62-X550CA',ok}]=[{Node,rpc:call(Node,db_lock,check_init,[],2000)}||Node<-Nodes],
   
    true=rpc:call(N3,db_lock,is_leader,[controller_lock,'a@joq62-X550CA'],2000),
    [{controller_lock,_,'a@joq62-X550CA'}]=rpc:call(N3,db_lock,read_all_info,[],5000),
    % kill N1

    slave:stop(N1),
    timer:sleep(300),
    [{'b@joq62-X550CA',ok},
     {'c@joq62-X550CA',ok}]=[{Node,rpc:call(Node,db_lock,check_init,[],2000)}||Node<-nodes()],
 %   timer:sleep(3000),
    [{controller_lock,_,'a@joq62-X550CA'}]=rpc:call(N3,db_lock,read_all_info,[],5000),
    [{controller_lock,_,'a@joq62-X550CA'}]=rpc:call(N2,db_lock,read_all_info,[],5000),
    
    %start N1 again
    HostId=net_adm:localhost(),
    Cookie=atom_to_list(erlang:get_cookie()),
    Args="-pa ebin -setcookie "++Cookie,
    {ok,N1}=slave:start(HostId,"a",Args),
    ok=rpc:call(N1,application,start,[dbase_dist],5000),
    
%    timer:sleep(2000),
    [{controller_lock,_,'a@joq62-X550CA'}]=rpc:call(N1,db_lock,read_all_info,[],5000),
    [{controller_lock,_,'a@joq62-X550CA'}]=rpc:call(N2,db_lock,read_all_info,[],5000),
    [{controller_lock,_,'a@joq62-X550CA'}]=rpc:call(N3,db_lock,read_all_info,[],5000),
    
    ok.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
dist_nodes(Nodes)->
    
    [Node||Node<-[glurk|Nodes],rpc:call(Node,db_lock,check_init,[],2000)=:=ok].


single()->
  %  cSlaves=create_nodes(),
  
    Nodes=[node()|nodes()],
    
    []=dist_nodes(Nodes),
    
    

     [{'test@joq62-X550CA',{error,[mnesia_not_started]}},
      {'a@joq62-X550CA',{error,[mnesia_not_started]}},
      {'b@joq62-X550CA',{error,[mnesia_not_started]}},
      {'c@joq62-X550CA',{error,[mnesia_not_started]}}]=[{Node,rpc:call(Node,db_lock,check_init,[],2000)}||Node<-Nodes],
    
    ok=init_distributed_mnesia(Nodes),

    []=dist_nodes(Nodes),
    
    [{'test@joq62-X550CA',{error,[not_initiated,db_lock]}},
     {'a@joq62-X550CA',{error,[not_initiated,db_lock]}},
     {'b@joq62-X550CA',{error,[not_initiated,db_lock]}},
     {'c@joq62-X550CA',{error,[not_initiated,db_lock]}}
    ]=[{Node,rpc:call(Node,db_lock,check_init,[],2000)}||Node<-Nodes],
    
    ok=lock(),
    [{'test@joq62-X550CA',ok},
     {'a@joq62-X550CA',ok},
     {'b@joq62-X550CA',ok},
     {'c@joq62-X550CA',ok}
    ]=[{Node,rpc:call(Node,db_lock,check_init,[],2000)}||Node<-Nodes],

    ['test@joq62-X550CA','a@joq62-X550CA',
     'b@joq62-X550CA','c@joq62-X550CA']=dist_nodes(Nodes),
    ok=loose_restart_node(),    
  %  ok=db_lock:create_table(),
  %  {atomic,ok}=db_lock:create(leader,0),
  %  [{leader,0,'test@joq62-X550CA'}]=db_lock:read_all_info(),
  %  true=db_lock:is_open(leader,node()),
  %  ['test@joq62-X550CA']=db_lock:leader(leader),
    
  %  true=db_lock:is_leader(leader,node()),
	     
  %  ['test@joq62-X550CA']=db_lock:leader(leader),
  %  timer:sleep(2500),
    
   % true=db_lock:is_open(leader,node1,2),
   % false=db_lock:is_leader(leader,node()),
   % true=db_lock:is_leader(leader,node1),
   % true=rpc:call(Node1,db_lock,is_open,[leader,Node1,1],2000),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
-define(NodeNames,["a","b","c"]).
create_nodes()->
    Cookie=atom_to_list(erlang:get_cookie()),
    NodeInfo=[{NodeName,"-pa ebin -setcookie "++Cookie}||NodeName<-?NodeNames],
    SlaveStart=[slave:start(net_adm:localhost(),NodeName,Arg)||{NodeName,Arg}<-NodeInfo],
    [{ok,'a@joq62-X550CA'},
     {ok,'b@joq62-X550CA'},
     {ok,'c@joq62-X550CA'}]=SlaveStart,
    Slaves=[Slave||{ok,Slave}<-SlaveStart],
   
    [pong,pong,pong]=[net_adm:ping(Slave)||Slave<-Slaves],
    
    Slaves.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
init_dist_mnesia()->
    AllNodes=[node()|nodes()],
    ok=init_distributed_mnesia(AllNodes),
    ok.

init_distributed_mnesia(Nodes)->
    StopResult=[rpc:call(Node,mnesia,stop,[],5*1000)||Node<-Nodes],
    Result=case [Error||Error<-StopResult,Error/=stopped] of
	       []->
		   case mnesia:delete_schema(Nodes) of
		       ok->
			   StartResult=[rpc:call(Node,mnesia,start,[],5*1000)||Node<-Nodes],
			   case [Error||Error<-StartResult,Error/=ok] of
			       []->
				   ok;
			       Reason->
				   {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]}
			   end;
		       Reason->
			   {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]}
		   end;
	       Reason->
		   {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]}
	   end,
    Result.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non

create_tables()->
    ok=db_lock:create_table(),
    [db_lock:add_node(Node,ram_copies)||Node<-nodes()],
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
loose_restart_node()->
    HostId=net_adm:localhost(),    
    KilledNode='a@joq62-X550CA',
    DistR1 =[{Node,rpc:call(Node,db_lock,read_all_info,[],5*1000)}||Node<-nodes()],
    [{'a@joq62-X550CA',[{host_lock,_,'a@joq62-X550CA'}]},
     {'b@joq62-X550CA',[{host_lock,_,'a@joq62-X550CA'}]},
     {'c@joq62-X550CA',[{host_lock,_,'a@joq62-X550CA'}]}]=DistR1,

 %   {atomic,glok}=rpc:call(KilledNode,db_lock,create,[leader,0,KilledNode],5*1000),
	
    slave:stop(KilledNode),
   % timer:sleep(100),
    pang=net_adm:ping(KilledNode),
    DistR2 =[{Node,rpc:call(Node,db_lock,read_all_info,[],5*1000)}||Node<-nodes()],
    [{'b@joq62-X550CA',[{host_lock,_,'a@joq62-X550CA'}]},
     {'c@joq62-X550CA',[{host_lock,_,'a@joq62-X550CA'}]}]=DistR2, 

    timer:sleep(1200),
    true=rpc:call('b@joq62-X550CA',db_lock,is_open,[host_lock,'b@joq62-X550CA',1],5*1000),    
    %Leader checks if a node is absent
  
    MissingNodes=check_missing_nodes(),
    [KilledNode]=MissingNodes,
    
    % Restart node
    [NodeName,HostId]=string:tokens(atom_to_list(KilledNode),"@"),
    Cookie=atom_to_list(erlang:get_cookie()),
    Arg="-pa ebin -setcookie "++Cookie,
    {ok,KilledNode}=slave:start(HostId,NodeName,Arg),    
    
    %% 

    % Add to cluster
    stopped=rpc:call(KilledNode,mnesia,stop,[],5*1000),
    ok=rpc:call(KilledNode,mnesia,start,[],5*1000),
    [Node1|_]=rpc:call(KilledNode,erlang,nodes,[],2000),
    
    ok=rpc:call(Node1,db_lock,add_node,[KilledNode,ram_copies],2000),

    DistR3 =[{Node,rpc:call(Node,db_lock,read_all_info,[],5*1000)}||Node<-nodes()],
    [{'b@joq62-X550CA',[{host_lock,_,'b@joq62-X550CA'}]},
     {'c@joq62-X550CA',[{host_lock,_,'b@joq62-X550CA'}]},
     {'a@joq62-X550CA',[{host_lock,_,'b@joq62-X550CA'}]}]=DistR3, 
    
    false=rpc:call('c@joq62-X550CA',db_lock,is_leader,[host_lock,'a@joq62-X550CA'],2000),
    ok. 

check_missing_nodes()->
    DBNodes=mnesia:system_info(db_nodes),
    RunningDBNodes=mnesia:system_info(running_db_nodes),
    MissingNodes=[Node||Node<-DBNodes,
		       false==lists:member(Node,RunningDBNodes)],
    MissingNodes.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
lock()->
    ok=db_lock:create_table(),
    [db_lock:add_node(Node,ram_copies)||Node<-nodes()],
    {atomic,ok}=db_lock:create(host_lock,1,node()),
  
    DistR1 =[{Node,rpc:call(Node,db_lock,read_all_info,[],5*1000)}||Node<-nodes()],
     [{'a@joq62-X550CA',[{host_lock,1,'test@joq62-X550CA'}]},
      {'b@joq62-X550CA',[{host_lock,1,'test@joq62-X550CA'}]},
      {'c@joq62-X550CA',[{host_lock,1,'test@joq62-X550CA'}]}
     ]=DistR1,


    ['test@joq62-X550CA']=db_lock:leader(host_lock),
    timer:sleep(1200),
    true=rpc:call('a@joq62-X550CA',db_lock,is_open,[host_lock,'a@joq62-X550CA'],5*1000),
    false=db_lock:is_open(host_lock,node()),
    Lock1 =[{Node,rpc:call(Node,db_lock,leader,[host_lock],5*1000)}||Node<-nodes()],
    [{'a@joq62-X550CA',['a@joq62-X550CA']},
     {'b@joq62-X550CA',['a@joq62-X550CA']},
     {'c@joq62-X550CA',['a@joq62-X550CA']}]=Lock1,
    

    

    ok.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_2()->
    
     ok.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

setup()->
    HostId=net_adm:localhost(),
    A="a@"++HostId,
    NodeA=list_to_atom(A),
    B="b@"++HostId,
    NodeB=list_to_atom(B),
    C="c@"++HostId,
    NodeC=list_to_atom(C),    
    Nodes=[NodeA,NodeB,NodeC],
    [rpc:call(Node,init,stop,[])||Node<-Nodes],
    Cookie=atom_to_list(erlang:get_cookie()),
    Args="-pa ebin -setcookie "++Cookie,
    [{ok,NodeA},
     {ok,NodeB},
     {ok,NodeC}]=[slave:start(HostId,NodeName,Args)||NodeName<-["a","b","c"]],
    
   ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
  %  application:stop(etcd),
  %  init:stop(),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
