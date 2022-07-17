%%% -------------------------------------------------------------------
%%% Author  : joqerlang
%%% Description :
%%% Desired state: one k3 on each host 
%%% state 0: host missing not running  
%%% state 1: host running and k3 running 
%%% state 10: host running and k3 missing first time
%%% state 11: host running and k3 missing second in row
%%% state 12: host running and k3 missing third in a row -> restart k3 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(k3_controller_orchistrate).   
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 
	 desired_state/1

	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
%% Each application is started in own node 
%% Node = serviceid_clusterid_uniquenumber@hostid
%% NumInstances >1 => 1) distribute on different hosts 2) same host
%% specific_host =>
%%
%% Possible hosts: running_host and part of specific_host
%% Prioritised: least applications loaded hosts and appl not running host
%% 

desired_state(DeploymentName)->
    {ok,ClusterId}=db_deployments:read(name,DeploymentName),
    {ok,CookieStr}=db_deployments:read(cookie,DeploymentName),
    {ok,Hosts}=db_deployments:read(hosts,DeploymentName),
    {ok,[DeploymentInfoSpec]}=db_deployments:read(deployments,DeploymentName),
    {ok,Applications}=db_deployment_info:read(appl_specs,DeploymentInfoSpec),
    {ok,NumInstances}=db_deployment_info:read(num_instances,DeploymentInfoSpec),
    {ok,Directive}=db_deployment_info:read(directive,DeploymentInfoSpec),
    
    CookieStr=atom_to_list(erlang:get_cookie()),
%    Reply=[start_appl_1(ApplId,NumInstances,Directive,ClusterId,CookieStr)||{ApplId,_}<-Applications],
%    Reply=[start_appl_2(ApplId,NumInstances,Hosts,Directive,ClusterId,CookieStr)||{ApplId,_}<-Applications],
    Reply=[multi_start(ApplId,NumInstances,Hosts,Directive,ClusterId,CookieStr)||{ApplId,_}<-Applications],
    
%[{"test_math","0.1.0"},{"test_add","0.1.0"},{"test_divi","0.1.0"},{"test_sub","0.1.0"}],
%                               3,
%                               [{all_or_nothing,false},{same_host,false}, {specifict_host,[]}]}
  
    Reply.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

multi_start(_ApplId,_NumInstances,[],_Directive,_ClusterId,_CookieStr)->
    ok;
multi_start(ApplId,NumInstances,[Host|T],Directive,ClusterId,CookieStr)->
    singel_start(ApplId,NumInstances,Host,Directive,ClusterId,CookieStr),
    multi_start(ApplId,NumInstances,T,Directive,ClusterId,CookieStr).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
singel_start(ApplId,NumInstances,Host,_Directive,ClusterId,CookieStr)->
    Appl=list_to_atom(ApplId),  
    NodesHost=sd:get_host(Appl,Host),
    Diff=NumInstances-list_length:start(NodesHost),
    singel_start(Diff,ApplId,Host,ClusterId,CookieStr).



singel_start(0,_ApplId,_Host,_ClusterId,_CookieStr)->
    ok;
singel_start(N,ApplId,Host,ClusterId,CookieStr)->
    %Appl=list_to_atom(ApplId),  
    case sd:get_host(k3_node,Host) of
	[]->
	    ok;
	[{K3Node,HostName}]->
	    NodeName=ClusterId++"_"++ApplId++"_"++integer_to_list(erlang:system_time(microsecond),34),
	    {ok,Cwd}=rpc:call(K3Node,file,get_cwd,[],5000),
	    NodeDir=filename:join(Cwd,ClusterId),
	    PaArgs=" ",
	    EnvArgs=" ",
	    {ok,SlaveNode}=rpc:call(K3Node,node,create,[HostName,NodeDir,NodeName,CookieStr,PaArgs,EnvArgs],2*5000),
	%    {ok,"common"}=start_appl("common",K3Node,SlaveNode,NodeDir),
	 %   {ok,"sd"}=start_appl("sd",K3Node,SlaveNode,NodeDir),
	    {ok,ApplId}=start_appl(ApplId,K3Node,SlaveNode,NodeDir)
	    
	  % NodeAppl=ApplId++".spec",
	  

	 %   {ok,NodeAppl}=db_application_spec:read(name,NodeAppl),
	 %   {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
	 %   {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
	 %   {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),
	    %% Start sd and common
%	    {ok,ApplId,_,_}=rpc:call(K3Node,node,load_start_appl,[SlaveNode,NodeDir,ApplId,ApplVsn,GitPath,StartCmd],5*5000)
    end,
    singel_start(N-1,ApplId,Host,ClusterId,CookieStr).

start_appl(ApplId,K3Node,SlaveNode,NodeDir)->
    ApplSpec=ApplId++".spec",
    {ok,ApplSpec}=db_application_spec:read(name,ApplSpec),
    {ok,ApplVsn}=db_application_spec:read(vsn,ApplSpec),
    {ok,GitPath}=db_application_spec:read(gitpath,ApplSpec),
    {ok,StartCmd}=db_application_spec:read(cmd,ApplSpec),
    {ok,ApplId,_,_}=rpc:call(K3Node,node,load_start_appl,[SlaveNode,NodeDir,ApplId,ApplVsn,GitPath,StartCmd],5*5000),
    {ok,ApplId}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

start_appl_1(ApplId,NumInstances,Directive,ClusterId,CookieStr)->
    NumToStart=num_to_start(ApplId,NumInstances),
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
				 {"DEBUG: ApplId, NumToStart "," ",ApplId," ",NumToStart}]),
    start_appl(ApplId,NumToStart,Directive,ClusterId,CookieStr).

start_appl(_ApplId,0,_Directive,_ClusterId,_CookieStr)->
    ok;
start_appl(ApplId,NumToStart,Directive,ClusterId,CookieStr)->

    PossibleNodes=possible_nodes(Directive),
    [{_Len,K3Node,HostName,_ApplInfo}|_]=priortize(PossibleNodes,ApplId),
    NodeName=ClusterId++"_"++ApplId++"_"++integer_to_list(erlang:system_time(microsecond),34),
    {ok,Cwd}=rpc:call(K3Node,file,get_cwd,[],5000),
    NodeDir=filename:join(Cwd,ClusterId),
    PaArgs=" ",
    EnvArgs=" ",
    {ok,SlaveNode}=rpc:call(K3Node,node,create,[HostName,NodeDir,NodeName,CookieStr,PaArgs,EnvArgs],2*5000),
    NodeAppl=ApplId++".spec",
    {ok,NodeAppl}=db_application_spec:read(name,NodeAppl),
    {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
    {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
    {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),
    {ok,ApplId,_,_}=rpc:call(K3Node,node,load_start_appl,[SlaveNode,NodeDir,ApplId,ApplVsn,GitPath,StartCmd],5*5000),
    start_appl(ApplId,NumToStart-1,Directive,ClusterId,CookieStr).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
num_to_start(ApplId,NumInstances)->
    Appl=list_to_atom(ApplId),
    Diff=NumInstances-list_length:start(sd:get(Appl)),
    NumToStart=if
		  Diff<1 ->
		       0;
		   true ->
		       Diff
	       end,
    NumToStart.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
priortize([],_ApplId)->
    [];
priortize(PossibleNodes,ApplId)->
    Appl=list_to_atom(ApplId),
    Stage1=[{num_nodes_per_host(HostName),Node,HostName,ApplInfo}||{Node,HostName,ApplInfo}<-sd:all(),
							 lists:member({Node,HostName},PossibleNodes)],
    SortedStage1=lists:keysort(1,Stage1),
    case list_length:start(SortedStage1) of
    	0->
	    [];
	_ ->
	    SortedStage1
    end.  
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

possible_nodes([{all_or_nothing,_},{same_host,_}, {specifict_host,[]}])->
    sd:get(k3_node);
possible_nodes([{all_or_nothing,_},{same_host,_},{specifict_host,HostList}]) ->
    NodeHostsList=sd:get(k3_node),
    [{Node,HostName}||{Node,HostName}<-NodeHostsList,
			  lists:keymember(HostName,2,HostList)].


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
num_nodes_per_host(WantedHostName)->
    list_length:start([Node||{Node,HostName,_ApplInfo}<-sd:all(),
			     HostName=:=WantedHostName]).
    
    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
%x(Appl,Num,Hosts)->
    

    
