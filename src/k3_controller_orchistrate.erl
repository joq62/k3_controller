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
desired_state(DeploymentName)->
    
    {ok,[DeploymentInfoSpec]}=db_deployments:read(deployments,DeploymentName),
    {ok,Applications}=db_deployment_info:read(appl_specs,DeploymentInfoSpec),
    {ok,NumInstances}=db_deployment_info:read(num_instances,DeploymentInfoSpec),
    {ok,Directive}=db_deployment_info:read(directive,DeploymentInfoSpec),
    K3Nodes=sd:get(k3),
    

%[{"test_math","0.1.0"},{"test_add","0.1.0"},{"test_divi","0.1.0"},{"test_sub","0.1.0"}],
%                               3,
%                               [{all_or_nothing,false},{same_host,false}, {specifict_host,[]}]}


    Reply={Applications,NumInstances,Directive},

  %  AllControllerNodes=sd_server:get(k3_controller),
  
    Reply.

check_start_appl({AppllId,ApplVsn},NumInstances,[{all_or_nothing,false},{same_host,false}, {specifict_host,[]}])->
    
    ok.


