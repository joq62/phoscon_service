%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_phoscon).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("device.spec").
%% --------------------------------------------------------------------

%% External exports
-export([

%	 all_light_maps/3,
%	 all_sensor_maps/3,
	 get_conbee_info/0,	 
	 what_devices/4,
	 get_maps/4,
	 set_state/7
	]). 


%% ====================================================================
%% External functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_conbee_info()->
    {ok,HostName}=net:gethostname(),
    {ok,AllFileNames}=rd:call(host,all_filenames,[],5000),
    [FileName]=[FileName||FileName<-AllFileNames,
			   {ok,HostName}==rd:call(host,get_hostname,[FileName],5000)],
    {ok,
     [{conbee,[{conbee_addr,ConbeeAddr},
	       {conbee_port,ConbeePort},
	       {conbee_key,ConbeeKey}
	      ]
      }
     ]
    }=rd:call(host,get_application_config,[FileName],5000),
    
    {ConbeeAddr,ConbeePort,ConbeeKey}.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_maps("sensors",ConbeeAddr,ConbeePort,Crypto)->
    get("sensors",ConbeeAddr,ConbeePort,Crypto);
get_maps("lights",ConbeeAddr,ConbeePort,Crypto)->
    get("lights",ConbeeAddr,ConbeePort,Crypto);
get_maps(NoMatch,ConbeeAddr,ConbeePort,Crypto)->
    [{error,[NoMatch,?MODULE,?LINE]}].

get(DeviceType,ConbeeAddr,ConbeePort,Crypto)->
    {ok, ConnPid} = gun:open(ConbeeAddr,ConbeePort),
    Cmd="/api/"++Crypto++"/"++DeviceType,
    Ref=gun:get(ConnPid,Cmd),
  %  Result= get_info(gun:await_body(ConnPid, Ref)),
    Result= case gun:await_body(ConnPid, Ref) of
		{ok,Body}->
		    jsx:decode(Body,[]);
		Reason ->
		    {error,[Reason,?MODULE,?LINE]}
	    end,
    ok=gun:close(ConnPid),
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_info({ok,Body})->
    get_info(Body);
get_info(Body)->
    Map=jsx:decode(Body,[]),
    format_info(Map).

format_info(Map)->
    L=maps:to_list(Map),
 %   io:format("L=~p~n",[{?MODULE,?LINE,L}]),
    format_info(L,[]).

format_info([],Formatted)->
    Formatted;
format_info([{IdBin,Map}|T],Acc)->
    NumId=binary_to_list(IdBin),
    Name=binary_to_list(maps:get(<<"name">> ,Map)),
    ModelId=binary_to_list(maps:get(<<"modelid">>,Map)),
    State=maps:get(<<"state">>,Map),
    NewAcc=[{Name,NumId,ModelId,State}|Acc],
    format_info(T,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
what_devices("lights",Ip,Port,Crypto)->
    Maps=get_maps(<<"lights">>,Ip,Port,Crypto),
    Maps;
what_devices("sensors",Ip,Port,Crypto)->
    Maps=get_maps(<<"sensors">>,Ip,Port,Crypto),
    Maps.



%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
set_state(Id,Key,Value,DeviceType,Ip,Port,Crypto)->
    Cmd="/api/"++Crypto++"/"++DeviceType++"/"++Id++"/state",
    Body=jsx:encode(#{Key => Value}),
    {ok, ConnPid} = gun:open(Ip,Port),
    StreamRef = gun:put(ConnPid, Cmd, 
			[{<<"content-type">>, "application/json"}],Body),
    Result=get_reply(ConnPid,StreamRef),
    ok=gun:close(ConnPid),
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_reply(ConnPid,StreamRef)->
    case gun:await(ConnPid, StreamRef) of
	{response, fin, Status, Headers} ->
%	    io:format(" no_data ~p~n", [{?MODULE,?LINE}]),
	    Body=[no_data];
	{response, nofin, Status, Headers} ->
%	    io:format(" ~p~n", [{?MODULE,?LINE}]),
	    {ok, Body} = gun:await_body(ConnPid, StreamRef),
	    Body
    end,
    {Status, Headers,Body}.
