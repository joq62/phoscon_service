%%% -------------------------------------------------------------------
%%% Author  : joqerlang
%%% Description :
%%% 1. API gateway to phoscon docker container which controls the zigbee devices
%%% 2. acts as an middle man by translates erlang and logical to REST api's
%%% 3. For each device model there is a gen_server  that translates erlang and logical context to REST api's phoscon
%%% 4. For each present device a local resource is added to resources discovery {device_name,{node(),corresponding gen_server module}} 
%%% 5. For each device that dissappers that local resoursours will be deleted from resources discovery
%%% 6. phoscon_control continues updates and keep the status of all devices. That information is provided to all gen_servers
%%%  
%%% Created :
%%% -------------------------------------------------------------------
-module(phoscon).
  
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").
-include("phoscon.rd").

%% --------------------------------------------------------------------
-define(ConbeeContainer,"deconz").
-define(SERVER,?MODULE).

%% --------------------------------------------------------------------
%% Resources
%% --------------------------------------------------------------------

%% External exports
-export([
	 %% basic
	 set_state/4,
	 set_config/4,
	 get_maps/0,
	 get_maps/1
	
	 
	]).


-export([
	 ping/0,
	 start_link/0,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------

-record(state,{device_info,
	       ip_addr,
	       ip_port,
	       crypto
	      }).
-record(device,{
		logic_name,
		num_id,
		module
	       }).
	

%% ====================================================================
%% External functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stop()-> gen_server:call(?SERVER, {stop},infinity).


%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_maps()->
    gen_server:call(?SERVER, {get_maps},infinity). 
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_maps(DeviceType)->
    gen_server:call(?SERVER, {get_maps,DeviceType},infinity). 


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
set_state(Id,Key,Value,DeviceType)->
    gen_server:call(?SERVER, {set_state,Id,Key,Value,DeviceType},infinity). 

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
set_config(Id,Key,Value,DeviceType)->
    gen_server:call(?SERVER, {set_config,Id,Key,Value,DeviceType},infinity). 

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
ping() ->
    gen_server:call(?SERVER, {ping}).


%% cast

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
   
    {ok, #state{},0
    }.   
 

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

handle_call({get_maps},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
 
    LightsMaps=lib_phoscon:get_maps("lights",ConbeeAddr,ConbeePort,Crypto),
    SensorsMaps=lib_phoscon:get_maps("sensors",ConbeeAddr,ConbeePort,Crypto),
    Reply=[{"lights",LightsMaps},{"sensors",SensorsMaps}],
    {reply, Reply, State};


%%---------------------------------------------------------------------
%% Lights 
%%---------------------------------------------------------------------
handle_call({get_maps,"lights"},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
 
    Reply=lib_phoscon:get_maps("lights",ConbeeAddr,ConbeePort,Crypto),
    {reply, Reply, State};

handle_call({set_state,Id,Key,Value,"lights"},_From, State) ->
    DeviceType="lights",
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=lib_phoscon:set_state(Id,Key,Value,DeviceType,ConbeeAddr,ConbeePort,Crypto),
    {reply, Reply, State};


%%---------------------------------------------------------------------
%%  Sensors 
%%---------------------------------------------------------------------
handle_call({get_maps,"sensors"},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,

    Reply=lib_phoscon:get_maps("sensors",ConbeeAddr,ConbeePort,Crypto),
    {reply, Reply, State};

handle_call({set_state,Id,Key,Value,"sensors"},_From, State) ->
    DeviceType="sensors",
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=lib_phoscon:set_state(Id,Key,Value,DeviceType,ConbeeAddr,ConbeePort,Crypto),
    {reply, Reply, State};

%%---------------------------------------------------------------------
%%  General 
%%---------------------------------------------------------------------


handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    ?LOG_WARNING("Unmatched signal",[Request]),
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?LOG_WARNING("Unmatched signal",[Msg]),
    
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_info({gun_up,_,http}, State) -> 
    {noreply, State};


handle_info({gun_response,_,_,_,_,_}, State) -> 
    {noreply, State};

handle_info(timeout, State) -> 
    io:format("timeout ~p~n",[{?MODULE,?LINE}]), 
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
 
    {ConbeeAddr,ConbeePort,ConbeeKey}=lib_phoscon:get_conbee_info(),
    application:ensure_all_started(gun),
    os:cmd("docker restart "++?ConbeeContainer),
    timer:sleep(5*1000),
    
    ?LOG_NOTICE("Server started ",
		[?MODULE,
		ip_addr,ConbeeAddr,
		ip_port,ConbeePort,
		crypto,ConbeeKey
		]),
    NewState=State#state{device_info=undefined,
			 ip_addr=ConbeeAddr,
			 ip_port=ConbeePort,
			 crypto=ConbeeKey},
    
    {noreply, NewState};

handle_info(Info, State) ->
    ?LOG_WARNING("Unmatched signal",[Info]),
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
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
connect_etcd()->
    Node='etcd_c201@c201',
    Result=case connect_etcd(Node,20*1000,1000,false) of
	       false->
		   {error,["Can't connect to Etcd",?MODULE,?LINE]};
	       true->
		   {ok,Node}
	   end,
    Result.
connect_etcd(_Node,0,_Sleep,Boolean)->
    Boolean;
connect_etcd(_Node,_TimeLeft,_Sleep,true)->
    true;
connect_etcd(Node,TimeLeft,Sleep,Boolean)->
    case rpc:call(Node,etcd,ping,[],5000) of
	pong->
	    NewTimeLeft=0,
	    NewBoolean=true;
	_ ->
	    timer:sleep(Sleep),
	    NewTimeLeft=TimeLeft-Sleep,
	    NewBoolean=false
    end,
    connect_etcd(Node,NewTimeLeft,Sleep,NewBoolean).
