{   
    application, gameserver,
    [   
        {description, ""},   
        {vsn, "1"},   
        {modules,[gameserver]},
        {registered, [gameserver_app]},
        {applications, [kernel, stdlib, sasl]},   
        {mod, {gameserver_app, []}},
        {start_phases, []},
        {env,[{server, ""}]}
    ]   
}.    
 
