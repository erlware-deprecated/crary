% This record characterises the connection from the browser to our
% server it is intended to be a consistent view derived from a bunch
% of different headers
-record(aitch_req,
        {vsn,                                % {Maj,Min}
         method,                             % 'GET'|'POST'|...
         uri,                                % Request URI
         headers,                            % aitch_headers
	 sock,                               % inet socket
	 opts = []                           % proplist
	}).


-define(HERE, io:format("~p:~p~n", [?MODULE, ?LINE])).
