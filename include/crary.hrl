
-record(crary_req,
        {vsn,                                % {Maj,Min}
         method,                             % "GET"|"POST"|...
         uri,                                % Request URI
         headers,                            % crary_headers()
         sock,                               % inet socket
         opts = []                           % proplist
        }).


-define(HERE, io:format("~p:~p~n", [?MODULE, ?LINE])).
