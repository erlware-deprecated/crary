%% This is the application resource file (.app file) for the aitch,
%% application.
{application, aitch, 
  [{description, "an aitch-tee-tee-pee (http) server for the REST of us"},
   {vsn, "0.1.0"},
   {modules, [
              aitch,
              aitch_app,
              aitch_body,
              aitch_headers,
              aitch_ctrl,
              aitch_port,
              aitch_sock,
              aitch_sup,
              aitch_util,
	      example
             ]},
   {registered,[aitch_sup]},
   {applications, [kernel, stdlib]},
   {mod, {aitch_app,[]}},
   {start_phases, []}]}.

