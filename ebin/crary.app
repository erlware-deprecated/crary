%% This is the application resource file (.app file) for the crary, application.
{application, crary,
  [{description, "An HTTP server for the REST of us"},
   {vsn, "0.2.5"},
   {modules, [
              crary,
              crary_app,
              crary_body,
              crary_headers,
              crary_ctrl,
              crary_port,
              crary_sock,
              crary_sup,
              crary_util
             ]},
   {registered,[crary_sup]},
   {applications, [kernel, stdlib, sasl, uri, gtime]},
   {versioned_dependencies, [{uri, "0.1.0", gte},
                             {gtime, "0.9.4", gte}]},
   {mod, {crary_app,[]}},
   {start_phases, []}]}.

