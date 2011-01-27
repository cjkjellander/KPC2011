{application, contest_server,
 [{description, "Simple login server for Klarna Programmer's Contest"},
  {vsn, "0.1.0"},
  {modules, [ cs_app
            , cs_sup
            , cs_server
            , cs_db
            ]},
  {registered, [cs_sup]},
  {applications, [kernel, stdlib]},
  {mod, {cs_app, []}}
]}.
