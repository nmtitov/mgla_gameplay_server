{application, gameplay_server,
  [{description, "An OTP application"},
    {vsn, "0.1.0"},
    {registered, []},
    {mod, { gameplay_server_app, []}},
    {applications,
      [kernel,
        stdlib,
        cowboy,
        jsx,
        gproc,
        lager
      ]},
    {env,[]},
    {modules, []},

    {maintainers, []},
    {links, []}
  ]}.