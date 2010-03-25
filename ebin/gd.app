{application, gd,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             gd,
             gd_test,
             gd_app,
             gd_sup
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { gd_app, []}},
  {env, []}
 ]}.
