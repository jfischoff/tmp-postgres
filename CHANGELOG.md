Changelog for tmp-postgres

1.7.1.0
  #35 Add Lenses for configuration.

1.8.0.0
  Get rid of `Partial` prefix. Add a `Complete` prefix to the internal configuration types.
  Remove the `partial` prefix.
  Expand abbreviations.
  Order the Event type creation time.

1.9.0.0
  #41 Configurable temporary directory.
  #59 Change EnvVars to EnvironmentVariables.

1.9.0.1
  Documentation fixes.

1.9.0.2
  Documentation fixes.

1.10.0.0
  #58 Add connection timeout.
  Rename partialPlanLoggerL to loggerL.
  Add temporaryDirectoryL.
  #31 A silent defaults.
  #20 Include stderr in errors.

1.11.0.0
  #90 Extend generated config to provide default Handles and connection timeout.
  #81 Create public Config module for better documentation organization.

1.12.0.0
  #64 Add the 'withNewDB' and 'withNewDBConfig' functions.

1.12.0.1
  Documentation fixes

1.13.0.0
  #108 'startNewDBConfig' functions added and related.

1.13.1.0
  #113 Faster shutdown using SIGQUIT and removed manual session termination.
  #115 Add password support to optionsToDefaultConfig

1.13.1.1
  #116 Don't create postgres or template1 databases in optionsToDefaultConfig

1.13.1.2
  #124 Respect database name in withNewDbConfig

1.14.0.0
  #126 Check for an empty data directory to give a better error message
  #114 Silent postgresql.conf
