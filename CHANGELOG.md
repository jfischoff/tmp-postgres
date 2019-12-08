Changelog for tmp-postgres

1.29.0.1
  #214 Parallel stop is 8 ms faster.

1.29.0.0
  #211 Revert behavior of Permanent to not create the directory if it does not exist
  #210 The signature of takeSnapshot is not as useful as it could be breaking change

1.28.1.0
  #208 Only hash important envs for initdb cache.

1.28.0.0
  #200 Change behavior of Permanent. We now create the directory if it does not exist. Other changes

1.27.0.4
  #193 and #194 Doc fixes

1.27.0.3
  #195, #196, #197, #198 Doc Fixes

1.27.0.2
  Doc Fixes

1.27.0.1
  Doc Fixes

1.27.0.0
  #191 Remove leading spaces in code examples
  #192 startConfig bullet points are out of date
  #189 Remove reloadConfig

1.26.0.0
  #187 Remove stopPostgresGracefully from public interface

1.25.0.1
  #188 Document all fields of Config

1.25.0.0
  #186 Rename CacheResources and cacheResourcesToConfig

1.24.0.0
  #185 rename makeDataDirPermanent to makeDataDirectoryPermanent

1.23.0.3
  #122 Try to minize failed deletes ... again.

1.23.0.2
  #183 More Doc

1.23.0.1
  #182 Doc Fixies

1.23.0.0
  #175 Flatten Config
  #178 Documentation Fixes
  #179 Remove Public Config.hs
  #180 Remove Lenses

1.22.0.0
  #174 Better postgresql.conf type.

1.21.1.1
  #172 Fix verboseLogging not working

1.21.1.0
  #135 Add defaultConfig_9_3_10
  #170 Expose cacheConfig
  #169 Documentation Reorg

1.21.0.0
  #165 Make silent the default.

1.20.0.1
  #164 Documentation fixes.

1.20.0.0
  #144 Make a Cache type.
  #162 Make a Snapshot alias.

1.19.0.1
  #146 Better version hashing for making cache directories.
  Documentation fixes

1.19.0.0
  #154 Change 'createDefaultCacheConfig` to `defaultCacheConfig`.

1.18.0.0
  #143 Remove the withNewDb API

1.17.0.0
  #156 Deprecate `NewDb` functions.
  #155 Better monoids for `initDbConfig` and `createDbConfig`.
  #142 Cluster save points.

1.16.1.0
  #152 Add stopPostgresGracefully function.

1.16.0.0
  #149 CopyDirectoryCommand partial and rename the old CopyDirectoryCommand to
  CompleteCopyDirectoryCommand.

1.15.1.1
  #141 Documentation fixes.

1.15.1.0
  #119 Add `initdb` cache
  #134 Expand tilde in Permanent DirectoryType setup.

1.15.0.0
  #137 Remove SocketClass and listen unconditionally on 127.0.0.1, ::1 and a UNIX socket.
  #138 Fix bug where optionsToDefaultConfig would make createdb plan even if one is not needed.

1.14.1.0
  #122 Fix bug that would prevent temporary directory removal

1.14.0.1
  #129 Fix resource leak in executeProcess

1.14.0.0
  #126 Check for an empty data directory to give a better error message
  #114 Silent postgresql.conf

1.13.1.2
  #124 Respect database name in withNewDbConfig

1.13.1.1
  #116 Don't create postgres or template1 databases in optionsToDefaultConfig

1.13.1.0
  #113 Faster shutdown using SIGQUIT and removed manual session termination.
  #115 Add password support to optionsToDefaultConfig

1.13.0.0
  #108 'startNewDBConfig' functions added and related.

1.12.0.1
  Documentation fixes

1.12.0.0
  #64 Add the 'withNewDB' and 'withNewDBConfig' functions.

1.11.0.0
  #90 Extend generated config to provide default Handles and connection timeout.
  #81 Create public Config module for better documentation organization.

1.10.0.0
  #58 Add connection timeout.
  Rename partialPlanLoggerL to loggerL.
  Add temporaryDirectoryL.
  #31 A silent defaults.
  #20 Include stderr in errors.

1.9.0.2
  Documentation fixes.

1.9.0.1
  Documentation fixes.

1.9.0.0
  #41 Configurable temporary directory.
  #59 Change EnvVars to EnvironmentVariables.

1.8.0.0
  Get rid of `Partial` prefix. Add a `Complete` prefix to the internal configuration types.
  Remove the `partial` prefix.
  Expand abbreviations.
  Order the Event type creation time.

1.7.1.0
  #35 Add Lenses for configuration.
