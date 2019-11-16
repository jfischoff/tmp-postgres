module Plans where
import qualified Database.PostgreSQL.Simple.Options as Client
import Database.Postgres.Temp.Internal.Config
import Database.Postgres.Temp.Internal
import Data.Monoid

-- Set all the things we support
optionsToDefaultConfigFilledOutConfig :: Int -> Config
optionsToDefaultConfigFilledOutConfig thePort = optionsToDefaultConfig mempty
  { Client.dbname   = pure "fancy"
  , Client.user     = pure "some_user"
  , Client.port     = pure thePort
  , Client.password = pure "password"
  , Client.host     = pure "localhost"
  }

-- TODO make unix socket optionsToDefaultConfig plan

defaultIpConfig :: Config
defaultIpConfig = silentConfig
  { socketClass = IpSocket $ Last Nothing
  }

-- TODO add check of actual host
specificHostIpConfig ::Config
specificHostIpConfig = silentConfig
  { socketClass = IpSocket $ pure "localhost"
  }

-- TODO make specific unix socket plan

-- extraConfig ::
