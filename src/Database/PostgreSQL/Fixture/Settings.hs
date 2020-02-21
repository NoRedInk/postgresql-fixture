module Database.PostgreSQL.Fixture.Settings
  ( Settings
      ( Settings,
        pgConnection
      ),
    ConnectionSettings
      ( ConnectionSettings,
        pgDatabase,
        pgUser,
        pgHost,
        pgPassword,
        pgPort
      ),
    PgDatabase (PgDatabase, unPgDatabase),
    PgUser (PgUser, unPgUser),
    PgHost (PgHost, unPgHost),
    PgPassword (PgPassword, unPgPassword),
    PgPort (PgPort, unPgPort),
    defaultSettings,
  )
where

-- import qualified Data.Text as Text (isPrefixOf)
-- import Database.PostgreSQL.Typed
--   ( PGDatabase,
--     defaultPGDatabase,
--     pgDBAddr,
--     pgDBName,
--     pgDBPass,
--     pgDBUser,
--   )
import Network.Socket (SockAddr (SockAddrUnix))
import System.FilePath ((</>))

data Settings
  = Settings
      { pgConnection :: ConnectionSettings
      }
  deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings
  { pgConnection = ConnectionSettings
      { pgDatabase = PgDatabase "noredink_dev",
        pgUser = PgUser "noredink_dev",
        pgHost = PgHost "localhost",
        pgPassword = PgPassword "",
        pgPort = PgPort 8088
      }
  }

data ConnectionSettings
  = ConnectionSettings
      { pgDatabase :: PgDatabase,
        pgUser :: PgUser,
        pgHost :: PgHost,
        pgPassword :: PgPassword,
        pgPort :: PgPort
      }
  deriving (Eq, Show)

newtype PgPort
  = PgPort {unPgPort :: Int}
  deriving (Eq, Show)

newtype PgPassword
  = PgPassword {unPgPassword :: String}
  deriving (Eq, Show)

newtype PgHost
  = PgHost {unPgHost :: String}
  deriving (Eq, Show)

newtype PgUser
  = PgUser {unPgUser :: String}
  deriving (Eq, Show)

newtype PgDatabase
  = PgDatabase {unPgDatabase :: String}
  deriving (Eq, Show)
-- toPGDatabase :: Settings -> PGDatabase
-- toPGDatabase
--   Settings
--     { pgConnection =
--         ConnectionSettings
--           { pgDatabase,
--             pgUser,
--             pgHost,
--             pgPassword,
--             pgPort
--           }
--     } =
--     defaultPGDatabase
--       { pgDBName = toS (unPgDatabase pgDatabase),
--         pgDBUser = toS (unPgUser pgUser),
--         pgDBPass = toS <| Log.unSecret (unPgPassword pgPassword),
--         pgDBAddr =
--           -- The rule that PostgreSQL/libpq applies to `host`:
--           --
--           --   If this begins with a slash, it specifies Unix-domain
--           --   communication rather than TCP/IP communication; the value is the
--           --   name of the directory in which the socket file is stored
--           --
--           -- https://www.postgresql.org/docs/9.6/libpq-connect.html#LIBPQ-CONNECT-HOST
--           if "/" `Text.isPrefixOf` host
--             then
--               toS host </> ".s.PGSQL." ++ show port
--                 |> SockAddrUnix
--                 |> Right
--             else Left (toS host, show port)
--       }
--     where
--       host = unPgHost pgHost
--       port = unPgPort pgPort
