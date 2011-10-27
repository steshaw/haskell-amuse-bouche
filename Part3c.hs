--
-- The "getHeader" example from the slides. See findMailbox.
--
module Part3c where

import qualified Data.Map as M
import Data.Time
import Data.Maybe (fromJust)
import System.Locale (defaultTimeLocale)

type Date = UTCTime

data MimeMessage = MimeMessage {headers :: M.Map String String}
  deriving (Show)

data Mailbox = Mailbox {name :: String}
  deriving (Show)

getHeader :: String -> MimeMessage -> Maybe String
getHeader name msg = M.lookup name (headers msg)

parseDate :: String -> Maybe Date
parseDate date = parseTime defaultTimeLocale "%d-%b-%Y" date

mailboxForDate :: Date -> Maybe Mailbox
mailboxForDate date = M.lookup date mailboxes

findMailbox :: MimeMessage -> Maybe Mailbox
findMailbox message = getHeader "Date" message >>= parseDate >>= mailboxForDate

dateOf :: String -> Date
dateOf s = fromJust (parseDate s)

mailboxes :: M.Map Date Mailbox
mailboxes = M.fromList 
  [(dateOf "01-Jan-2011", Mailbox {name = "Mailbox for 01-Jan-2011"})
  ,(dateOf "01-Jan-2010", Mailbox {name = "other"})
  ]

msg1 = MimeMessage {
  headers = M.fromList [("Date", "01-Jan-2011"), ("Subject", "greetings"), ("Body", "hi")]
}
msg2 = MimeMessage {
  headers = M.fromList [("Date", "oops"), ("Subject", "what the?"), ("Body", "blah blah")]
}
msg3 = MimeMessage {
  headers = M.fromList [("Date", "21-Feb-1955")]
}

eg1 = map findMailbox [msg1, msg2, msg3]
