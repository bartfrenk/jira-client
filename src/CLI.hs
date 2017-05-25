module CLI where


import Data.Text

type JQL = Text

data Command
  = Search
    { jql :: JQL }

