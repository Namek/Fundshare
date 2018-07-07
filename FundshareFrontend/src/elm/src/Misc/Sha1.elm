module Misc.Sha1 exposing (sha1)

import Native.Sha1


sha1 : String -> String
sha1 =
    Native.Sha1.sha1
