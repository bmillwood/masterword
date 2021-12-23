module Words exposing (newWord)

import Task

newWord : Cmd (List Char)
newWord =
  Task.perform
    identity
    (Task.succeed ['C', 'H', 'E', 'A', 'T'])
