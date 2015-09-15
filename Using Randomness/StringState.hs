import UCState

type StringState a = State String a

testState :: StringState Int
testState = do
    initState <- get -- Pull out current state
    let newState = "Hello " ++ initState
    put newState -- Replace old state
    return 1 
