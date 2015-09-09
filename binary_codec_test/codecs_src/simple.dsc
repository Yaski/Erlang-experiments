module : #3F
version : 0.1

connect(server)

say(server)
    user : uint8
    setting : int32
    distance : float
    message : uint8[]

listen(client)
    setting : uint8
    port : float

message(client)
    session : uint8
    distance : float
    text : uint8[]
