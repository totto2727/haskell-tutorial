robot :: (a, b, c) -> ((a, b, c) -> t) -> t
robot (name, attack, hp) message = message (name, attack, hp)

name :: (a, b, c) -> a
name (n,_,_)=n