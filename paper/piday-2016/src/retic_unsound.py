from retic import List

## No chaperones = no blame!
## retic.transient.CheckError: 

def unsafe(x):
  x[0] = "hi"
  return

def typed(x : List(int)) -> int :
  unsafe(x)
  return x[0]

typed([1,2])

