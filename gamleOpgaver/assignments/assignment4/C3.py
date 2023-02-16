m = int(input())
n = int(input())

def swap(x,y):
  a = x
  x = y
  y = a
  return x,y

def gcd():
  global m
  global n
  g = 0
  if (n == 0):
    g += m
    print(g)
    g = 0
    assert(n==0)
  else:
    g += m % n
    m,n = swap(m,n)
    g,n = swap(g,n)
    gcd()
    g,n = swap(g,n)
    m,n = swap(m,n)
    assert(n!=0)

gcd()

print(m)
m = 0

print(n)
n = 0
