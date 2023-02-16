def fac(n):
  factorial = 1

  if n < 0:
    print("Sorry, factorial does not exist for negative numbers")
  elif n == 0:
    return factorial
  else:
    for i in range(1, n + 1):
      factorial = factorial*i
  return(factorial)

def  binom(i, n, s):
    return  fac(n)/(fac(i)*fac(n-i))*(s ** i)*((1-s)**(n-i))

res1 = binom(2, 39, 1/9)*(1-binom(0,37,1/9))
res2 = binom(1, 39, 1/9)*(1-(binom(0,38,1/9) + binom(1,38,1/9)))
res3 = binom(0, 39, 1/9)*(1-(binom(0,39,1/9) + binom(1,39,1/9) + binom(2,39,1/9)))
print(res1)
print(res2)
print(res3)
res = res1 + res2 + res3

print("The probability is", res)
