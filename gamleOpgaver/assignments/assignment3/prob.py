# Number of rolls and sides on dice
die_rolls = 39
die_sides = 9

# Probability that result is lower than 8
lo_prob = (7 / die_sides)
# Probability that result a specific roll, here either 8 or 9
hi_prob = (1 / die_sides)

# Cases for the first 3 dice. 'e' is for 8, 'n' is for 9, and 'x' is for 1-7.
# The probability of the case is multiplied by the number of permutations.

# All cases where neither 8 nor 9 is included
xxx = ((7**3) / (9**3))
# Cases xx8 x8x 8xx
exx = (lo_prob**2 * hi_prob) * 3
# Cases xx9 x9x 9xx
nxx = (lo_prob**2 * hi_prob) * 3
# Cases 88x 8x8 x88
eex = (lo_prob * hi_prob**2) * 3
# Cases 99x 9x9 x99
nnx = (lo_prob * hi_prob**2) * 3
# Cases 98x 89x 8x9 9x8 x89 x98
nex = (lo_prob * hi_prob**2) * 6
# Cases 998 989 899
nne = (hi_prob**3) * 3
# Cases 889 898 988
nee = (hi_prob**3) * 3
# Case 888
eee = hi_prob**3


for i in range(die_rolls - 3):
    tmp_xxx = xxx * lo_prob
    tmp_exx = (exx * lo_prob) + (xxx * hi_prob)
    tmp_nxx = (nxx * lo_prob) + (xxx * hi_prob)
    tmp_eex = (eex * lo_prob) + (exx * hi_prob)
    tmp_nnx = (nnx * lo_prob) + (nxx * hi_prob)
    tmp_nex = (nex * lo_prob) + (nxx * hi_prob) + (exx * hi_prob)
    tmp_eee = (eee * lo_prob) + (eex * hi_prob) + (eee * hi_prob)
    tmp_nne = (nne * lo_prob) + (nne * hi_prob) + (nee * hi_prob) + (nex * hi_prob) + (nnx * hi_prob)
    tmp_nee = (nee * lo_prob) + (nee * hi_prob) + (eee * hi_prob) + (nex * hi_prob) + (eex * hi_prob)
    xxx = tmp_xxx
    exx = tmp_exx
    nxx = tmp_nxx
    eex = tmp_eex
    nnx = tmp_nnx
    nex = tmp_nex
    eee = tmp_eee
    nne = tmp_nne
    nee = tmp_nee


# Print the result with 9 decimals
print("The probability is %.9f" % (eee + nee + nne))
