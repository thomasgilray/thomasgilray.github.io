# Here is our code from class and some exercises to try at home.
#
#  1. Is this code correct, can you write some tests for it? Write at least a
#     dozen, and check your answers against Python's `re` module.
#
#  2. If it's correct, is it efficient? What would efficient mean for
#     matching strings against regular expressions? Try taking derivatives of
#     a regular expression over a 20-character string, printing the regular
#     expression at each step. What do you observe?
#
#  3. Can you make it any more efficient? Are there different coding
#     approaches to doing this? What's the cleanest way to improve this code?
#
#  4. Can you improve the code rendering regular expressions as strings so it
#     always prints them with the minimal number of parentheses? This is the
#     hardest one here; you will need to think about precedence.
#
#  5. Can you turn our matches function from class into a matches method that
#     all regular expressions inherit?
#
#  6. Can you employ method dispatch to avoid using `isinstance` in Python?
#     What method could you add to every class so that the caller never has
#     to ask what type it is?
#
#  7. How would you use your code to match a phone number? Can you support a
#     few different phone-number formats?
#
#  8. Right now Char('a') == Char('a') is False. Can you add `__eq__` (and
#     `__hash__`) so that two regular expressions built separately are equal
#     when they have the same structure? Then use it to write a simplify
#     method, and check that simplifying twice does no more than simplifying
#     once.
#
#  9. (Optional) I wouldn't ask you to understand this for class or on an
#     exam, but can you derive our derivatives for regular expressions from
#     our interpreter for regular expressions and our definition of D_c(L)
#     for formal languages L? Can you relate our code to the traditional
#     chain rule or product rule for derivatives?

class Empty:
  def __str__(self):
    return "∅"
  def D(self, c):
    return self
  def containsEmpty(self):
    return False

class Epsilon:
  def __str__(self):
    return "ε"
  def D(self, c):
    return Empty()
  def containsEmpty(self):
    return True

class Char:
  def __init__(self, x):
    self.x = x
  def __str__(self):
    return self.x
  def D(self, c):
    if c == self.x:
      return Epsilon()
    else:
      return Empty()
  def containsEmpty(self):
    return False

class Disj:
  def __init__(self, r0, r1):
    self.r0 = r0
    self.r1 = r1
  def __str__(self):
    return f"({self.r0}|{self.r1})"
  def D(self, c):
    re0 = self.r0.D(c)
    re1 = self.r1.D(c)
    if isinstance(re0, Empty): return re1
    if isinstance(re1, Empty): return re0
    return Disj(re0, re1)
  def containsEmpty(self):
    return self.r0.containsEmpty() or self.r1.containsEmpty()

class Seq:
  def __init__(self, r0, r1):
    self.r0 = r0
    self.r1 = r1
  def __str__(self):
    return f"({self.r0}{self.r1})"
  def D(self, c):
    d0 = self.r0.D(c)
    if self.r0.containsEmpty():
      if isinstance(d0, Empty): return self.r1.D(c)
      return Disj(Seq(d0, self.r1), self.r1.D(c))
    else:
      return Seq(d0, self.r1)
  def containsEmpty(self):
    return self.r0.containsEmpty() and self.r1.containsEmpty()

class Star:
  def __init__(self, r0):
    self.r0 = r0
  def __str__(self):
    return f"({self.r0}*)"
  def D(self, c):
    d0 = self.r0.D(c)

    return Seq(d0, self)
  def containsEmpty(self):
    return True

def matches(re, s):
  for c in s:
    re = re.D(c)
  return re.containsEmpty()


# A few examples to run.

# The regular expression a, matched against the string "a".
print(matches(Char('a'), "a"))

# (a|b)*: any string of as and bs at all.
ab_star = Star(Disj(Char('a'), Char('b')))
print(ab_star, "matches", "abba", "?", matches(ab_star, "abba"))

# ab*c: an a, then any number of bs, then a c.
abc = Seq(Char('a'), Seq(Star(Char('b')), Char('c')))
print(abc, "matches", "abbbc", "?", matches(abc, "abbbc"))

# A fun one: can you figure out what this language encodes? Can you state it
# in plain english?
aa = Seq(Char('a'), Char('a'))
bb = Seq(Char('b'), Char('b'))
ab_ba = Disj(Seq(Char('a'), Char('b')), Seq(Char('b'), Char('a')))
mystery = Star(Disj(Disj(aa, bb), Seq(ab_ba, Seq(Star(Disj(aa, bb)), ab_ba))))
for s in ["", "ab", "aa", "abba", "aabb", "abab", "baab", "aab", "abaa"]:
  print(f"{s!r}: {matches(mystery, s)}")
