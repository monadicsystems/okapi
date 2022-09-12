---
title: Algebra
description: A bit about seeing Okapi as an algebra.
---

If we understand Okapi's operations and combinators as an algebra, we can apply laws like the distributive property of multiplication to simplify our server's logic.

---

## Okapi as an algebra

We can look at Okapi as an algebra where the parsers are values, and parser combinators like `<|>` are operations that act on those values. Okapi is a near-semiring. They are analogous to the values and operations on the set of all real
numbers:

{% table %}
* Okapi
* Algebra
---
* next
* 0
---
* N/A
* 1
---
* <|>
* \+
---
* do
* \*
---
{% /table %}

### Distributive Property

```haskell
-- ab + ac = a(b + c)
x = (do pa; pb;) <|> (do pa; pc;)
y = do
  pa
  (pb <|> pc)
-- x = y
```

### Zero Product Rule

```haskell
-- 0 * a = 0 * a = 0
x = do
  next
  pa
y = do
  pa
  next
z = next
-- x = y = z
```

### Zero Addition Rule

```haskell
-- 0 + a = a + 0 = a
x = next <|> pa
y = pa <|> next
z = pa
-- x = y = z
```


We could simplify `pingpong` by factoring out the common `methodGET` parser from the definitions of `ping` and `pong`:

```haskell
pingpong = do
  methodGET
  ping <|> pong

ping = do
  pathParam `is` "ping"
  return $ setJSON "pong" $ ok

pong = do
  pathParam `is` "pong"
  return $ setJSON "ping" $ ok
```

In many cases, we can apply algebraic properties like the distributive property to simplify our parsers.



