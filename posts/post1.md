---
title: Example Blog Post
author: Stephen Diehl
date: 2013-11-13
mathjax: on
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam
non est in neque luctus eleifend. Sed tincidunt vestibulum
facilisis. Aenean ut pulvinar massa.

Some math:

$$
\begin{aligned}
\nabla \times \vec{\mathbf{B}} -\, \frac1c\, \frac{\partial\vec{\mathbf{E}}}{\partial t} & = \frac{4\pi}{c}\vec{\mathbf{j}} \\   \nabla \cdot \vec{\mathbf{E}} & = 4 \pi \rho \\
\nabla \times \vec{\mathbf{E}}\, +\, \frac1c\, \frac{\partial\vec{\mathbf{B}}}{\partial t} & = \vec{\mathbf{0}} \\
\nabla \cdot \vec{\mathbf{B}} & = 0 \end{aligned}
$$

Some code:

```haskell
newtype MonadMonoid m a = MonadMonoid { unMonad :: a -> m a }

instance Monad m => Monoid (MonadMonoid m a) where
  mempty = MonadMonoid return
  mappend f g = MonadMonoid (f >=> g)
```

Fin.
