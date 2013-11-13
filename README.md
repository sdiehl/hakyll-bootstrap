hakyll-bootstrap
================

<p align="center" style="padding: 20px; width: 50%">
<img src="https://raw.github.com/sdiehl/hakyll-bootstrap/master/sample.png">
</p>

A template for a small corporate Hakyll site.

To get started run:

```shell
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal run preview
```

To compile the full executable site generator use:

```shell
$ ghc --make build.hs
$ ./build rebuild
```

The default static pages are renderd with plain HTML with mixins
from the ``/templates`` folder..

```
index.html

pages/
  about.html
  contact.html
  privacy.html
  signup.html
  team.html
  tos.html
```

Blog posts are placed under the ``/posts`` folder and are
composed in Markdown.

Inline math is enabled via setting the ``mathjax`` metadata to
``on``.

```text
---
title: Example Blog Post
author: Stephen Diehl
date: 2013-11-13
mathjax: on
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam
non est in neque luctus eleifend. Sed tincidunt vestibulum
facilisis. Aenean ut pulvinar massa.
```

License
--------

Released under MIT License. Do what you wish.
