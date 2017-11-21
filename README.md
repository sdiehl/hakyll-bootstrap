hakyll-bootstrap
================

<p align="center" style="padding: 20px; width: 50%">
<img src="https://raw.github.com/sdiehl/hakyll-bootstrap/master/sample.png">
</p>

A template for a small corporate Hakyll site.

**Using stack**

```bash
$ stack build
$ stack exec blog -- preview
```

**Using cabal**

To get started run:

```shell
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal run preview
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
generated from Markdown.

Inline math is enabled via setting the ``mathjax`` metadata to
``on``, by default MathJax is disabled.

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

Released under MIT License.
