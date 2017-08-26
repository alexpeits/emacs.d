(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s")

  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")

  (defengine wiktionary
    "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s")

  (defengine python
    "https://docs.python.org/3/search.html?q=%s&check_keywords=yes&area=default"
    :keybinding "p")

  (defengine hoogle
    "http://www.haskell.org/hoogle/?q=%s"
    :keybinding "h"))

(provide 'my-engines)
