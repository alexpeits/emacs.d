# Usage:
# python solarized_brighten.py [args]

# args are:
# -b/--black to create another theme with black background
# -a/--amount to tweak the amount of brightness
# -c/--custom custom lisp folder name (not path, inside .emacs.d in my case)
# -t/--themes custom theme folder name (not path, inside .emacs.d in my case)

# This creates 2 files, one in the custom folder and one in the themes folder.
# If the -b/--black flag is used, then the files are not overwritten, but a
# "-black" extension is used.

# Themes then have the name solarized-brighter-dark and
# solarized-brighter-black-dark
import os
import re
import argparse

import requests


EMACS_DIR = os.path.expanduser('~/.emacs.d')
BLACK_BASE03 = '#222322'
BLACK_BASE02 = '#3b3b3b'


def parse_args():
    parser = argparse.ArgumentParser(
        description='Bootstrap customizations for solarized theme'
    )
    parser.add_argument(
        '-a', '--amount', dest='amount', action='store', type=int, default=9,
        help='Amount to brighten theme'
    )
    parser.add_argument(
        '-b', '--black', dest='black', action='store_true', default=False,
        help='Create black background theme'
    )
    parser.add_argument(
        '-c', '--custom', dest='custom', action='store', required=True,
        help='Custom lisp directory name'
    )
    parser.add_argument(
        '-t', '--themes', dest='themes', action='store', required=True,
        help='Custom themes directory name'
    )
    return parser.parse_args()


def create_repl_func(black, amount):

    def _repl_func(m):
        n = m.group(1)
        v = m.group(2)
        newv = '(color-lighten-name "{}" {})'
        amt = amount

        if n == 's-base03' and black:
            return '({} "{}")'.format(n, BLACK_BASE03)
        if n == 's-base02' and black:
            return '({} "{}")'.format(n, BLACK_BASE02)

        if n in ['s-base00', 's-base0']:
            amt = int(amt * 1.2)
        elif n == 's-base01':
            amt = int(amt - amt * 0.3)
        elif n in ['green', 'yellow']:
            amt = int(amt * 0.5)
        elif 'base' in n:
            return '({} "{}")'.format(n, v)

        return '({} {})'.format(n, newv.format(v, amt))

    return _repl_func


def get_lisp_fn(custom_dir, black):
    if black:
        fn = 'solarized-brighter-black.el'
    else:
        fn = 'solarized-brighter.el'
    return os.path.join(EMACS_DIR, custom_dir, fn)


def get_theme_fn(theme_dir, black):
    if black:
        fn = 'solarized-brighter-black-dark-theme.el'
    else:
        fn = 'solarized-brighter-dark-theme.el'
    return os.path.join(EMACS_DIR, theme_dir, fn)


def main():
    args = parse_args()

    base_url = (
        'https://raw.githubusercontent.com/bbatsov/solarized-emacs/master'
    )
    solarized = requests.get('{}/{}'.format(base_url, 'solarized.el'))
    solarized_dark = requests.get(
        '{}/{}'.format(base_url, 'solarized-dark-theme.el')
    )

    if args.black:
        repl = 'solarized-brighter-black'
    else:
        repl = 'solarized-brighter'

    lisp_file = get_lisp_fn(args.custom, args.black)
    theme_file = get_theme_fn(args.themes, args.black)
    repl_func = create_repl_func(args.black, args.amount)

    tmp = re.sub(
        r'\(([a-z0-9\-]+)\ +"([#a-fa-f0-9]+)"\)', repl_func, solarized.text
    )
    sol_bright = re.sub(r'solarized', repl, tmp)

    sol_dark_bright = re.sub(r'solarized', repl, solarized_dark.text)

    with open(lisp_file, 'w') as lf:
        lf.write(sol_bright)

    with open(theme_file, 'w') as lf:
        lf.write(sol_dark_bright)


if __name__ == '__main__':
    main()
