#!/usr/bin/env python
################################################################################
#
# sudoku-solver.py
#
# A fully-featured sudoku solving Python project.
#
# Pierce Darragh
# (c) 2016
#
################################################################################

import argparse
import sys

def version():
    return "[version information]"

def usage(command=None):
    print(version())

    information = {}
    information['solve'] = '\n'.join([
        "solve"
    ])

    information['verify'] = '\n'.join([
        "verify"
    ])

    information['generate'] = '\n'.join([
        "generate"
    ])

    if command in information:
        print(information[command])
    else:
        print('\n'.join([
            "generic"
        ]))

if __name__ == '__main__':
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument('-v', '--version', action='store_true')

    args = parser.parse_known_args()
    if args[0].version:
        print(version())
        sys.exit(0)

    print(args)

    subparsers = parser.add_subparsers(dest='subcommand')

    parser_version = subparsers.add_parser('version', add_help=False)

    parser_help = subparsers.add_parser('help', add_help=False)
    parser_help.add_argument(
        'command',
        choices = [
            'solve',
            'verify',
            'generate'
        ],
        nargs = '?',
        default = None
    )

    parser_solve = subparsers.add_parser('solve', add_help=False)
    parser_solve.add_argument('filename')

    parser_verify = subparsers.add_parser('verify', add_help=False)
    parser_verify.add_argument('filename')

    parser_generate = subparsers.add_parser('generate', add_help=False)
    parser_generate.add_argument('width', type=int, nargs='?', default=3)
    parser_generate.add_argument('height', type=int, nargs='?', default=3)
    parser_generate.add_argument('holes', type=int, nargs='?', default=45)

    for subparser in [parser_solve, parser_verify, parser_generate]:
        subparser.add_argument('-h', '--help', action='store_true')

    show_help = False

    if '-h' in args[1] or '--help' in args[1]:
        usage()
        sys.exit(0)

    args = parser.parse_args(args[1])

    if args.subcommand == 'help':
        usage(command=args.command)
        sys.exit(0)

    if args.subcommand == 'version':
        print(version())
        sys.exit(0)

    print "reached the end"
