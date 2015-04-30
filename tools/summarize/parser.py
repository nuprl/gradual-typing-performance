"""
Parsing command-line arguments
"""

import constants
import graphdict
import sys
import util

def parse_options(argv):
    """
        Pre-processing for command line arguments.
        Pull the options out, return the filtered arguments
        and the parsed dictionary of options
    """
    # TODO actually implement this
    options = { "verbose" : constants.DEBUG }
    return argv[-1], options

def parse_args(argv):
    """ (-> (Listof String) (Pairof (Listof Any) (Dictof String Any)))
        Parse command-line arguments.
        Return the parsed args and a dictionary of option settings.
    """
    if len(argv) == 0:
        return None, None
    elif any((x in argv for x in ["help", "-help", "--help"])):
        return None, None
    target, options = parse_options(argv)
    gfile = graphdict.infer(target)
    if gfile is None:
        raise ValueError("Error: Could not find corresponding graph for file %s.\n  Suggestion: create a file '%s.graph' with columns 'MODULE\tINDEX\tREQUIRES' documenting\n  - The important modules in the project\n  - Their indexes in the configuration bitstrings\n  - The files these modules require." % (target, util.strip_suffix(target)))
    d = graphdict.of_file(gfile)
    return [target, d], options

