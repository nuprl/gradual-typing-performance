"""
    Representation of a module graph.

    Nodes are module names. For each node, we have:
    - an index, i.e. the module's position in a configuration bitstring
    - a list of required modules. These are edges in the graph.

    Someday we might abstract this to other configurations, input formats, and representations.
"""

import constants
import os
import shell
import util

class ModuleGraph(object):
    ### Fields ##############################################################################
    module_names = None ## (Listof String), names of the modules in the project
    indices      = {}   ## String -> Natural, map from module names to config indices
    requires     = {}   ## String -> (Listof String), map from module names to their requires
    source_file  = None ## Raw truth file

    ### API ####################################################################

    def __init__(self, fname):
        if fname.endswith(".graph"):
            self.source_file = fname
        else:
            self.source_file = self.infer_graph(fname)
        self.module_names = []
        self.init_from_graph(self.source_file)

    def edges_iter(self):
        """
            Iterate over the "require" edges represented in
            the GraphDict `d`.
        """
        for mod_name in self.module_names:
            for req in self.requires[mod_name]:
                yield(mod_name, req)

    def get_module_names(self):
        return self.module_names

    def index_of_module(self, mod_name):
        return self.indices[mod_name]

    def requires_of_module(self, mod_name):
        return self.requires[mod_name]

    ### Helpers ################################################################

    def init_from_graph(self, gname):
        """ (-> Path-String Void)
            Convert a .graph file to a GraphDict object
        """
        with open(gname, "r") as f:
            self.check_titles(next(f).strip().split(constants.SEP), gname)
            for line in f:
                [mname, i, requires] = self.check_column(line.strip().split(constants.SEP))
                self.module_names.append(mname)
                self.indices[mname] = i
                self.requires[mname] = requires

    ### Input Validation 
    def check_titles(self, col_names, fname):
        """
            Check that the given column titles are well-formed.
            (These titles probably came from a .graph file)
        """
        len_ok = (len(col_names) == 3)
        if not len_ok:
            raise ValueError("expected 3 columns in '%s', got %d columns" % (fname, len(col_names)))
        c0_ok = col_names[0] == "MODULE"
        if not c0_ok:
            raise ValueError("expected first column of '%s' to have title 'MODULE', instead has title '%s'" % (fname, col_names[0]))
        c1_ok = col_names[1] == "INDEX"
        if not c1_ok:
            raise ValueError("expected second column of '%s' to have title 'INDEX', instead has title '%s'" % (fname, col_names[1]))
        c2_ok = col_names[2] == "REQUIRES"
        if not c2_ok:
            raise ValueError("expected third column of '%s' to have title 'REQUIRES', instead has title '%s'" % (fname, col_names[2]))
        #Everything's good!

    def check_column(self, values):
        # Check that column values are well-formed.
        # If so, return the parsed values.
        len_ok = 1 < len(values) < 4 # Expecting 2-3 columns (requires are optional)
        if not len_ok:
            raise ValueError("expected 2 or 3 columns in row, got %d columns" % len(values))
        module_name = values[0]
        if not module_name.endswith(".rkt"):
            raise ValueError("expected module name to end with '.rkt', instead got '%s'" % module_name)
        index = None
        try:
            index = int(values[1])
        except ValueError:
            raise ValueError("expected integer INDEX, got %s" % values[1])
        requires = values[2].split(",") if len(values) == 3 else []
        if not (all((rq.endswith(".rkt") for rq in requires))):
            raise ValueError("expected each REQUIRE to be a .rkt filename, instead got '%s'" % requires)
        return [module_name, index, requires]

    def infer_graph(self, fname):
        """
            Try to find the .graph file associated with `fname`
        """
        prefix = util.strip_suffix(fname)
        gfile1 = "%s.graph" % prefix
        gfile2 = "%s.graph" % prefix.split("-", 1)[0]
        tag = prefix.rsplit("/", 1)[-1]
        gfile3 = "%s.graph" % tag
        gfile4 = "%s/%s.graph" % (tag, tag)
        if os.path.exists(gfile1):
            return gfile1
        elif os.path.exists(gfile2):
            return gfile2
        elif os.path.exists(gfile3):
            return gfile3
        elif os.path.exists(gfile4):
            return gfile4
        else: ## Last resort, try searching for the first result
            return shell.find_file(gfile3)

