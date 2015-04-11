# (-> String String String String)
def gen_name(fname, tag, suffix):
    # Cut original suffix from `fname`, append `tag`, a dot, and `suffix`
    return "%s-%s.%s" % (fname.split(".", 1)[0], tag, suffix)

# (-> Path-String Nat)
def count_lines(fname):
    # Count the number of lines in a file
    count = 0
    with open(fname, "r") as f:
        for line in f:
            count += 1
    return count

# (-> Path-String Nat)
def count_modules(fname):
    # Return the number of modules in the project that
    # the '.tab' file `fname` reports on
    with open(fname, "r") as f:
        _     = next(f)
        row   = next(f)
        title = row.split("\t", 1)[0]
    return len(title)

