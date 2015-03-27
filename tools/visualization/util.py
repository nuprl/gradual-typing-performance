
def gen_name(fname, tag, suffix):
    # Cut suffix from `fname`, append `tag`
    return "%s-%s.%s" % (fname.split(".", 1)[0], tag, suffix)

