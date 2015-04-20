"""
Very simple API for working with lists as sorted, bounded buffers.
"""

def insert(xs, val, metric, i):
    """
        Insert `val` into the reverse-order sorted list `xs`.
        The function `metric` is the sorting function (<)
        (i.e, the greatest element is at the head of the list)
        Do not increase the length of `xs`.
    """
    if i == len(xs):
        # Done, ignore `val`
        return xs
    elif xs[i] is None:
        # List unpopulated, just overwrite
        xs[i] = val
        return xs
    elif metric(xs[i], val):
        # `val` beats current list element,
        # replace and push current element back.
        tmp = xs[i]
        xs[i] = val
        val = tmp
    return insert(xs, val, metric, i+1)

