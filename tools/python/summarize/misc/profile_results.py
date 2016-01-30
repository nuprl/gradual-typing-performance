import math
import statistics
import sys

Z = 1.96

def parse(fname):
    # Collect runtimes and thread counts
    times = []
    threads = []
    with open(fname, "r") as f:
       for line in f:
           line = line.strip()
           if line.startswith("Total cpu time observed:"):
               times.append(int(line.split()[4][:-2]))
           elif line.startswith("Threads observed:"):
               threads.append(int(line.split()[2]))
    if len(times) != len(threads):
        print("WARNING: have %s runtimes and %s threads" % (len(times), len(threads)))
    return times, threads

def print_stats(times, tag):
    print("Num. samples: %s" % len(times))
    mean = statistics.mean(times)
    print("Average %s: %.2f" % (tag, mean))
    print("Median %s: %s" % (tag, statistics.median(times)))
    print("Min %s: %s" % (tag, min(times)))
    print("Max %s: %s" % (tag, max(times)))
    delta = Z * (math.sqrt(statistics.variance(times)) / math.sqrt(len(times)))
    print("95%% confidence: %.2f -- %.2f" % (mean - delta, mean + delta))

def run(fname):
    times, threads = parse(fname)
    print_stats(times, "running time")
    print("====")
    print_stats(threads, "num. threads observed")

if __name__ == "__main__":
    if len(sys.argv) == 2:
        run(sys.argv[1])
    else:
        print("Usage: profile_results.py PROFILE-OUTPUT.log")
