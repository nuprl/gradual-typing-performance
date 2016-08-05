# to check: cpufreq-info
# `lscpu` is also useful

# 0 ondemand
# 1 conservative
# 2 userspace
# 3 powersave
# 4 performance

# Next try:
# https://software.intel.com/en-us/articles/intel-power-gadget-20
# https://software.intel.com/en-us/articles/intel-performance-counter-monitor

for i in {0..31}; do
  cpufreq-set -g performance -c $i
done
