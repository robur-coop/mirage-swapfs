## swappy

This example unikernel exercises swapfs.
It generates 50 MiB of data in 1 KiB chunks.
The whole data is consumed and then verified to contain all `E`s.

The default mode is to store the whole data as a string in memory.
With the `--use-swap` option a `swapfs` is used instead.

At the time of writing `swappy --use-swap` can run fine with just 10 MB of memory using a 100 MB block device.
With 10 MB of memory it can use up to (but not including) ~124 GB block device as swap before running out of memory.
Meanwhile `swappy` without `--use-swap` requires ~300 MB of memory to run without running out of memory.

```shell
$ fallocate -l 100M swap
$ time solo5-hvt --mem=10 --block:swap=swap -- dist/swappy.hvt --solo5:quiet --use-swap
2024-11-04T14:25:30-00:00:  [application] Success!

real	0m2.490s
user	0m2.057s
sys	0m0.368s
$ solo5-hvt --mem=256 --block:swap=swap -- dist/swappy.hvt --solo5:quiet  --backtrace false
Fatal error: exception Out of memory
$ time solo5-hvt --mem=300 --block:swap=swap -- dist/swappy.hvt --solo5:quiet 
2024-11-04T14:23:43-00:00:  [application] Success!

real	0m0.342s
user	0m0.180s
sys	0m0.094s
```
