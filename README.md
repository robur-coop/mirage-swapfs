## mirage-swapfs

Append-only ephemeral, anonymous files.

Swapfs is useful for temporarily putting large data on persistent storage,
for example if the data is too large to comfortably sit in memory.
The data can then be read back in chunks.

See also [mirage/README.md](mirage/README.md) with an example unikernel.
