# First-class Functions

This branch has modifications to pass functions around as values; it has the
code we wrote in class plus:

- The necessary code in the EApp case to handle identifiers-as-functions
- A tweak to the Makefile to avoid some warnings about position-independent
  executables on OSX
- New instructions for jumping to a register value and aligning instruction
  addresses

See the changes in https://github.com/ucsd-cse131-sp17/lecture23/commit/0b2865b9334c92ba456ecfb4ba531268769780d6

To build what we were doing in class, use:

```
make output/lists.s
make output/lists.run
./output/lists.run
```
