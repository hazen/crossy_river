crossy_river
============

A farmer is traveling with his dog, a chicken, and a bag of grain. He comes to a river,
which has a small ferry boat. The boat has room for the farmer, and one other item
(dog, chicken, or grain). The farmer knows the dog will eat the chicken if left alone
with it; likewise, the chicken will eat the grain. How does he get everything to the other
side uneaten? The current state of the puzzle can be represented by a string:
f farmer d dog c chicken g grain ~ the river And moves by using an arrow:
`xy>` or `<xy fdcg~ fc> dg~fc`
Your program should take an initial state, and a sequence of moves, one per line.
Compute the final state resulting from applying the moves, or stop whenever something gets eaten.
 
Bonus parts:
- **generalize** -- use a list of items, and disallowed pairings (e.g. c eats g)
- **solve** -- generate a winning sequence of moves
 
Sample Input 1:
```
fdcg~
fc>
<f
fd>
```
Sample Output 1:
```
g~fcd
```
Sample Input 2:
```
fdcg~
fc>
<f
fg>
<f
```
Sample Output 2:
```
fd~cg
Grain was eaten.
```

Running
-------
Once the project has been built with `rebar3 compile`, use the included shell
script to launch the project.  Options include:

```bash
$ bin/crossy_river -r <replay file> -c <custom file> -s
```

The replay file has a format of
```
<initial state>
<move 1>
...
<move n>
``` 
Where the initial state has one letter for each item (f, c, d or g) by default as
outlined in Sample Input 1 above.  The moves have the letters of the farmer and
the items being moved along with the direction `<` or `>` also in the above
example

The custom file allows non-standard actors to be defined.  Each item has a letter
and a longer, human-readable name.  Optionally some items may wish to eat other
items which are defined as the following example file:
```
c is Chicken
d is Dog
g is Grain
c eats g
d eats c
```
This allows for completely different scenarios to be attempted.

Finally the `-s` flag means to allow the program to successfully move everything
from the left riverbank to the other successfully, i.e., nothing gets eaten and
the attempted series of moves is returned.

Examples
-------
To test out some of the above configurations, you can try these:
- Partial solution
```bash
$ bin/crossy_river -r samples/replay1
```
- Item is eaten
```bash
$ bin/crossy_river -r samples/replay2
```
- Successful crossing
```bash
$ bin/crossy_river -r samples/replay3
```
- Successful crossing with a standard customization file
```bash
$ bin/crossy_river -r samples/replay3 -c samples/custom1
```
- Item is eaten with a modified customization file
```bash
$ bin/crossy_river -r samples/replay4 -c samples/custom2
```