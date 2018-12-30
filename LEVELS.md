### Level 0 Broken bcast

- Crash model
- FairLossLink (UDP) + Bcast
- Use Drop to get a different result in one (correct) node

### Level 1 Fix Bcast (beb broadcast)

- PerfectLink (TCP)
- Drop but Send again

### Level 2 (broken beb broadcast)

- Crash sender a node to get a different result in a correct process

### Level 3 fix level 2 (regular broadcast) 

- 

### Level 4 (broken regular broadcast)

- Crash a node to get a different result in a (non correct) process

### Level 5 fix level 4 (uniform reliable broadcast)

- Crash a node and get the same result in all processes