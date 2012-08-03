  - Race, Feat, etc. and Class should have similar implementations. Rewrite all
    of them to use either the records approach or the Class + existential type
    approach. The advantage of the latter is that it would allow for easier 
    pattern-matching if we put them all in a GADT.
    NB: Feat currently lacks any implementation at all.
  - Getters for computed values (total value of a skill, etc.) should be
    implemented.
  - Try and implement a race, a class, a feat, etc. and see if it works, and if
    not, make it works.
