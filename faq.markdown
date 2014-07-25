---
title: FAQ
---

 0. **Question:** this spec appears to be unclear about....

    **Answer:** We may have fixed it already!

    We have been making clarifications and in a few cases fixes to the
    specification in response to questions. You can see a [log of the changes
    we've made in the repository for this site](https://github.com/icfpcontest2014/icfpcontest2014.github.io/commits/source).

 1. **Question:** What happens when the Lambda-Man moves into an empty square and a fruit
    appears in that same tick? In the next Lambda-Man move scheduled for 127 ticks
    ahead, or 137?

    **Answer:** 127. In this extremely rare scenario, the fruit basically is pushed right
    into Lambda-Man's mouth, and doesn't slow him down. This is because he schedules
    his next move right after he moves, which is before the fruit appears, so he
    technically isn't eating at that stage.

