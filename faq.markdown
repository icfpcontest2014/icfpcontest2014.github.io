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

 2. **Question:** Do ghost programs get executed every tick or just the ticks
    when the ghost is scheduled to move?

    **Answer:** Only on the ticks when the ghost is scheduled to move. We have
    updated that paragraph in the spec to be less confusing.

 3.  **Question:** The JavaScript simulator just stops and I see an error
     message in the browser debug console. What's up?

     **Answer:** You've managed to tickle an error case in the simulator
     and it's thrown an exception and then stopped. Report it and we'll try
     and fix it.

     For example:

         World/Simulator.hs:290:9-36: Irrefutable pattern failed for pattern Data.Maybe.Just n

     This one appeared in the game simulator between 3am and 10am UK time
     because we accidentally left some debug code in the sim, sorry!

 4.  **Question:** How do I access the world's iniital state? I tried `LD 0 0`
     in main, but I get an error!

     **Answer** You're probably trying to load main in the Lambda-Man
     simulator. The main function is initialised in the [game simulator](http://icfpcontest.org/game.html).
