If we don't spark new task until first argument of `par` becomes WHNF, two arguments of `par` will not be executed parallelly, since second should wait first one.
