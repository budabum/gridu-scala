def _while(cond: => Boolean)(cmd: => Unit): Unit = {
  if(cond){
    cmd
    _while(cond)(cmd)
  }
//  else{}
}

var t=0
_while(t < 10){
  println(t)
  t += 1
}

def _repeat(cmd: => Unit)(cond: => Boolean): Unit = {
  cmd
  if(cond) _repeat(cmd)(cond)
}

def _until(cond: => Boolean): Boolean = cond

def _repeatu(cmd: => Unit)(cond: => Boolean): Unit = {
  cmd
  if(cond) _repeatu(cmd)(cond)
}

t=10
_repeat{
  println(t)
  t-=1
} (t > 0)

t=10
_repeatu{
  println(t)
  t-=1
} _until (t > 0)
