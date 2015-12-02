import sys

def main(argv=None):
  try:
    if argv is None:
      argv = sys.argv[1:]
    tick_rate = float(argv[0])
    frame_rate = float(argv[1])
    precision = int(argv[2])
  except Exception, e:
    print "Error", repr(e), "on args", argv
    print """
Please provide a server tick rate, a client frame rate, and the desired 
precision as parameters!
Example:
  python THIS.py 50 20 3

Here, the server ticks with 50 Hz, and the client processes frames at 20 Hz. 
I'll round the step size (which is the reciprocal of the rate) internally 
to 3 decimal digits, which gives milliseconds precision.
  """
    return 1

  tick_steps = int(10**precision * round(1/tick_rate, precision))
  frame_steps = int(10**precision * round(1/frame_rate, precision))
  total_delay = 0
  mismatches = 0

  for time_step in xrange(1, tick_steps * frame_steps + 1):
    if time_step % tick_steps == 0:
      # Careful, we count in 10**precision slices, not seconds!
      total_delay += time_step % frame_steps
      mismatches += 1
      if time_step % frame_steps == 0:
        # It's not a mismatch if both rates momentarily sync
        mismatches -= 1
        break

  # Adjust for time requantization
  total_delay = float(total_delay) / 10**precision

  try:
    average_delay = total_delay / mismatches
  except ZeroDivisionError:
    if total_delay == 0:
      average_delay = 0
    else:
      raise Exeption("We had 0 mismatches, but still a total_delay of " + 
          str(total_delay) + " which is suspicious.")

  print mismatches, "mismatches,", total_delay, "total_delay,", average_delay, 
  print "average_delay,", time_step, "steps_evaluated"



# I stole this architecture for `main` etc. from Guido,
# http://www.artima.com/weblogs/viewpost.jsp?thread=4829
if __name__ == "__main__":
  sys.exit(main())

