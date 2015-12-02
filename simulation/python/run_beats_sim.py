import tick_frame_rate_beats

PRECISION = 5

for tick_rate in xrange(10, 210, 10):
  for frame_rate in xrange(10, 210, 10):
    print tick_rate, frame_rate,
    # The `tick_...` funtion calls print itself, yuck.
    tick_frame_rate_beats.main((tick_rate, frame_rate, PRECISION))
