#! /usr/bin/env tclsh

# mol modstyle 0 top CPK 1.0 0.3 15 15
# color Display Background white
set num_frames [molinfo top get numframes]
for {set i 0} {$i < $num_frames} {incr i} {
    animate goto $i
    render POV3 [format frame%03d.pov $i]
}
exit

