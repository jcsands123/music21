# -*- coding: utf-8 -*-
# ------------------------------------------------------------------------------
# Name:         alpha/analysis/omrMidiCorrector.py
# Purpose:      Tool for using the omrMidiCorrector.Corrector
#
# Authors:      Janelle Sands
#
# Copyright:    Copyright © 2016 Michael Scott Cuthbert and the music21 Project
# License:      LGPL or BSD, see license.txt
# ------------------------------------------------------------------------------
from music21 import converter, corpus
from music21.alpha.analysis import omrMidiCorrector

###### 1. LOAD CONTENT ######
# Change these to the correct filepath to run this code!
haydnMidi = "/Users/janellesands/Desktop/MIT/meng/Research/Music/yale_midi_archive/Haydn/String Quartet Op1 No1 i Bb major.xml"
haydnOmr = "/Users/janellesands/Desktop/MIT/meng/Research/Music/omr/Haydn/mit39080016591155_fullViolin1_String_Quartet_Op1_No1.xml"

###### 2. PRE-FORMAT CONTENT ######
midi = converter.parse(haydnMidi)
omr = converter.parse(haydnOmr).measures(0,64)
groundTruth = corpus.parse('haydn/opus1no1/movement1').measures(0,63)

###### 3. RUN CORRECTOR ######
partNum = 0
corrector = omrMidiCorrector.Corrector(midi, omr, groundTruth, partNum)
finalScore, scores = corrector.fix()
finalScore.show()
