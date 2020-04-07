from music21 import *
from copy import deepcopy

def shiftMusic(streamToShift, durationToShift, shiftRight=True, skipLayout=True):
    '''
    returns a copy of the streamToShift moved durationToShift to the right or left

    streamToShift is a stream, possibly composed of multiple substreams
    durationToShift is a duration.Duration
    shiftRight indicates whether the shift should be to the left or the right
    when skip layout is true, ignores layouts (mostly)

    can only shift for streams where shifting would allow previous meter changes to stay at starts of measures
    when no time signature is provided, uses the default one of music21 (4/4)

    TODO: deal with end bar line, key signatures, codas and friends, spanners, skippingLayout fully,
        chords and rests that go over the bar line
    '''
    eps = 10e-5

    newStream = deepcopy(streamToShift) # so that returns correct type of stream
    newStream.clear()

    # deal with stream made up of substreams
    hasSubstreams = any(isinstance(x, stream.Stream) and not isinstance(x, stream.Measure) for x in streamToShift)
    if hasSubstreams:
        subStreamMapping = {}

        # recursively deal with substreams
        for x in streamToShift:
            if isinstance(x, stream.Stream):
                shiftedSubStream = shiftMusic(x, durationToShift, shiftRight, skipLayout)
                shiftedSubStream.id = x.id
                newStream.append(shiftedSubStream)
                subStreamMapping[x] = newStream
            # TODO want skip all layout??
            # elif skipLayout and isinstance(x, (layout.PageLayout, layout.SystemLayout, layout.ScoreLayout)):
            #     continue
            else:
                newStream.append(x)

        # update spanners
        for spannerToUpdate in newStream.getElementsByClass(spanner.Spanner):
            for origSpanElem in spannerToUpdate.getSpannedElements():
                if origSpanElem in subStreamMapping:
                    newSpanElem = subStreamMapping[origSpanElem]
                    spannerToUpdate.replaceSpannedElement(origSpanElem, newSpanElem)

        # all done
        return newStream

    # at this point, no substreams in stream

    # make sure stream has measures
    preppedStreamToShift = deepcopy(streamToShift)
    if not preppedStreamToShift.hasMeasures():
        preppedStreamToShift = preppedStreamToShift.makeMeasures()

    # redo creating stream to return
    newStream = deepcopy(preppedStreamToShift)  # so that returns correct type of stream
    newStream.clear()

    # copy metadata
    newStream.metadata = streamToShift.metadata

    # For keeping State
    lastTimeSigSeen = None
    justSawTimeSigOutsideMeasure = False
    fullMeasureDurationQl = 100000.0 # maybe should be none?
    measureLeftPickupOffsetQl = None

    fillingMeasure = stream.Measure()
    isFirstMeasure = True
    isAfterRepeatMeasure = False
    afterRepeatOffset = None

    # Internal Helper Functions
    def getAmountFull(measure, fullDurationQl):
        '''
        returns number of quarter lengths in the measure (stream)
        when a full measure has fullDurationQl (number)
        '''
        mDuration = duration.Duration(fullDurationQl)
        amount = fullDurationQl * measure.barDurationProportion(barDuration=mDuration)
        return amount

    def initTimeSignature(timeSignature, measurePaddingLeft = 0):
        '''
        given a timeSignature and number of padding on the left of that measure,
        return a tuple (mDurationQl, offsetQl) where
        mDurationQl is the number of quarter lengths in a full measure (a number)
        offsetQl is the left offset of the pickup measure (a number)
        '''
        mDurationQl = timeSignature._getBarDuration().quarterLength
        shiftRightOffsetQl = (durationToShift.quarterLength) % mDurationQl
        shiftLeftOffsetQl = (mDurationQl - shiftRightOffsetQl) % mDurationQl

        if shiftRight:
            offsetQl = (shiftRightOffsetQl + measurePaddingLeft) % mDurationQl
        else:
            offsetQl = (shiftLeftOffsetQl + measurePaddingLeft) %  mDurationQl

        return mDurationQl, offsetQl

    # Bulk of copying over musical content
    for x in preppedStreamToShift:
        if isinstance(x, stream.Measure):
            m = x
            if justSawTimeSigOutsideMeasure and not lastTimeSigSeen is None:
                fullMeasureDurationQl, measureLeftPickupOffsetQl = initTimeSignature(lastTimeSigSeen, m.paddingLeft)
                # lastTimeSigSeen set when justSawTimeSigOutsideMeasure set to True
                justSawTimeSigOutsideMeasure = False
            if isinstance(m.leftBarline, bar.Repeat) and (isAfterRepeatMeasure or isinstance(newStream.measure(-1).rightBarline, bar.Repeat)):
                fillingMeasure.leftBarline = deepcopy(m.leftBarline)
            for elem in m:
                if isinstance(elem, meter.TimeSignature):
                    # save time signature settings
                    fullMeasureDurationQl, measureLeftPickupOffsetQl  = initTimeSignature(elem, m.paddingLeft)
                    lastTimeSigSeen = elem
                if skipLayout and isinstance(elem, (layout.PageLayout, layout.SystemLayout)):
                    # layouts no longer applicable because notes shifting
                    continue
                if isinstance(elem, bar.Repeat):
                    # repeat signs added in a different way
                    continue
                if isinstance(elem, note.GeneralNote):
                    if isFirstMeasure and measureLeftPickupOffsetQl is None:
                        # use default time signature when unknown
                        defaultTimeSig = meter.TimeSignature()
                        fullMeasureDurationQl, measureLeftPickupOffsetQl = initTimeSignature(defaultTimeSig, m.paddingLeft)
                        lastTimeSigSeen = defaultTimeSig
                        fillingMeasure.append(defaultTimeSig)

                    spaceNeeded = elem.duration.quarterLength
                    amountFull = getAmountFull(fillingMeasure, fullMeasureDurationQl)
                    spaceLeft = fullMeasureDurationQl - amountFull

                    if isFirstMeasure:
                        spaceLeft -= measureLeftPickupOffsetQl
                    elif isAfterRepeatMeasure:
                        if afterRepeatOffset is None:
                            # more of an internal error, shouldn't happen
                            raise ValueError("After Repeat Offset shouldn't be none")
                        spaceLeft -= afterRepeatOffset

                    # place note
                    if spaceLeft > spaceNeeded + eps:
                        # space for note and enough space for another note after
                        noteCopy = deepcopy(elem)
                        fillingMeasure.append(noteCopy)
                    else:
                        noteFitsCompletely = abs(spaceLeft - spaceNeeded) <= eps

                        # 1. deal with first note
                        if noteFitsCompletely:
                            noteCopy = deepcopy(elem)
                            fillingMeasure.append(noteCopy)
                        else:
                            # split note between measures
                            n1 = deepcopy(elem)

                            if elem.tie is None:
                                n1.tie = tie.Tie("start")
                            elif elem.tie.style == "continue" or elem.tie.style == "stop":
                                n1.tie = tie.Tie("continue")
                            else:  # elem.tie.style == "start'
                                n1.tie = tie.Tie("start")

                            n1.duration = duration.Duration(spaceLeft)
                            fillingMeasure.append(n1)

                        # 2. finish current measure and start another
                        # pad measure when done with it if first measure
                        if (isFirstMeasure or isAfterRepeatMeasure) and measureLeftPickupOffsetQl != 0:
                            fillingMeasure.padAsAnacrusis()
                        newStream.append(fillingMeasure)
                        # make new next measure
                        fillingMeasure = stream.Measure()
                        isFirstMeasure = False
                        isAfterRepeatMeasure = False

                        # 3. deal with possible second note (tied in)
                        # shouldn't need to make a 3rd note because the 2nd note always fits into the new measure
                        if not noteFitsCompletely:
                            # put n2 in next measure
                            n2 = deepcopy(elem)
                            if elem.tie is not None and elem.tie.style == "continue":
                                n2.tie = tie.Tie("continue")
                            else:
                                n2.tie = tie.Tie("stop")

                            n2.tie = tie.Tie("stop")
                            n2.duration = duration.Duration(spaceNeeded - spaceLeft)
                            fillingMeasure.append(n2)
                else: # not a note.GeneralNote
                    elemCopy = deepcopy(elem)
                    fillingMeasure.append(elemCopy)
            if m.rightBarline is not None and isinstance(m.rightBarline, bar.Repeat):
                amountFull = getAmountFull(fillingMeasure, fullMeasureDurationQl)
                if amountFull == 0:
                    # will really only happen if shifting by %0
                    lastMeasureAdded = newStream.measure(-1)
                    lastMeasureAdded.rightBarline = m.rightBarline
                else:
                    fillingMeasure.rightBarline = m.rightBarline
                    isAfterRepeatMeasure = True
                    afterRepeatOffset = amountFull

                    # finish current measure and start another
                    newStream.append(fillingMeasure)
                    fillingMeasure = stream.Measure()
        else:
            if isinstance(x, meter.TimeSignature):
                # save time signature settings
                # but dont calculate related fields yet because need to know left offset of first measure after
                justSawTimeSigOutsideMeasure = True
                lastTimeSigSeen = x
            newStream.append(deepcopy(x))

    # include last measure filling up
    if getAmountFull(fillingMeasure, fullMeasureDurationQl) != 0:
        newStream.append(fillingMeasure)
        # never need to pad as anacrusis
        # since will not contain pickups to another measure since no measure after

    # TODO Redo spanners

    # rebeam and clear stem directions
    newStream.makeBeams(inPlace=True)
    newStream.makeRests(inPlace=True)
    newStream.makeAccidentals(inPlace=True)
    for n in newStream.flat.notes:
        n.stemDirection = None
    return newStream
