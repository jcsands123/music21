# -*- coding: utf-8 -*-
# ------------------------------------------------------------------------------
# Name:         alpha/analysis/omrMidiCorrector.py
# Purpose:      Detect and fix errors in a stream generated using OMR using
#               a stream generated using MIDI
#
# Authors:      Janelle Sands
#
# Copyright:    Copyright © 2016 Michael Scott Cuthbert and the music21 Project
# License:      LGPL or BSD, see license.txt
# ------------------------------------------------------------------------------

from copy import deepcopy
from enum import Enum
from typing import Optional, Tuple, TypeVar

from music21 import duration, expressions, metadata, pitch, repeat, stream
from music21.alpha.analysis import aligner
from music21.alpha.analysis import shifter

_V = TypeVar('_V')

# The status of an individual music element, like a note or rest
class CorrectionStatus(Enum):
    DISCREPANCY = "red"
    CONFIDENT_FIX = "green"
    TENTATIVE_FIX = "orange"
    NO_DISCREPANCY = "black"
    TESTING = "purple"

# For fuzzy comparison of rhythms with slightly different float values
EPSILON = 10e-5

class CorrectionGeneration:
    '''
    Represents a generation of correcting OMR and MIDI streams.
    A generation consists of an omrPre stream, omrPost stream,
        midiPre stream, and midiPost stream
    Errors detected and resolved during this generation are the
        improvements between pre and post streams.
    '''
    def __init__(self, omrPre, midiPre, *, midiPost = None, omrPost = None):
        '''
        :param omrPre: OMR stream at the start of this fixing generation
        :param midiPre: MIDI stream at the start of this fixing generation
        :param omrPost: optional copy of omrPre modified with fixes from this generation
                        when not provided, is created as identical copy
        :param midiPost: optional copy of midiPre modified with fixes from this generation
                        when not provided, is created as identical copy
        '''
        self.omrPre = omrPre
        self.omrPost = deepcopy(omrPre) if omrPost is None else omrPost
        self.midiPre = midiPre
        self.midiPost = deepcopy(midiPre) if midiPost is None else midiPost

        # To map between corresponding elements in all streams:

        # 1. To map element id to element for all elements in each of the streams
        # {int id: music21 element}
        self.omrPreIdToElem = {}
        self.omrPostIdToElem = {}
        self.midiPreIdToElem = {}
        self.midiPostIdToElem = {}

        # 2. To map between Pre and Post streams
        # {int id: int id}
        self.omrPreIdToOmrPostId = {}
        self.omrPostIdToOmrPreId = {}
        self.midiPostIdToMidiPreId = {}
        self.midiPreIdToMidiPostId = {}

        # 3. Maps from aligned OMR (pre) and MIDI (pre) using the aligner's change list
        # Includes all MIDI (pre) elements possibly derived from each OMR (pre) element
        # and the corresponding change from the OMR element to achieve each MIDI element
        # {int omrPreId: [(int midiPreElem, aligner.ChangeOps), ...]}
        self.omrToMidiMap = {}

        # Tracks omrPre elements that have aligner.ChangeOps not of NoChange
        # with at least one other element in midiPre
        self.problematicOmrPreElements = []

        self.align()

    def align(self):
        '''
        Aligns omrPre with midiPre and fills in all mappings defined in __init__
        to map between corresponding streams and track problematic elements.
        :return: the aligner.StreamAligner with source omrPre and target midiPre
        '''
        streamAligner = aligner.StreamAligner(
            sourceStream=self.omrPre,
            targetStream=self.midiPre
        )
        streamAligner.align()
        streamAligner.showChanges()  # Necessary to generate the change list

        self.setUpMappings(streamAligner.changes)
        self.updateCorrectionStatusFromAlignment()

        return streamAligner

    def setUpMappings(self, changes):
        '''
        Fills in all mappings defined in __init__ to map between
        corresponding streams and track problematic elements.

        :param changes: list returned by aligner.StreamAligner
            elements are tuples (midiNoteRef, omrPreNoteRef, aligner.ChangeOps)
        '''
        for omrPreElem in self.omrPre.recurse():
            self.omrPreIdToElem[omrPreElem.id] = omrPreElem

        for omrPostElem in self.omrPost.recurse():
            self.omrPostIdToElem[omrPostElem.id] = omrPostElem
            preElemCopiedFrom = omrPostElem.derivation.origin
            if preElemCopiedFrom is None:  # Case for P1: Violin I: Violin
                continue
            self.omrPreIdToOmrPostId[preElemCopiedFrom.id] = omrPostElem.id
            self.omrPostIdToOmrPreId[omrPostElem.id] = preElemCopiedFrom.id

        for midiPreElem in self.midiPre.recurse():
            self.midiPreIdToElem[midiPreElem.id] = midiPreElem

        for midiPostElem in self.midiPost.recurse():
            self.midiPostIdToElem[midiPostElem.id] = midiPostElem
            preElemCopiedFrom = midiPostElem.derivation.origin
            if preElemCopiedFrom is None:  # Case for P1: Violin I: Violin
                continue
            self.midiPreIdToMidiPostId[preElemCopiedFrom.id] = midiPostElem.id
            self.midiPostIdToMidiPreId[midiPostElem.id] = preElemCopiedFrom.id

        for midiNoteRef, omrPreNoteRef, change in changes:
            # Skip the occasional things that got in there that shouldn't be there
            if omrPreNoteRef.id not in self.omrPreIdToElem:
                continue
            if midiNoteRef.id not in self.midiPreIdToElem:
                continue

            # Save all changes
            if omrPreNoteRef.id not in self.omrToMidiMap:
                self.omrToMidiMap[omrPreNoteRef.id] = [(midiNoteRef, change)]
            else:
                self.omrToMidiMap[omrPreNoteRef.id].append((midiNoteRef, change))

            # Collect problematic Omr references
            # "Problematic" means has a change to a Midi element that is not NoChange
            alreadyAdded = self.isProblematicPreElement(omrPreNoteRef)
            if not change == aligner.ChangeOps.NoChange and not alreadyAdded:
                self.problematicOmrPreElements.append(omrPreNoteRef)

    def updateCorrectionStatusFromAlignment(self):
        '''
        Updates the editorial.correctionStatus of elements in omrPost
        to instances of enum CorrectionStatus.

        When there's a discrepancy at that element with Midi, mark as CorrectionStatus.DISCREPANCY.
        When there's no discrepancy at that element with Midi, mark as:
            * CorrectionStatus.DISCREPANCY if it wasn't fixed before or was
                CorrectionStatus.NO_DISCREPANCY before
            * CorrectionStatus.CONFIDENT_FIX if it had a discrepancy before
                (This was a side effect of aligning that was "fixed" by other things being fixed)
            * CorrectionStatus.CONFIDENT_FIX if was that before
              CorrectionStatus.TENTATIVE_FIX if was that before
                (These would look resolved because they were fixed previously, but we
                don't want to overwrite nuanced fixing information from before.)
        '''
        for postElem in self.omrPost.recurse():
            preElem = self.getOmrPostFromPre(postElem, reverse=True)
            if preElem is None:
                continue

            if self.isProblematicPreElement(preElem):
                postElem.editorial.correctionStatus = CorrectionStatus.DISCREPANCY
                continue

            # At this point, postElem is non-problematic

            # If it wasn't fixed before, simply NO_DISCREPANCY
            if "correctionStatus" not in preElem.editorial:
                postElem.editorial.correctionStatus = CorrectionStatus.NO_DISCREPANCY
                continue

            previousCorrectionStatus = preElem.editorial.correctionStatus

            # This was a side effect of aligning that was "fixed" by other things being fixed
            if previousCorrectionStatus == CorrectionStatus.DISCREPANCY:
                markConfidentFix(postElem)

            # Leave remaining TENTATIVE_FIX, CONFIDENT_FIX, NO_DISCREPANCY cases as is

    def idToElem(self, elemId, *, useOmr, usePre):
        '''
        Find the music21 element from the appropriate stream with id of elemId
        :param elemId: int, id of the element to return
        :param useOmr: boolean, to use an Omr Stream (versus Midi Stream)
        :param usePre: boolean, to use a Pre Stream(versus Post Stream)
        :return: music21 element or None if there is no such element
        '''
        if useOmr:
            useDict = self.omrPreIdToElem if usePre else self.omrPostIdToElem
        else:
            useDict = self.midiPreIdToElem if usePre else self.midiPostIdToElem

        if elemId in useDict:
            return useDict[elemId]
        return None

    def getOmrPostFromPre(self, elem, reverse=False):
        '''
        Convert between corresponding elements in OMR streams
        :param elem: music21 element to find corresponding element of in other OMR stream
        :param reverse: when True, elem is in omrPre and corresponding element is in omrPost
                        when False, elem is in omrPost and corresponding element in in omrPre
        :return: corresponding music21 element or None if there is no such element
        '''
        lookUpDictionary = self.omrPreIdToOmrPostId if not reverse else self.omrPostIdToOmrPreId
        if elem.id not in lookUpDictionary:
            return None
        correspondingElemId = lookUpDictionary[elem.id]
        usePre = reverse
        return self.idToElem(correspondingElemId, useOmr=True, usePre=usePre)

    def getMidiPostFromPre(self, elem, reverse=False):
        '''
        Convert between corresponding elements in MIDI streams
        :param elem: music21 element to find corresponding element of in other MIDI stream
        :param reverse: when True, elem is in midiPre and corresponding element is in midiPost
                        when False, elem is in midiPost and corresponding element in in midiPre
        :return: corresponding music21 element or None if there is no such element
        '''
        lookUpDictionary = self.midiPreIdToMidiPostId if not reverse else self.midiPostIdToMidiPreId
        if elem.id not in lookUpDictionary:
            return None
        correspondingElemId = lookUpDictionary[elem.id]
        usePre = reverse
        return self.idToElem(correspondingElemId, useOmr=False, usePre=usePre)

    def getCorrespondingMidiFromOmr(self, elem):
        '''
        Convert element elem in OmrPre to corresponding elements e and their change c in midiPre
        If elem was changed with c, it would give e.
        There can be multiple corresponding elements in the midi from the omr.
        :param elem: music21 element in omrPre
            to find corresponding element(s) of in other midiPre stream
        :return: list of tuples of (midiElem, change to get there)
        '''
        if elem.id not in self.omrToMidiMap:
            return []
        return self.omrToMidiMap[elem.id]

    def getFirstProblematicMidiFromOmr(self, elem):
        '''
        Convert element elem in OmrPre to corresponding elements e and their change c in midiPre
        If elem was changed with c, it would give e to align the streams.
        There can be multiple corresponding elements in the midi from the omr, but this returns
        the first one that is not aligner.ChangeOps.NoChange
        :param elem: music21 element in omrPre
            to find corresponding elements of in other midiPre stream
        :return: tuple (midiElem, aligner.ChangeOps to get there)
                or (None, None) if there is no such mapping
        '''
        midiList = self.getCorrespondingMidiFromOmr(elem)
        for e,c in midiList:
            if c != aligner.ChangeOps.NoChange:
                return (e,c)
        return (None, None)

    def isProblematicPreElement(self, elem):
        '''
        Returns true if elem is in self.problematicOmrPreElements.
        An element is considered in self.problematicOmrPreElements if
            there is an element in self.problematicOmrPreElements with
            the same id.
        :param elem: music21 element
        :return: boolean, whether elem is problematic
        '''
        return any(x.id == elem.id for x in self.problematicOmrPreElements)

    def makeNextGeneration(self: _V) -> _V:
        '''
        Makes the next generation from this one.
        For both OMR and MIDI separately, next generation pre is
        a copy of this generation's Post, without styling.

        :return: CorrectionGeneration, generation following this one
        '''
        newOmrPre = deepcopy(self.omrPost)
        clearStyle(newOmrPre)
        newMidiPre = deepcopy(self.midiPost)
        clearStyle(newMidiPre)

        return CorrectionGeneration(newOmrPre, newMidiPre)

    def generateCorrectedScore(self, *, show=False, groundTruth = None, displayOmrPre= None, displayMidiPre = None, title= None):
        '''
        Displays a score of parts with title when given.
        Part 1: Labeled "MIDI" is self.midiPre unless displayMidiPre is provided
        Part 2: Labeled "Omr-Pre" is self.omrPre unless displayOmrPre is provided
        Part 3: Labeled "Omr-Post" is self.omrPost
        Optional Part 4: Labeled "Ground Truth" (abbreviated "Truth")
            is groundTruth when provided

        Parts 1 and 2 show alignment markings from the aligner.StreamAligner.
        Part 3 is colored according to the correctionStatus (see CorrectionStatus enum)

        :param show: boolean whether to show the score in a music editor pop up
        :param groundTruth: optional stream.Part to include in the score
        :param displayMidiPre: optional stream.Part to replace part 1,
            must be same length as other parts in score
        :param displayOmrPre: optional stream.Part to replace part 2
            must be same length as other parts in score
        :param title: str, optional score title

        :return: A stream.Score with all parts.

                 Modifies partName, partAbbreviation, id of input streams
                 and omrPre, omrPost, midiPre, midiPost streams of this instance.
        '''
        scoreDisplay = stream.Score()
        scoreDisplay.insert(0, metadata.Metadata())
        if not title is None:
            scoreDisplay.metadata.title = title

        omrPrePartDisplay = self.omrPre if displayOmrPre is None else displayOmrPre
        midiPrePartDisplay = self.midiPre if displayMidiPre is None else displayMidiPre

        assert isinstance(midiPrePartDisplay, stream.Part)
        midiPrePartDisplay.partName = 'MIDI'
        midiPrePartDisplay.partAbbreviation = 'MIDI'
        midiPrePartDisplay.id = 'MIDI'

        assert isinstance(omrPrePartDisplay, stream.Part)
        omrPrePartDisplay.partName = 'OMR-pre'
        omrPrePartDisplay.partAbbreviation = 'OMR-pre'
        omrPrePartDisplay.id = 'OMR-pre'

        omrPostPart = self.omrPost
        assert isinstance(omrPostPart, stream.Part)
        omrPostPart.partName = 'OMR-post'
        omrPostPart.partAbbreviation = 'OMR-post'
        omrPostPart.id = 'OMR-post'
        colorCorrectionStatuses(omrPostPart)

        if groundTruth is not None:
            assert isinstance(groundTruth, stream.Part)
            groundTruth.partName = "Ground Truth"
            groundTruth.partAbbreviation = "Truth"
            groundTruth.id = "Ground Truth"

        scoreDisplay.append([midiPrePartDisplay, omrPrePartDisplay, omrPostPart])
        if groundTruth is not None:
            scoreDisplay.append(groundTruth)

        if show:
            scoreDisplay.show()

        return scoreDisplay


class Corrector:
    '''
    A tool for identifying and fixing errors in a stream generated
    using OMR using a stream generated using MIDI.
    '''
    def __init__(self, midiStream, omrStream, groundTruthStream = None, partNum = 0):
        '''
        :param midiStream: Stream.stream, generated from MIDI and used to help fix omrStream
        :param omrStream: Stream.stream, generated from OMR software and needs to be fixed
        :param groundTruthStream: Stream.stream, (optional) the desired output
        :param partNum: int, the specific part to compare
        '''
        self.partNum = partNum

        # Store original passed in streams. Don't modify.
        self.midiOriginal = midiStream
        self.omrOriginal = omrStream
        self.groundTruth = groundTruthStream
        self.groundTruthPart = None  # Set in setUp

        # Parts are iteratively fixed and progress is stored here
        # List of CorrectionGenerations
        #   next element is the generation following the previous element
        # First generation in the list has no fixes applied
        #   for observing initial discrepancy between OMR and MIDI
        self.correctionGenerations = []

        # Functions to call on each problematic element in an iteration of fix on a generation.
        # All take element omrPreElem, CorrectionGeneration generation as inputs
        # All return tuple: (bool fixedSomething,
        # set of ints omrPreIdsToIgnore, set of ints midiPreIdsToIgnore)
        self.fixerLoopHelpers = [fixerHelperDifferentRestRep,
                                 fixerHelperDottedRhythm,
                                 fixerHelperPitchAccidentals]
        self.fixerInitialHelpers = []

        self.canCorrect, self.cantCorrectError = self.setUp()

    def setUp(self) -> Tuple[bool, Optional[Exception]]:
        '''
        Adjusts the inputted parts for displayed alignment and
            returns an error if they are too different to fix.
        Sets self.groundTruthPart when groundTruthStream exists.
        Extracts parts from inputted midi and omr streams and
            returns and error if requested partNum is greater than
            available zero-indexed parts. For input which
            are parts already, simply extract the stream itself.
        Creates first generation in self.correctionGenerations.

        :return: tuple (boolean set up successful, Exception if any (or None))
        '''
        # Adjusting
        midiModified = deepcopy(self.midiOriginal)
        omrModified = deepcopy(self.omrOriginal)

        # Condense repeats to match Omr notation
        midiModified = repeat.RepeatFinder(midiModified).simplify()

        # Shift midi so omr and midi measures line up
        # TODO Hardcoded. Need to detect measure misalignment & generalize shifting.
        midiModified = shifter.shiftMusic(midiModified, duration.Duration(.5), shiftRight=False)

        replaceTablaturesWithRehearsalMarks(omrModified)
        adjustKey(omrModified, midiModified)

        # TODO throw an error if the streams have different lengths!

        # Extract parts
        if self.groundTruth is not None:
            if isinstance(self.groundTruth, stream.Part):
                self.groundTruthPart = self.groundTruth
            else:
                if len(self.groundTruth.parts) <= self.partNum:
                    err = ValueError("Ground truth doesn't have " + self.partNum + " parts")
                    return (False, err)
                self.groundTruthPart = self.groundTruth.parts[self.partNum]

        if isinstance(midiModified, stream.Part):
            midiPrePart = midiModified
        else:
            if len(midiModified.parts) <= self.partNum:
                err = ValueError("MIDI doesn't have " + self.partNum + " parts")
                return (False, err)
            midiPrePart = midiModified.parts[self.partNum]

        if isinstance(omrModified, stream.Part):
            omrPrePart = omrModified
        else:
            if len(omrModified.parts) <= self.partNum:
                err = ValueError("OMR doesn't have " + self.partNum + " parts")
                return (False, err)
            omrPrePart = omrModified.parts[self.partNum]

        # Create first generation with these pres
        generationZero = CorrectionGeneration(omrPrePart, midiPrePart)
        self.correctionGenerations.append(generationZero)
        return (True, None) # Success if got to this point!

    def getMostRecentGeneration(self):
        '''
        Returns last CorrectionGeneration in self.correctionGenerations
        :return: CorrectionGeneration or None
        '''
        if len(self.correctionGenerations) == 0:
            return None
        return self.correctionGenerations[-1]

    def makeNextGeneration(self)-> Tuple[bool, Optional[Exception]]:
        '''
        Makes the next generation from the the last CorrectionGeneration
            in self.correctionGenerations and adds it to the end of
            self.correctionGenerations.
        :return: tuple, (boolean success, Exception if any (or None))
        '''
        latestGeneration = self.getMostRecentGeneration()
        if latestGeneration is None:
            err = IndexError("Cannot make a next generation when no generations exist.")
            return (False, err)
        nextGeneration = latestGeneration.makeNextGeneration()
        self.correctionGenerations.append(nextGeneration)
        return (True, None)

    def getOmrFirstAligned(self):
        '''
        :return: First omr part modified and copied from original, and aligned with midi
        '''
        return self.correctionGenerations[0].omrPre

    def getMidiFirstAligned(self):
        '''
        :return: First midi part modified and copied from original, and aligned with omr
        '''
        return self.correctionGenerations[0].midiPre

    def fixOnePass(self, generation) :
        '''
        Goes through all problematic elements in generation.problematicOmrPreElements.
        Attempts to fix them by modifying generation's omrPost and midiPost,
        marking these changes on the editorial.correctionStatus as CONFIDENT_FIX or TENTATIVE_FIX.

        When a fix is made, don't make fixes to those element or related elements.
        during any more passes of this call of the function.

        :return: True if fixed something, False otherwise
        '''
        fixedSomething = False
        midiPreDontTouchIds = set()
        omrPreDontTouchIds = set()

        for omrPreElem in generation.problematicOmrPreElements:
            midiPreElem, _ = generation.getFirstProblematicMidiFromOmr(omrPreElem)
            if omrPreElem.id in omrPreDontTouchIds or midiPreElem.id in midiPreDontTouchIds:
                continue

            for fixerHelper in self.fixerLoopHelpers:
                result = fixerHelper(omrPreElem, generation)
                fixed, impactedOmrPreIds, impactedMidiPreIds = result
                if fixed:
                    for omrPreId in impactedOmrPreIds:
                        omrPreDontTouchIds.add(omrPreId)
                    for midiPreId in impactedMidiPreIds:
                        midiPreDontTouchIds.add(midiPreId)
                    fixedSomething = True
                    # Don't apply any other fixers to this element now!
                    continue
        return fixedSomething

    def fix(self, maxLoops = 7):
        '''
        Generates and fixes another generation until the
        generated generation cannot be fixed or until maxLoops
        is reached.

        Tries to fix the post versions of the current generation
        Passes through making fixes until makes no more progress
        :return: tuple (finalVersion, list of versions)
            finalVersion is the stream.Score showing changes from very beginning to very end
            versions are each a stream.Score showing changes for each sequential generation
                versions has length of self.correctionGenerations
        '''
        versions = []
        loopCount = 1
        passCorrectedThings = True
        gen = self.getMostRecentGeneration()

        while passCorrectedThings:
            print("Fix pass : ", loopCount)
            gen = self.getMostRecentGeneration()
            newGen = gen.generateCorrectedScore(groundTruth=self.groundTruthPart)
            versions.append(newGen)
            self.makeNextGeneration()

            gen = self.getMostRecentGeneration()
            passCorrectedThings = self.fixOnePass(gen)

            loopCount += 1
            if loopCount >= maxLoops:
                break

        gT = self.groundTruthPart
        omrPre = self.getOmrFirstAligned()
        midiPre = self.getMidiFirstAligned()

        final = gen.generateCorrectedScore(groundTruth = gT, displayOmrPre= omrPre, displayMidiPre = midiPre)

        return (final, versions)


def rehearsalMarkFromTablature(chordTab):
    '''
    Converts misread chord tablature marking into the
    rehearsal letter marking.
    For example, the chord for A turns into Rehearsal Mark "A".

    :param chordTab: music21 tablature.ChordWithFretBoard
    :return: expressions.RehearsalMark
    '''
    letter = chordTab.figure
    return expressions.RehearsalMark(letter)

def replaceTablaturesWithRehearsalMarks(s):
    '''
    Replaces all tablature.ChordWithFretBoard in a stream
    with a rehearsal mark with the appropriate letter
    according to rehearsalMarkFromTablature
    :param s: stream.Stream to modify
    '''
    for chordTab in s.recurse(classFilter="ChordWithFretBoard"):
        s.replace(chordTab, rehearsalMarkFromTablature(chordTab))

def adjustKey(omrStream, midiStream):
    '''
    Modifies the key of midiStream when each stream has exactly one unique key,
    the keys are different, and the omrStream has the predicted key.
    :param omrStream: stream.Stream
    :param midiStream: stream.Stream
    '''
    bestKey = midiStream.analyze('key')

    omrKeySignatures = list(omrStream.recurse(classFilter="KeySignature"))
    midiKeySignatures = list(midiStream.recurse(classFilter="KeySignature"))

    # These three cases eventually can account for, but for simplicity now, don't.
    if len(omrKeySignatures) == 0 or len(midiKeySignatures) == 0:
        # One has a key signature and the other doesn't
        return
    if not all(k.sharps == omrKeySignatures[0].sharps for k in omrKeySignatures):
        # Omr has more than one unique key signature
        return
    if not all(k.sharps == midiKeySignatures[0].sharps for k in midiKeySignatures):
        # Midi has more than one unique key signature
        return

    # At this point, have lists with at least one key but all are the same key
    omrKeySignature = omrKeySignatures[0]
    midiKeySignature = midiKeySignatures[0]
    bestKeyIsOmrKey = omrKeySignature.sharps == bestKey.sharps

    if omrKeySignature != midiKeySignature and bestKeyIsOmrKey:
        for k in midiKeySignatures:
            k.sharps = bestKey.sharps

def clearStyle(s):
    '''
    Clear style of all elements in stream s
    :param s: stream.Stream to modify
    '''
    for elem in s.recurse():
        elem.style = None

def colorCorrectionStatuses(s):
    '''
    Sets the color of every element in the stream
    according to the value of the editorial.correctionStatus
    of the element as mapped by the CorrectionStatus enum.
    :param s: stream.Stream to color elements in
    '''
    for elem in s.recurse():
        if "correctionStatus" not in elem.editorial:
            elem.style.color = CorrectionStatus.NO_DISCREPANCY.value
        else:
            elem.style.color = elem.editorial.correctionStatus.value

def markConfidentFix(elem):
    '''
    Sets correctionStatus of elem's editorial to CorrectionStatus.CONFIDENT_FIX
    :param elem: element to modify
    '''
    elem.editorial.correctionStatus = CorrectionStatus.CONFIDENT_FIX

def markTentativeFix(elem):
    '''
    Sets correctionStatus of elem's editorial to CorrectionStatus.TENTATIVE_FIX
    :param elem: element to modify
    '''
    elem.editorial.correctionStatus = CorrectionStatus.TENTATIVE_FIX

def getNextRests(startingRest, *, referenceStream=None, returnOrig = False):
    '''
    Gets the next consecutive rests as a new stream from and including startingRest
    When referenceStream is defined, uses active site in reference stream for startingRest
    Returns either the original or a deepcopy of the rests.

    Consecutive rests can be interrupted by barlines, but cannot be interrupted by notes or chords

    :param startingRest: note.Rest
    :param referenceStream: stream.Stream
    :param returnOrig: boolean, whether should return original elements or deepcopy
    :return: stream.Stream
    '''
    if referenceStream:
        startingRest.activeSite = referenceStream

    rests = stream.Stream()
    nextElem = startingRest.next('GeneralNote', activeSiteOnly=True)

    if returnOrig:
        rests.append(startingRest)
    else:
        rests.append(deepcopy(startingRest))

    while nextElem:
        if not nextElem.isRest:
            break

        currentRest = nextElem
        nextElem = currentRest.next('GeneralNote', activeSiteOnly=True)

        if returnOrig:
            rests.append(currentRest)
        else:
            rests.append(deepcopy(currentRest))
    return rests

def replaceSnippet(snippetToRemove, snippetToAdd, operateOnStream):
    '''
    Replaces snippetToRemove with snippetToAdd in operateOnStream.

    All notes in snippetToRemove must exist in operateOnStream.
    snippetToRemove and snippetToAdd must have the same total duration.

    :param snippetToRemove: stream.Stream
    :param snippetToAdd: stream.Stream
    :param operateOnStream: boolean, whether should return original elements or deepcopy
    '''
    firstElementToRemove = snippetToRemove.elements[0]
    containerToRemoveFrom = operateOnStream.containerInHierarchy(firstElementToRemove)
    newContainer = containerToRemoveFrom.cloneEmpty()

    addedElements = False

    for origElem in containerToRemoveFrom.recurse():
        if origElem in snippetToRemove:
            if not addedElements:
                for addElem in snippetToAdd:
                    newContainer.append(addElem)
                addedElements = True
            else:
                continue
        else:
            newContainer.append(origElem)

    operateOnStream.replace(containerToRemoveFrom, newContainer)


def getCorrespondingElementsFromPreElem(omrPreElem, generation):
    '''
    Gets corresponding elements to omrPreElem in the other streams

    :param omrPreElem: music21 element in the omrPre stream of generation
    :param generation: CorrectionGeneration to find corresponding elements in
    :return: list of the corresponding [omrPostElem, midiPreElem, midiPostElem]
        in generation. Elements in list are music21 elements or None.
        midiPreElem is first real change element corresponding to omrPreElem in alignment
    '''
    assert(isinstance(generation, CorrectionGeneration))
    correspondingOmrPostElem = generation.getOmrPostFromPre(omrPreElem)
    correspondingMidiPreElem = generation.getFirstProblematicMidiFromOmr(omrPreElem)[0]
    if correspondingMidiPreElem is None:
        correspondingMidiPostElem = None
    else:
        correspondingMidiPostElem = generation.getMidiPostFromPre(correspondingMidiPreElem)

    return [correspondingOmrPostElem, correspondingMidiPreElem, correspondingMidiPostElem]

def fixerHelperDifferentRestRep(omrPreElem, generation, epsilon = EPSILON):
    '''
    Fixes when omr and midi have rests totalling the same duration,
    but with different representation

    Modifies omrPost and midiPost of current generation and adds editorial info about fix
    on elements in omrPost if fixes

    :param omrPreElem: music21 element in the omrPre stream of CorrectionGeneration
    :param generation: CorrectionGeneration to fix
    :param epsilon: acceptable difference in rhythm duration to be considered "equal" duration
    :return: tuple: (bool fixedSomething, omrPreIdsImpacted, midiPreIdsImpacted)
        sets omrPreIdsImpacted and midiPreIdsImpacted contain ints of elements ids
        which could be impacted by the fix (even if indirectly)
    '''
    correspondingElements = getCorrespondingElementsFromPreElem(omrPreElem, generation)
    if any(e is None for e in correspondingElements):
        return (False, set(), set())
    omrPostElem, midiPreElem, midiPostElem = correspondingElements

    omrPreIdsImpacted = set()
    omrPreIdsImpacted.add(omrPreElem.id)
    midiPreIdsImpacted = set()

    if not omrPreElem.isRest or not omrPostElem.isRest:
        return (False, set(), set())

    nextOmrPreRests = getNextRests(omrPreElem, returnOrig=True)
    nextMidiPreRests = getNextRests(midiPreElem, returnOrig=True)

    # If these rests are them same, don't try to fix.
    if len(nextOmrPreRests.elements) == len(nextMidiPreRests.elements):
        sameRests = True
        for i in range(len(nextOmrPreRests.elements)):
            if nextOmrPreRests.elements[i].duration.quarterLength != nextMidiPreRests.elements[
                i].duration.quarterLength:
                sameRests = False
                break
        if sameRests:  # the rests are the same
            return (False, set(), set())

    omrRestsQl = nextOmrPreRests.duration.quarterLength
    midiRestsQl = nextMidiPreRests.duration.quarterLength
    if abs(omrRestsQl - midiRestsQl) <= epsilon:
        # Detected: both parts have the same total duration of rests, but represented differently
        # Fix: Keep omr rests, confidently
        nextOmrPostRests = getNextRests(omrPostElem, returnOrig=True)
        for okRest in nextOmrPostRests.recurse():
            markConfidentFix(okRest)
        nextMidiPostRests = getNextRests(midiPostElem, returnOrig=True)
        replaceSnippet(nextMidiPostRests, nextOmrPostRests, generation.midiPost)

        # Update impacted ids
        for omrPreRest in nextOmrPreRests.recurse():
            omrPreIdsImpacted.add(omrPreRest.id)
            restChangesList = generation.getCorrespondingMidiFromOmr(omrPreRest)
            for e, _ in restChangesList:
                midiPreIdsImpacted.add(e.id)
        for nextMidiRest in nextMidiPreRests.recurse():
            midiPreIdsImpacted.add(nextMidiRest.id)
        changeList = generation.getCorrespondingMidiFromOmr(omrPreElem)
        for e, _ in changeList:
            midiPreIdsImpacted.add(e.id)
        return (True, omrPreIdsImpacted, midiPreIdsImpacted)

    return (False, omrPreIdsImpacted, midiPreIdsImpacted)

def fixerHelperPitchAccidentals(omrPreElem, generation,):
    '''
    Fixes when omr and midi have different pitches because of different enharmonic
    representations of the pitch or missing or misread accidentals in the omr part
    Modifies omrPost and midiPost of current generation and adds editorial info about fix
    on elements in omrPost if fixes

    :param omrPreElem: music21 element in the omrPre stream of CorrectionGeneration
    :param generation: CorrectionGeneration to fix
    :return: tuple: (bool fixedSomething, omrPreIdsImpacted, midiPreIdsImpacted)
        sets omrPreIdsImpacted and midiPreIdsImpacted contain ints of elements ids
        which could be impacted by the fix (even if indirectly)
    '''
    correspondingElements = getCorrespondingElementsFromPreElem(omrPreElem, generation)
    if any(e is None for e in correspondingElements):
        return (False, set(), set())
    omrPostElem, midiPreElem, midiPostElem = correspondingElements

    omrPreIdsImpactedIfFix = set()
    omrPreIdsImpactedIfFix.add(omrPreElem.id)
    midiPreIdsImpactedIfFix = set()

    if not omrPreElem.isNote or not midiPreElem.isNote:
        return (False, set(), set())
    if omrPreElem.pitch == midiPreElem.pitch:
        return (False, set(), set())

    # Add the impacted notes if correct something
    changeList = generation.getCorrespondingMidiFromOmr(omrPreElem)
    for e, _ in changeList:
        midiPreIdsImpactedIfFix.add(e.id)
    omrPreIdsImpactedIfFix.add(omrPreElem.id)

    # Different representation of same pitch
    if omrPreElem.pitch.isEnharmonic(midiPreElem.pitch):
        # Keep omr representation
        markConfidentFix(omrPostElem)
        # Now update midi
        midiPostElem.pitch = deepcopy(omrPreElem.pitch)
        markConfidentFix(midiPostElem)
        return (True, omrPreIdsImpactedIfFix, midiPreIdsImpactedIfFix)

    # If omr has a natural, it could be a misread sharp or flat
    natural = pitch.Accidental('natural')
    if omrPreElem.pitch.accidental is None or omrPreElem.pitch.accidental == natural:
        newDownPitch = omrPreElem.pitch.transpose('-m2')
        newUpPitch = omrPreElem.pitch.transpose('m2')
        if newDownPitch.isEnharmonic(midiPreElem.pitch):
            omrPostElem.pitch.accidental = pitch.Accidental('flat')
            markConfidentFix(omrPostElem)
            midiPostElem.pitch = deepcopy(omrPostElem.pitch)
            markConfidentFix(midiPostElem)
            return (True, omrPreIdsImpactedIfFix, midiPreIdsImpactedIfFix)
        if newUpPitch.isEnharmonic(midiPreElem.pitch):
            omrPostElem.pitch.accidental = pitch.Accidental('sharp')
            markConfidentFix(omrPostElem)
            midiPostElem.pitch = deepcopy(omrPostElem.pitch)
            markConfidentFix(midiPostElem)
            return (True, omrPreIdsImpactedIfFix, midiPreIdsImpactedIfFix)

    # If omr has a sharp, it could have been accidentally added or could be a misread natural
    elif omrPreElem.pitch.accidental == pitch.Accidental('sharp'):
        newPitch = omrPreElem.pitch.transpose('-m2')  # down
        if newPitch.isEnharmonic(midiPreElem.pitch):
            # Setting to None, means a natural sign will be put in
            # front if necessary for the display
            omrPostElem.pitch.accidental = None
            markConfidentFix(omrPostElem)
            midiPostElem.pitch = deepcopy(omrPostElem.pitch)
            markConfidentFix(midiPostElem)
            return (True, omrPreIdsImpactedIfFix, midiPreIdsImpactedIfFix)

    # If omr has a flat, it could have been accidentally added or could be a misread natural
    elif omrPreElem.pitch.accidental == pitch.Accidental('flat'):
        newPitch = omrPreElem.pitch.transpose('m2')  # up
        if newPitch.isEnharmonic(midiPreElem.pitch):
            # Setting to None, means a natural sign will be put in
            # front if necessary for the display
            omrPostElem.pitch.accidental = None
            markConfidentFix(omrPostElem)
            midiPostElem.pitch = deepcopy(omrPostElem.pitch)
            markConfidentFix(midiPostElem)
            return (True, omrPreIdsImpactedIfFix, midiPreIdsImpactedIfFix)

    return (False, set(), set())

def fixerHelperDottedRhythm(omrPreElem, generation, epsilon = EPSILON):
    '''
    Fixes when omr and midi have a differing duration due to misread dot for rhythm
    Modifies omrPost and midiPost of current generation and adds editorial info about fix
    on elements in omrPost if fixes

    :param omrPreElem: music21 element in the omrPre stream of CorrectionGeneration
    :param generation: CorrectionGeneration to fix
    :param epsilon: acceptable difference in rhythm duration to be considered "equal" duration
    :return: tuple: (bool fixedSomething, omrPreIdsImpacted, midiPreIdsImpacted)
        sets omrPreIdsImpacted and midiPreIdsImpacted contain ints of elements ids
        which could be impacted by the fix (even if indirectly)

    # TODO still need to shift other offsets since fixed note will take up more or less space now!
    # TODO sharing enharmonic pitch could boost confidence
    '''
    correspondingElements = getCorrespondingElementsFromPreElem(omrPreElem, generation)
    if any(e is None for e in correspondingElements):
        return (False, set(), set())
    omrPostElem, midiPreElem, midiPostElem = correspondingElements

    omrPreIdsImpacted = set()
    omrPreIdsImpacted.add(omrPreElem.id)
    midiPreIdsImpacted = set()

    if not omrPreElem.isNote or not midiPreElem.isNote:
        return (False, set(), set())

    if omrPreElem.duration == midiPreElem.duration:
        return (False, set(), set())

    omrPreElemDurationCopy = deepcopy(omrPreElem.duration)
    midiPreElemDurationCopy = deepcopy(midiPreElem.duration)
    omrPreElemDurationCopy.dots = 0
    midiPreElemDurationCopy.dots = 0

    durationDiff = omrPreElemDurationCopy.quarterLength - midiPreElemDurationCopy.quarterLength
    if abs(durationDiff) <= epsilon:
        # Keep midi duration
        omrPostElem.duration.quarterLength = midiPreElem.duration.quarterLength
        markConfidentFix(omrPostElem)
        markConfidentFix(midiPostElem)

        # Determine impacted elements
        changeList = generation.getCorrespondingMidiFromOmr(omrPreElem)
        for e, _ in changeList:
            midiPreIdsImpacted.add(e.id)
        omrPreIdsImpacted.add(omrPreElem.id)
        return (True, omrPreIdsImpacted, midiPreIdsImpacted)

    return (False, set(), set())

