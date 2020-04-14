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

from copy import copy, deepcopy
from enum import Enum
from fractions import Fraction
from typing import Optional, Tuple, TypeVar
from statistics import median

from music21 import duration, expressions, key, metadata, layout, meter, note, pitch, repeat, stream, tie
from music21.alpha.analysis import aligner
from music21.alpha.analysis.fixer import *
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

        self.aligner = self.align()

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
        clearLayout(newOmrPre)
        clearLyrics(newOmrPre)
        newMidiPre = deepcopy(self.midiPost)
        clearStyle(newMidiPre)
        clearLayout(newMidiPre)
        clearLyrics(newMidiPre)

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
        colorCorrectionStatuses(midiPrePartDisplay)

        assert isinstance(omrPrePartDisplay, stream.Part)
        omrPrePartDisplay.partName = 'OMR-pre'
        omrPrePartDisplay.partAbbreviation = 'OMR-pre'
        omrPrePartDisplay.id = 'OMR-pre'
        colorCorrectionStatuses(omrPrePartDisplay)

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
                                 fixerHelperDifferentArticulation,
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
        adjustMeter(omrModified, midiModified)
        adjustKey(omrModified, midiModified)

        # TODO throw an error if the streams have different lengths!

        # Extract parts
        if self.groundTruth is not None:
            if isinstance(self.groundTruth, stream.Part):
                self.groundTruthPart = self.groundTruth
            elif self.partNum < len(self.groundTruth.parts):
                    self.groundTruthPart = self.groundTruth.parts[self.partNum]
            elif len(self.groundTruth.parts) == 1:
                self.groundTruthPart = self.groundTruth.parts[0]
            else:
                err = ValueError("Ground truth doesn't have " + str(self.partNum) + " parts")
                return (False, err)


        if isinstance(midiModified, stream.Part):
            midiPrePart = midiModified
        elif self.partNum < len(midiModified.parts):
            midiPrePart = midiModified.parts[self.partNum]
        elif len(midiModified.parts) == 1:
            midiPrePart = midiModified.parts[0]
        else:
            err = ValueError("MIDI doesn't have " + str(self.partNum) + " parts")
            return (False, err)


        if isinstance(omrModified, stream.Part):
            omrPrePart = omrModified
        elif self.partNum < len(omrModified.parts):
            omrPrePart = omrModified.parts[self.partNum]
        elif len(omrModified.parts) == 1:
            omrPrePart = omrModified.parts[0]
        else:
            err = ValueError("OMR doesn't have " + str(self.partNum) + " parts")
            return (False, err)

        adjustOmrMeasures(omrPrePart, midiPrePart)
        modifiedMidiPrePart, _ = fillInMidiPart(omrPrePart, midiPrePart, midiModified, self.partNum)

        generationZero = CorrectionGeneration(omrPrePart, modifiedMidiPrePart)
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
            print("fix pass: ", loopCount)
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
    Modifies the key of midiStream and the omrStream to the most likely key.
    Only works for pieces with a single key.

    :param omrStream: stream.Stream
    :param midiStream: stream.Stream
    '''
    bestKey = midiStream.analyze('key')
    bestKeySig = None

    omrKeySignatures = list(omrStream.recurse(classFilter="KeySignature"))
    midiKeySignatures = list(midiStream.recurse(classFilter="KeySignature"))

    # First choice is if any omrKeySignatures match bestKey
    if len(omrKeySignatures) != 0:
        for k in omrKeySignatures:
            if k.sharps == bestKey.sharps:
                bestKeySig = k
                break

    # Second choice is if any midiKeySignatures match bestKey
    elif bestKeySig is None and len(midiKeySignatures) != 0:
        for k in omrKeySignatures:
            if k.sharps == bestKey.sharps:
                bestKeySig = k
                break

    # Third choice is most common omrKeySignature
    elif bestKeySig is None and len(omrKeySignatures) != 0:
        sortedOmrKeySignatures = sorted(omrKeySignatures, key=lambda keySig: keySig.sharps)

        highestKeyFreq = 0
        currentKey = sortedOmrKeySignatures[0]
        currentKeyFreq = 1

        for i in range(1, len(sortedOmrKeySignatures)):
            k = sortedOmrKeySignatures[i]
            if k.sharps != currentKey.sharps:
                if currentKeyFreq >= highestKeyFreq:
                    highestKeyFreq = currentKeyFreq
                    bestKeySig = sortedOmrKeySignatures[i-1] # previous key
                currentKey = k
                currentKeyFreq = 0
            currentKeyFreq += 1

        if currentKeyFreq >= highestKeyFreq:
            bestKeySig = sortedOmrKeySignatures[-1]

    # Fourth choice is analyzed key
    elif bestKeySig is None:
        bestKeySig = key.KeySignature(bestKey.sharps)

    if not bestKeySig is None: # should always exist
        for k in omrKeySignatures:
            omrStream.remove(k, recurse = True)
        for k in midiKeySignatures:
            midiStream.remove(k, recurse = True)

        # inserting into the outer score stream or stream isn't enough--- needs to be inside the part!
        omrStream.insert(0, bestKeySig)
        parts = omrStream.parts
        for part in parts:
            part.insert(0, bestKeySig)

        midiStream.insert(0, bestKeySig)
        parts = midiStream.parts
        for part in parts:
            part.insert(0, bestKeySig)

def evaluateMeter(timeSig, measures, measureDurationQl):
    checkedMeasures = 0
    measuresCorrectlyBeamed = 0
    for measure in measures:
        if not measureDurationQl is None:
            if abs(measure.duration.quarterLength - measureDurationQl) > EPSILON:
                continue
        checkedMeasures += 1
        notes = measure.recurse(classFilter=('NotRest',), restoreActiveSites=False)

        # check measures.stream.Status.haveBeamsBeenMade()...?
        actualBeams = []
        for n in notes:
            actualBeams.append(n.beams)

        projectedBeams = timeSig.getBeams(measure)
        if len(projectedBeams) == len(actualBeams):
            if projectedBeams == actualBeams:
                measuresCorrectlyBeamed += 1
                continue
        projectedBeamsNotNone = []
        for beams in projectedBeams:
            if not beams is None:
                projectedBeamsNotNone.append(beams)

        if projectedBeamsNotNone == actualBeams:
            measuresCorrectlyBeamed += 1
            continue

    if checkedMeasures > 0:
        return measuresCorrectlyBeamed / checkedMeasures
    return 0

def makeMeterFromDenominator(measureDurationQl, denominator, currentNumerator=None):
    denomDuration = duration.Duration(Fraction(4, denominator))
    numerator = measureDurationQl / denomDuration.quarterLength

    if abs(int(numerator) - numerator) <= EPSILON:
        meterStr = str(int(numerator)) + "/" + str(denominator)
        return meter.TimeSignature(meterStr)
    return None

def makeMeterFromNumerator(measureDurationQl, numerator, currentDenominator=None):
    denomDuration = measureDurationQl/numerator
    denominator = denomDuration/4

    if abs(int(denominator) - denominator) <= EPSILON:
        meterStr = str(int(numerator)) + "/" + str(denominator)
        return meter.TimeSignature(meterStr)
    return None

def adjustMeter(omrStream, midiStream):
    omrMeters = list(omrStream.recurse(classFilter="TimeSignature"))
    midiMeters = list(midiStream.recurse(classFilter="Time Signature"))
    rounding = 2

    # get mean measure duration
    durations = []
    for s in (omrStream, midiStream):
        for m in list(s.recurse(classFilter="Measure")):
            if not allRests(m):
                durations.append(round(m.duration.quarterLength,rounding))
    meanDuration = median(durations)

    # find best meter
    bestMeter = None

    # try first with omr, in order
    for omrMeter in omrMeters:
        if round(omrMeter.barDuration.quarterLength, rounding) == meanDuration:
            bestMeter = omrMeter
            break

    # then try fixing omr meters
    omrMeasures = list(omrStream.recurse(classFilter="Measure"))
    for omrMeter in omrMeters:
        changeNum = makeMeterFromDenominator(meanDuration, omrMeter.denominator, omrMeter.numerator)
        changeDenom = makeMeterFromNumerator(meanDuration, omrMeter.numerator, omrMeter.denominator)

        changeNumRes, changeDenomRes = 0, 0
        if not changeNum is None:
            changeNumRes = evaluateMeter(changeNum, omrMeasures, meanDuration)
        if not changeDenom is None:
            changeDenomRes = evaluateMeter(changeDenom, omrMeasures, meanDuration)
        if changeNumRes == 0 and changeDenomRes == 0:
            continue
        elif changeDenomRes > changeDenomRes:
            bestMeter = changeDenom
            break
        else:
            bestMeter = changeNum
            break

    # then try mid if unsuccessful
    if bestMeter is None:
        for midiMeter in midiMeters:
            if round(midiMeter.barDuration.quarterLength, rounding) == meanDuration:
                bestMeter = midiMeter
                break


    def getMeters(measureDuration):
        createdMeters = []
        maxDenom = 13
        for denom in range(1,maxDenom):
            newMeter = makeMeterFromDenominator(meanDuration, denom)
            if not newMeter is None:
                createdMeters.append(newMeter)
        return createdMeters

    # try to generate the common ones
    if bestMeter is None:
        meters = getMeters(meanDuration)
        if len(meters) > 0:
            bestEval = 0
        for m in meters:
            res = evaluateMeter(m, omrMeasures, meanDuration)
            if res < bestEval:
                bestMeter = m
                bestEval = res

        # otherwise, oh well, it stays None

    # tidy up meters in streams
    meterLists = ((omrMeters, omrStream), (midiMeters, midiStream))
    fixedOmrMeterQls = []
    if not bestMeter is None:
        # remove extra meter markings (assume this piece in one meter)
        for meterList, part in meterLists:
            for m in meterList:
                atZero = m.getOffsetInHierarchy(part) == 0
                correctMeter = m.ratioString == bestMeter.ratioString
                if not (atZero and correctMeter):
                    if meterList == omrMeters:
                        fixedOmrMeterQls.append(round(m.barDuration.quarterLength,2))
                    part.remove(m, recurse=True)

        # fix whole bar rest measures that are messed up because of wrong key sig
        if len(fixedOmrMeterQls) > 0:
            for m in omrMeasures:
                if allRests(m) and 0 < m.duration.quarterLength < bestMeter.barDuration.quarterLength:
                    if round(m.duration.quarterLength,2) in fixedOmrMeterQls:
                        # then we fix
                        origRest = m.getElementsByClass(note.Rest)[0]
                        newBarRest = note.Rest()
                        newBarRest.duration = bestMeter.barDuration
                        markConfidentFix(newBarRest)
                        m.replace(origRest, newBarRest)


        # add meter if necessary
        omrMeters = list(omrStream.recurse(classFilter="TimeSignature"))
        midiMeters = list(midiStream.recurse(classFilter="Time Signature"))
        if len(omrMeters) == 0:
            if isinstance(omrStream, stream.Part):
                omrStream.insert(0, bestMeter)
            else:
                omrStream.parts[0].insert(0, bestMeter)  # must insert into the part, not just the outer stream
        if len(midiMeters) == 0:
            if isinstance(midiStream, stream.Part):
                midiStream.insert(0, bestMeter)
            else:
                midiStream.parts[0].insert(0, bestMeter)

def adjustOmrMeasures(omrPart, midiPart):
    '''
    :param omrPart: stream.Part with measures
    :param midiPart: stream.Part with measures
    :return:
    '''
    # sometimes stretches of rests have a number above which is missed so there should be more rests than actually are

    loop = True
    lastI = 0
    while loop:

        omrMeasures = omrPart.getElementsByClass('Measure')
        midiMeasures = midiPart.getElementsByClass('Measure')
        if len(omrMeasures) == len(midiMeasures):
            # all correct
            return

        loop = False

        for i in range(lastI, len(omrMeasures)):
            if loop: # something else was fixed, let's leave and recalibrate
                # dont' keep going through this for loop
                lastI = i
                break

            omrMeasure = omrMeasures[i]

            # if it's rests and it only has one rest in it!
            if allRests(omrMeasure) and len(omrMeasure.getElementsByClass(note.Rest)) == 1:
                if i  > len(midiMeasures):
                    continue

                midiMeasure = midiMeasures[i]
                if not allRests(midiMeasure):
                    # issue if is empty, but shouldn't be!
                    omrPart.remove(omrMeasure, shiftOffsets=True)
                    loop = True
                    break

                nextIndex = i + 1
                if nextIndex >= len(omrMeasures):
                    continue

                nextOmrMeasure = omrMeasures[nextIndex]
                if allRests(nextOmrMeasure): # ignore if not just isolated empty measure
                    continue

                findingRestMeasures = True
                restMeasuresToAdd = 0

                while findingRestMeasures:
                    if nextIndex >= len(midiMeasures):
                        break

                    nextMidiMeasure = midiMeasures[nextIndex]
                    if allRests(nextMidiMeasure):
                        restMeasuresToAdd += 1
                        nextIndex += 1
                    else:
                        findingRestMeasures = False

                measureDuration = omrMeasure.duration.quarterLength
                insertIndex = omrMeasure.getOffsetInHierarchy(omrPart)
                while restMeasuresToAdd > 0:
                    loop = True
                    newMeasure = deepcopy(omrMeasure)
                    for e in newMeasure.elements:
                        markConfidentFix(e)
                    omrPart.insertAndShift(insertIndex, newMeasure)
                    insertIndex += measureDuration
                    restMeasuresToAdd -= 1

def fillInMidiPart(omrPart, midiPart, midiStream, midiPartNum):
    midiParts = len(midiStream.parts)
    if len(midiStream.parts) <= 1:
       return
    midiModifed = deepcopy(midiPart)
    omrModified = deepcopy(omrPart)
    gen = CorrectionGeneration(omrModified, midiModifed)
    bestSimilarity = gen.aligner.similarityScore
    gens = [gen]
    keepLooping = True
    somethingFixed = False
    problematicElementToCheck = 0
    partToCheck = 0

    while keepLooping:
        keepLooping = False  # set true when something is fixed

        for i in range(problematicElementToCheck, len(gen.problematicOmrPreElements)):
            omrElem= gen.problematicOmrPreElements[i]
            if keepLooping: # for the while loop
                break

            omrElemOffset = omrElem.getOffsetInHierarchy(gen.omrPre)

            midiElem = gen.getFirstProblematicMidiFromOmr(omrElem)[0]
            # midiElem = gen.getMidiPostFromPre(midiElemPre)

            if not omrElem.isRest and midiElem.isRest:
                midiStartingOffset = midiElem.getOffsetInHierarchy(gen.midiPre)
                nextMidiRests = getNextRests(midiElem, referenceStream=gen.midiPre, returnOrig=True)
                maxDuration = nextMidiRests.duration

                for partNum in range(partToCheck, midiParts):
                    # maybe should go through all parts and find the best substitute?
                    if partNum >= midiPartNum:
                        continue

                    part = midiStream.parts[partNum]
                    partMeasure = part.getElementAtOrBefore(midiStartingOffset, classList = [stream.Measure])
                    if partMeasure is None:
                         continue
                    partElems = list(partMeasure.getElementsByOffset(midiStartingOffset-partMeasure.offset, classList = [note.NotRest]))
                    if len(partElems) == 0:
                        continue
                    partElem = partElems[0]
                    potentialReplacement = getNotesWithinDuration(partElem, maxDuration, referenceStream=part)
                    if len(potentialReplacement.elements) == 0:
                        continue

                    for e in potentialReplacement.elements:
                        markTentativeFix(e)

                    # might be cleaner to replace in midiPost, but deal with that later...
                    replaceSnippet(nextMidiRests, potentialReplacement, gen.midiPre)
                    nextGeneration = CorrectionGeneration(gen.omrPre, gen.midiPre)
                    nextGenSim = nextGeneration.aligner.similarityScore
                    if nextGenSim > bestSimilarity:
                        somethingFixed = True
                        bestSimilarity = nextGenSim
                        gens.append(gen)
                        gen = nextGeneration
                        problematicElementToCheck = 0
                        partToCheck = 0
                        keepLooping = True
                        break # from the for loop
                    else:
                        partToCheck = partNum + 1 if partNum + 1 < midiParts else 0
                        problematicElementToCheck = i+1 if partToCheck == 0 else i
                        gen = gens[-1].makeNextGeneration()
                        keepLooping = True if problematicElementToCheck < len(gen.problematicOmrPreElements) else False
                        break
    returnStream = gens[-1].midiPre
    clearLayout(returnStream)
    clearLyrics(returnStream)
    return (returnStream, somethingFixed)

def allRests(s):
    for e in s.recurse():
        if isinstance(e, note.NotRest):
            return False
    return True

def isDoubleMeasure(s):
    pass

def clearStyle(s):
    '''
    Clear style of all elements in stream s
    :param s: stream.Stream to modify
    '''
    for elem in s.recurse():
        elem.style = None

def clearLayout(s):
    for e in s.recurse():
        if isinstance(e, layout.LayoutBase):
            s.remove(e, recurse= True)

def clearLyrics(s):
    for e in s.recurse():
        if isinstance(e, note.GeneralNote) and e.lyrics and len(e.lyrics) > 0:
            e.lyrics = []

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
        container = referenceStream.containerInHierarchy(startingRest)
        startingRest.activeSite = container

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

def replaceSnippet(snippetToRemove, snippetToAdd, operateOnStream, epsilon = EPSILON):
    '''
    Replaces snippetToRemove with snippetToAdd in operateOnStream.

    All notes in snippetToRemove must exist in operateOnStream.
    When snippetToRemove is longer than snippetToAdd, pad the ending with rests.
    snippetToAdd cannot be longer than snippetToRemove

    doesn't have entire extra measures at the beginning of snippetToRemove that shouldn't be removed

    :param snippetToRemove: stream.Stream
    :param snippetToAdd: stream.Stream
    :param operateOnStream: boolean, whether should return original elements or deepcopy
    '''

    snippetToAddCopy = deepcopy(snippetToAdd)
    if snippetToAddCopy.duration.quarterLength == 0 and snippetToRemove.duration.quarterLength == 0:
        return

    diff = snippetToRemove.duration.quarterLength - snippetToAddCopy.duration.quarterLength
    equalizingRest = note.Rest(quarterLength=abs(diff))

    if diff > epsilon:
        snippetToAddCopy.append(equalizingRest)
    elif diff < -epsilon:
        print("A PROBLEM!!")
        return

    firstElementToRemove = snippetToRemove.elements[0]
    containerToRemoveFrom = operateOnStream.containerInHierarchy(firstElementToRemove)
    newContainer = containerToRemoveFrom.cloneEmpty()

    containersToRemoveFrom = []
    newContainers = []

    keepLooping = True

    for origElem in containerToRemoveFrom.recurse():
        if origElem not in snippetToRemove:
            newContainer.append(origElem)
        else:
            while len(snippetToAddCopy.elements) > 0 :
                    addElem = snippetToAddCopy.elements[0]

                    spaceLeft = containerToRemoveFrom.duration.quarterLength - newContainer.duration.quarterLength
                    if spaceLeft < epsilon:
                        containersContainer = operateOnStream.containerInHierarchy(containerToRemoveFrom)
                        containerToRemoveFrom.activeSite = containersContainer

                        containersToRemoveFrom.append(containerToRemoveFrom)
                        newContainers.append(newContainer)

                        containerToRemoveFrom = containerToRemoveFrom.next('Measure', activeSiteOnly=True)
                        newContainer = containerToRemoveFrom.cloneEmpty()

                        spaceLeft = containerToRemoveFrom.duration.quarterLength - newContainer.duration.quarterLength

                    addElemQl = addElem.duration.quarterLength if not addElem.duration is None else 0

                    if  spaceLeft > addElemQl - epsilon: # change me!
                        newContainer.append(addElem)
                        snippetToAddCopy.remove(addElem)
                    else:
                        addElemCopy = deepcopy(addElem)
                        addElemCopy.duration.quarterLength = spaceLeft
                        origTie = addElem.tie
                        nextElem = addElem
                        nextElem.duration.quarterLength = addElem.duration.quarterLength - spaceLeft

                        if isinstance(nextElem, note.NotRest):
                            if origTie.tie is None:
                                addElemCopy.tie = tie.Tie("start")
                            elif origTie.tie.style == "continue" or addElem.tie.style == "stop":
                                addElemCopy.tie = tie.Tie("continue")
                            else:
                                addElemCopy.tie = tie.Tie("start")

                            if origTie.tie is not None and origTie.tie.style == "continue":
                                nextElem.tie = tie.Tie("continue")
                            else:
                                nextElem.tie = tie.Tie("stop")
                        newContainer.append(addElemCopy)
            break

    # fill up last measure with appropriate elements from end of that measure
    if abs(containerToRemoveFrom.duration.quarterLength - newContainer.duration.quarterLength) > epsilon:
        for origElem in containerToRemoveFrom.recurse():
            if origElem.offset + epsilon >= newContainer.duration.quarterLength:
                newContainer.append(origElem)

    containersToRemoveFrom.append(containerToRemoveFrom)
    newContainers.append(newContainer)

    for i in range(len(containersToRemoveFrom)):
        containerToRemoveFrom = containersToRemoveFrom[i]
        newContainer = newContainers[i]
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
    midiPreIdsImpacted.add(midiPreElem.id)

    if not omrPreElem.isNote or not midiPreElem.isNote:
        return (False, set(), set())

    if omrPreElem.duration == midiPreElem.duration:
        return (False, set(), set())

    if omrPreElem.duration.dots == 0 and midiPreElem.duration.dots == 0:
        return (False, set(), set())

    omrPreElemDurationCopy = deepcopy(omrPreElem.duration)
    midiPreElemDurationCopy = deepcopy(midiPreElem.duration)
    omrPreElemDurationCopy.dots = 0
    midiPreElemDurationCopy.dots = 0

    durationDiff = omrPreElemDurationCopy.quarterLength - midiPreElemDurationCopy.quarterLength
    if abs(durationDiff) <= epsilon:
        omrPostElem.duration.quarterLength = midiPreElem.duration.quarterLength
        markConfidentFix(omrPostElem)
        markConfidentFix(midiPostElem)

        # Determine impacted elements
        changeList = generation.getCorrespondingMidiFromOmr(omrPreElem)
        for e, _ in changeList:
            midiPreIdsImpacted.add(e.id)
        return (True, omrPreIdsImpacted, midiPreIdsImpacted)

    return (False, set(), set())

def fixerHelperDifferentArticulation(omrPreElem, generation, epsilon = EPSILON):
    correspondingElements = getCorrespondingElementsFromPreElem(omrPreElem, generation)
    if any(e is None for e in correspondingElements):
        return (False, set(), set())
    omrPostElem, midiPreElem, midiPostElem = correspondingElements

    omrPreIdsImpacted = set()
    omrPreIdsImpacted.add(omrPreElem.id)
    midiPreIdsImpacted = set()
    midiPreIdsImpacted.add(midiPreElem.id)

    changesList = generation.getCorrespondingMidiFromOmr(omrPreElem)
    for e, _ in changesList:
        midiPreIdsImpacted.add(e.id)


    if not omrPreElem.isNote or not midiPreElem.isNote:
        return (False, set(), set())

    if omrPreElem.duration == midiPreElem.duration:
        return (False, set(), set())

    if omrPreElem.duration.quarterLength > midiPreElem.duration.quarterLength:
        # ex) omr has quarter, midi has eighth + eighth rest
        midiPreNotesDuring = getNotesWithinDuration(midiPreElem, omrPreElem.duration, referenceStream=generation.midiPre)
        if len(midiPreNotesDuring.elements) > 1:
            allRestsAfter = True
            for el in midiPreNotesDuring[1:]:
                validTie = not el.tie is None and el.tie.type != "start"
                if not (el.isRest or validTie):
                    allRestsAfter = False
            if allRestsAfter:
                # keep OMR representation
                markConfidentFix(omrPostElem)
                midiPostNotesDuring = getNotesWithinDuration(midiPostElem, omrPreElem.duration, referenceStream=generation.midiPost, returnOrig=True)
                omrPreElemStream = stream.Stream()
                omrPreElemStream.append(omrPreElem)
                replaceSnippet(midiPostNotesDuring, omrPreElemStream, generation.midiPost)

                # Update impacted ids
                for nextMidiElem in midiPreNotesDuring.recurse():
                    midiPreIdsImpacted.add(nextMidiElem.id)

                return (True, omrPreIdsImpacted, midiPreIdsImpacted)

    else:
        omrPreNotesDuring = getNotesWithinDuration(omrPreElem, midiPreElem.duration, referenceStream=generation.omrPre)
        if len(omrPreNotesDuring.elements) > 1:
            allRestsAfter = True
            for el in omrPreNotesDuring[1:]:
                validTie = not el.tie is None and el.tie.type != "start"
                if not (el.isRest or validTie):
                    allRestsAfter = False
            if allRestsAfter:
                # keep OMR representation
                midiPostElemStream = stream.Stream()
                midiPostElemStream.append(midiPostElem)
                replaceSnippet(midiPostElemStream, omrPreNotesDuring, generation.midiPost)

                omrPostNotesDuring = getNotesWithinDuration(omrPostElem, midiPreElem.duration, referenceStream=generation.omrPost, returnOrig=True)
                for n in omrPostNotesDuring:
                    markConfidentFix(n)

                # Update impacted ids
                for n in omrPreNotesDuring:
                    changesList = generation.getCorrespondingMidiFromOmr(n)
                    for e, _ in changesList:
                        midiPreIdsImpacted.add(e.id)

                return (True, omrPreIdsImpacted, midiPreIdsImpacted)


    return (False, set(), set())
