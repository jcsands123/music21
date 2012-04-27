# -*- coding: utf-8 -*-
#-------------------------------------------------------------------------------
# Name:         instrumentLookup.py
# Purpose:      Musical translation tables
# Authors:      Jose Cabal-Ugaz
#
# Copyright:    (c) 2012 The music21 Project
# License:      LGPL
#-------------------------------------------------------------------------------

abbreviationToEnglish = \
{u'a.': u'alto',
 u'acc.': u'accordion',
 u'accdn': u'accordion',
 u'arp.': u'harp',
 u'b.': u'bass',
 u'b.cl.': u'bass clarinet',
 u'b.dr.': u'bass drum',
 u'bar.': u'baritone',
 u'bj.': u'banjo',
 u'bkl.': u'bass clarinet',
 u'bn.': u'bassoon',
 u'br.': u'viola',
 u'bssn.': u'bassoon',
 u'c.c.': u'snare drum',
 u'cas.': u'castanets',
 u'casts': u'castanets',
 u'cb.': u'contrabass',
 u'cel.': u'celesta',
 u'cl.': u'clarinet',
 u'clvd.': u'clavichord',
 u'cor.': u'horn',
 u'cr.tr.': u'bass drum',
 u'e. hn.': u'english horn',
 u'e.gtr.': u'electric guitar',
 u'e.h.': u'english horn',
 u'eng.hn.': u'english horn',
 u'fg.': u'bassoon',
 u'fing.cym.': u'finger cymbals',
 u'fl.': u'flute',
 u'g.c.': u'bass drum',
 u'glck.': u'glockenspiel',
 u'glock.': u'glockenspiel',
 u'glsp.': u'glockenspiel',
 u'gng': u'gong',
 u'gr.cassa.': u'bass drum',
 u'harp': u'harp',
 u'hb.': u'oboe',
 u'hn.': u'horn',
 u'hp.': u'harp',
 u'hpd.': u'harpsichord',
 u'hpe.': u'harp',
 u'hrp.': u'harp',
 u'k. dr.': u'timpani',
 u'kas.': u'castanets',
 u'kl.': u'clarinet',
 u'mand.': u'mandolin',
 u'mar.': u'marimba',
 u'mez.': u'mezzo-soprano',
 u'mezz.': u'mezzo-soprano',
 u'ob.': u'oboe',
 u'pf.': u'piano',
 u'pfte.': u'piano',
 u'pic.': u'piccolo',
 u'picc.': u'piccolo',
 u'pk.': u'timpani',
 u'pno.': u'piano',
 u'rec.': u'recorder',
 u's.': u'soprano',
 u'sand.bl.': u'sandpaper blocks',
 u'sax.': u'saxophone',
 u'sax.a.': u'alto saxophone',
 u'sn.dr.': u'snare drum',
 u't.': u'tenor',
 u'tamb.': u'tambourine',
 u'tamtam': u'gong',
 u'tb.': u'tuba',
 u'tbe': u'trumpet',
 u'tbni': u'trombone',
 u'ten.dr.': u'tenor drum',
 u'timp.': u'timpani',
 u'tmbn.': u'tambourine',
 u'tpt.': u'trumpet',
 u'tr.': u'trumpet',
 u'trb.': u'trombone',
 u'trgl.': u'triangle',
 u'tri.': u'triangle',
 u'tuba': u'tuba',
 u'v.': u'violin',
 u'va.': u'viola',
 u'vc.': u'violoncello',
 u'vcelle.': u'violoncello',
 u'vcl.': u'violoncello',
 u'vio.': u'violin',
 u'vl.': u'violin',
 u'vla.': u'viola',
 u'vlc.': u'violoncello',
 u'vlon.': u'violin',
 u'vn.': u'violin',
 u'vni.': u'violin',
 u'voc.': u'voice',
 u'vv.': u'violin',
 u'windmachine': u'wind machine',
 u'xil.': u'xylophone',
 u'xyl.': u'xylophone'}

bestNameToInstrumentClass = \
{'accordion': 'Accordion',
 'acoustic bass': 'AcousticBass',
 'acoustic guitar': 'AcousticGuitar',
 'agogo': 'Agogo',
 'alto': 'Alto',
 'alto saxophone': 'AltoSaxophone',
 'bagpipes': 'Bagpipes',
 'banjo': 'Banjo',
 'baritone': 'Baritone',
 'baritone saxophone': 'BaritoneSaxophone',
 'bass': 'Bass',
 'bass clarinet': 'BassClarinet',
 'bass drum': 'BassDrum',
 'bassoon': 'Bassoon',
 'bongo drums': 'BongoDrums',
 'castanets': 'Castanets',
 'celesta': 'Celesta',
 'clarinet': 'Clarinet',
 'clavichord': 'Clavichord',
 'conga drum': 'CongaDrum',
 'contrabass': 'Contrabass',
 'cowbells': 'Cowbells',
 'crash cymbals': 'CrashCymbals',
 'dulcimer': 'Dulcimer',
 'electric bass': 'ElectricBass',
 'electric guitar': 'ElectricGuitar',
 'electric organ': 'ElectricOrgan',
 'english horn': 'EnglishHorn',
 'finger cymbals': 'FingerCymbals',
 'flute': 'Flute',
 'fretless bass': 'FretlessBass',
 'glockenspiel': 'Glockenspiel',
 'gong': 'Gong',
 'handbells': 'Handbells',
 'harmonica': 'Harmonica',
 'harp': 'Harp',
 'harpsichord': 'Harpsichord',
 'hi-hat cymbal': 'HiHatCymbal',
 'horn': 'Horn',
 'kalimba': 'Kalimba',
 'koto': 'Koto',
 'mandolin': 'Mandolin',
 'maracas': 'Maracas',
 'marimba': 'Marimba',
 'mezzo-soprano': 'MezzoSoprano',
 'oboe': 'Oboe',
 'ocarina': 'Ocarina',
 'pan flute': 'PanFlute',
 'piano': 'Piano',
 'piccolo': 'Piccolo',
 'pipe organ': 'PipeOrgan',
 'ratchet': 'Ratchet',
 'recorder': 'Recorder',
 'reed organ': 'ReedOrgan',
 'sandpaper blocks': 'SandpaperBlocks',
 'saxophone': 'Saxophone',
 'shakuhachi': 'Shakuhachi',
 'shamisen': 'Shamisen',
 'shehnai': 'Shehnai',
 'siren': 'Siren',
 'sitar': 'Sitar',
 'sizzle cymbal': 'SizzleCymbal',
 'sleigh bells': 'SleighBells',
 'snare drum': 'SnareDrum',
 'soprano': 'Soprano',
 'soprano saxophone': 'SopranoSaxophone',
 'steel drum': 'SteelDrum',
 'suspended cymbal': 'SuspendedCymbal',
 'taiko': 'Taiko',
 'tam-tam': 'TamTam',
 'tambourine': 'Tambourine',
 'temple block': 'TempleBlock',
 'tenor': 'Tenor',
 'tenor drum': 'TenorDrum',
 'tenor saxophone': 'TenorSaxophone',
 'timbales': 'Timbales',
 'timpani': 'Timpani',
 'tom-tom': 'TomTom',
 'triangle': 'Triangle',
 'trombone': 'Trombone',
 'trumpet': 'Trumpet',
 'tuba': 'Tuba',
 'tubular bells': 'TubularBells',
 'ukulele': 'Ukulele',
 'viola': 'Viola',
 'violin': 'Violin',
 'violoncello': 'Violoncello',
 'voice': 'Vocalist',
 'whip': 'Whip',
 'whistle': 'Whistle',
 'wind machine': 'WindMachine',
 'woodblock': 'Woodblock',
 'xylophone': 'Xylophone'}

frenchToEnglish = \
{u'accord\xe9on': u'accordion',
 u'alto': u'viola',
 u'banjo': u'banjo',
 u'bariton': u'baritone',
 u'baryton': u'baritone',
 u'bas-dessus': u'soprano',
 u'basse': u'bass',
 u'basson': u'bassoon',
 u'bloc de bois': u'woodblock',
 u'bongos': u'bongo drums',
 u'caisse claire': u'snare drum',
 u'caisse roulante': u'tenor drum',
 u'castagnettes': u'castanets',
 u'claquebois': u'xylophone',
 u'clarinette': u'clarinet',
 u'clarinette basse': u'bass clarinet',
 u'clavecin': u'harpsichord',
 u'clavessin': u'harpsichord',
 u'clave\xe7in': u'harpsichord',
 u'clavicorde': u'clavichord',
 u'cloches': u'tubular bells',
 u'cloches de vache': u'cowbells',
 u'cloches tubolaires': u'tubular bells',
 u'cloches tubulaires': u'tubular bells',
 u'cloches \xe0 vache': u'cowbells',
 u'clochettes \u2021 main': u'handbells',
 u'conga': u'conga drum',
 u'contralto': u'alto',
 u'cor': u'horn',
 u'cor anglais': u'english horn',
 u'crash': u'crash cymbals',
 u'cr\xe9celle': u'ratchet',
 u'cymbale sur tiges': u'sizzle cymbal',
 u'cymbale suspendue': u'suspended cymbal',
 u'cymbales': u'crash cymbals',
 u'cymbales digitales': u'finger cymbals',
 u'c\xe9lesta': u'celesta',
 u'droite': u'recorder',
 u'eoliphone': u'wind machine',
 u'fl\xfbte': u'flute',
 u'fl\xfbte de pan': u'pan flute',
 u'fl\xfbte douce': u'recorder',
 u'fl\xfbte droite': u'recorder',
 u'fl\xfbte piccolo': u'piccolo',
 u'fl\xfbte traversi\xe8re': u'flute',
 u'fl\xfbte \xe0 bec': u'recorder',
 u'fouet': u'whip',
 u'gencerros': u'cowbells',
 u'glockenspiel': u'glockenspiel',
 u'gong': u'gong',
 u'grande fl\xfbte': u'flute',
 u'grelots': u'sleigh bells',
 u'grosse caisse': u'bass drum',
 u'guitarre \xe9lectrique': u'electric guitar',
 u'harmonica': u'harmonica',
 u'harmonica de bois': u'xylophone',
 u'harpe': u'harp',
 u'hautbois': u'oboe',
 u'jeu de timbres': u'glockenspiel',
 u'machine \xe0 vent': u'wind machine',
 u'mandoline': u'mandolin',
 u'maracas': u'maracas',
 u'marimba': u'marimba',
 u'ocarina': u'ocarina',
 u'papier de verre': u'sandpaper blocks',
 u'petite fl\xfbte': u'piccolo',
 u'piano': u'piano',
 u'pianoforte': u'piano',
 u'sagates': u'finger cymbals',
 u'sagattes': u'finger cymbals',
 u'saxophon alto': u'alto saxophone',
 u'saxophone': u'saxophone',
 u'saxophone alto': u'alto saxophone',
 u'saxophone baryton': u'baritone saxophone',
 u'saxophone soprano': u'soprano saxophone',
 u'saxophone t\xe9nor': u'tenor saxophone',
 u'sir\xe8ne': u'siren',
 u'syrinx': u'pan flute',
 u'taille': u'tenor',
 u'tam-tam': u'tam-tam',
 u'tambour': u'snare drum',
 u'tambour bata': u'bass drum',
 u'tambour congo': u'conga drum',
 u"tambour d'acier": u'steel drum',
 u'tambour de basque': u'tambourine',
 u'tambourin': u'tenor drum',
 u'timbale': u'timpani',
 u'timbales': u'timpani',
 u'timbales cr\xe9oles': u'timbales',
 u'timbales cubaines': u'timbales',
 u'timbales latines': u'timbales',
 u'tom': u'tom-tom',
 u'tom-tom': u'tom-tom',
 u'triangle': u'triangle',
 u'trombone': u'trombone',
 u'trompette': u'trumpet',
 u'tuba': u'tuba',
 u'tumbadora': u'conga drum',
 u't\xe9nor': u'tenor',
 u'ukul\xe9l\xe9': u'ukulele',
 u'violon': u'violin',
 u'violoncelle': u'violoncello',
 u'voix': u'voice',
 u'wood-bloc': u'woodblock',
 u'xylophone': u'xylophone',
 u'zill': u'finger cymbals',
 u'\xc8chelettes': u'xylophone',
 u'\xe9oliphone': u'wind machine'}

germanToEnglish = \
{u'aeolophon': u'wind machine',
 u'akkordeon': u'accordion',
 u'alt': u'alto',
 u'altgeige': u'viola',
 u'altsaxophon': u'alto saxophone',
 u'arpicordo': u'harpsichord',
 u'banjo': u'banjo',
 u'bariton': u'baritone',
 u'baritonsaxophon': u'baritone saxophone',
 u'bass': u'bass',
 u'bassklarinette': u'bass clarinet',
 u'becken freih\xe4ngend': u'suspended cymbal',
 u'becken gew\xf6nlich': u'crash cymbals',
 u'becken-paar': u'crash cymbals',
 u'beckfl\xf6te': u'recorder',
 u'blockfl\xf6te': u'recorder',
 u'bongos': u'bongo drums',
 u'bratsche': u'viola',
 u'celesta': u'celesta',
 u'cello': u'violoncello',
 u'cembalo': u'harpsichord',
 u'clavicembalo': u'harpsichord',
 u'clavichord': u'clavichord',
 u'clavicimbel': u'harpsichord',
 u'conga': u'conga drum',
 u'conga-trommel': u'conga drum',
 u'crashbecken': u'crash cymbals',
 u'elektrische gitarre': u'electric guitar',
 u'englischhorn': u'english horn',
 u'fagott': u'bassoon',
 u'fingerzimbeln': u'finger cymbals',
 u'fl\xf6te': u'flute',
 u'geige': u'violin',
 u'glocken': u'tubular bells',
 u'glockenspiel': u'glockenspiel',
 u'gong': u'gong',
 u'grosse trommel': u'bass drum',
 u'hackbrett': u'dulcimer',
 u'handglocken': u'handbells',
 u'handharmonika': u'accordion',
 u'harfe': u'harp',
 u'herdenglocken': u'cowbells',
 u'hirtenfl\xf6te': u'pan flute',
 u'hoboe': u'oboe',
 u'holzblock': u'woodblock',
 u'holzklapper': u'whip',
 u'horn': u'horn',
 u'h\xe4ngendes becken': u'suspended cymbal',
 u'kastagnetten': u'castanets',
 u'kesselpauke': u'timpani',
 u'kesseltrommel': u'timpani',
 u'kielfl\xfcgel': u'harpsichord',
 u'klarinette': u'clarinet',
 u'klavichord': u'clavichord',
 u'klavier': u'piano',
 u'kleine fl\xf6te': u'piccolo',
 u'kleine trommel': u'snare drum',
 u'knarre': u'ratchet',
 u'kuba-pauken': u'timbales',
 u'kuhglocken': u'cowbells',
 u'leinentrommel': u'snare drum',
 u'lyra': u'glockenspiel',
 u'mandoline': u'mandolin',
 u'maracas': u'maracas',
 u'marimba': u'marimba',
 u'marimbaphon': u'marimba',
 u'marschtrommel': u'snare drum',
 u'mundharmonika': u'harmonica',
 u'nietenbecken': u'sizzle cymbal',
 u'oboe': u'oboe',
 u'octavfl\xf6te': u'piccolo',
 u'okarina': u'ocarina',
 u'panfl\xf6te': u'pan flute',
 u'papagenopfeife': u'pan flute',
 u'pauke': u'timpani',
 u'pauken': u'timpani',
 u'peitsche': u'whip',
 u'piano': u'piano',
 u'pianoforte': u'piano',
 u'piccolo': u'piccolo',
 u'pickelfl\xf6te': u'piccolo',
 u'pikkolofl\xf6te': u'piccolo',
 u'posaune': u'trombone',
 u'querfl\xf6te': u'flute',
 u'ratsche': u'ratchet',
 u'rohrenglocke': u'tubular bells',
 u'rollschellen': u'sleigh bells',
 u'r\xfchrtrommel': u'tenor drum',
 u'sandbl\xf6cke': u'sandpaper blocks',
 u'sandpapier': u'sandpaper blocks',
 u'saxophon': u'saxophone',
 u'schellen': u'sleigh bells',
 u'schellentrommel': u'tambourine',
 u'schnabelfl\xf6te': u'recorder',
 u'schnarre': u'ratchet',
 u'schnarrtrommel': u'snare drum',
 u'sirene': u'siren',
 u'sopransaxophon': u'soprano saxophone',
 u'stahltrommel': u'steel drum',
 u'stimme': u'voice',
 u'strohfiedel': u'xylophone',
 u'syrinx': u'pan flute',
 u'tamburin': u'tambourine',
 u'tamtam': u'tam-tam',
 u'tenor': u'tenor',
 u'tenorsaxophon': u'tenor saxophone',
 u'tenortrommel': u'tenor drum',
 u'timbales': u'timbales',
 u'tom': u'tom-tom',
 u'tom tom': u'tom-tom',
 u'tom-tom': u'tom-tom',
 u'triangel': u'triangle',
 u'trompete': u'trumpet',
 u'tuba': u'tuba',
 u'tumba': u'conga drum',
 u't\xfcrkisches h\xe4ngebecken': u'suspended cymbal',
 u'ukulele': u'ukulele',
 u'ventilhorn': u'horn',
 u'viehschellen': u'cowbells',
 u'viola': u'viola',
 u'viole': u'viola',
 u'violine': u'violin',
 u'violoncell': u'violoncello',
 u'violoncello': u'violoncello',
 u'windmaschine': u'wind machine',
 u'wirbeltrommel': u'tenor drum',
 u'xylophon': u'xylophone',
 u'ziehharmonika': u'accordion'}

italianToEnglish = \
{u'a becco': u'recorder',
 u'armonica a bocca': u'harmonica',
 u'arpa': u'harp',
 u'arpe': u'harp',
 u'arpicordo': u'harpsichord',
 u'banjo': u'banjo',
 u'baritono': u'baritone',
 u'basso': u'bass',
 u'blocco di legno': u'woodblock',
 u'blocco di legno cinese': u'woodblock',
 u'bonghi': u'bongo drums',
 u'bongos': u'bongo drums',
 u'campanacci': u'cowbells',
 u'campane': u'tubular bells',
 u'campane tubolari': u'tubular bells',
 u'campane tubulari': u'tubular bells',
 u'campanelli': u'glockenspiel',
 u'campanelli a mano': u'handbells',
 u'campanelli da mucca': u'cowbells',
 u'campanelli di vacca': u'cowbells',
 u'carta vetrata': u'sandpaper blocks',
 u'cassa': u'bass drum',
 u'cassa chiara': u'snare drum',
 u'cassa rullante': u'tenor drum',
 u'cassetina': u'woodblock',
 u'castagnette': u'castanets',
 u'celesta': u'celesta',
 u'celeste': u'celesta',
 u'cello': u'violoncello',
 u'cembalo': u'harpsichord',
 u'ceppi di carta vetro': u'sandpaper blocks',
 u'chitarra elettrica': u'electric guitar',
 u'cimbalini': u'finger cymbals',
 u'cimbalo': u'harpsichord',
 u'cinelli': u'crash cymbals',
 u'clarinetto': u'clarinet',
 u'clarinetto basso': u'bass clarinet',
 u'clarino': u'trumpet',
 u'clavicembalo': u'harpsichord',
 u'clavicordo': u'clavichord',
 u'contralto': u'alto',
 u'corno': u'horn',
 u'corno inglese': u'english horn',
 u'dritto': u'recorder',
 u'eolifono': u'wind machine',
 u'fagotto': u'bassoon',
 u'fisarmonica': u'accordion',
 u'flauto': u'flute',
 u'flauto a becco': u'recorder',
 u'flauto di pan': u'pan flute',
 u'flauto diritto': u'recorder',
 u'flauto dolce': u'recorder',
 u'flauto dritto': u'recorder',
 u'flauto piccolo': u'piccolo',
 u'flauto traverso': u'flute',
 u'frusta': u'whip',
 u'gigelira': u'xylophone',
 u'glockenspiel': u'glockenspiel',
 u'gong': u'gong',
 u'gran cassa': u'bass drum',
 u'grancassa': u'bass drum',
 u'macchina del vento': u'wind machine',
 u'machina a venti': u'wind machine',
 u'mandolino': u'mandolin',
 u'maracas': u'maracas',
 u'marimba': u'marimba',
 u'metallofono': u'glockenspiel',
 u'nacchere': u'castanets',
 u'oboe': u'oboe',
 u'ocarina': u'ocarina',
 u'ottavino': u'piccolo',
 u'piano': u'piano',
 u'pianoforte': u'piano',
 u'piatti': u'crash cymbals',
 u'piatto chiodati': u'sizzle cymbal',
 u'piatto sospeso': u'suspended cymbal',
 u'raganella': u'ratchet',
 u'rullante': u'snare drum',
 u'salterio': u'dulcimer',
 u'sassofono': u'saxophone',
 u'sassofono alto': u'alto saxophone',
 u'sassofono baritono': u'baritone saxophone',
 u'sassofono contralto': u'alto saxophone',
 u'sassofono soprano': u'soprano saxophone',
 u'sassofono tenore': u'tenor saxophone',
 u'sax': u'saxophone',
 u'saxofono': u'saxophone',
 u'silofono': u'xylophone',
 u'sirena': u'siren',
 u'sirena a mano': u'siren',
 u'siringa': u'pan flute',
 u'sonagli': u'sleigh bells',
 u'sonagliera': u'sleigh bells',
 u'tam-tam': u'tam-tam',
 u'tamborone': u'bass drum',
 u'tamburello': u'tambourine',
 u'tamburino': u'tambourine',
 u'tamburo basco': u'tambourine',
 u"tamburo d'acciaio": u'steel drum',
 u'tamburo grande': u'bass drum',
 u'tamburo grosso': u'bass drum',
 u'tamburo militare': u'snare drum',
 u'tamburo rullante': u'tenor drum',
 u'tenore': u'tenor',
 u'timbales': u'timbales',
 u'timbales latinoamericani': u'timbales',
 u'timballi': u'timpani',
 u'timballo': u'timpani',
 u'timpanetti': u'timbales',
 u'timpani': u'timpani',
 u'timpano': u'timpani',
 u'tom-tom': u'tom-tom',
 u'triangolo': u'triangle',
 u'tromba': u'trumpet',
 u'trombone': u'trombone',
 u'tuba': u'tuba',
 u'tumba': u'conga drum',
 u'tympani': u'timpani',
 u'ukulele': u'ukulele',
 u'viola': u'viola',
 u'violino': u'violin',
 u'violoncello': u'violoncello',
 u'voca': u'voice',
 u'voce': u'voice',
 u'xilifono': u'xylophone',
 u'xilofono': u'xylophone'}

russianToEnglish = \
{u"al't": u'alto',
 u'angliiskii rozhok': u'english horn',
 u'arfa': u'harp',
 u'bariton': u'baritone',
 u'bas': u'bass',
 u'bass-klarnet': u'bass clarinet',
 u'blokfleita': u'recorder',
 u"bol'shoi baraban": u'bass drum',
 u'chelesta': u'celesta',
 u'chembalo': u'harpsichord',
 u'fagot': u'bassoon',
 u'fleita': u'flute',
 u'fleita pikkolo': u'piccolo',
 u"fortep'iano": u'piano',
 u'frantsuzskii baraban': u'snare drum',
 u'goboi': u'oboe',
 u'golos': u'voice',
 u'gorn': u'horn',
 u'klarnet': u'clarinet',
 u'klavesin': u'harpsichord',
 u'klavikord': u'clavichord',
 u"kolokol'chiki": u'glockenspiel',
 u"kontral'to": u'alto',
 u'ksilofon': u'xylophone',
 u'litavra': u'timpani',
 u'malaia fleita': u'piccolo',
 u'mandolina': u'mandolin',
 u'marimba': u'marimba',
 u'pikkolo': u'piccolo',
 u'rog': u'horn',
 u'rozhok': u'horn',
 u'saksofon': u'saxophone',
 u'skripka': u'violin',
 u'tenor': u'tenor',
 u'trombon': u'trombone',
 u'truba': u'trumpet',
 u'tsilindricheskii baraban': u'tenor drum',
 u'tsimbaly': u'dulcimer',
 u'tuba': u'tuba',
 u'viola': u'viola',
 u"violonchel'": u'violoncello'}

spanishToEnglish = \
{u'acorde\xf3n': u'accordion',
 u'arm\xf3nica de boca': u'harmonica',
 u'arpa': u'harp',
 u'atabal': u'timpani',
 u'bajo': u'bass',
 u'banjo': u'banjo',
 u'bar\xedtono': u'baritone',
 u'bombo': u'bass drum',
 u'bongos': u'bongo drums',
 u'caja china': u'woodblock',
 u'caja clara': u'snare drum',
 u'caja redoblante': u'tenor drum',
 u'caja rodante': u'tenor drum',
 u'campanas': u'tubular bells',
 u'campanas de mano': u'handbells',
 u'campanas tubulares': u'tubular bells',
 u'campanos': u'cowbells',
 u'campan\xf3logo': u'glockenspiel',
 u'carraca': u'ratchet',
 u'cascabels': u'sleigh bells',
 u'casta\xf1uelas': u'castanets',
 u'celesta': u'celesta',
 u'cello': u'violoncello',
 u'cencerros': u'cowbells',
 u'chelo': u'violoncello',
 u'chinchines': u'finger cymbals',
 u'clarinete': u'clarinet',
 u'clarinete bajo': u'bass clarinet',
 u'clavec\xe9mbalo': u'harpsichord',
 u'clavec\xedn': u'harpsichord',
 u'clavicordio': u'clavichord',
 u'clavic\xe9mbalo': u'harpsichord',
 u'clavic\xedmbalo': u'harpsichord',
 u'con tensores': u'snare drum',
 u'conga': u'conga drum',
 u'contralto': u'alto',
 u'corneta inglesa': u'english horn',
 u'corno': u'english horn',
 u'corno franc\xe9s': u'horn',
 u'corno ingl\xe9s': u'english horn',
 u'cr\xf3talos': u'finger cymbals',
 u'cuerno': u'horn',
 u'cuerno ingl\xe9s': u'english horn',
 u'c\xe9mbalo': u'harpsichord',
 u'de pico': u'recorder',
 u'de timbres': u'glockenspiel',
 u'dulce': u'recorder',
 u'dulcema': u'dulcimer',
 u'fagot': u'bassoon',
 u'flauta': u'flute',
 u'flauta de boehm': u'flute',
 u'flauta de concierto': u'flute',
 u'flauta de pico': u'recorder',
 u'flauta dulce': u'recorder',
 u'flauta piccolo': u'piccolo',
 u'flauta recta': u'recorder',
 u'flauta traversa': u'flute',
 u'flauta travesera': u'flute',
 u'flautas de pan': u'pan flute',
 u'flaut\xedn': u'piccolo',
 u'gong': u'gong',
 u'gran caja': u'bass drum',
 u'gravic\xe9mbalo': u'harpsichord',
 u'guitarra el\xe9ctrica': u'electric guitar',
 u'juego': u'glockenspiel',
 u'juego de timbres': u'glockenspiel',
 u'liro': u'glockenspiel',
 u'l\xe1tigo': u'whip',
 u'mandolina': u'mandolin',
 u'maracas': u'maracas',
 u'marimba': u'marimba',
 u'matraca': u'ratchet',
 u'm\xe1quina de viento': u'wind machine',
 u'oboe': u'oboe',
 u'ocarina': u'ocarina',
 u'octavillo': u'piccolo',
 u'ottavino': u'piccolo',
 u'pailas criollas': u'timbales',
 u'pandereta': u'tambourine',
 u'papel de lija': u'sandpaper blocks',
 u'piano': u'piano',
 u'platillo sizzle': u'sizzle cymbal',
 u'platillo suspendido': u'suspended cymbal',
 u'platillos crash': u'crash cymbals',
 u'redoblante': u'snare drum',
 u'saxof\xf3n': u'saxophone',
 u'saxof\xf3no': u'saxophone',
 u'saxof\xf3no alto': u'alto saxophone',
 u'saxof\xf3no bar\xedtono': u'baritone saxophone',
 u'saxof\xf3no soprano': u'soprano saxophone',
 u'saxof\xf3no tenor': u'tenor saxophone',
 u'sirena': u'siren',
 u'siringa': u'pan flute',
 u'tam-tam': u'tam-tam',
 u'tambor afinable': u'snare drum',
 u'tambor de mano': u'tambourine',
 u'tambor mayor': u'tenor drum',
 u'tambor met\xe1lico de trinidad y tobago': u'steel drum',
 u'tenor': u'tenor',
 u'timbal': u'timpani',
 u'timbales': u'timpani',
 u'timbals': u'timpani',
 u'tom-tom': u'tom-tom',
 u'tomtom': u'tom-tom',
 u'tri\xe1ngulo': u'triangle',
 u'tromb\xf3n': u'trombone',
 u'trompa': u'horn',
 u'trompeta': u'trumpet',
 u'tuba': u'tuba',
 u'tumbadora': u'conga drum',
 u'ukelele': u'ukulele',
 u'viola': u'viola',
 u'violoncelo': u'violoncello',
 u'violonchelo': u'violoncello',
 u'viol\xedn': u'violin',
 u'voz': u'voice',
 u'xilof\xf3n': u'xylophone',
 u'xilof\xf3no': u'xylophone',
 u'xil\xf3fono': u'xylophone',
 u'zampo\xf1as': u'pan flute',
 u'\xf3rgano de': u'glockenspiel',
 u'\xf3rgano de campanas': u'glockenspiel'}

transliteration = \
{'frenchToEnglish': {'accordeon': u'accord\xe9on',
                     'celesta': u'c\xe9lesta',
                     'clavecin': u'clave\xe7in',
                     'cloches a vache': u'cloches \xe0 vache',
                     'clochettes ++ main': u'clochettes \u2021 main',
                     'crecelle': u'cr\xe9celle',
                     'echelettes': u'\xc8chelettes',
                     'eoliphone': u'\xe9oliphone',
                     'flute': u'fl\xfbte',
                     'flute a bec': u'fl\xfbte \xe0 bec',
                     'flute de pan': u'fl\xfbte de pan',
                     'flute douce': u'fl\xfbte douce',
                     'flute droite': u'fl\xfbte droite',
                     'flute piccolo': u'fl\xfbte piccolo',
                     'flute traversiere': u'fl\xfbte traversi\xe8re',
                     'grande flute': u'grande fl\xfbte',
                     'guitarre electrique': u'guitarre \xe9lectrique',
                     'machine a vent': u'machine \xe0 vent',
                     'petite flute': u'petite fl\xfbte',
                     'saxophone tenor': u'saxophone t\xe9nor',
                     'sirene': u'sir\xe8ne',
                     'tenor': u't\xe9nor',
                     'timbales creoles': u'timbales cr\xe9oles',
                     'ukulele': u'ukul\xe9l\xe9'},
 'germanToEnglish': {'becken freihangend': u'becken freih\xe4ngend',
                     'becken gewonlich': u'becken gew\xf6nlich',
                     'beckflote': u'beckfl\xf6te',
                     'blockflote': u'blockfl\xf6te',
                     'flote': u'fl\xf6te',
                     'hangendes becken': u'h\xe4ngendes becken',
                     'hirtenflote': u'hirtenfl\xf6te',
                     'kielflugel': u'kielfl\xfcgel',
                     'kleine flote': u'kleine fl\xf6te',
                     'octavflote': u'octavfl\xf6te',
                     'panflote': u'panfl\xf6te',
                     'pickelflote': u'pickelfl\xf6te',
                     'pikkoloflote': u'pikkolofl\xf6te',
                     'querflote': u'querfl\xf6te',
                     'ruhrtrommel': u'r\xfchrtrommel',
                     'sandblocke': u'sandbl\xf6cke',
                     'schnabelflote': u'schnabelfl\xf6te',
                     'turkisches hangebecken': u't\xfcrkisches h\xe4ngebecken'},
 'spanishToEnglish': {'acordeon': u'acorde\xf3n',
                      'armonica de boca': u'arm\xf3nica de boca',
                      'baritono': u'bar\xedtono',
                      'campanologo': u'campan\xf3logo',
                      'castanuelas': u'casta\xf1uelas',
                      'cembalo': u'c\xe9mbalo',
                      'clavecembalo': u'clavec\xe9mbalo',
                      'clavecin': u'clavec\xedn',
                      'clavicembalo': u'clavic\xe9mbalo',
                      'clavicimbalo': u'clavic\xedmbalo',
                      'corno frances': u'corno franc\xe9s',
                      'corno ingles': u'corno ingl\xe9s',
                      'crotalos': u'cr\xf3talos',
                      'cuerno ingles': u'cuerno ingl\xe9s',
                      'flautin': u'flaut\xedn',
                      'gravicembalo': u'gravic\xe9mbalo',
                      'guitarra electrica': u'guitarra el\xe9ctrica',
                      'latigo': u'l\xe1tigo',
                      'maquina de viento': u'm\xe1quina de viento',
                      'organo de': u'\xf3rgano de',
                      'organo de campanas': u'\xf3rgano de campanas',
                      'saxofon': u'saxof\xf3n',
                      'saxofono': u'saxof\xf3no',
                      'saxofono alto': u'saxof\xf3no alto',
                      'saxofono baritono': u'saxof\xf3no bar\xedtono',
                      'saxofono soprano': u'saxof\xf3no soprano',
                      'saxofono tenor': u'saxof\xf3no tenor',
                      'tambor metalico de trinidad y tobago': u'tambor met\xe1lico de trinidad y tobago',
                      'triangulo': u'tri\xe1ngulo',
                      'trombon': u'tromb\xf3n',
                      'violin': u'viol\xedn',
                      'xilofon': u'xilof\xf3n',
                      'xilofono': u'xilof\xf3no',
                      'zamponas': u'zampo\xf1as'}}