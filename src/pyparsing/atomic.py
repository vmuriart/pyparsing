# -*- coding: utf-8 -*-

import re
import sre_constants
import warnings

from pyparsing.exceptions import ParseException
from pyparsing.pyparsing import (ParserElement, _ustr, alphanums, _MAX_INT,
                                 _escapeRegexRangeChars, basestring, range)


class Token(ParserElement):
    """Abstract C{ParserElement} subclass, for defining atomic matching
    patterns."""

    def __init__(self):
        super(Token, self).__init__(savelist=False)


class Empty(Token):
    """An empty token, will always match."""

    def __init__(self):
        super(Empty, self).__init__()
        self.name = "Empty"
        self.mayReturnEmpty = True
        self.mayIndexError = False


class NoMatch(Token):
    """A token that will never match."""

    def __init__(self):
        super(NoMatch, self).__init__()
        self.name = "NoMatch"
        self.mayReturnEmpty = True
        self.mayIndexError = False
        self.errmsg = "Unmatchable token"

    def parseImpl(self, instring, loc, doActions=True):
        raise ParseException(instring, loc, self.errmsg, self)


class Literal(Token):
    """Token to exactly match a specified string."""

    def __init__(self, matchString):
        super(Literal, self).__init__()
        self.match = matchString
        self.matchLen = len(matchString)
        try:
            self.firstMatchChar = matchString[0]
        except IndexError:
            warnings.warn("null string passed to Literal; use Empty() instead",
                          SyntaxWarning, stacklevel=2)
            self.__class__ = Empty
        self.name = '"{0!s}"'.format(_ustr(self.match))
        self.errmsg = "Expected " + self.name
        self.mayReturnEmpty = False
        self.mayIndexError = False

    # Performance tuning: this routine gets called a *lot*. if this is a single
    # character match string  and the first character matches, short-circuit as
    # quickly as possible, and avoid calling startswith
    # ~ @profile
    def parseImpl(self, instring, loc, doActions=True):
        if (instring[loc] == self.firstMatchChar and
                (self.matchLen == 1 or instring.startswith(self.match, loc))):
            return loc + self.matchLen, self.match
        raise ParseException(instring, loc, self.errmsg, self)


class Keyword(Token):
    """Token to exactly match a specified string as a keyword, that is, it must
    be immediately followed by a non-keyword character.
    Compare with C{L{Literal}}:
    - C{Literal("if")} will match the leading C{'if'} in C{'ifAndOnlyIf'}.
    - C{Keyword("if")} will not; it will only match the leading C{'if'}
      in C{'if x=1'}, or C{'if(y==2)'}

    Accepts two optional constructor arguments in addition to the keyword str:
    - C{identChars} is a string of characters that would be valid identifier
      characters, defaulting to all alphanumerics + "_" and "$"
    - C{caseless} allows case-insensitive matching, default is C{False}.
    """
    DEFAULT_KEYWORD_CHARS = alphanums + "_$"

    def __init__(self, matchString, identChars=DEFAULT_KEYWORD_CHARS,
                 caseless=False):
        super(Keyword, self).__init__()
        self.match = matchString
        self.matchLen = len(matchString)
        try:
            self.firstMatchChar = matchString[0]
        except IndexError:
            warnings.warn("null string passed to Keyword; use Empty() instead",
                          SyntaxWarning, stacklevel=2)
        self.name = '"{0!s}"'.format(self.match)
        self.errmsg = "Expected " + self.name
        self.mayReturnEmpty = False
        self.mayIndexError = False
        self.caseless = caseless
        if caseless:
            self.caselessmatch = matchString.upper()
            identChars = identChars.upper()
        self.identChars = set(identChars)

    def parseImpl(self, instring, loc, doActions=True):
        if self.caseless:
            if ((instring[
                 loc:loc + self.matchLen].upper() == self.caselessmatch) and
                    (loc >= len(instring) - self.matchLen or instring[
                            loc + self.matchLen].upper() not in self.identChars) and
                    (loc == 0 or instring[
                            loc - 1].upper() not in self.identChars)):
                return loc + self.matchLen, self.match
        else:
            if (instring[loc] == self.firstMatchChar and
                    (self.matchLen == 1 or instring.startswith(self.match,
                                                               loc)) and
                    (loc >= len(instring) - self.matchLen or instring[
                            loc + self.matchLen] not in self.identChars) and
                    (loc == 0 or instring[loc - 1] not in self.identChars)):
                return loc + self.matchLen, self.match
        raise ParseException(instring, loc, self.errmsg, self)

    def copy(self):
        c = super(Keyword, self).copy()
        c.identChars = Keyword.DEFAULT_KEYWORD_CHARS
        return c

    @staticmethod
    def setDefaultKeywordChars(chars):
        """Overrides the default Keyword chars
        """
        Keyword.DEFAULT_KEYWORD_CHARS = chars


class CaselessLiteral(Literal):
    """Token to match a specified string, ignoring case of letters.
    Note: the matched results will always be in the case of the given
    match string, NOT the case of the input text.
    """

    def __init__(self, matchString):
        super(CaselessLiteral, self).__init__(matchString.upper())
        # Preserve the defining literal.
        self.returnString = matchString
        self.name = "'{0!s}'".format(self.returnString)
        self.errmsg = "Expected " + self.name

    def parseImpl(self, instring, loc, doActions=True):
        if instring[loc:loc + self.matchLen].upper() == self.match:
            return loc + self.matchLen, self.returnString
        raise ParseException(instring, loc, self.errmsg, self)


class CaselessKeyword(Keyword):
    def __init__(self, matchString, identChars=Keyword.DEFAULT_KEYWORD_CHARS):
        super(CaselessKeyword, self).__init__(matchString, identChars,
                                              caseless=True)

    def parseImpl(self, instring, loc, doActions=True):
        if ((instring[
             loc:loc + self.matchLen].upper() == self.caselessmatch) and
                (loc >= len(instring) - self.matchLen or instring[
                        loc + self.matchLen].upper() not in self.identChars)):
            return loc + self.matchLen, self.match
        raise ParseException(instring, loc, self.errmsg, self)


class Word(Token):
    """Token for matching words composed of allowed character sets.
    Defined with string containing all allowed initial characters,
    an optional string containing allowed body characters (if omitted,
    defaults to the initial character set), and an optional minimum,
    maximum, and/or exact length.  The default value for C{min} is 1 (a
    minimum value < 1 is not valid); the default values for C{max} and C{exact}
    are 0, meaning no maximum or exact length restriction. An optional
    C{excludeChars} parameter can list characters that might be found in
    the input C{bodyChars} string; useful to define a word of all printables
    except for one or two characters, for instance.
    """

    def __init__(self, initChars, bodyChars=None, min=1, max=0, exact=0,
                 asKeyword=False, excludeChars=None):
        super(Word, self).__init__()
        if excludeChars:
            initChars = ''.join(c for c in initChars if c not in excludeChars)
            if bodyChars:
                bodyChars = ''.join(
                    c for c in bodyChars if c not in excludeChars)
        self.initCharsOrig = initChars
        self.initChars = set(initChars)
        if bodyChars:
            self.bodyCharsOrig = bodyChars
            self.bodyChars = set(bodyChars)
        else:
            self.bodyCharsOrig = initChars
            self.bodyChars = set(initChars)

        self.maxSpecified = max > 0

        if min < 1:
            raise ValueError("cannot specify a minimum length < 1; "
                             "use Optional(Word()) if zero-length word "
                             "is permitted")

        self.minLen = min

        if max > 0:
            self.maxLen = max
        else:
            self.maxLen = _MAX_INT

        if exact > 0:
            self.maxLen = exact
            self.minLen = exact

        self.name = _ustr(self)
        self.errmsg = "Expected " + self.name
        self.mayIndexError = False
        self.asKeyword = asKeyword

        if ' ' not in self.initCharsOrig + self.bodyCharsOrig and (
                            min == 1 and max == 0 and exact == 0):
            if self.bodyCharsOrig == self.initCharsOrig:
                self.reString = "[{0!s}]+".format(_escapeRegexRangeChars(
                    self.initCharsOrig))
            elif len(self.initCharsOrig) == 1:
                self.reString = "{0!s}[{1!s}]*".format(re.escape(self.initCharsOrig),
                                 _escapeRegexRangeChars(self.bodyCharsOrig))
            else:
                self.reString = "[{0!s}][{1!s}]*".format(_escapeRegexRangeChars(self.initCharsOrig),
                                 _escapeRegexRangeChars(self.bodyCharsOrig))
            if self.asKeyword:
                self.reString = r"\b" + self.reString + r"\b"
            try:
                self.re = re.compile(self.reString)
            except:
                self.re = None

    def parseImpl(self, instring, loc, doActions=True):
        if self.re:
            result = self.re.match(instring, loc)
            if not result:
                raise ParseException(instring, loc, self.errmsg, self)

            loc = result.end()
            return loc, result.group()

        if not (instring[loc] in self.initChars):
            raise ParseException(instring, loc, self.errmsg, self)

        start = loc
        loc += 1
        instrlen = len(instring)
        bodychars = self.bodyChars
        maxloc = start + self.maxLen
        maxloc = min(maxloc, instrlen)
        while loc < maxloc and instring[loc] in bodychars:
            loc += 1

        throwException = False
        if loc - start < self.minLen:
            throwException = True
        if self.maxSpecified and loc < instrlen and instring[loc] in bodychars:
            throwException = True
        if self.asKeyword:
            if (start > 0 and instring[start - 1] in bodychars) or (
                            loc < instrlen and instring[loc] in bodychars):
                throwException = True

        if throwException:
            raise ParseException(instring, loc, self.errmsg, self)

        return loc, instring[start:loc]

    def __str__(self):
        try:
            return super(Word, self).__str__()
        except:
            pass

        if self.strRepr is None:

            def charsAsStr(s):
                if len(s) > 4:
                    return s[:4] + "..."
                else:
                    return s

            if self.initCharsOrig != self.bodyCharsOrig:
                self.strRepr = "W:({0!s},{1!s})".format(
                    charsAsStr(self.initCharsOrig),
                    charsAsStr(self.bodyCharsOrig))
            else:
                self.strRepr = "W:({0!s})".format(charsAsStr(self.initCharsOrig))

        return self.strRepr


class Regex(Token):
    """Token for matching strings that match a given regular expression.

    Defined with string specifying the regular expression in a form recognized
    by the inbuilt Python re module.
    """
    compiledREtype = type(re.compile("[A-Z]"))

    def __init__(self, pattern, flags=0):
        """The parameters C{pattern} and C{flags} are passed to the
        C{re.compile()} function as-is. See the Python C{re} module for an
        explanation of the acceptable patterns and flags."""
        super(Regex, self).__init__()

        if isinstance(pattern, basestring):
            if not pattern:
                warnings.warn(
                    "null string passed to Regex; use Empty() instead",
                    SyntaxWarning, stacklevel=2)

            self.pattern = pattern
            self.flags = flags

            try:
                self.re = re.compile(self.pattern, self.flags)
                self.reString = self.pattern
            except sre_constants.error:
                warnings.warn("invalid pattern ({0!s}) passed to Regex".format(pattern),
                              SyntaxWarning, stacklevel=2)
                raise

        elif isinstance(pattern, Regex.compiledREtype):
            self.re = pattern
            self.pattern = \
                self.reString = str(pattern)
            self.flags = flags

        else:
            raise ValueError("Regex may only be constructed with a string "
                             "or a compiled RE object")

        self.name = _ustr(self)
        self.errmsg = "Expected " + self.name
        self.mayIndexError = False
        self.mayReturnEmpty = True

    def parseImpl(self, instring, loc, doActions=True):
        result = self.re.match(instring, loc)
        if not result:
            raise ParseException(instring, loc, self.errmsg, self)

        loc = result.end()
        d = result.groupdict()
        ret = ParseResults(result.group())
        if d:
            for k in d:
                ret[k] = d[k]
        return loc, ret

    def __str__(self):
        try:
            return super(Regex, self).__str__()
        except:
            pass

        if self.strRepr is None:
            self.strRepr = "Re:({0!s})".format(repr(self.pattern))

        return self.strRepr


class QuotedString(Token):
    """Token for matching strings that are delimited by quoting characters.
    """

    def __init__(self, quoteChar, escChar=None, escQuote=None, multiline=False,
                 unquoteResults=True, endQuoteChar=None,
                 convertWhitespaceEscapes=True):
        """Defined with the following parameters:
        - quoteChar - string of one or more characters defining the quote
          delimiting string
        - escChar - character to escape quotes, typically backslash
          (default=None)
        - escQuote - special quote sequence to escape an embedded quote
          string (such as SQL's "" to escape an embedded ") (default=None)
        - multiline - boolean indicating whether quotes can span multiple
          lines (default=C{False})
        - unquoteResults - boolean indicating whether the matched text
          should be unquoted (default=C{True})
        - endQuoteChar - string of one or more characters defining the end
          of the quote delimited string (default=C{None}=> same as quoteChar)
        - convertWhitespaceEscapes - convert escaped whitespace (C{'\t'},
          C{'\n'}, etc.) to actual whitespace (default=C{True})
        """
        super(QuotedString, self).__init__()

        # remove white space from quote chars - wont work anyway
        quoteChar = quoteChar.strip()
        if not quoteChar:
            warnings.warn("quoteChar cannot be the empty string",
                          SyntaxWarning, stacklevel=2)
            raise SyntaxError()

        if endQuoteChar is None:
            endQuoteChar = quoteChar
        else:
            endQuoteChar = endQuoteChar.strip()
            if not endQuoteChar:
                warnings.warn("endQuoteChar cannot be the empty string",
                              SyntaxWarning, stacklevel=2)
                raise SyntaxError()

        self.quoteChar = quoteChar
        self.quoteCharLen = len(quoteChar)
        self.firstQuoteChar = quoteChar[0]
        self.endQuoteChar = endQuoteChar
        self.endQuoteCharLen = len(endQuoteChar)
        self.escChar = escChar
        self.escQuote = escQuote
        self.unquoteResults = unquoteResults
        self.convertWhitespaceEscapes = convertWhitespaceEscapes

        if multiline:
            self.flags = re.MULTILINE | re.DOTALL
            self.pattern = r'{0!s}(?:[^{1!s}{2!s}]'.format(re.escape(self.quoteChar),
                            _escapeRegexRangeChars(self.endQuoteChar[0]),
                            (escChar is not None and _escapeRegexRangeChars(
                                escChar) or ''))
        else:
            self.flags = 0
            self.pattern = r'{0!s}(?:[^{1!s}\n\r{2!s}]'.format(re.escape(self.quoteChar),
                            _escapeRegexRangeChars(self.endQuoteChar[0]),
                            (escChar is not None and _escapeRegexRangeChars(
                                escChar) or ''))
        if len(self.endQuoteChar) > 1:
            self.pattern += (
                '|(?:' + ')|(?:'.join(
                    "{0!s}[^{1!s}]".format(re.escape(self.endQuoteChar[:i]),
                                 _escapeRegexRangeChars(self.endQuoteChar[i]))
                    for i in range(len(self.endQuoteChar) - 1, 0, -1)) + ')'
            )
        if escQuote:
            self.pattern += (r'|(?:{0!s})'.format(re.escape(escQuote)))
        if escChar:
            self.pattern += (r'|(?:{0!s}.)'.format(re.escape(escChar)))
            self.escCharReplacePattern = re.escape(self.escChar) + "(.)"
        self.pattern += (r')*{0!s}'.format(re.escape(self.endQuoteChar)))

        try:
            self.re = re.compile(self.pattern, self.flags)
            self.reString = self.pattern
        except sre_constants.error:
            warnings.warn(
                "invalid pattern ({0!s}) passed to Regex".format(self.pattern),
                SyntaxWarning, stacklevel=2)
            raise

        self.name = _ustr(self)
        self.errmsg = "Expected " + self.name
        self.mayIndexError = False
        self.mayReturnEmpty = True

    def parseImpl(self, instring, loc, doActions=True):
        result = instring[loc] == self.firstQuoteChar and self.re.match(
            instring, loc) or None
        if not result:
            raise ParseException(instring, loc, self.errmsg, self)

        loc = result.end()
        ret = result.group()

        if self.unquoteResults:

            # strip off quotes
            ret = ret[self.quoteCharLen:-self.endQuoteCharLen]

            if isinstance(ret, basestring):
                # replace escaped whitespace
                if '\\' in ret and self.convertWhitespaceEscapes:
                    ws_map = {
                        r'\t': '\t',
                        r'\n': '\n',
                        r'\f': '\f',
                        r'\r': '\r',
                    }
                    for wslit, wschar in ws_map.items():
                        ret = ret.replace(wslit, wschar)

                # replace escaped characters
                if self.escChar:
                    ret = re.sub(self.escCharReplacePattern, "\g<1>", ret)

                # replace escaped quotes
                if self.escQuote:
                    ret = ret.replace(self.escQuote, self.endQuoteChar)

        return loc, ret

    def __str__(self):
        try:
            return super(QuotedString, self).__str__()
        except:
            pass

        if self.strRepr is None:
            self.strRepr = "quoted string, starting with {0!s} ending with {1!s}".format(
                self.quoteChar, self.endQuoteChar)

        return self.strRepr


class CharsNotIn(Token):
    """Token for matching words composed of characters *not* in a given set.
    Defined with string containing all disallowed characters, and an optional
    minimum, maximum, and/or exact length.  Default value for C{min} is 1 (a
    minimum value < 1 is not valid); the default values for C{max} and C{exact}
    are 0, meaning no maximum or exact length restriction.
    """

    def __init__(self, notChars, min=1, max=0, exact=0):
        super(CharsNotIn, self).__init__()
        self.skipWhitespace = False
        self.notChars = notChars

        if min < 1:
            raise ValueError("cannot specify a minimum length < 1; "
                             "use Optional(CharsNotIn()) "
                             "if zero-length char group is permitted")

        self.minLen = min

        if max > 0:
            self.maxLen = max
        else:
            self.maxLen = _MAX_INT

        if exact > 0:
            self.maxLen = exact
            self.minLen = exact

        self.name = _ustr(self)
        self.errmsg = "Expected " + self.name
        self.mayReturnEmpty = (self.minLen == 0)
        self.mayIndexError = False

    def parseImpl(self, instring, loc, doActions=True):
        if instring[loc] in self.notChars:
            raise ParseException(instring, loc, self.errmsg, self)

        start = loc
        loc += 1
        notchars = self.notChars
        maxlen = min(start + self.maxLen, len(instring))
        while loc < maxlen and \
                (instring[loc] not in notchars):
            loc += 1

        if loc - start < self.minLen:
            raise ParseException(instring, loc, self.errmsg, self)

        return loc, instring[start:loc]

    def __str__(self):
        try:
            return super(CharsNotIn, self).__str__()
        except:
            pass

        if self.strRepr is None:
            if len(self.notChars) > 4:
                self.strRepr = "!W:({0!s}...)".format(self.notChars[:4])
            else:
                self.strRepr = "!W:({0!s})".format(self.notChars)

        return self.strRepr


class White(Token):
    """Special matching class for matching whitespace.  Normally, whitespace is
    ignored by pyparsing grammars.  This class is included when some whitespace
    structures are significant.  Define with a string containing the whitespace
    characters to be matched; default is C{" \\t\\r\\n"}.
    Also takes optional C{min}, C{max}, and C{exact} arguments, as defined for
    the C{L{Word}} class."""
    whiteStrs = {
        " ": "<SPC>",
        "\t": "<TAB>",
        "\n": "<LF>",
        "\r": "<CR>",
        "\f": "<FF>",
    }

    def __init__(self, ws=" \t\r\n", min=1, max=0, exact=0):
        super(White, self).__init__()
        self.matchWhite = ws
        self.setWhitespaceChars(
            "".join(c for c in self.whiteChars if c not in self.matchWhite))
        # ~ self.leaveWhitespace()
        self.name = ("".join(White.whiteStrs[c] for c in self.matchWhite))
        self.mayReturnEmpty = True
        self.errmsg = "Expected " + self.name

        self.minLen = min

        if max > 0:
            self.maxLen = max
        else:
            self.maxLen = _MAX_INT

        if exact > 0:
            self.maxLen = exact
            self.minLen = exact

    def parseImpl(self, instring, loc, doActions=True):
        if not (instring[loc] in self.matchWhite):
            raise ParseException(instring, loc, self.errmsg, self)
        start = loc
        loc += 1
        maxloc = start + self.maxLen
        maxloc = min(maxloc, len(instring))
        while loc < maxloc and instring[loc] in self.matchWhite:
            loc += 1

        if loc - start < self.minLen:
            raise ParseException(instring, loc, self.errmsg, self)

        return loc, instring[start:loc]


class _PositionToken(Token):
    def __init__(self):
        super(_PositionToken, self).__init__()
        self.name = self.__class__.__name__
        self.mayReturnEmpty = True
        self.mayIndexError = False
