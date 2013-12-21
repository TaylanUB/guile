;;; char.scm --- The R7RS char library

;;      Copyright (C) 2013 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(define-library (scheme char)
  (export char-alphabetic?
          char-ci<?
          char-ci>=?
          char-downcase
          char-lower-case?
          char-upcase
          char-whitespace?
          string-ci<=?
          string-ci=?
          string-ci>?
          string-foldcase
          char-ci<=?
          char-ci=?
          char-ci>?
          char-foldcase
          char-numeric?
          char-upper-case?
          digit-value
          string-ci<?
          string-ci>=?
          string-downcase
          string-upcase)
  (import (scheme base)
          (rnrs unicode))
  (begin
    (define digit-value
      (let* ((digit-table
              ;; Derived from http://www.unicode.org/Public/6.3.0/ucd/extracted/DerivedNumericValues.txt
              #((#x00030 . 0) ; DIGIT ZERO
                (#x00031 . 1) ; DIGIT ONE
                (#x00032 . 2) ; DIGIT TWO
                (#x00033 . 3) ; DIGIT THREE
                (#x00034 . 4) ; DIGIT FOUR
                (#x00035 . 5) ; DIGIT FIVE
                (#x00036 . 6) ; DIGIT SIX
                (#x00037 . 7) ; DIGIT SEVEN
                (#x00038 . 8) ; DIGIT EIGHT
                (#x00039 . 9) ; DIGIT NINE
                (#x00660 . 0) ; ARABIC-INDIC DIGIT ZERO
                (#x00661 . 1) ; ARABIC-INDIC DIGIT ONE
                (#x00662 . 2) ; ARABIC-INDIC DIGIT TWO
                (#x00663 . 3) ; ARABIC-INDIC DIGIT THREE
                (#x00664 . 4) ; ARABIC-INDIC DIGIT FOUR
                (#x00665 . 5) ; ARABIC-INDIC DIGIT FIVE
                (#x00666 . 6) ; ARABIC-INDIC DIGIT SIX
                (#x00667 . 7) ; ARABIC-INDIC DIGIT SEVEN
                (#x00668 . 8) ; ARABIC-INDIC DIGIT EIGHT
                (#x00669 . 9) ; ARABIC-INDIC DIGIT NINE
                (#x006F0 . 0) ; EXTENDED ARABIC-INDIC DIGIT ZERO
                (#x006F1 . 1) ; EXTENDED ARABIC-INDIC DIGIT ONE
                (#x006F2 . 2) ; EXTENDED ARABIC-INDIC DIGIT TWO
                (#x006F3 . 3) ; EXTENDED ARABIC-INDIC DIGIT THREE
                (#x006F4 . 4) ; EXTENDED ARABIC-INDIC DIGIT FOUR
                (#x006F5 . 5) ; EXTENDED ARABIC-INDIC DIGIT FIVE
                (#x006F6 . 6) ; EXTENDED ARABIC-INDIC DIGIT SIX
                (#x006F7 . 7) ; EXTENDED ARABIC-INDIC DIGIT SEVEN
                (#x006F8 . 8) ; EXTENDED ARABIC-INDIC DIGIT EIGHT
                (#x006F9 . 9) ; EXTENDED ARABIC-INDIC DIGIT NINE
                (#x007C0 . 0) ; NKO DIGIT ZERO
                (#x007C1 . 1) ; NKO DIGIT ONE
                (#x007C2 . 2) ; NKO DIGIT TWO
                (#x007C3 . 3) ; NKO DIGIT THREE
                (#x007C4 . 4) ; NKO DIGIT FOUR
                (#x007C5 . 5) ; NKO DIGIT FIVE
                (#x007C6 . 6) ; NKO DIGIT SIX
                (#x007C7 . 7) ; NKO DIGIT SEVEN
                (#x007C8 . 8) ; NKO DIGIT EIGHT
                (#x007C9 . 9) ; NKO DIGIT NINE
                (#x00966 . 0) ; DEVANAGARI DIGIT ZERO
                (#x00967 . 1) ; DEVANAGARI DIGIT ONE
                (#x00968 . 2) ; DEVANAGARI DIGIT TWO
                (#x00969 . 3) ; DEVANAGARI DIGIT THREE
                (#x0096A . 4) ; DEVANAGARI DIGIT FOUR
                (#x0096B . 5) ; DEVANAGARI DIGIT FIVE
                (#x0096C . 6) ; DEVANAGARI DIGIT SIX
                (#x0096D . 7) ; DEVANAGARI DIGIT SEVEN
                (#x0096E . 8) ; DEVANAGARI DIGIT EIGHT
                (#x0096F . 9) ; DEVANAGARI DIGIT NINE
                (#x009E6 . 0) ; BENGALI DIGIT ZERO
                (#x009E7 . 1) ; BENGALI DIGIT ONE
                (#x009E8 . 2) ; BENGALI DIGIT TWO
                (#x009E9 . 3) ; BENGALI DIGIT THREE
                (#x009EA . 4) ; BENGALI DIGIT FOUR
                (#x009EB . 5) ; BENGALI DIGIT FIVE
                (#x009EC . 6) ; BENGALI DIGIT SIX
                (#x009ED . 7) ; BENGALI DIGIT SEVEN
                (#x009EE . 8) ; BENGALI DIGIT EIGHT
                (#x009EF . 9) ; BENGALI DIGIT NINE
                (#x00A66 . 0) ; GURMUKHI DIGIT ZERO
                (#x00A67 . 1) ; GURMUKHI DIGIT ONE
                (#x00A68 . 2) ; GURMUKHI DIGIT TWO
                (#x00A69 . 3) ; GURMUKHI DIGIT THREE
                (#x00A6A . 4) ; GURMUKHI DIGIT FOUR
                (#x00A6B . 5) ; GURMUKHI DIGIT FIVE
                (#x00A6C . 6) ; GURMUKHI DIGIT SIX
                (#x00A6D . 7) ; GURMUKHI DIGIT SEVEN
                (#x00A6E . 8) ; GURMUKHI DIGIT EIGHT
                (#x00A6F . 9) ; GURMUKHI DIGIT NINE
                (#x00AE6 . 0) ; GUJARATI DIGIT ZERO
                (#x00AE7 . 1) ; GUJARATI DIGIT ONE
                (#x00AE8 . 2) ; GUJARATI DIGIT TWO
                (#x00AE9 . 3) ; GUJARATI DIGIT THREE
                (#x00AEA . 4) ; GUJARATI DIGIT FOUR
                (#x00AEB . 5) ; GUJARATI DIGIT FIVE
                (#x00AEC . 6) ; GUJARATI DIGIT SIX
                (#x00AED . 7) ; GUJARATI DIGIT SEVEN
                (#x00AEE . 8) ; GUJARATI DIGIT EIGHT
                (#x00AEF . 9) ; GUJARATI DIGIT NINE
                (#x00B66 . 0) ; ORIYA DIGIT ZERO
                (#x00B67 . 1) ; ORIYA DIGIT ONE
                (#x00B68 . 2) ; ORIYA DIGIT TWO
                (#x00B69 . 3) ; ORIYA DIGIT THREE
                (#x00B6A . 4) ; ORIYA DIGIT FOUR
                (#x00B6B . 5) ; ORIYA DIGIT FIVE
                (#x00B6C . 6) ; ORIYA DIGIT SIX
                (#x00B6D . 7) ; ORIYA DIGIT SEVEN
                (#x00B6E . 8) ; ORIYA DIGIT EIGHT
                (#x00B6F . 9) ; ORIYA DIGIT NINE
                (#x00BE6 . 0) ; TAMIL DIGIT ZERO
                (#x00BE7 . 1) ; TAMIL DIGIT ONE
                (#x00BE8 . 2) ; TAMIL DIGIT TWO
                (#x00BE9 . 3) ; TAMIL DIGIT THREE
                (#x00BEA . 4) ; TAMIL DIGIT FOUR
                (#x00BEB . 5) ; TAMIL DIGIT FIVE
                (#x00BEC . 6) ; TAMIL DIGIT SIX
                (#x00BED . 7) ; TAMIL DIGIT SEVEN
                (#x00BEE . 8) ; TAMIL DIGIT EIGHT
                (#x00BEF . 9) ; TAMIL DIGIT NINE
                (#x00C66 . 0) ; TELUGU DIGIT ZERO
                (#x00C67 . 1) ; TELUGU DIGIT ONE
                (#x00C68 . 2) ; TELUGU DIGIT TWO
                (#x00C69 . 3) ; TELUGU DIGIT THREE
                (#x00C6A . 4) ; TELUGU DIGIT FOUR
                (#x00C6B . 5) ; TELUGU DIGIT FIVE
                (#x00C6C . 6) ; TELUGU DIGIT SIX
                (#x00C6D . 7) ; TELUGU DIGIT SEVEN
                (#x00C6E . 8) ; TELUGU DIGIT EIGHT
                (#x00C6F . 9) ; TELUGU DIGIT NINE
                (#x00CE6 . 0) ; KANNADA DIGIT ZERO
                (#x00CE7 . 1) ; KANNADA DIGIT ONE
                (#x00CE8 . 2) ; KANNADA DIGIT TWO
                (#x00CE9 . 3) ; KANNADA DIGIT THREE
                (#x00CEA . 4) ; KANNADA DIGIT FOUR
                (#x00CEB . 5) ; KANNADA DIGIT FIVE
                (#x00CEC . 6) ; KANNADA DIGIT SIX
                (#x00CED . 7) ; KANNADA DIGIT SEVEN
                (#x00CEE . 8) ; KANNADA DIGIT EIGHT
                (#x00CEF . 9) ; KANNADA DIGIT NINE
                (#x00D66 . 0) ; MALAYALAM DIGIT ZERO
                (#x00D67 . 1) ; MALAYALAM DIGIT ONE
                (#x00D68 . 2) ; MALAYALAM DIGIT TWO
                (#x00D69 . 3) ; MALAYALAM DIGIT THREE
                (#x00D6A . 4) ; MALAYALAM DIGIT FOUR
                (#x00D6B . 5) ; MALAYALAM DIGIT FIVE
                (#x00D6C . 6) ; MALAYALAM DIGIT SIX
                (#x00D6D . 7) ; MALAYALAM DIGIT SEVEN
                (#x00D6E . 8) ; MALAYALAM DIGIT EIGHT
                (#x00D6F . 9) ; MALAYALAM DIGIT NINE
                (#x00E50 . 0) ; THAI DIGIT ZERO
                (#x00E51 . 1) ; THAI DIGIT ONE
                (#x00E52 . 2) ; THAI DIGIT TWO
                (#x00E53 . 3) ; THAI DIGIT THREE
                (#x00E54 . 4) ; THAI DIGIT FOUR
                (#x00E55 . 5) ; THAI DIGIT FIVE
                (#x00E56 . 6) ; THAI DIGIT SIX
                (#x00E57 . 7) ; THAI DIGIT SEVEN
                (#x00E58 . 8) ; THAI DIGIT EIGHT
                (#x00E59 . 9) ; THAI DIGIT NINE
                (#x00ED0 . 0) ; LAO DIGIT ZERO
                (#x00ED1 . 1) ; LAO DIGIT ONE
                (#x00ED2 . 2) ; LAO DIGIT TWO
                (#x00ED3 . 3) ; LAO DIGIT THREE
                (#x00ED4 . 4) ; LAO DIGIT FOUR
                (#x00ED5 . 5) ; LAO DIGIT FIVE
                (#x00ED6 . 6) ; LAO DIGIT SIX
                (#x00ED7 . 7) ; LAO DIGIT SEVEN
                (#x00ED8 . 8) ; LAO DIGIT EIGHT
                (#x00ED9 . 9) ; LAO DIGIT NINE
                (#x00F20 . 0) ; TIBETAN DIGIT ZERO
                (#x00F21 . 1) ; TIBETAN DIGIT ONE
                (#x00F22 . 2) ; TIBETAN DIGIT TWO
                (#x00F23 . 3) ; TIBETAN DIGIT THREE
                (#x00F24 . 4) ; TIBETAN DIGIT FOUR
                (#x00F25 . 5) ; TIBETAN DIGIT FIVE
                (#x00F26 . 6) ; TIBETAN DIGIT SIX
                (#x00F27 . 7) ; TIBETAN DIGIT SEVEN
                (#x00F28 . 8) ; TIBETAN DIGIT EIGHT
                (#x00F29 . 9) ; TIBETAN DIGIT NINE
                (#x01040 . 0) ; MYANMAR DIGIT ZERO
                (#x01041 . 1) ; MYANMAR DIGIT ONE
                (#x01042 . 2) ; MYANMAR DIGIT TWO
                (#x01043 . 3) ; MYANMAR DIGIT THREE
                (#x01044 . 4) ; MYANMAR DIGIT FOUR
                (#x01045 . 5) ; MYANMAR DIGIT FIVE
                (#x01046 . 6) ; MYANMAR DIGIT SIX
                (#x01047 . 7) ; MYANMAR DIGIT SEVEN
                (#x01048 . 8) ; MYANMAR DIGIT EIGHT
                (#x01049 . 9) ; MYANMAR DIGIT NINE
                (#x01090 . 0) ; MYANMAR SHAN DIGIT ZERO
                (#x01091 . 1) ; MYANMAR SHAN DIGIT ONE
                (#x01092 . 2) ; MYANMAR SHAN DIGIT TWO
                (#x01093 . 3) ; MYANMAR SHAN DIGIT THREE
                (#x01094 . 4) ; MYANMAR SHAN DIGIT FOUR
                (#x01095 . 5) ; MYANMAR SHAN DIGIT FIVE
                (#x01096 . 6) ; MYANMAR SHAN DIGIT SIX
                (#x01097 . 7) ; MYANMAR SHAN DIGIT SEVEN
                (#x01098 . 8) ; MYANMAR SHAN DIGIT EIGHT
                (#x01099 . 9) ; MYANMAR SHAN DIGIT NINE
                (#x017E0 . 0) ; KHMER DIGIT ZERO
                (#x017E1 . 1) ; KHMER DIGIT ONE
                (#x017E2 . 2) ; KHMER DIGIT TWO
                (#x017E3 . 3) ; KHMER DIGIT THREE
                (#x017E4 . 4) ; KHMER DIGIT FOUR
                (#x017E5 . 5) ; KHMER DIGIT FIVE
                (#x017E6 . 6) ; KHMER DIGIT SIX
                (#x017E7 . 7) ; KHMER DIGIT SEVEN
                (#x017E8 . 8) ; KHMER DIGIT EIGHT
                (#x017E9 . 9) ; KHMER DIGIT NINE
                (#x01810 . 0) ; MONGOLIAN DIGIT ZERO
                (#x01811 . 1) ; MONGOLIAN DIGIT ONE
                (#x01812 . 2) ; MONGOLIAN DIGIT TWO
                (#x01813 . 3) ; MONGOLIAN DIGIT THREE
                (#x01814 . 4) ; MONGOLIAN DIGIT FOUR
                (#x01815 . 5) ; MONGOLIAN DIGIT FIVE
                (#x01816 . 6) ; MONGOLIAN DIGIT SIX
                (#x01817 . 7) ; MONGOLIAN DIGIT SEVEN
                (#x01818 . 8) ; MONGOLIAN DIGIT EIGHT
                (#x01819 . 9) ; MONGOLIAN DIGIT NINE
                (#x01946 . 0) ; LIMBU DIGIT ZERO
                (#x01947 . 1) ; LIMBU DIGIT ONE
                (#x01948 . 2) ; LIMBU DIGIT TWO
                (#x01949 . 3) ; LIMBU DIGIT THREE
                (#x0194A . 4) ; LIMBU DIGIT FOUR
                (#x0194B . 5) ; LIMBU DIGIT FIVE
                (#x0194C . 6) ; LIMBU DIGIT SIX
                (#x0194D . 7) ; LIMBU DIGIT SEVEN
                (#x0194E . 8) ; LIMBU DIGIT EIGHT
                (#x0194F . 9) ; LIMBU DIGIT NINE
                (#x019D0 . 0) ; NEW TAI LUE DIGIT ZERO
                (#x019D1 . 1) ; NEW TAI LUE DIGIT ONE
                (#x019D2 . 2) ; NEW TAI LUE DIGIT TWO
                (#x019D3 . 3) ; NEW TAI LUE DIGIT THREE
                (#x019D4 . 4) ; NEW TAI LUE DIGIT FOUR
                (#x019D5 . 5) ; NEW TAI LUE DIGIT FIVE
                (#x019D6 . 6) ; NEW TAI LUE DIGIT SIX
                (#x019D7 . 7) ; NEW TAI LUE DIGIT SEVEN
                (#x019D8 . 8) ; NEW TAI LUE DIGIT EIGHT
                (#x019D9 . 9) ; NEW TAI LUE DIGIT NINE
                (#x01A80 . 0) ; TAI THAM HORA DIGIT ZERO
                (#x01A81 . 1) ; TAI THAM HORA DIGIT ONE
                (#x01A82 . 2) ; TAI THAM HORA DIGIT TWO
                (#x01A83 . 3) ; TAI THAM HORA DIGIT THREE
                (#x01A84 . 4) ; TAI THAM HORA DIGIT FOUR
                (#x01A85 . 5) ; TAI THAM HORA DIGIT FIVE
                (#x01A86 . 6) ; TAI THAM HORA DIGIT SIX
                (#x01A87 . 7) ; TAI THAM HORA DIGIT SEVEN
                (#x01A88 . 8) ; TAI THAM HORA DIGIT EIGHT
                (#x01A89 . 9) ; TAI THAM HORA DIGIT NINE
                (#x01A90 . 0) ; TAI THAM THAM DIGIT ZERO
                (#x01A91 . 1) ; TAI THAM THAM DIGIT ONE
                (#x01A92 . 2) ; TAI THAM THAM DIGIT TWO
                (#x01A93 . 3) ; TAI THAM THAM DIGIT THREE
                (#x01A94 . 4) ; TAI THAM THAM DIGIT FOUR
                (#x01A95 . 5) ; TAI THAM THAM DIGIT FIVE
                (#x01A96 . 6) ; TAI THAM THAM DIGIT SIX
                (#x01A97 . 7) ; TAI THAM THAM DIGIT SEVEN
                (#x01A98 . 8) ; TAI THAM THAM DIGIT EIGHT
                (#x01A99 . 9) ; TAI THAM THAM DIGIT NINE
                (#x01B50 . 0) ; BALINESE DIGIT ZERO
                (#x01B51 . 1) ; BALINESE DIGIT ONE
                (#x01B52 . 2) ; BALINESE DIGIT TWO
                (#x01B53 . 3) ; BALINESE DIGIT THREE
                (#x01B54 . 4) ; BALINESE DIGIT FOUR
                (#x01B55 . 5) ; BALINESE DIGIT FIVE
                (#x01B56 . 6) ; BALINESE DIGIT SIX
                (#x01B57 . 7) ; BALINESE DIGIT SEVEN
                (#x01B58 . 8) ; BALINESE DIGIT EIGHT
                (#x01B59 . 9) ; BALINESE DIGIT NINE
                (#x01BB0 . 0) ; SUNDANESE DIGIT ZERO
                (#x01BB1 . 1) ; SUNDANESE DIGIT ONE
                (#x01BB2 . 2) ; SUNDANESE DIGIT TWO
                (#x01BB3 . 3) ; SUNDANESE DIGIT THREE
                (#x01BB4 . 4) ; SUNDANESE DIGIT FOUR
                (#x01BB5 . 5) ; SUNDANESE DIGIT FIVE
                (#x01BB6 . 6) ; SUNDANESE DIGIT SIX
                (#x01BB7 . 7) ; SUNDANESE DIGIT SEVEN
                (#x01BB8 . 8) ; SUNDANESE DIGIT EIGHT
                (#x01BB9 . 9) ; SUNDANESE DIGIT NINE
                (#x01C40 . 0) ; LEPCHA DIGIT ZERO
                (#x01C41 . 1) ; LEPCHA DIGIT ONE
                (#x01C42 . 2) ; LEPCHA DIGIT TWO
                (#x01C43 . 3) ; LEPCHA DIGIT THREE
                (#x01C44 . 4) ; LEPCHA DIGIT FOUR
                (#x01C45 . 5) ; LEPCHA DIGIT FIVE
                (#x01C46 . 6) ; LEPCHA DIGIT SIX
                (#x01C47 . 7) ; LEPCHA DIGIT SEVEN
                (#x01C48 . 8) ; LEPCHA DIGIT EIGHT
                (#x01C49 . 9) ; LEPCHA DIGIT NINE
                (#x01C50 . 0) ; OL CHIKI DIGIT ZERO
                (#x01C51 . 1) ; OL CHIKI DIGIT ONE
                (#x01C52 . 2) ; OL CHIKI DIGIT TWO
                (#x01C53 . 3) ; OL CHIKI DIGIT THREE
                (#x01C54 . 4) ; OL CHIKI DIGIT FOUR
                (#x01C55 . 5) ; OL CHIKI DIGIT FIVE
                (#x01C56 . 6) ; OL CHIKI DIGIT SIX
                (#x01C57 . 7) ; OL CHIKI DIGIT SEVEN
                (#x01C58 . 8) ; OL CHIKI DIGIT EIGHT
                (#x01C59 . 9) ; OL CHIKI DIGIT NINE
                (#x0A620 . 0) ; VAI DIGIT ZERO
                (#x0A621 . 1) ; VAI DIGIT ONE
                (#x0A622 . 2) ; VAI DIGIT TWO
                (#x0A623 . 3) ; VAI DIGIT THREE
                (#x0A624 . 4) ; VAI DIGIT FOUR
                (#x0A625 . 5) ; VAI DIGIT FIVE
                (#x0A626 . 6) ; VAI DIGIT SIX
                (#x0A627 . 7) ; VAI DIGIT SEVEN
                (#x0A628 . 8) ; VAI DIGIT EIGHT
                (#x0A629 . 9) ; VAI DIGIT NINE
                (#x0A8D0 . 0) ; SAURASHTRA DIGIT ZERO
                (#x0A8D1 . 1) ; SAURASHTRA DIGIT ONE
                (#x0A8D2 . 2) ; SAURASHTRA DIGIT TWO
                (#x0A8D3 . 3) ; SAURASHTRA DIGIT THREE
                (#x0A8D4 . 4) ; SAURASHTRA DIGIT FOUR
                (#x0A8D5 . 5) ; SAURASHTRA DIGIT FIVE
                (#x0A8D6 . 6) ; SAURASHTRA DIGIT SIX
                (#x0A8D7 . 7) ; SAURASHTRA DIGIT SEVEN
                (#x0A8D8 . 8) ; SAURASHTRA DIGIT EIGHT
                (#x0A8D9 . 9) ; SAURASHTRA DIGIT NINE
                (#x0A900 . 0) ; KAYAH LI DIGIT ZERO
                (#x0A901 . 1) ; KAYAH LI DIGIT ONE
                (#x0A902 . 2) ; KAYAH LI DIGIT TWO
                (#x0A903 . 3) ; KAYAH LI DIGIT THREE
                (#x0A904 . 4) ; KAYAH LI DIGIT FOUR
                (#x0A905 . 5) ; KAYAH LI DIGIT FIVE
                (#x0A906 . 6) ; KAYAH LI DIGIT SIX
                (#x0A907 . 7) ; KAYAH LI DIGIT SEVEN
                (#x0A908 . 8) ; KAYAH LI DIGIT EIGHT
                (#x0A909 . 9) ; KAYAH LI DIGIT NINE
                (#x0A9D0 . 0) ; JAVANESE DIGIT ZERO
                (#x0A9D1 . 1) ; JAVANESE DIGIT ONE
                (#x0A9D2 . 2) ; JAVANESE DIGIT TWO
                (#x0A9D3 . 3) ; JAVANESE DIGIT THREE
                (#x0A9D4 . 4) ; JAVANESE DIGIT FOUR
                (#x0A9D5 . 5) ; JAVANESE DIGIT FIVE
                (#x0A9D6 . 6) ; JAVANESE DIGIT SIX
                (#x0A9D7 . 7) ; JAVANESE DIGIT SEVEN
                (#x0A9D8 . 8) ; JAVANESE DIGIT EIGHT
                (#x0A9D9 . 9) ; JAVANESE DIGIT NINE
                (#x0AA50 . 0) ; CHAM DIGIT ZERO
                (#x0AA51 . 1) ; CHAM DIGIT ONE
                (#x0AA52 . 2) ; CHAM DIGIT TWO
                (#x0AA53 . 3) ; CHAM DIGIT THREE
                (#x0AA54 . 4) ; CHAM DIGIT FOUR
                (#x0AA55 . 5) ; CHAM DIGIT FIVE
                (#x0AA56 . 6) ; CHAM DIGIT SIX
                (#x0AA57 . 7) ; CHAM DIGIT SEVEN
                (#x0AA58 . 8) ; CHAM DIGIT EIGHT
                (#x0AA59 . 9) ; CHAM DIGIT NINE
                (#x0ABF0 . 0) ; MEETEI MAYEK DIGIT ZERO
                (#x0ABF1 . 1) ; MEETEI MAYEK DIGIT ONE
                (#x0ABF2 . 2) ; MEETEI MAYEK DIGIT TWO
                (#x0ABF3 . 3) ; MEETEI MAYEK DIGIT THREE
                (#x0ABF4 . 4) ; MEETEI MAYEK DIGIT FOUR
                (#x0ABF5 . 5) ; MEETEI MAYEK DIGIT FIVE
                (#x0ABF6 . 6) ; MEETEI MAYEK DIGIT SIX
                (#x0ABF7 . 7) ; MEETEI MAYEK DIGIT SEVEN
                (#x0ABF8 . 8) ; MEETEI MAYEK DIGIT EIGHT
                (#x0ABF9 . 9) ; MEETEI MAYEK DIGIT NINE
                (#x0FF10 . 0) ; FULLWIDTH DIGIT ZERO
                (#x0FF11 . 1) ; FULLWIDTH DIGIT ONE
                (#x0FF12 . 2) ; FULLWIDTH DIGIT TWO
                (#x0FF13 . 3) ; FULLWIDTH DIGIT THREE
                (#x0FF14 . 4) ; FULLWIDTH DIGIT FOUR
                (#x0FF15 . 5) ; FULLWIDTH DIGIT FIVE
                (#x0FF16 . 6) ; FULLWIDTH DIGIT SIX
                (#x0FF17 . 7) ; FULLWIDTH DIGIT SEVEN
                (#x0FF18 . 8) ; FULLWIDTH DIGIT EIGHT
                (#x0FF19 . 9) ; FULLWIDTH DIGIT NINE
                (#x104A0 . 0) ; OSMANYA DIGIT ZERO
                (#x104A1 . 1) ; OSMANYA DIGIT ONE
                (#x104A2 . 2) ; OSMANYA DIGIT TWO
                (#x104A3 . 3) ; OSMANYA DIGIT THREE
                (#x104A4 . 4) ; OSMANYA DIGIT FOUR
                (#x104A5 . 5) ; OSMANYA DIGIT FIVE
                (#x104A6 . 6) ; OSMANYA DIGIT SIX
                (#x104A7 . 7) ; OSMANYA DIGIT SEVEN
                (#x104A8 . 8) ; OSMANYA DIGIT EIGHT
                (#x104A9 . 9) ; OSMANYA DIGIT NINE
                (#x11066 . 0) ; BRAHMI DIGIT ZERO
                (#x11067 . 1) ; BRAHMI DIGIT ONE
                (#x11068 . 2) ; BRAHMI DIGIT TWO
                (#x11069 . 3) ; BRAHMI DIGIT THREE
                (#x1106A . 4) ; BRAHMI DIGIT FOUR
                (#x1106B . 5) ; BRAHMI DIGIT FIVE
                (#x1106C . 6) ; BRAHMI DIGIT SIX
                (#x1106D . 7) ; BRAHMI DIGIT SEVEN
                (#x1106E . 8) ; BRAHMI DIGIT EIGHT
                (#x1106F . 9) ; BRAHMI DIGIT NINE
                (#x110F0 . 0) ; SORA SOMPENG DIGIT ZERO
                (#x110F1 . 1) ; SORA SOMPENG DIGIT ONE
                (#x110F2 . 2) ; SORA SOMPENG DIGIT TWO
                (#x110F3 . 3) ; SORA SOMPENG DIGIT THREE
                (#x110F4 . 4) ; SORA SOMPENG DIGIT FOUR
                (#x110F5 . 5) ; SORA SOMPENG DIGIT FIVE
                (#x110F6 . 6) ; SORA SOMPENG DIGIT SIX
                (#x110F7 . 7) ; SORA SOMPENG DIGIT SEVEN
                (#x110F8 . 8) ; SORA SOMPENG DIGIT EIGHT
                (#x110F9 . 9) ; SORA SOMPENG DIGIT NINE
                (#x11136 . 0) ; CHAKMA DIGIT ZERO
                (#x11137 . 1) ; CHAKMA DIGIT ONE
                (#x11138 . 2) ; CHAKMA DIGIT TWO
                (#x11139 . 3) ; CHAKMA DIGIT THREE
                (#x1113A . 4) ; CHAKMA DIGIT FOUR
                (#x1113B . 5) ; CHAKMA DIGIT FIVE
                (#x1113C . 6) ; CHAKMA DIGIT SIX
                (#x1113D . 7) ; CHAKMA DIGIT SEVEN
                (#x1113E . 8) ; CHAKMA DIGIT EIGHT
                (#x1113F . 9) ; CHAKMA DIGIT NINE
                (#x111D0 . 0) ; SHARADA DIGIT ZERO
                (#x111D1 . 1) ; SHARADA DIGIT ONE
                (#x111D2 . 2) ; SHARADA DIGIT TWO
                (#x111D3 . 3) ; SHARADA DIGIT THREE
                (#x111D4 . 4) ; SHARADA DIGIT FOUR
                (#x111D5 . 5) ; SHARADA DIGIT FIVE
                (#x111D6 . 6) ; SHARADA DIGIT SIX
                (#x111D7 . 7) ; SHARADA DIGIT SEVEN
                (#x111D8 . 8) ; SHARADA DIGIT EIGHT
                (#x111D9 . 9) ; SHARADA DIGIT NINE
                (#x116C0 . 0) ; TAKRI DIGIT ZERO
                (#x116C1 . 1) ; TAKRI DIGIT ONE
                (#x116C2 . 2) ; TAKRI DIGIT TWO
                (#x116C3 . 3) ; TAKRI DIGIT THREE
                (#x116C4 . 4) ; TAKRI DIGIT FOUR
                (#x116C5 . 5) ; TAKRI DIGIT FIVE
                (#x116C6 . 6) ; TAKRI DIGIT SIX
                (#x116C7 . 7) ; TAKRI DIGIT SEVEN
                (#x116C8 . 8) ; TAKRI DIGIT EIGHT
                (#x116C9 . 9) ; TAKRI DIGIT NINE
                (#x1D7CE . 0) ; MATHEMATICAL BOLD DIGIT ZERO
                (#x1D7CF . 1) ; MATHEMATICAL BOLD DIGIT ONE
                (#x1D7D0 . 2) ; MATHEMATICAL BOLD DIGIT TWO
                (#x1D7D1 . 3) ; MATHEMATICAL BOLD DIGIT THREE
                (#x1D7D2 . 4) ; MATHEMATICAL BOLD DIGIT FOUR
                (#x1D7D3 . 5) ; MATHEMATICAL BOLD DIGIT FIVE
                (#x1D7D4 . 6) ; MATHEMATICAL BOLD DIGIT SIX
                (#x1D7D5 . 7) ; MATHEMATICAL BOLD DIGIT SEVEN
                (#x1D7D6 . 8) ; MATHEMATICAL BOLD DIGIT EIGHT
                (#x1D7D7 . 9) ; MATHEMATICAL BOLD DIGIT NINE
                (#x1D7D8 . 0) ; MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
                (#x1D7D9 . 1) ; MATHEMATICAL DOUBLE-STRUCK DIGIT ONE
                (#x1D7DA . 2) ; MATHEMATICAL DOUBLE-STRUCK DIGIT TWO
                (#x1D7DB . 3) ; MATHEMATICAL DOUBLE-STRUCK DIGIT THREE
                (#x1D7DC . 4) ; MATHEMATICAL DOUBLE-STRUCK DIGIT FOUR
                (#x1D7DD . 5) ; MATHEMATICAL DOUBLE-STRUCK DIGIT FIVE
                (#x1D7DE . 6) ; MATHEMATICAL DOUBLE-STRUCK DIGIT SIX
                (#x1D7DF . 7) ; MATHEMATICAL DOUBLE-STRUCK DIGIT SEVEN
                (#x1D7E0 . 8) ; MATHEMATICAL DOUBLE-STRUCK DIGIT EIGHT
                (#x1D7E1 . 9) ; MATHEMATICAL DOUBLE-STRUCK DIGIT NINE
                (#x1D7E2 . 0) ; MATHEMATICAL SANS-SERIF DIGIT ZERO
                (#x1D7E3 . 1) ; MATHEMATICAL SANS-SERIF DIGIT ONE
                (#x1D7E4 . 2) ; MATHEMATICAL SANS-SERIF DIGIT TWO
                (#x1D7E5 . 3) ; MATHEMATICAL SANS-SERIF DIGIT THREE
                (#x1D7E6 . 4) ; MATHEMATICAL SANS-SERIF DIGIT FOUR
                (#x1D7E7 . 5) ; MATHEMATICAL SANS-SERIF DIGIT FIVE
                (#x1D7E8 . 6) ; MATHEMATICAL SANS-SERIF DIGIT SIX
                (#x1D7E9 . 7) ; MATHEMATICAL SANS-SERIF DIGIT SEVEN
                (#x1D7EA . 8) ; MATHEMATICAL SANS-SERIF DIGIT EIGHT
                (#x1D7EB . 9) ; MATHEMATICAL SANS-SERIF DIGIT NINE
                (#x1D7EC . 0) ; MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO
                (#x1D7ED . 1) ; MATHEMATICAL SANS-SERIF BOLD DIGIT ONE
                (#x1D7EE . 2) ; MATHEMATICAL SANS-SERIF BOLD DIGIT TWO
                (#x1D7EF . 3) ; MATHEMATICAL SANS-SERIF BOLD DIGIT THREE
                (#x1D7F0 . 4) ; MATHEMATICAL SANS-SERIF BOLD DIGIT FOUR
                (#x1D7F1 . 5) ; MATHEMATICAL SANS-SERIF BOLD DIGIT FIVE
                (#x1D7F2 . 6) ; MATHEMATICAL SANS-SERIF BOLD DIGIT SIX
                (#x1D7F3 . 7) ; MATHEMATICAL SANS-SERIF BOLD DIGIT SEVEN
                (#x1D7F4 . 8) ; MATHEMATICAL SANS-SERIF BOLD DIGIT EIGHT
                (#x1D7F5 . 9) ; MATHEMATICAL SANS-SERIF BOLD DIGIT NINE
                (#x1D7F6 . 0) ; MATHEMATICAL MONOSPACE DIGIT ZERO
                (#x1D7F7 . 1) ; MATHEMATICAL MONOSPACE DIGIT ONE
                (#x1D7F8 . 2) ; MATHEMATICAL MONOSPACE DIGIT TWO
                (#x1D7F9 . 3) ; MATHEMATICAL MONOSPACE DIGIT THREE
                (#x1D7FA . 4) ; MATHEMATICAL MONOSPACE DIGIT FOUR
                (#x1D7FB . 5) ; MATHEMATICAL MONOSPACE DIGIT FIVE
                (#x1D7FC . 6) ; MATHEMATICAL MONOSPACE DIGIT SIX
                (#x1D7FD . 7) ; MATHEMATICAL MONOSPACE DIGIT SEVEN
                (#x1D7FE . 8) ; MATHEMATICAL MONOSPACE DIGIT EIGHT
                (#x1D7FF . 9))); MATHEMATICAL MONOSPACE DIGIT NINE
             (num-digits (vector-length digit-table)))
        (lambda (c)
          (let ((ci (char->integer c)))
            (let search ((lo 0) (hi num-digits))
              (and (< lo hi)
                   (let* ((i (quotient (+ lo hi) 2))
                          (entry (vector-ref digit-table i))
                          (delta (- ci (car entry))))
                     (cond ((positive? delta)
                            (search (+ i 1) hi))
                           ((negative? delta)
                            (search lo i))
                           (else (cdr entry))))))))))))
