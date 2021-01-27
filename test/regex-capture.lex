i
There sooon were  were some repeated repeated words.
.
%
s/  +/ /g
s/([^ ]+) \1/\1/g
s/([aeiouy]){3,}/\1\1/g
w test/regex-capture.lex.out
q
