.PHNOY: all clean empty run

flexSrc := lex.l
bisonSrc := parse.y
tabSrc := parse.tab.c
tabHeader := parse.tab.h
otherSrc := main.c error.cpp type.cpp gen_x86.cpp semantic.cpp regalloc.cpp gen_ir.cpp optimise.cpp
main := main

testdir := ./test_contest
testFile := 000 001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 019 020 021 022 023 024 025 026 027 028 029 030 031 032 033 034 035 036 037 038 039 040 041 042 043 044 045 046 047 048 049 050 051 052 053 054 055 056 057 058 059 060 061 062 063 064 065 066 067 068 069 070 071 072 073 074 075 076 077 078 079 080
testFile := $(addprefix $(testdir)/,$(testFile) )
testSrc := $(addsuffix .sy,$(testFile))
testAns := $(addsuffix .out,$(testFile))

$(main):$(tabSrc) lex.yy.c $(otherSrc)
	g++ -g -O0 -o $(main) lex.yy.c $(tabSrc) $(otherSrc) -lfl -std=c++11


clean:
	rm $(main) $(tabSrc) $(tabHeader) *~ lex.yy.c main


run:
	$(foreach n,$(testFile),\
	       echo $(n);\
	       ./$(main) $(addsuffix .sy,$(n)) > $(addsuffix .S,$(n));\
	       gcc -o $(n) $(addsuffix .S,$(n)) lib.c;\
	       $(n) > $(addsuffix .log,$(n));\
               diff $(addsuffix .log,$(n)) $(addsuffix .out,$(n));\
		   rm $(n)  \
	)
	

lex.yy.c:$(flexSrc)
	flex $(flexSrc)

$(tabSrc):$(bisonSrc)
	bison -d $(bisonSrc)

	
#    rm $(n) $(addsuffix .log,$(n)) $(addsuffix .S,$(n));\


