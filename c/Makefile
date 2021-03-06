# Toy-bignum package, developed just for the fun of it
# 
# Copyright (c) 2021, Paulo Custodio
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

ifeq ($(OS),Windows_NT)
  EXE 	:= .exe
else
  EXE 	?=
endif

CC 	?= gcc
CFLAGS 	+= -Wall
ASTYLE	= astyle \
	   --indent=spaces \
	   --style=attach  \
	   --pad-oper \
	   --align-pointer=type \
	   --keep-one-line-blocks \
	   --keep-one-line-statements \
	   --break-closing-braces \
	   --remove-braces \
	   --attach-return-type \
	   --max-code-length=120 \
	   --lineend=linux \
	   --suffix=~ \
	   --formatted

SRCS	= $(wildcard *.c)
HDRS	= $(wildcard *.h)

all: bignum$(EXE) leibnitz-pi$(EXE)

test: bignum$(EXE)
	./bignum$(EXE)

bignum$(EXE): $(SRCS) $(HDRS)
	$(CC) $(CFLAGS) -DTEST -o $@ $(SRCS) $(LDFLAGS)

leibnitz-pi$(EXE): $(SRCS) $(HDRS)
	$(CC) $(CFLAGS) -o $@ $(SRCS) $(LDFLAGS)

astyle:
	astyle $(SRCS) bignum.h

clean:
	$(RM) bignum$(EXE) leibnitz-pi$(EXE) $(OBJS) *.bak *~ *.orig
