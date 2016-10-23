PROJECT = erl_vector

ERLC_OPTS = -W0

CC=gcc
CFLAGS += -g -std=c11 -O2 -march=native

#SHELL_OPTS = -s ${PROJECT}_app

include erlang.mk
