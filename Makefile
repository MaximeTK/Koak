##
## EPITECH PROJECT, 2021
## makefile
## File description:
## makefile
##

BINARY_PATH	:=	$(shell stack path --local-install-root)

UNITTEST_PATH := $(shell stack path --project-root)

NAME	=	koak

all:	$(NAME)

$(NAME): fclean
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)
	rm -rf *.cabal
	rm -rf *.lock
	rm -rf *~

launch:
	llvm-as main.ll -o test.bc
	llc -filetype=obj test.bc -o test.o
	clang test.o -o a.out
	rm -rf *.bc
	rm -rf *.o

tf:
	clang -emit-llvm -S main.c

clean:
	stack clean

fclean:	clean
	rm -f $(NAME)

re:	fclean all

.PHONY: all test clean fclean re

.SILENT:
