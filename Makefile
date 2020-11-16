# Build Options
OBJNAME := gbc
HC :=      ghc
HFLAGS :=  
SRCFLDR := src
SRC :=     $(wildcard $(SRCFLDR)/*.hs)

# Helper Targets
.PHONY : all
all : $(OBJNAME)

.PHONY : clean
clean :
	rm -rf src/*.o
	rm -rf src/*.hi
	rm -rf $(OBJNAME)

# Main targets
$(OBJNAME) : $(OBJS)
	$(HC) -o $@ $(SRC)
