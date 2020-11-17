# Custom Build Options
OBJNAME :=  gbc
CPPC :=     g++
CPPFLAGS := -O2 -Wall -Werror -std=c++17
LD :=       g++
LDFLAGS :=  
SRCFLDR :=  src
OBJFLDR :=  obj
INCFLDR :=  include

# Helper targets
.PHONY : all
all : $(OBJNAME)

.PHONY : clean
clean :
	rm -rf $(OBJFLDR)
	rm -rf $(OBJNAME)

# Auto generated options
CPPSRC :=   $(wildcard $(SRCFLDR)/*.cpp)
HFILES :=   $(wildcard $(INCFLDR)/*.hpp)
OBJS :=     $(subst .cpp,.cpp.o,$(subst $(SRCFLDR),$(OBJFLDR),$(CPPSRC)))

$(OBJFLDR)/%.cpp.o : $(SRCFLDR)/%.cpp $(HFILES)
	mkdir -p $(OBJFLDR)
	$(CPPC) $(CPPFLAGS) -I$(INCFLDR) -o $@ -c $<

# Main targets
$(OBJNAME) : $(OBJS)
	$(LD) -o $(OBJNAME) $(OBJS) $(LDFLAGS)
