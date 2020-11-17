OBJNAME :=       gbc
PROJNAME :=      good-BASIC
TARGET_FRMWRK := netcoreapp5.0
RUNTIME :=       linux-x64
SRC :=           $(wildcard src/*.cs) $(wildcard src/*/*.cs)

$(OBJNAME) : $(SRCFILES)
	dotnet publish $(PROJNAME).csproj \
	    -f $(TARGET_FRMWRK) -r $(RUNTIME) \
	    -p:PublishSingleFile=true --self-contained false
	cp bin/Debug/$(TARGET_FRMWRK)/$(RUNTIME)/publish/$(PROJNAME) ./$(OBJNAME)
	chmod +x $(OBJNAME)

.PHONY : clean
clean :
	rm -rf bin
	rm -rf obj
	rm -rf $(OBJNAME)
	rm -rf /var/tmp/.net
