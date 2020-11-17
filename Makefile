OBJNAME :=       gbc
PROJNAME :=      good-BASIC
TARGET_FRMWRK := netcoreapp3.1
RUNTIME :=       linux-x64
SRC :=           $(wildcard src/*.cs) $(wildcard src/*/*.cs)

$(OBJNAME) : $(SRCFILES)
	dotnet publish $(PROJNAME).csproj -f $(TARGET_FRMWRK) \
	    -p:PublishSingleFile=true -r $(RUNTIME)
	cp bin/Debug/$(TARGET_FRMWRK)/$(RUNTIME)/publish/$(PROJNAME) ./$(OBJNAME)
	chmod +x $(OBJNAME)

.PHONY : clean
clean :
	rm -rf bin
	rm -rf obj
	rm -rf $(OBJNAME)
	rm -rf /var/tmp/.net
