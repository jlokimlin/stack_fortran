
include ../make.inc

LINKS = \
-I$(OBJ_DIRECTORY) \
-I$(LIB_DIRECTORY)

EXECUTABLE = ./build.exe

type_Node.o: \
type_Node.f90
	$(FC) $(FFLAGS) -c type_Node.f90 $(LINKS) \
	 -o $(OBJ_DIRECTORY)/$@ -J$(LIB_DIRECTORY)

type_Stack.o: \
type_Node.o\
type_Stack.f90
	$(FC) $(FFLAGS) -c type_Stack.f90 $(LINKS) \
	 -o $(OBJ_DIRECTORY)/$@ -J$(LIB_DIRECTORY)

main.o: \
type_Stack.o \
main.f90
	$(FC) $(FFLAGS) -c main.f90 $(LINKS) \
	-o $(OBJ_DIRECTORY)/$@

exec: $(OBJ)
	$(FC) $(FFLAGS) -o $(EXECUTABLE) $(OBJS) $(LINKS)

run:
	$(MAKE) clean
	$(MAKE) exec; $(EXECUTABLE)

clean:
	rm -f $(OBJS) $(EXECUTABLE) $(LIB_DIRECTORY)/*.mod