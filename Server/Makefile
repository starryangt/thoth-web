OBJS = Monads.fs Download.fs Parse.fs Epub.fs Process.fs Main.fs
CC = fsharpc
LINKER = -r Suave.dll -r NSoup.dll -r Newtonsoft.Json.dll -r System.IO.Compression.FileSystem -r System.IO.Compression.dll
OBJ_NAME = Server.exe

all:
	$(CC) $(LINKER) $(OBJS) -o $(OBJ_NAME)
run:
	./$(OBJ_NAME)
stand:
	$(CC) --standalone $(LINKER) $(OBJS) -o $(OBJ_NAME)
