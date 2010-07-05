TARGET = b18n-combined

all : $(TARGET)

$(TARGET): Main.hs AST.hs Parser.hs Util.hs Type.hs Shapify.hs CodeGen.hs
	ghc --make -o $(TARGET) Main.hs

clean:
	rm $(TARGET)
	rm -rf *.o *.hi
	rm *~