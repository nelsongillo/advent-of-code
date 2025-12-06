SOURCE := main.ml
TARGET := main

all: data

build:
	ocamlc $(SOURCE) -o $(TARGET)

example: build
	./$(TARGET) example.txt

data: build
	./$(TARGET) data.txt

clean:
	rm *.cmi *.cmo $(TARGET)