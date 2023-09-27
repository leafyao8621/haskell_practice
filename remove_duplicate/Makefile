HC = ghc
SRC = remove_duplicate.hs
BIN = main

$(BIN): $(SRC)
	$(HC) -o $(BIN) $(SRC)

.PHONY: run clean
run: $(BIN)
	@./$(BIN)
clean:
	@rm *.hi
	@rm *.o
	@rm $(BIN)
