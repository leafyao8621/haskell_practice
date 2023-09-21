HC = ghc
SRC = merge_sort.hs
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
