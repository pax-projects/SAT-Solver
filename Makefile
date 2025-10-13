# ===============================
# Couleurs terminal (ANSI)
# ===============================
RESET  = \033[0m
BOLD   = \033[1m
RED    = \033[31m
GREEN  = \033[32m
YELLOW = \033[33m
BLUE   = \033[34m
MAGENTA= \033[35m
CYAN   = \033[36m
WHITE  = \033[37m

# ===============================
# Makefile for dpll_solver project
# ===============================

EXEC=dpll_solver

# Directories
SRC_DIR=.
BIN_DIR=$(SRC_DIR)/bin
LIB_DIR=$(SRC_DIR)/lib
TEST_DIR=$(SRC_DIR)/test

# ===============================
# Main targets
# ===============================

all:
	@$(call run_cmd,dune build)

# Execute DPLL
run:
	@$(call run_cmd,dune exec $(BIN_DIR)/main.exe)

# Execute solver on .cnf (ex : make solve FILE=examples/SAT/sudoku-4x4.cnf)
solve:
	@$(call run_cmd,dune exec dpll_solver -- $(FILE))

# Run tests
test:
	@$(call run_cmd,dune runtest)

# Clean
clean:
	dune clean

# Force clean
fclean: clean
	rm -rf _build

# Clean and compile
re: fclean all

# Syntax check
check:
	@$(call run_cmd,dune build @check)

# ===============================
# Help
# ===============================
help:
	@echo "Commandes disponibles :"
	@echo "  make all        - Compile le projet"
	@echo "  make run        - Exécute le solveur DPLL"
	@echo "  make solve FILE=<file_name.cnf> - Lance le solveur sur un fichier CNF"
	@echo "  make test       - Lance les tests unitaires"
	@echo "  make clean      - Supprime les fichiers de compilation"
	@echo "  make fclean     - Supprime _build et autres artefacts"
	@echo "  make re         - Recompile depuis zéro"
	@echo "  make fmt        - Formate le code avec dune fmt"
	@echo "  make check      - Vérifie la syntaxe et la compilation"

# ===============================
# Help
# ===============================
define run_cmd
	@echo "│ => $(WHITE)Executing: $(YELLOW)$(1)$(RESET)"

	@$(1)

	@echo "│ => $(GREEN)Operation successful$(RESET)"
endef

.PHONY: all run solve test clean fclean re check help