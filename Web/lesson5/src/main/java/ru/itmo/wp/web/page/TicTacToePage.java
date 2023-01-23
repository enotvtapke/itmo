package ru.itmo.wp.web.page;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused", "RedundantSuppression"})
public class TicTacToePage {
    private void action(HttpServletRequest request, Map<String, Object> view) {
        State state = new State();
        request.getSession().setAttribute("state", state);
        view.put("state", state);
    }

    private void newGame(HttpServletRequest request, Map<String, Object> view) {
        action(request, view);
    }

    private void onMove(HttpServletRequest request, Map<String, Object> view) {
        int row = -1;
        int col = -1;
        for (Map.Entry<String, String[]> e : request.getParameterMap().entrySet()) {
            if (e.getKey().startsWith("cell_")) {
                row = e.getKey().charAt(5) - '0';
                col = e.getKey().charAt(6) - '0';
            }
        }
        State state = (State) request.getSession().getAttribute("state");
        state.makeMove(row, col);
        view.put("state", state);
    }

    public static class State {
        public enum Phase {
            RUNNING,
            WON_X,
            WON_O,
            DRAW
        }

        enum Cell {
            X, O
        }

        static final int size = 3;
        Cell[][] cells;
        boolean crossesMove;
        Phase phase;

        public State() {
            this.cells = new Cell[size][size];;
            this.crossesMove = true;
            phase = Phase.RUNNING;
        }

        private Cell turn() {
            return crossesMove ? Cell.X : Cell.O;
        }

        private void makeMove(int row, int col) {
            if (row >= size || row < 0 || col >= size || col < 0 ||
                    cells[row][col] != null || phase != Phase.RUNNING) {
                return;
            }
            cells[row][col] = turn();
            int countInRow;
            int countInColumn;
            int countInLeadingDiag = 0;
            int countInSecondaryDiag = 0;
            int freeCells = 0;
            for (int i = 0; i < size; i++) {
                countInRow = 0;
                countInColumn = 0;
                for (int j = 0; j < size; j++) {
                    if (cells[i][j] == turn()) {
                        countInRow++;
                    }
                    if (cells[j][i] == turn()) {
                        countInColumn++;
                    }
                    if (cells[i][j] == null) {
                        freeCells++;
                    }
                }
                if (cells[i][i] == turn()) {
                    countInLeadingDiag++;
                }
                if (cells[i][size - i - 1] == turn()) {
                    countInSecondaryDiag++;
                }
                if (countInColumn == size || countInRow == size || countInLeadingDiag == size ||
                        countInSecondaryDiag == size) {
                    phase = crossesMove ? Phase.WON_X : Phase.WON_O;
                    return;
                }
            }
            if (freeCells == 0) {
                phase = Phase.DRAW;
                return;
            }
            crossesMove = !crossesMove;
        }

        public Phase getPhase() {
            return phase;
        }

        public int getSize() {
            return size;
        }

        public Cell[][] getCells() {
            return cells;
        }

        public boolean isCrossesMove() {
            return crossesMove;
        }
    }

}



