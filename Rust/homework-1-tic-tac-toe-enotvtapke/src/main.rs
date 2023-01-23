use std::fmt;
use std::io::BufRead;
use std::iter::{Rev, Skip, StepBy};
use std::slice::Iter;

#[derive(Clone, PartialEq, Copy)]
enum Mark {
    X,
    O,
    Empty,
}

impl fmt::Display for Mark {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mark::X => write!(f, "X"),
            Mark::O => write!(f, "O"),
            Mark::Empty => write!(f, " "),
        }
    }
}

struct Board {
    size: usize,
    board_impl: Vec<Mark>,
}

#[derive(PartialEq)]
enum State {
    XWon,
    OWon,
    Draw,
    Null,
}

impl State {
    fn is_terminal(&self) -> bool {
        *self != State::Null
    }
}

impl Board {
    fn new(size: usize) -> Self {
        Board {
            size,
            board_impl: vec![Mark::Empty; size * size],
        }
    }

    fn set_mark(&mut self, row: usize, col: usize, mark: Mark) {
        self.board_impl[row * self.size + col] = mark
    }

    fn is_null(&self, row: usize, col: usize) -> bool {
        self.board_impl[row * self.size + col] == Mark::Empty
    }

    fn get_row(&self, row: usize) -> Iter<'_, Mark> {
        self.board_impl[row * self.size..(row + 1) * self.size].iter()
    }

    fn get_col(&self, col: usize) -> StepBy<Skip<Iter<'_, Mark>>> {
        self.board_impl.iter().skip(col).step_by(self.size)
    }

    fn get_primary_diagonal(&self) -> StepBy<Iter<'_, Mark>> {
        self.board_impl.iter().step_by(self.size + 1)
    }

    fn get_secondary_diagonal(&self) -> Skip<Rev<StepBy<Skip<Iter<'_, Mark>>>>> {
        self.board_impl
            .iter()
            .skip(self.size - 1)
            .step_by(self.size - 1)
            .rev()
            .skip(1)
    }

    fn hash(&self) -> usize {
        let mut init = 0_usize;
        for i in 0..self.board_impl.len() {
            init += self.board_impl[i] as usize * 3_i32.pow(i.try_into().unwrap()) as usize;
        }
        init
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut result = String::new();
        result += "\n+---+---+---+\n";
        for i in 0..self.board_impl.len() {
            if i != 0 && i % self.size == 0 {
                result += "|\n";
            }
            result += "| ";
            result.push_str(&self.board_impl[i].to_string());
            result += " ";
        }
        result += "|\n";
        result += "+---+---+---+";
        write!(f, "{}", result)
    }
}

fn parse_pos(s: &str) -> Option<(usize, usize)> {
    let mut parts = s.split(' ');
    let row = parts.next()?.trim().parse().ok()?;
    let col = parts.next()?.trim().parse().ok()?;
    Some((row, col))
}

struct HumanPlayer;

impl HumanPlayer {
    //noinspection ALL
    fn get_move(board: &Board) -> (usize, usize) {
        let mut stdin = std::io::stdin().lock();
        let mut line = String::new();
        let mut row: usize;
        let mut col: usize;
        loop {
            println!("Enter your turn: ");
            line.clear();
            stdin.read_line(&mut line).unwrap();
            let parsed = parse_pos(&*line);
            if parsed.is_none() {
                println!("Invalid input. Please, try again.");
                continue;
            }
            (row, col) = parsed.unwrap();
            if row < board.size && col < board.size && board.is_null(row, col) {
                break;
            } else {
                println!("Invalid input. Please, try again.");
            }
        }
        (row, col)
    }
}

struct AIPlayer;

impl AIPlayer {
    fn get_move(board: &Board, hash: &[(usize, usize)]) -> (usize, usize) {
        println!("AI move: {:?}", hash[board.hash()]);
        hash[board.hash()]
    }

    fn retract_move(game: &mut TicTacToe, pos: (usize, usize)) {
        if game.turn == 0 {
            panic!("Can not retract first move")
        }
        if game.board.is_null(pos.0, pos.1) {
            panic!("Can not retract move. Position {:?} is not occupied", pos)
        }
        game.board.set_mark(pos.0, pos.1, Mark::Empty);
        game.state = game.get_new_state(pos.0, pos.1);
        game.turn -= 1;
    }

    fn minimax(game: &mut TicTacToe, depth: u32, hash: &mut Vec<(usize, usize)>) -> i32 {
        if game.state.is_terminal() {
            return match game.state {
                State::Draw => 0,
                State::XWon => 1,
                State::OWon => -1,
                _ => panic!("Can not evaluate terminal state"),
            };
        } else if depth == 0 {
            println!("Wrong");
            return 0;
        }

        let mut pos: (usize, usize) = (usize::MAX, usize::MAX);

        let op: fn(i32, i32) -> bool;
        let mut cur;
        if game.get_current_mark() == Mark::X {
            op = |x: i32, y: i32| x >= y;
            cur = i32::MIN;
        } else {
            op = |x: i32, y: i32| x <= y;
            cur = i32::MAX;
        }

        for i in 0..game.board.size {
            for j in 0..game.board.size {
                if game.board.is_null(i, j) {
                    game.make_move((i, j));
                    let new = AIPlayer::minimax(game, depth - 1, hash);
                    if op(new, cur) {
                        cur = new;
                        pos = (i, j);
                    }
                    AIPlayer::retract_move(game, (i, j))
                }
            }
        }
        hash[game.board.hash()] = pos;
        cur
    }
}

struct TicTacToe {
    board: Board,
    turn: usize,
    state: State,
}

impl TicTacToe {
    fn new() -> Self {
        TicTacToe {
            board: Board::new(3),
            turn: 0,
            state: State::Null,
        }
    }

    fn get_current_mark(&self) -> Mark {
        if self.turn % 2 == 0 {
            Mark::X
        } else {
            Mark::O
        }
    }

    fn get_new_state(&mut self, modified_row: usize, modified_col: usize) -> State {
        if self.turn == self.board.size * self.board.size - 1 {
            return State::Draw;
        }
        let current_mark = self.get_current_mark();

        if self
            .board
            .get_row(modified_row)
            .all(|it| *it == current_mark)
            || self
                .board
                .get_col(modified_col)
                .all(|it| *it == current_mark)
            || self
                .board
                .get_primary_diagonal()
                .all(|it| *it == current_mark)
            || self
                .board
                .get_secondary_diagonal()
                .all(|it| *it == current_mark)
        {
            if current_mark == Mark::X {
                State::XWon
            } else {
                State::OWon
            }
        } else {
            State::Null
        }
    }

    fn make_move(&mut self, pos: (usize, usize)) {
        if !self.board.is_null(pos.0, pos.1) {
            panic!("Can not make move. Position {:?} is already occupied", pos)
        }
        self.board.set_mark(pos.0, pos.1, self.get_current_mark());
        self.state = self.get_new_state(pos.0, pos.1);
        self.turn += 1;
    }

    fn play(&mut self, inverse_turns: bool) {
        let mut v = vec![(usize::MAX, usize::MAX); 10000000];
        AIPlayer::minimax(&mut TicTacToe::new(), 12, &mut v);
        loop {
            println!("{}", self.board);
            if self.get_current_mark() == (if inverse_turns { Mark::X } else { Mark::O }) {
                self.make_move(AIPlayer::get_move(&self.board, &v));
            } else {
                self.make_move(HumanPlayer::get_move(&self.board));
            }
            if self.state != State::Null {
                println!("{}", self.board);
                println!("Game Over");
                break;
            }
        }
    }
}

fn main() {
    let mut game = TicTacToe::new();
    println!("Enter, who will be move first (1 - AI, 0 - You): ");
    let inverse_turns;
    let mut stdin = std::io::stdin().lock();
    let mut line = String::new();
    loop {
        line.clear();
        stdin.read_line(&mut line).unwrap();
        match &*line {
            "1\n" => inverse_turns = true,
            "0\n" => inverse_turns = false,
            _ => {
                println!("Invalid input. Please, try again.");
                continue;
            }
        }
        break;
    }
    drop(stdin);
    game.play(inverse_turns);
}
