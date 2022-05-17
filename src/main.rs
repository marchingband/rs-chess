#![warn(clippy::all)]
use crossterm::{
    cursor::{MoveTo, MoveToNextLine},
    event::{read, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode},
    terminal::{Clear, ClearType},
};
use rand::prelude::*;
use rayon::prelude::*;
use std::cmp::Ordering;
use std::fmt;
use std::io;
use std::io::stdout;
use std::sync::{Arc, Mutex};
use Color::*;
use Kind::*;

static DUMMY_TURN: Turn = Turn {
    to: (0, 0),
    from: (0, 0),
};

static NEW_BOARD: Game = Game {
    cur: (1, 1),
    brd: [
        [
            Some(Piece {
                kind: Rook,
                color: White,
            }),
            Some(Piece {
                kind: Knight,
                color: White,
            }),
            Some(Piece {
                kind: Bishop,
                color: White,
            }),
            Some(Piece {
                kind: King,
                color: White,
            }),
            Some(Piece {
                kind: Queen,
                color: White,
            }),
            Some(Piece {
                kind: Bishop,
                color: White,
            }),
            Some(Piece {
                kind: Knight,
                color: White,
            }),
            Some(Piece {
                kind: Rook,
                color: White,
            }),
        ],
        [
            Some(Piece {
                kind: Pawn,
                color: White,
            }),
            Some(Piece {
                kind: Pawn,
                color: White,
            }),
            Some(Piece {
                kind: Pawn,
                color: White,
            }),
            Some(Piece {
                kind: Pawn,
                color: White,
            }),
            Some(Piece {
                kind: Pawn,
                color: White,
            }),
            Some(Piece {
                kind: Pawn,
                color: White,
            }),
            Some(Piece {
                kind: Pawn,
                color: White,
            }),
            Some(Piece {
                kind: Pawn,
                color: White,
            }),
        ],
        [None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None],
        [
            Some(Piece {
                kind: Pawn,
                color: Black,
            }),
            Some(Piece {
                kind: Pawn,
                color: Black,
            }),
            Some(Piece {
                kind: Pawn,
                color: Black,
            }),
            Some(Piece {
                kind: Pawn,
                color: Black,
            }),
            Some(Piece {
                kind: Pawn,
                color: Black,
            }),
            Some(Piece {
                kind: Pawn,
                color: Black,
            }),
            Some(Piece {
                kind: Pawn,
                color: Black,
            }),
            Some(Piece {
                kind: Pawn,
                color: Black,
            }),
        ],
        [
            Some(Piece {
                kind: Rook,
                color: Black,
            }),
            Some(Piece {
                kind: Knight,
                color: Black,
            }),
            Some(Piece {
                kind: Bishop,
                color: Black,
            }),
            Some(Piece {
                kind: King,
                color: Black,
            }),
            Some(Piece {
                kind: Queen,
                color: Black,
            }),
            Some(Piece {
                kind: Bishop,
                color: Black,
            }),
            Some(Piece {
                kind: Knight,
                color: Black,
            }),
            Some(Piece {
                kind: Rook,
                color: Black,
            }),
        ],
    ],
    moving: false,
    from: (0, 0),
    mover: Piece {
        kind: Rook,
        color: White,
    },
    turn: White,
    captured_by_black: vec![],
    captured_by_white: vec![],
    white_can_castle_left: true,
    white_can_castle_right: true,
    black_can_castle_left: true,
    black_can_castle_right: true,
};

static PIECE_VALUES: [u8; 6] = [
    10,  // Rook,
    6,   // Knight,
    6,   // Bishop,
    255, // King,
    18,  // Queen,
    2,   // Pawn,
];

static POSITION_VALUE_GRID: [[i8; 8]; 8] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 1, 1, 1, 0, 0],
    [0, 0, 1, 2, 2, 1, 0, 0],
    [0, 0, 1, 2, 2, 1, 0, 0],
    [0, 0, 1, 1, 1, 1, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
];

static INF: i32 = 10_000;
static N_INF: i32 = -10_000;

type Square = Option<Piece>;
type Pos = (usize, usize);
type Board = [[Square; 8]; 8];

#[derive(Copy, Clone, Debug, PartialEq)]
enum Kind {
    Rook,
    Knight,
    Bishop,
    King,
    Queen,
    Pawn,
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Color {
    Black,
    White,
}

#[derive(Copy, Clone, Debug)]
struct Piece {
    color: Color,
    kind: Kind,
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match (self.kind, self.color) {
            (Rook, Black) => "♜".to_string(),
            (Rook, White) => "♖".to_string(),
            (Knight, Black) => "♞".to_string(),
            (Knight, White) => "♘".to_string(),
            (Bishop, Black) => "♝".to_string(),
            (Bishop, White) => "♗".to_string(),
            (King, Black) => "♚".to_string(),
            (King, White) => "♔".to_string(),
            (Queen, Black) => "♛".to_string(),
            (Queen, White) => "♕".to_string(),
            (Pawn, Black) => "♟".to_string(),
            (Pawn, White) => "♙".to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone)]
struct Game {
    cur: Pos,
    brd: Board,
    moving: bool,
    from: Pos,
    mover: Piece,
    turn: Color,
    captured_by_black: Vec<Piece>,
    captured_by_white: Vec<Piece>,
    white_can_castle_left: bool,
    white_can_castle_right: bool,
    black_can_castle_left: bool,
    black_can_castle_right: bool,
}

#[derive(Clone, Copy)]
struct Turn {
    from: Pos,
    to: Pos,
}

struct Node {
    turn: Turn,
    children: Option<Vec<Node>>,
}

impl Node {
    fn grow(&mut self, game: &mut Game, color: Color) {
        let taken = game.move_piece(self.turn);
        if let Some(children) = &mut self.children {
            for child in children {
                child.grow(game, color);
            }
        } else {
            self.children = Some(
                game.all_pieces(color)
                    .iter()
                    .flat_map(|from| {
                        game.legal_moves(*from, color)
                            .iter()
                            .map(|to| Node {
                                turn: Turn {
                                    from: *from,
                                    to: *to,
                                },
                                children: None,
                            })
                            .collect::<Vec<Node>>()
                    })
                    .collect(),
            );
        }
        game.undo_move(&self.turn, taken);
    }
    fn minmax(&self, game: &mut Game, alpha: i32, beta: i32, is_max: bool) -> i32 {
        let mut a = alpha;
        let mut b = beta;
        let taken = game.move_piece(self.turn);
        let res = if let Some(children) = &self.children {
            match is_max {
                true => {
                    let mut max = N_INF;
                    for child in children {
                        let val = child.minmax(game, a, b, false);
                        max = max.max(val);
                        a = a.max(val);
                        if b <= a {
                            break;
                        }
                    }
                    max
                }
                false => {
                    let mut min = INF;
                    for child in children {
                        let val = child.minmax(game, a, b, true);
                        min = min.min(val);
                        b = b.min(val);
                        if b <= a {
                            break;
                        }
                    }
                    min
                }
            }
        } else {
            game.evaluate(Black)
        };
        game.undo_move(&self.turn, taken);
        res
    }
}

impl Game {
    fn render_board(&self) -> Vec<String> {
        let from: Pos = if self.moving { self.from } else { self.cur };
        let moves: Vec<Pos> = self.legal_moves(from, self.turn);
        self.brd
            .iter()
            .enumerate()
            .map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .map(|(x, place)| match place {
                        Some(piece) => format!("  {}  ", piece),
                        None => {
                            if moves.contains(&(x, y)) {
                                ("  ⃞  ").to_string()
                            } else {
                                ("  ◼  ").to_string()
                            }
                        }
                    })
                    .collect::<String>()
            })
            .collect::<Vec<String>>()
    }
    fn print(&self) {
        clear_screen();
        let top_arrow = if self.moving { "↡" } else { "↓" };
        let side_arrow = if self.moving { "↞" } else { "←" };
        print!(
            "{}",
            (0..8)
                .map(|pos| format!(
                    "  {}  ",
                    String::from(if pos == self.cur.0 { top_arrow } else { " " })
                ))
                .collect::<String>()
        );
        next_line();
        for (num, line) in self.render_board().iter().enumerate() {
            print!(
                "{}{}",
                line,
                if num == self.cur.1 { side_arrow } else { " " }
            );
            next_line();
        }
        print!(
            "{}",
            self.captured_by_black
                .iter()
                .fold("".into(), |acc, el| format!("{} {}", acc, el))
        );
        next_line();
        print!(
            "{}",
            self.captured_by_white
                .iter()
                .fold("".into(), |acc, el| format!("{} {}", acc, el))
        );
        next_line();
        print!(
            "{} to {}",
            self.evaluate(White),
            self.evaluate(Black)
        );
        next_line();
    }
    fn capture(&mut self, piece: Piece) {
        match self.turn {
            White => self.captured_by_white.push(piece),
            Black => self.captured_by_black.push(piece),
        }
    }
    fn all_pieces(&self, color: Color) -> Vec<Pos> {
        let mut result: Vec<Pos> = vec![];
        for (y, row) in self.brd.iter().enumerate() {
            for (x, square) in row.iter().enumerate() {
                if let Some(piece) = square {
                    if piece.color == color {
                        result.push((x, y));
                    }
                }
            }
        }
        result
    }
    fn move_piece(&mut self, turn: Turn) -> Square {
        let (a, b) = turn.to;
        let (x, y) = turn.from;
        let taken = self.brd[b][a];
        self.brd[b][a] = self.brd[y][x];
        self.brd[y][x] = None;
        taken
    }
    fn undo_move(&mut self, turn: &Turn, taken: Square) {
        let (a, b) = turn.from;
        let (x, y) = turn.to;
        self.brd[b][a] = self.brd[y][x];
        self.brd[y][x] = taken;
    }
    fn at(&self, at: Pos) -> Square {
        self.brd[at.1][at.0]
    }
    fn execute_move(&mut self, turn: Turn) {
        let did_castle = self.castling(turn);
        if !did_castle {
            if let Some(piece) = self.move_piece(turn) {
                self.capture(piece)
            }
        }
        self.moving = false;
        self.turn = if self.turn == Black { White } else { Black };
    }
    fn cpu_turn(&mut self) {
        let nodes = self.get_nodes();
        let scores = Arc::new(Mutex::new(vec![(0, DUMMY_TURN); nodes.len()]));
        nodes.into_par_iter().enumerate().for_each(|(i, mut node)| {
            let mut game = self.clone(); // new temporary game to test on
            let a = N_INF;
            let b = INF;
            node.grow(&mut game, White);
            node.grow(&mut game, Black);
            node.grow(&mut game, White);
            node.grow(&mut game, Black);
            scores.lock().unwrap()[i] = (node.minmax(&mut game, a, b, false), node.turn);
        });
        let best: Vec<(i32, Turn)> =
            scores
                .lock()
                .unwrap()
                .iter()
                .fold(vec![(N_INF, DUMMY_TURN)], |acc, val| {
                    match val.0.cmp(&acc[0].0) {
                        Ordering::Greater => vec![*val],
                        Ordering::Equal => [acc, vec![*val]].concat(),
                        _ => acc,
                    }
                });
        let mut rng = thread_rng();
        let im_so_random = rng.gen_range(0, best.len());
        let choice = best[im_so_random].1;

        self.execute_move(Turn {
            from: choice.from,
            to: choice.to,
        });
        self.cur = choice.to;
    }
    fn try_castle(&self, moves: &mut Vec<Pos>, mover: Piece) {
        match mover.color {
            White => {
                if self.white_can_castle_left
                    && ![(1, 0), (2, 0)]
                        .iter()
                        .any(|&square| self.at(square).is_some())
                {
                    moves.push((1, 0));
                }
                if self.white_can_castle_right
                    && ![(4, 0), (5, 0), (6, 0)]
                        .iter()
                        .any(|&square| self.at(square).is_some())
                {
                    moves.push((6, 0));
                }
            }
            Black => {
                if self.black_can_castle_left
                    && ![(1, 7), (2, 7)]
                        .iter()
                        .any(|&square| self.at(square).is_some())
                {
                    moves.push((1, 0));
                }
                if self.black_can_castle_right
                    && ![(4, 7), (5, 7), (6, 7)]
                        .iter()
                        .any(|&square| self.at(square).is_some())
                {
                    moves.push((6, 0));
                }
            }
        }
    }
    fn legal_moves(&self, location: Pos, turn: Color) -> Vec<Pos> {
        let square = self.at(location);
        let mut moves: Vec<Pos> = vec![];
        let piece = match square {
            Some(occ) => {
                if occ.color != turn {
                    // not your turn
                    return moves;
                }
                occ
            }
            None => {
                return moves;
            }
        };
        let x: i8 = location.0 as i8;
        let y: i8 = location.1 as i8;
        match piece.kind {
            Rook => {
                for _x in (x + 1)..8 {
                    if !self.try_push(&mut moves, _x, y, piece) {
                        break;
                    }
                }
                for _x in (0..x).rev() {
                    if !self.try_push(&mut moves, _x, y, piece) {
                        break;
                    }
                }
                for _y in (y + 1)..8 {
                    if !self.try_push(&mut moves, x, _y, piece) {
                        break;
                    }
                }
                for _y in (0..y).rev() {
                    if !self.try_push(&mut moves, x, _y, piece) {
                        break;
                    }
                }
            }
            Knight => {
                self.try_push(&mut moves, x + 2, y + 1, piece);
                self.try_push(&mut moves, x + 2, y - 1, piece);
                self.try_push(&mut moves, x - 2, y + 1, piece);
                self.try_push(&mut moves, x - 2, y - 1, piece);
                self.try_push(&mut moves, x + 1, y + 2, piece);
                self.try_push(&mut moves, x + 1, y - 2, piece);
                self.try_push(&mut moves, x - 1, y + 2, piece);
                self.try_push(&mut moves, x - 1, y - 2, piece);
            }
            Bishop => {
                for num in (-7..0).rev() {
                    if !self.try_push(&mut moves, x - num, y + num, piece) {
                        break;
                    }
                }
                for num in 1..7 {
                    if !self.try_push(&mut moves, x - num, y + num, piece) {
                        break;
                    }
                }
                for num in (-7..0).rev() {
                    if !self.try_push(&mut moves, x + num, y + num, piece) {
                        break;
                    }
                }
                for num in 1..7 {
                    if !self.try_push(&mut moves, x + num, y + num, piece) {
                        break;
                    }
                }
            }
            Queen => {
                for _x in (x + 1)..8 {
                    if !self.try_push(&mut moves, _x, y, piece) {
                        break;
                    }
                }
                for _x in (0..x).rev() {
                    if !self.try_push(&mut moves, _x, y, piece) {
                        break;
                    }
                }
                for _y in (y + 1)..8 {
                    if !self.try_push(&mut moves, x, _y, piece) {
                        break;
                    }
                }
                for _y in (0..y).rev() {
                    if !self.try_push(&mut moves, x, _y, piece) {
                        break;
                    }
                }
                for num in (-7..0).rev() {
                    if !self.try_push(&mut moves, x - num, y + num, piece) {
                        break;
                    }
                }
                for num in 1..7 {
                    if !self.try_push(&mut moves, x - num, y + num, piece) {
                        break;
                    }
                }
                for num in (-7..0).rev() {
                    if !self.try_push(&mut moves, x + num, y + num, piece) {
                        break;
                    }
                }
                for num in 1..7 {
                    if !self.try_push(&mut moves, x + num, y + num, piece) {
                        break;
                    }
                }
            }
            King => {
                self.try_push(&mut moves, x + 1, y, piece);
                self.try_push(&mut moves, x + 1, y + 1, piece);
                self.try_push(&mut moves, x + 1, y - 1, piece);
                self.try_push(&mut moves, x - 1, y, piece);
                self.try_push(&mut moves, x - 1, y + 1, piece);
                self.try_push(&mut moves, x - 1, y - 1, piece);
                self.try_push(&mut moves, x, y + 1, piece);
                self.try_push(&mut moves, x, y - 1, piece);
                self.try_castle(&mut moves, piece);
            }
            Pawn => {
                let home = if turn == White { 1 } else { 6 };
                let dir: i8 = if turn == White { 1 } else { -1 };
                if self.push_if_empty(&mut moves, x, y + dir) && y == home {
                    self.push_if_empty(&mut moves, x, y + (dir * 2));
                }
                self.push_if_not_empty(&mut moves, x + 1, y + dir, piece);
                self.push_if_not_empty(&mut moves, x - 1, y + dir, piece);
            }
        }
        moves
    }
    fn castling(&mut self, turn: Turn) -> bool {
        if let Some(piece) = self.at(turn.from) {
            match (piece.kind, piece.color, turn.from, turn.to) {
                (Rook, White, (0, 0), _) => {
                    self.white_can_castle_left = false;
                    false
                }
                (Rook, White, (7, 0), _) => {
                    self.white_can_castle_right = false;
                    false
                }
                (Rook, Black, (0, 7), _) => {
                    self.black_can_castle_left = false;
                    false
                }
                (Rook, Black, (7, 7), _) => {
                    self.black_can_castle_right = false;
                    false
                }
                (King, White, (3, 0), (1, 0)) => {
                    self.white_can_castle_left = false;
                    self.white_can_castle_right = false;
                    self.move_piece(turn);
                    self.move_piece(Turn {
                        from: (0, 0),
                        to: (2, 0),
                    }); // castle left
                    true
                }
                (King, White, (3, 0), (6, 0)) => {
                    self.white_can_castle_left = false;
                    self.white_can_castle_right = false;
                    self.move_piece(turn);
                    self.move_piece(Turn {
                        from: (7, 0),
                        to: (5, 0),
                    }); // castle right
                    true
                }
                (King, Black, (3, 7), (1, 7)) => {
                    self.white_can_castle_left = false;
                    self.white_can_castle_right = false;
                    self.move_piece(turn);
                    self.move_piece(Turn {
                        from: (0, 7),
                        to: (2, 7),
                    }); // castle left
                    true
                }
                (King, Black, (3, 7), (6, 7)) => {
                    self.white_can_castle_left = false;
                    self.white_can_castle_right = false;
                    self.move_piece(turn);
                    self.move_piece(Turn {
                        from: (7, 7),
                        to: (5, 7),
                    }); // castle right
                    true
                }
                (King, White, _, _) => {
                    self.white_can_castle_left = false;
                    self.white_can_castle_right = false;
                    false
                }
                (King, Black, _, _) => {
                    self.black_can_castle_left = false;
                    self.black_can_castle_right = false;
                    false
                }
                _ => false,
            }
        } else {
            false
        }
    }
    fn try_push(&self, moves: &mut Vec<Pos>, x: i8, y: i8, mover: Piece) -> bool {
        if x > 7 || y > 7 || x < 0 || y < 0 {
            return false;
        }
        let piece: Square = self.brd[y as usize][x as usize];
        match piece {
            None => {
                moves.push((x as usize, y as usize));
                true
            }
            Some(occ) => {
                if occ.color != mover.color && mover.kind != Pawn {
                    moves.push((x as usize, y as usize));
                }
                false
            }
        }
    }
    fn push_if_not_empty(&self, moves: &mut Vec<Pos>, x: i8, y: i8, mover: Piece) {
        if x > 7 || y > 7 || x < 0 || y < 0 {
            return;
        }
        if let Some(piece) = self.brd[y as usize][x as usize] {
            if piece.color != mover.color {
                moves.push((x as usize, y as usize));
            }
        }
    }
    fn push_if_empty(&self, moves: &mut Vec<Pos>, x: i8, y: i8) -> bool {
        if x > 7 || y > 7 || x < 0 || y < 0 {
            return false;
        }
        match self.brd[y as usize][x as usize] {
            None => {
                moves.push((x as usize, y as usize));
                true
            }
            Some(_) => false,
        }
    }
    fn get_nodes(&self) -> Vec<Node> {
        let mut nodes: Vec<Node> = vec![];
        let pieces: Vec<Pos> = self.all_pieces(Black);
        for from in pieces {
            let moves = self.legal_moves(from, Black);
            for to in moves {
                nodes.push(Node {
                    turn: Turn { to, from },
                    children: None,
                })
            }
        }
        nodes
    }
    fn evaluate(&self, color: Color) -> i32 {
        let mut score: i32 = 0;
        for (y, row) in self.brd.iter().enumerate() {
            for (x, square) in row.iter().enumerate() {
                if let Some(piece) = square {
                    if piece.color == color {
                        score += PIECE_VALUES[piece.kind as usize] as i32;
                        score += POSITION_VALUE_GRID[y][x] as i32;
                    } else {
                        score -= PIECE_VALUES[piece.kind as usize] as i32;
                        score -= POSITION_VALUE_GRID[y][x] as i32;
                    }
                }
            }
        }
        score
    }

}

fn clear_screen() {
    if let Err(e) = execute!(stdout(), Clear(ClearType::All), MoveTo(0, 0)) {
        print!("terminal error: {}", e);
        std::process::exit(1);
    }
}

fn next_line() {
    if let Err(e) = execute!(stdout(), MoveToNextLine(2)) {
        print!("terminal error: {}", e);
        std::process::exit(1);
    }
}

fn handle_event(game: &mut Game) -> bool {
    if let Ok(Event::Key(event)) = read() {
        match event.code {
            KeyCode::Up => {
                if game.cur.1 > 0 {
                    game.cur.1 -= 1;
                }
                true
            }
            KeyCode::Down => {
                if game.cur.1 < 7 {
                    game.cur.1 += 1;
                };
                true
            }
            KeyCode::Left => {
                if game.cur.0 > 0 {
                    game.cur.0 -= 1;
                }
                true
            }
            KeyCode::Right => {
                if game.cur.0 < 7 {
                    game.cur.0 += 1;
                };
                true
            }
            KeyCode::Enter => {
                if !game.moving {
                    match game.at(game.cur) {
                        Some(piece) => {
                            if piece.color != game.turn {
                                return true;
                            }
                            game.from = game.cur;
                            game.mover = piece;
                            game.moving = true;
                        }
                        None => {
                            return true;
                        }
                    }
                } else {
                    let moves = game.legal_moves(game.from, game.turn);
                    if moves.contains(&game.cur) {
                        game.execute_move(Turn {
                            from: game.from,
                            to: game.cur,
                        });
                        game.print();
                        game.cpu_turn();
                    } else if game.cur == game.from {
                        // drop the piece
                        game.moving = false;
                    }
                }
                true
            }
            KeyCode::Esc => false,
            _ => true,
        }
    } else {
        true
    }
}

fn main() -> io::Result<()> {
    let mut game = NEW_BOARD.clone();
    if let Err(e) = enable_raw_mode() {
        print! {"terminal error: {}", e};
        std::process::exit(1);
    }
    game.print();
    while handle_event(&mut game) {
        game.print();
    }
    disable_raw_mode()?;
    Ok(())
}
