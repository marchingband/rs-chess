#![warn(clippy::all)]
use std::cmp::Ordering;
use std::fmt;
use std::io;
use std::io::stdout;
use std::sync::atomic;
use std::sync::{Arc, Mutex};
const REL: atomic::Ordering = atomic::Ordering::Relaxed;
use crossterm::{
    cursor::{MoveTo, MoveToNextLine},
    event::{read, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode},
    terminal::{Clear, ClearType},
};
use rand::prelude::*;
use rayon::prelude::*;
use Color::*;
use Kind::*;

static WHITE_CAN_CASTLE_LEFT: atomic::AtomicBool = atomic::AtomicBool::new(true);
static WHITE_CAN_CASTLE_RIGHT: atomic::AtomicBool = atomic::AtomicBool::new(true);
static BLACK_CAN_CASTLE_LEFT: atomic::AtomicBool = atomic::AtomicBool::new(true);
static BLACK_CAN_CASTLE_RIGHT: atomic::AtomicBool = atomic::AtomicBool::new(true);

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
}

struct Node {
    turn: Turn,
    children: Option<Vec<Node>>,
}

impl Node {
    fn grow(&mut self, board: &mut Board, color: Color) {
        let taken = board.move_piece(self.turn);
        if let Some(children) = &mut self.children {
            for child in children {
                child.grow(board, color);
            }
        } else {
            self.children = Some(
                board
                    .all_pieces(color)
                    .iter()
                    .flat_map(|from| {
                        legal_moves(board, *from, color)
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
        board.undo_move(&self.turn, taken);
    }
    fn minmax(&self, board: &mut Board, alpha: i32, beta: i32, is_max: bool) -> i32 {
        let mut a = alpha;
        let mut b = beta;
        let taken = board.move_piece(self.turn);
        let res = if let Some(children) = &self.children {
            match is_max {
                true => {
                    let mut max = N_INF;
                    for child in children {
                        let val = child.minmax(board, a, b, false);
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
                        let val = child.minmax(board, a, b, true);
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
            board.evaluate(Black)
        };
        board.undo_move(&self.turn, taken);
        res
    }
}

impl Game {
    fn render(&self) -> Vec<String> {
        let from: Pos = if self.moving { self.from } else { self.cur };
        let moves: Vec<Pos> = legal_moves(&self.brd, from, self.turn);
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
        for (num, line) in self.render().iter().enumerate() {
            print!(
                "{}{}",
                line,
                if num == self.cur.1 { side_arrow } else { " " }
            );
            next_line();
        }
        // print!("{:?}", legal_moves(&game, game.cur));
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
            self.brd.evaluate(White),
            self.brd.evaluate(Black)
        );
        next_line();
    }
    fn capture(&mut self, piece: Piece) {
        match self.turn {
            White => self.captured_by_white.push(piece),
            Black => self.captured_by_black.push(piece),
        }
    }
}

#[derive(Clone, Copy)]
struct Turn {
    from: Pos,
    to: Pos,
}

trait Chess {
    fn all_pieces(&self, color: Color) -> Vec<Pos>;
    fn at(&self, at: Pos) -> Square;
    fn evaluate(&self, color: Color) -> i32;
    fn move_piece(&mut self, turn: Turn) -> Square;
    fn undo_move(&mut self, turn: &Turn, taken: Square);
}

impl Chess for Board {
    fn evaluate(&self, color: Color) -> i32 {
        let mut score: i32 = 0;
        for (y, row) in self.iter().enumerate() {
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
    fn all_pieces(&self, color: Color) -> Vec<Pos> {
        let mut result: Vec<Pos> = vec![];
        for (y, row) in self.iter().enumerate() {
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
        let taken = self[b][a];
        self[b][a] = self[y][x];
        self[y][x] = None;
        taken
    }
    fn at(&self, at: Pos) -> Square {
        self[at.1][at.0]
    }
    fn undo_move(&mut self, turn: &Turn, taken: Square) {
        let (a, b) = turn.from;
        let (x, y) = turn.to;
        self[b][a] = self[y][x];
        self[y][x] = taken;
    }
}

fn clear_screen() {
    execute!(stdout(), Clear(ClearType::All), MoveTo(0, 0));
}

fn next_line() {
    execute!(stdout(), MoveToNextLine(2),);
}

fn try_push(board: &Board, moves: &mut Vec<Pos>, x: i8, y: i8, mover: Piece) -> bool {
    if x > 7 || y > 7 || x < 0 || y < 0 {
        return false;
    }
    let piece: Square = board[y as usize][x as usize];
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

fn push_if_not_empty(board: &Board, moves: &mut Vec<Pos>, x: i8, y: i8, mover: Piece) {
    if x > 7 || y > 7 || x < 0 || y < 0 {
        return;
    }
    if let Some(piece) = board[y as usize][x as usize] {
        if piece.color != mover.color {
            moves.push((x as usize, y as usize));
        }
    }
}

fn push_if_empty(board: &Board, moves: &mut Vec<Pos>, x: i8, y: i8) -> bool {
    if x > 7 || y > 7 || x < 0 || y < 0 {
        return false;
    }
    match board[y as usize][x as usize] {
        None => {
            moves.push((x as usize, y as usize));
            true
        }
        Some(_) => false,
    }
}

fn try_castle(board: &Board, moves: &mut Vec<Pos>, mover: Piece) {
    match mover.color {
        White => {
            if WHITE_CAN_CASTLE_LEFT.load(REL)
                && ![(1, 0), (2, 0)]
                    .iter()
                    .any(|&square| board.at(square).is_some())
            {
                moves.push((1, 0));
            }
            if WHITE_CAN_CASTLE_RIGHT.load(REL)
                && ![(4, 0), (5, 0), (6, 0)]
                    .iter()
                    .any(|&square| board.at(square).is_some())
            {
                moves.push((6, 0));
            }
        }
        Black => {
            if BLACK_CAN_CASTLE_LEFT.load(REL)
                && ![(1, 7), (2, 7)]
                    .iter()
                    .any(|&square| board.at(square).is_some())
            {
                moves.push((1, 0));
            }
            if BLACK_CAN_CASTLE_RIGHT.load(REL)
                && ![(4, 7), (5, 7), (6, 7)]
                    .iter()
                    .any(|&square| board.at(square).is_some())
            {
                moves.push((6, 0));
            }
        }
    }
}

fn legal_moves(board: &Board, location: Pos, turn: Color) -> Vec<Pos> {
    let square = board.at(location);
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
                if !try_push(board, &mut moves, _x, y, piece) {
                    break;
                }
            }
            for _x in (0..x).rev() {
                if !try_push(board, &mut moves, _x, y, piece) {
                    break;
                }
            }
            for _y in (y + 1)..8 {
                if !try_push(board, &mut moves, x, _y, piece) {
                    break;
                }
            }
            for _y in (0..y).rev() {
                if !try_push(board, &mut moves, x, _y, piece) {
                    break;
                }
            }
        }
        Knight => {
            try_push(board, &mut moves, x + 2, y + 1, piece);
            try_push(board, &mut moves, x + 2, y - 1, piece);
            try_push(board, &mut moves, x - 2, y + 1, piece);
            try_push(board, &mut moves, x - 2, y - 1, piece);
            try_push(board, &mut moves, x + 1, y + 2, piece);
            try_push(board, &mut moves, x + 1, y - 2, piece);
            try_push(board, &mut moves, x - 1, y + 2, piece);
            try_push(board, &mut moves, x - 1, y - 2, piece);
        }
        Bishop => {
            for num in (-7..0).rev() {
                if !try_push(board, &mut moves, x - num, y + num, piece) {
                    break;
                }
            }
            for num in 1..7 {
                if !try_push(board, &mut moves, x - num, y + num, piece) {
                    break;
                }
            }
            for num in (-7..0).rev() {
                if !try_push(board, &mut moves, x + num, y + num, piece) {
                    break;
                }
            }
            for num in 1..7 {
                if !try_push(board, &mut moves, x + num, y + num, piece) {
                    break;
                }
            }
        }
        Queen => {
            for _x in (x + 1)..8 {
                if !try_push(board, &mut moves, _x, y, piece) {
                    break;
                }
            }
            for _x in (0..x).rev() {
                if !try_push(board, &mut moves, _x, y, piece) {
                    break;
                }
            }
            for _y in (y + 1)..8 {
                if !try_push(board, &mut moves, x, _y, piece) {
                    break;
                }
            }
            for _y in (0..y).rev() {
                if !try_push(board, &mut moves, x, _y, piece) {
                    break;
                }
            }
            for num in (-7..0).rev() {
                if !try_push(board, &mut moves, x - num, y + num, piece) {
                    break;
                }
            }
            for num in 1..7 {
                if !try_push(board, &mut moves, x - num, y + num, piece) {
                    break;
                }
            }
            for num in (-7..0).rev() {
                if !try_push(board, &mut moves, x + num, y + num, piece) {
                    break;
                }
            }
            for num in 1..7 {
                if !try_push(board, &mut moves, x + num, y + num, piece) {
                    break;
                }
            }
        }
        King => {
            try_push(board, &mut moves, x + 1, y, piece);
            try_push(board, &mut moves, x + 1, y + 1, piece);
            try_push(board, &mut moves, x + 1, y - 1, piece);
            try_push(board, &mut moves, x - 1, y, piece);
            try_push(board, &mut moves, x - 1, y + 1, piece);
            try_push(board, &mut moves, x - 1, y - 1, piece);
            try_push(board, &mut moves, x, y + 1, piece);
            try_push(board, &mut moves, x, y - 1, piece);
            try_castle(board, &mut moves, piece);
        }
        Pawn => {
            let home = if turn == White { 1 } else { 6 };
            let dir: i8 = if turn == White { 1 } else { -1 };
            if push_if_empty(board, &mut moves, x, y + dir) && y == home {
                push_if_empty(board, &mut moves, x, y + (dir * 2));
            }
            push_if_not_empty(board, &mut moves, x + 1, y + dir, piece);
            push_if_not_empty(board, &mut moves, x - 1, y + dir, piece);
        }
    }
    moves
}

fn castling(game: &mut Game, turn: Turn) -> bool {
    if let Some(piece) = game.brd.at(turn.from) {
        match (piece.kind, piece.color, turn.from, turn.to) {
            (Rook, White, (0, 0), _) => {
                WHITE_CAN_CASTLE_LEFT.store(false, REL);
                false
            }
            (Rook, White, (7, 0), _) => {
                WHITE_CAN_CASTLE_RIGHT.store(false, REL);
                false
            }
            (Rook, Black, (0, 7), _) => {
                BLACK_CAN_CASTLE_LEFT.store(false, REL);
                false
            }
            (Rook, Black, (7, 7), _) => {
                BLACK_CAN_CASTLE_RIGHT.store(false, REL);
                false
            }
            (King, White, (3, 0), (1, 0)) => {
                WHITE_CAN_CASTLE_LEFT.store(false, REL);
                WHITE_CAN_CASTLE_RIGHT.store(false, REL);
                game.brd.move_piece(turn);
                game.brd.move_piece(Turn {
                    from: (0, 0),
                    to: (2, 0),
                }); // castle left
                true
            }
            (King, White, (3, 0), (6, 0)) => {
                WHITE_CAN_CASTLE_LEFT.store(false, REL);
                WHITE_CAN_CASTLE_RIGHT.store(false, REL);
                game.brd.move_piece(turn);
                game.brd.move_piece(Turn {
                    from: (7, 0),
                    to: (5, 0),
                }); // castle right
                true
            }
            (King, Black, (3, 7), (1, 7)) => {
                WHITE_CAN_CASTLE_LEFT.store(false, REL);
                WHITE_CAN_CASTLE_RIGHT.store(false, REL);
                game.brd.move_piece(turn);
                game.brd.move_piece(Turn {
                    from: (0, 7),
                    to: (2, 7),
                }); // castle left
                true
            }
            (King, Black, (3, 7), (6, 7)) => {
                WHITE_CAN_CASTLE_LEFT.store(false, REL);
                WHITE_CAN_CASTLE_RIGHT.store(false, REL);
                game.brd.move_piece(turn);
                game.brd.move_piece(Turn {
                    from: (7, 7),
                    to: (5, 7),
                }); // castle right
                true
            }
            (King, White, _, _) => {
                WHITE_CAN_CASTLE_LEFT.store(false, REL);
                WHITE_CAN_CASTLE_RIGHT.store(false, REL);
                false
            }
            (King, Black, _, _) => {
                BLACK_CAN_CASTLE_RIGHT.store(false, REL);
                BLACK_CAN_CASTLE_LEFT.store(false, REL);
                false
            }
            _ => false,
        }
    } else {
        false
    }
}

fn execute_move(game: &mut Game, turn: Turn) {
    let did_castle = castling(game, turn);
    if !did_castle {
        if let Some(piece) = game.brd.move_piece(turn) {
            game.capture(piece)
        }
    }
    game.moving = false;
    game.turn = if game.turn == Black { White } else { Black };
}

fn get_nodes(board: &Board) -> Vec<Node> {
    let mut nodes: Vec<Node> = vec![];
    let pieces: Vec<Pos> = board.all_pieces(Black);
    for from in pieces {
        let moves = legal_moves(board, from, Black);
        for to in moves {
            nodes.push(Node {
                turn: Turn { to, from },
                children: None,
            })
        }
    }
    nodes
}

fn cpu_turn(game: &mut Game) {
    let nodes = get_nodes(&game.brd);
    let scores = Arc::new(Mutex::new(vec![(0, DUMMY_TURN); nodes.len()]));
    nodes.into_par_iter().enumerate().for_each(|(i, mut nod)| {
        let mut temp_board = game.brd;
        let a = N_INF;
        let b = INF;
        nod.grow(&mut temp_board, White);
        nod.grow(&mut temp_board, Black);
        nod.grow(&mut temp_board, White);
        nod.grow(&mut temp_board, Black);
        scores.lock().unwrap()[i] = (nod.minmax(&mut temp_board, a, b, false), nod.turn);
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

    execute_move(
        game,
        Turn {
            from: choice.from,
            to: choice.to,
        },
    );
    game.cur = choice.to;
}

fn handle_event(game: &mut Game) -> bool {
    match read().unwrap() {
        Event::Key(event) => match event.code {
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
                    match game.brd.at(game.cur) {
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
                    let moves = legal_moves(&game.brd, game.from, game.turn);
                    if moves.contains(&game.cur) {
                        execute_move(
                            game,
                            Turn {
                                from: game.from,
                                to: game.cur,
                            },
                        );
                        game.print();
                        cpu_turn(game);
                    } else if game.cur == game.from {
                        // drop the piece
                        game.moving = false;
                    }
                }
                true
            }
            KeyCode::Esc => false,
            _ => true,
        },
        _ => true,
    }
}

fn main() -> io::Result<()> {
    let mut game = NEW_BOARD.clone();
    enable_raw_mode()?;
    game.print();
    while handle_event(&mut game) {
        game.print();
    }
    disable_raw_mode()?;
    Ok(())
}