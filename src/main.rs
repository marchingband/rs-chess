#![warn(clippy::all)]
use std::sync::atomic;
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
use std::cmp::Ordering;
use std::io;
use std::io::stdout;
use std::process::exit;
use std::sync::{Arc, Mutex};
use std::{thread, time};
use Color::*;
use Kind::*;

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

use std::fmt;

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

type Square = Option<Piece>;
type Pos = (usize, usize);
type Board = [[Square; 8]; 8];

#[derive(Clone, Copy)]
struct Move {
    from: Pos,
    to: Pos,
    score: i32,
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

static WHITE_CAN_CASTLE_LEFT: atomic::AtomicBool = atomic::AtomicBool::new(true);
static WHITE_CAN_CASTLE_RIGHT: atomic::AtomicBool = atomic::AtomicBool::new(true);
static BLACK_CAN_CASTLE_LEFT: atomic::AtomicBool = atomic::AtomicBool::new(true);
static BLACK_CAN_CASTLE_RIGHT: atomic::AtomicBool = atomic::AtomicBool::new(true);

impl Game {
    fn render(&self) -> Vec<String> {
        let from: Pos = if self.moving { self.from } else { self.cur };
        let moves: Vec<Pos> = allow_moves(&self.brd, from, self.turn);
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
        // print!("{:?}", allow_moves(&game, game.cur));
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

static POSITIONAL_VALUES: [(Pos, usize); 16] = [
    ((3, 3), 2),
    ((4, 3), 2),
    ((4, 4), 2),
    ((3, 4), 2),
    ((2, 2), 1),
    ((2, 3), 1),
    ((2, 4), 1),
    ((2, 5), 1),
    ((3, 5), 1),
    ((4, 5), 1),
    ((5, 5), 1),
    ((5, 4), 1),
    ((5, 3), 1),
    ((5, 2), 1),
    ((4, 2), 1),
    ((3, 2), 1),
];

fn clear_screen() {
    execute!(stdout(), Clear(ClearType::All), MoveTo(0, 0));
}

fn next_line() {
    execute!(stdout(), MoveToNextLine(2),);
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

fn try_push(board: &Board, moves: &mut Vec<Pos>, x: i8, y: i8, mover: Piece) -> bool {
    if x > 7 || y > 7 || x < 0 || y < 0 {
        return false;
    }
    let piece: Option<Piece> = board[y as usize][x as usize];
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

fn allow_moves(board: &Board, location: Pos, turn: Color) -> Vec<Pos> {
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

fn castling(game: &mut Game, from: Pos, to: Pos) -> bool {
    if let Some(piece) = game.brd.at(from) {
        match (piece.kind, piece.color, from, to) {
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
                game.brd.move_piece(from, to);
                game.brd.move_piece((0, 0), (2, 0)); // castle left
                true
            }
            (King, White, (3, 0), (6, 0)) => {
                WHITE_CAN_CASTLE_LEFT.store(false, REL);
                WHITE_CAN_CASTLE_RIGHT.store(false, REL);
                game.brd.move_piece(from, to);
                game.brd.move_piece((7, 0), (5, 0)); // castle right
                true
            }
            (King, Black, (3, 7), (1, 7)) => {
                WHITE_CAN_CASTLE_LEFT.store(false, REL);
                WHITE_CAN_CASTLE_RIGHT.store(false, REL);
                game.brd.move_piece(from, to);
                game.brd.move_piece((0, 7), (2, 7)); // castle left
                true
            }
            (King, Black, (3, 7), (6, 7)) => {
                WHITE_CAN_CASTLE_LEFT.store(false, REL);
                WHITE_CAN_CASTLE_RIGHT.store(false, REL);
                game.brd.move_piece(from, to);
                game.brd.move_piece((7, 7), (5, 7)); // castle right
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

fn execute_move(game: &mut Game, from: Pos, to: Pos) {
    let did_castle = castling(game, from, to);
    if !did_castle {
        if let Some(piece) = game.brd.move_piece(from, to) {
            game.capture(piece)
        }
    }
    game.moving = false;
    game.turn = if game.turn == Black { White } else { Black };
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
                    let moves = allow_moves(&game.brd, game.from, game.turn);
                    if moves.contains(&game.cur) {
                        execute_move(game, game.from, game.cur);
                        game.print();
                        // ai_turn(game);
                        // comp_turn(game);
                        // comp_test(game);
                        // cpu_turn(game);
                        // take_turn(game);
                        take_turn2(game);
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

fn black_respond(board: &Board) -> Board {
    let mut responses: Vec<Board> = vec![];
    let pieces: Vec<Pos> = board.get_all(Black);
    for from in pieces {
        let moves = allow_moves(board, from, Black);
        for to in moves {
            let mut option = *board;
            option.move_piece(from, to);
            responses.push(option);
        }
    }
    responses.iter().fold(responses[0], |acc, val| {
        if acc.evaluate(Black) > val.evaluate(Black) {
            acc
        } else {
            *val
        }
    })
}

fn white_respond(board: &Board) -> Board {
    let mut responses: Vec<Board> = vec![];
    let pieces: Vec<Pos> = board.get_all(White);
    for from in pieces {
        let moves = allow_moves(board, from, White);
        for to in moves {
            let mut option = *board;
            option.move_piece(from, to);
            responses.push(option);
        }
    }
    // pick the best for white based on the black simple response
    responses.iter().fold(responses[0], |acc, val| {
        if black_respond(&acc).evaluate(White) > black_respond(val).evaluate(White) {
            acc
        } else {
            *val
        }
    })
}

fn get_opts(board: &Board) -> Vec<Opt> {
    let mut options: Vec<Opt> = vec![];
    let pieces: Vec<Pos> = board.get_all(Black);
    for from in pieces {
        let moves = allow_moves(board, from, Black);
        for to in moves {
            let mut option = *board;
            option.move_piece(from, to);
            options.push(Opt {
                from,
                to,
                tree: Node {
                    board: option,
                    children: None,
                },
            })
        }
    }
    options
}
fn get_nods(board: &Board) -> Vec<Nod> {
    let mut nods: Vec<Nod> = vec![];
    let pieces: Vec<Pos> = board.get_all(Black);
    for from in pieces {
        let moves = allow_moves(board, from, Black);
        for to in moves {
            nods.push(Nod {
                turn: Turn{
                    to,
                    from
                },
                children: None,
            })
        }
    }
    nods
}

fn pick_option(options: Vec<Opt>) -> Move {
    // let list = Arc::new(Mutex::new(vec!()));
    // options.into_par_iter().for_each(|option| {
    //     let choices: Vec<Board> = option.tree.dig();
    //     let high_score: i32 = choices.iter().fold(choices[0].evaluate(Black), |acc, val| {
    //         let score = val.evaluate(Black);
    //         if acc > score {
    //             acc
    //         } else {
    //             score
    //         }
    //     });
    //     list.lock().unwrap().push(Move{
    //         from: option.from,
    //         to: option.to,
    //         score: high_score
    //     })
    // });
    let mut moves: Vec<Move> = vec![];
    for option in options {
        let choices: Vec<Board> = option.tree.dig();
        let high_score: i32 = choices.iter().fold(choices[0].evaluate(Black), |acc, val| {
            let score = val.evaluate(Black);
            if acc > score {
                acc
            } else {
                score
            }
        });
        moves.push(Move {
            from: option.from,
            to: option.to,
            score: high_score,
        })
    }
    let mut best_moves: Vec<Move> = vec![moves[0]];
    for candidate in moves.iter().skip(1) {
        match (candidate.score).cmp(&best_moves[0].score) {
            Ordering::Greater => best_moves = vec![*candidate],
            Ordering::Equal => best_moves.push(*candidate),
            _ => {}
        }
    }
    let mut rng = thread_rng();
    let im_so_random = rng.gen_range(0, best_moves.len());
    // print!("found {} options, chose {}", best_moves.len(), it);
    // next_line();
    // thread::sleep(time::Duration::from_secs(2));
    best_moves[im_so_random]
    // list.iter().fold(list[0], |acc, val| {
    //     if acc.score > val.score {
    //         acc
    //     } else {
    //         *val
    //     }
    // })
    // let list2 = list.lock().unwrap();
    // list2.iter().fold(list2[0], |acc, val| {
    //     if acc.score > val.score {
    //         acc
    //     } else {
    //         *val
    //     }
    // })
}

#[derive(Clone)]
struct Node {
    board: Board,
    children: Option<Vec<Node>>,
}

static INF: i32 = 10_000;
static N_INF: i32 = -10_000;

impl Node {
    fn grow(&mut self) {
        if let Some(children) = &mut self.children {
            children.into_par_iter().for_each(|child| child.grow());
            // for i in 0..children.len() {
            //     children[i].grow();
            // }
        } else {
            let mut children: Vec<Node> = vec![];
            let board = white_respond(&self.board);
            let pieces: Vec<Pos> = board.get_all(Black);
            for from in pieces {
                let moves = allow_moves(&board, from, Black);
                for to in moves {
                    let mut option = board;
                    option.move_piece(from, to);
                    children.push(Node {
                        board: option,
                        children: None,
                    })
                }
            }
            self.children = Some(children);
        }
    }
    fn branch(&mut self, color: Color) {
        if let Some(children) = &mut self.children {
            // children
            //     .into_par_iter()
            //     .for_each(|child| child.branch(color));
            for child in children {
                child.grow();
            }
        } else {
            let mut children: Vec<Node> = vec![];
            let pieces: Vec<Pos> = self.board.get_all(color);
            for from in pieces {
                let moves = allow_moves(&self.board, from, color);
                for to in moves {
                    let mut option = self.board;
                    option.move_piece(from, to);
                    children.push(Node {
                        board: option,
                        children: None,
                    })
                }
            }
            self.children = Some(children);
        }
    }
    fn dig(&self) -> Vec<Board> {
        if let Some(children) = &self.children {
            // let descendants = Arc::new(Mutex::new(vec![]));
            // children.into_par_iter().for_each(|child| {
            //     let grand_children: Vec<Board> = child.dig();
            //     for j in 0..grand_children.len() {
            //         descendants.lock().unwrap().push(grand_children[j]);
            //     }
            // });
            // return descendants.lock().unwrap().clone();
            let mut descendants: Vec<Board> = vec![];
            for child in children {
                let grand_children: Vec<Board> = child.dig();
                for grand_child in grand_children {
                    descendants.push(grand_child);
                }
            }
            // return descendants.lock().unwrap();
            descendants
        } else {
            return vec![self.board];
        }
    }
    fn minimax(&self, mut a: i32, mut b: i32, is_max: bool) -> i32 {
        if let Some(children) = &self.children {
            match is_max {
                true => {
                    let mut max = N_INF;
                    for child in children {
                        let val = child.minimax(a, b, false);
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
                        let val = child.minimax(a, b, true);
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
            self.board.evaluate(Black)
        }
    }
}

#[derive(Clone, Copy)]
struct Turn {
    from: Pos,
    to: Pos,
}

trait Chess {
    fn get_all(&self, color: Color) -> Vec<Pos>;
    fn move_piece(&mut self, from: Pos, to: Pos) -> Square;
    fn at(&self, at: Pos) -> Square;
    fn evaluate(&self, color: Color) -> i32;
    fn get_moves(&self, color: Color) -> Vec<Turn>;
    fn exec(&mut self, turn: &Turn) -> Option<Piece>;
    fn rev(&mut self, turn: &Turn, taken: Option<Piece>);
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
        // let mut score: i32 = 0;
        // let mine = self.get_all(color);
        // let them = self.get_all(if color == Black { White } else { Black });
        // for pos in mine {
        //     if let Some(piece) = self.at(pos) {
        //         score += PIECE_VALUES[piece.kind as usize] as i32;
        //     }
        // }
        // for pos in them {
        //     if let Some(piece) = self.at(pos) {
        //         score -= PIECE_VALUES[piece.kind as usize] as i32;
        //     }
        // }
        // for position in POSITIONAL_VALUES {
        //     let (pos, val) = position;
        //     if let Some(piece) = self.at(pos) {
        //         if piece.color == color {
        //             score += val as i32;
        //         } else {
        //             score -= val as i32;
        //         }
        //     }
        // }
        // score
    }
    fn get_all(&self, color: Color) -> Vec<Pos> {
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
    fn move_piece(&mut self, from: Pos, to: Pos) -> Square {
        let (a, b) = to;
        let (x, y) = from;
        let taken = self[b][a];
        self[b][a] = self[y][x];
        self[y][x] = None;
        taken
    }
    fn at(&self, at: Pos) -> Square {
        self[at.1][at.0]
    }
    fn get_moves(&self, color: Color) -> Vec<Turn> {
        let mut result: Vec<Turn> = vec![];
        let froms: Vec<Pos> = self.get_all(color);
        for from in froms {
            let tos = allow_moves(self, from, color);
            for to in tos {
                result.push(Turn { from, to })
            }
        }
        result
    }
    fn exec(&mut self, turn: &Turn) -> Option<Piece> {
        let (a, b) = turn.to;
        let (x, y) = turn.from;
        let taken = self[b][a];
        self[b][a] = self[y][x];
        self[y][x] = None;
        taken
    }
    fn rev(&mut self, turn: &Turn, taken: Option<Piece>) {
        let (a, b) = turn.from;
        let (x, y) = turn.to;
        self[b][a] = self[y][x];
        self[y][x] = taken;
    }
}

#[derive(Clone)]
struct Opt {
    from: Pos,
    to: Pos,
    tree: Node,
}

fn cpu_turn(game: &mut Game) {
    let mut options: Vec<Opt> = get_opts(&game.brd);
    for option in &mut options {
        option.tree.grow();
        // option.tree.grow();
        // option.tree.grow();
    }
    let choice: Move = pick_option(options);
    execute_move(game, choice.from, choice.to);
    game.cur = choice.to;
}

fn ai_turn(game: &mut Game) {
    // let choices: Vec<(Turn, i32)> = game.brd.get_moves(Black).into_iter().map(|b1| { // list of black choices
    let choices: Vec<Turn> = game.brd.get_moves(Black); // list of black choices
    let choice_tree: Vec<(&Turn, i32)> = choices
        .iter()
        .map(|b1| {
            let taken = game.brd.exec(b1);
            let best_white = game
                .brd
                .get_moves(White)
                .iter()
                .map(|w1| {
                    // list of white responses
                    let taken = game.brd.exec(w1);
                    let best_black = game
                        .brd
                        .get_moves(Black)
                        .iter()
                        .map(|b2| {
                            // list of black responses
                            let taken = game.brd.exec(b2);
                            let best_white = game
                                .brd
                                .get_moves(White)
                                .iter()
                                .map(|w2| {
                                    // list of white responses
                                    let taken = game.brd.exec(w2);
                                    let best_black = game
                                        .brd
                                        .get_moves(Black)
                                        .iter()
                                        .map(|b3| {
                                            // list of black responses
                                            let taken = game.brd.exec(b3);
                                            let score = game.brd.evaluate(Black);
                                            game.brd.rev(b3, taken);
                                            score
                                        })
                                        .max()
                                        .unwrap();
                                    // let score = game.brd.evaluate(Black);
                                    game.brd.rev(w2, taken);
                                    best_black
                                    // score
                                    // }).min().unwrap_or(10000000);
                                })
                                .min()
                                .unwrap();
                            game.brd.rev(b2, taken);
                            best_white
                            // }).max().unwrap_or(-10000000); // the best for black
                        })
                        .max()
                        .unwrap(); // the best for black
                    game.brd.rev(w1, taken);
                    best_black
                })
                .min()
                .unwrap(); // the best for white
                           // }).min().unwrap_or(10000000); // the best for white
            game.brd.rev(b1, taken);
            (b1, best_white) // score of whites best response
        })
        .collect();
    let choice = choice_tree
        .iter()
        .fold(
            &choice_tree[0],
            |acc, val| if acc.1 > val.1 { acc } else { val },
        )
        .0;
    execute_move(game, choice.from, choice.to);
}

use std::time::Instant;

fn comp_turn(game: &mut Game) {
    // let start = Instant::now();
    let options: Vec<Opt> = get_opts(&game.brd);
    let mut nodes: Vec<Node> = options.iter().map(|o| o.tree.clone()).collect();
    let mut scores = vec![0; nodes.len()];
    let a = N_INF;
    let b = INF;
    for (i, node) in nodes.iter_mut().enumerate() {
        // let start_branching = Instant::now();
        node.branch(White);
        node.branch(Black);
        node.branch(White);
        node.branch(Black);
        // let done_branching = Instant::now();
        scores[i] = node.minimax(a, b, false);
        // let done_minimax = Instant::now();
        // print!("{:?} {:?} {:?}", start_branching, done_branching, done_minimax);
    }
    let mut best_score_indicies: Vec<usize> = vec![0];
    for (i, score) in scores.iter().enumerate() {
        match (score).cmp(&scores[best_score_indicies[0]]) {
            Ordering::Greater => best_score_indicies = vec![i],
            Ordering::Equal => best_score_indicies.push(i),
            _ => {}
        }
    }
    let mut rng = thread_rng();
    let im_so_random = rng.gen_range(0, best_score_indicies.len());

    execute_move(
        game,
        options[best_score_indicies[im_so_random]].from,
        options[best_score_indicies[im_so_random]].to,
    );
}

fn comp_test(game: &mut Game) {
    let a = N_INF;
    let b = INF;
    // let options: Vec<Opt> = get_opts(&game.brd);
    // let nodes: Vec<Node> = options.iter().map(|o| o.tree.clone()).collect();
    let mut root: Node = Node {
        board: game.brd,
        children: None,
    };
    root.branch(Black);
    root.branch(White);
    root.branch(Black);
    // root.branch(White);
    let choice = root.minimax(a, b, true);
    print!("{}", choice);
    print!("done");
}

struct Nod {
    turn: Turn,
    children: Option<Vec<Nod>>,
}

struct NodVal {
    turn: Turn,
    score: i32,
}

fn wait() {
    match read().unwrap() {
        Event::Key(event) => match event.code {
            KeyCode::Esc => {
                exit(1);
            }
            _ => {}
        },
        _ => {}
    }
}

impl Nod {
    fn grow(&mut self, board: &mut Board, color: Color, has_move: bool) {
        let mut taken = None;
        if has_move {
            taken = board.exec(&self.turn)
        };

        if let Some(children) = &mut self.children {
            for child in children {
                child.grow(board, color, true);
            }
        } else {
            let mut children: Vec<Nod> = vec![];
            let pieces: Vec<Pos> = board.get_all(color);
            for from in pieces {
                let moves = allow_moves(board, from, color);
                for to in moves {
                    children.push(Nod {
                        turn: Turn { from, to },
                        children: None,
                    })
                }
            }
            self.children = Some(children);
        }

        if has_move {
            board.rev(&self.turn, taken);
        }
    }

    fn minimax(
        &self,
        // mut board: Board,
        game: &mut Game,
        alpha: i32,
        beta: i32,
        is_max: bool,
        is_root: bool,
    ) -> NodVal {
        let mut a = alpha;
        let mut b = beta;
        if let Some(children) = &self.children {
            let mut taken = None;
            if !is_root {
                taken = game.brd.exec(&self.turn)
            };
            // game.print();
            // read().unwrap();
            let res = match is_max {
                true => {
                    let mut max = NodVal {
                        score: N_INF,
                        turn: Turn {
                            from: (0, 0),
                            to: (0, 0),
                        },
                    };
                    for child in children {
                        let val = child.minimax(game, a, b, false, false);
                        let m = val.score;
                        max = if max.score > val.score { max } else { val };
                        // a = if a > max.score { a } else { max.score };
                        a = a.max(m);
                        // game.print();
                        // print!("score:{} max:{} alpha:{}", m, max.score, a);
                        // next_line();
                        // wait();

                        if b <= a {
                            break;
                        }
                    }
                    // board.rev(&self.turn, taken);
                    if is_root {
                        NodVal {
                            score: max.score,
                            turn: max.turn,
                        }
                    } else {
                        NodVal {
                            score: max.score,
                            turn: self.turn,
                        }
                    }
                }
                false => {
                    // let taken = board.exec(&self.turn);
                    let mut min = NodVal {
                        score: INF,
                        turn: Turn {
                            from: (0, 0),
                            to: (0, 0),
                        },
                    };
                    for child in children {
                        let val = child.minimax(game, a, b, true, false);
                        let m = val.score;
                        min = if val.score < min.score { val } else { min };
                        b = b.min(m);

                        // game.print();
                        // print!("score:{} min:{} beta:{}", m, min.score, b);
                        // next_line();
                        // wait();

                        if b <= a {
                            break;
                        }
                    }
                    // board.rev(&self.turn, taken);
                    if is_root {
                        NodVal {
                            score: min.score,
                            turn: min.turn,
                        }
                    } else {
                        NodVal {
                            score: min.score,
                            turn: self.turn,
                        }
                    }
                }
            };
            if !is_root {
                game.brd.rev(&self.turn, taken);
            }
            res
        } else {
            let taken = game.brd.exec(&self.turn);
            let score = game.brd.evaluate(Black);
            // game.print();
            // print!("{}", score);
            // next_line();
            // wait();
            game.brd.rev(&self.turn, taken);
            NodVal {
                turn: self.turn,
                score,
            }
        }
    }
    fn minmax(&self, board: &mut Board, alpha: i32, beta: i32, is_max: bool) -> i32 {
        let mut a = alpha;
        let mut b = beta;
        let taken = board.exec(&self.turn);
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
        board.rev(&self.turn, taken);
        res
    }
}

fn take_turn(game: &mut Game) {
    let mut board = game.brd;
    let mut root = Nod {
        turn: Turn {
            to: (0, 0),
            from: (0, 0),
        },
        children: None,
    };
    root.grow(&mut board, Black, false);
    root.grow(&mut board, White, false);
    root.grow(&mut board, Black, false);
    // root.grow(&mut board, White, false);
    // root.grow(&mut board, Black, false);
    let a = N_INF;
    let b = INF;
    let choice: NodVal = root.minimax(game, a, b, false, true);
    print!("got {}", choice.score);
    next_line();
    thread::sleep(time::Duration::from_secs(2));
    execute_move(game, choice.turn.from, choice.turn.to);
}

static DUMMY_TURN: Turn = Turn {
    to: (0, 0),
    from: (0, 0),
};

fn take_turn2(game: &mut Game) {
    let nods = get_nods(&game.brd);
    let scores = Arc::new(Mutex::new(vec!((0, DUMMY_TURN); nods.len())));
    nods.into_par_iter().enumerate().for_each(|(i, mut nod)| {
        let mut temp_board = game.brd;
        let a = N_INF;
        let b = INF;
        nod.grow(&mut temp_board, White, true);
        nod.grow(&mut temp_board, Black, true);
        nod.grow(&mut temp_board, White, true);
        // nod.grow(&mut temp_board, Black, true);
        scores.lock().unwrap()[i] = (nod.minmax(&mut temp_board, a, b, false), nod.turn);
    });
    
    let best: Vec<(i32, Turn)> =
        scores.lock().unwrap()
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

    execute_move(game, choice.from, choice.to);
    game.cur = choice.to;
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
