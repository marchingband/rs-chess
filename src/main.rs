use std::io;
use std::io::stdout;

use crossterm::{
    cursor::{MoveTo, MoveToNextLine},
    event::{read, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode},
    terminal::{Clear, ClearType},
};

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

trait Chess {
    fn get_all(&self, color: Color) -> Vec<Pos>;
    fn move_piece(&mut self, from: Pos, to: Pos) -> Square;
    fn at(&self, at: Pos) -> Square;
    fn evaluate(&self, color: Color) -> i32;
}

impl Chess for Board {
    fn evaluate(&self, color: Color) -> i32 {
        let mut score: i32 = 0;
        let mine = self.get_all(color);
        let them = self.get_all(if color == Black { White } else { Black });
        for pos in mine {
            if let Some(piece) = self.at(pos) {
                score += PIECE_VALUES[piece.kind as usize] as i32;
            }
        }
        for pos in them {
            if let Some(piece) = self.at(pos) {
                score -= PIECE_VALUES[piece.kind as usize] as i32;
            }
        }
        for position in POSITIONAL_VALUES {
            let (pos, val) = position;
            if let Some(piece) = self.at(pos) {
                if piece.color == color {
                    score += val as i32;
                } else {
                    score -= val as i32;
                }
            }
        }
        score
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
}

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

fn push_if_empty(board: &Board, moves: &mut Vec<Pos>, x: i8, y: i8, piece: Piece) -> bool {
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
    let piece: Piece;
    match square {
        Some(occ) => {
            if occ.color != turn {
                // not your turn
                return moves;
            }
            piece = occ;
        }
        None => {
            return moves;
        }
    }
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
        }
        Pawn => {
            let home = if turn == White { 1 } else { 6 };
            let dir: i8 = if turn == White { 1 } else { -1 };
            if push_if_empty(board, &mut moves, x, y + dir, piece) && y == home {
                push_if_empty(board, &mut moves, x, y + (dir * 2), piece);
            }
            push_if_not_empty(board, &mut moves, x + 1, y + dir, piece);
            push_if_not_empty(board, &mut moves, x - 1, y + dir, piece);
        }
    }
    moves
}

fn execute_move(game: &mut Game, from: Pos, to: Pos) {
    if let Some(piece) = game.brd.move_piece(from, to) {
        game.capture(piece)
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

fn black_respond(board: &Board) -> Board {
    let mut responses: Vec<Board> = vec![];
    let pieces: Vec<Pos> = board.get_all(Black);
    for from in pieces {
        let moves = allow_moves(&board, from, Black);
        for to in moves {
            let mut option = board.clone();
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
        let moves = allow_moves(&board, from, White);
        for to in moves {
            let mut option = board.clone();
            option.move_piece(from, to);
            responses.push(option);
        }
    }
    // pick the best for white based on the black simple response
    responses.iter().fold(responses[0], |acc, val| {
        if black_respond(&acc).evaluate(White) > black_respond(&val).evaluate(White) {
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
        let moves = allow_moves(&board, from, Black);
        for to in moves {
            let mut option = board.clone();
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

fn pick_option(options: &Vec<Opt>) -> Move{
    let mut list: Vec<Move> = vec![];
    for i in 0..options.len() {
        let choices: Vec<Board> = options[i].tree.dig();
        let high_score: i32 = choices.iter().fold(choices[0].evaluate(Black), |acc, val| {
            let score = val.evaluate(Black);
            if acc > score {
                acc
            } else {
                score
            }
        });
        list.push(Move{
            from: options[i].from,
            to: options[i].to,
            score: high_score
        })
    }
    list.iter().fold(list[0], |acc, val| {
        if acc.score > val.score {
            acc
        } else {
            *val
        }
    })
}

struct Node {
    board: Board,
    children: Option<Vec<Node>>,
}

impl Node {
    fn grow(& mut self) {
        if let Some(children) = &mut self.children {
            for i in 0..children.len() {
                children[i].grow();
            }
        } else {
            let mut children: Vec<Node> = vec![];
            let board = white_respond(&self.board);
            let pieces: Vec<Pos> = board.get_all(Black);
            for from in pieces {
                let moves = allow_moves(&board, from, Black);
                for to in moves {
                    let mut option = board.clone();
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
            let mut list: Vec<Board> = vec![];
            for i in 0..children.len() {
                let grand_children: Vec<Board> = children[i].dig();
                for j in 0..grand_children.len() {
                    list.push(grand_children[j]);
                }
            }
            return list;
        } else {
            return vec![self.board];
        }
    }
}

struct Opt {
    from: Pos,
    to: Pos,
    tree: Node,
}

fn cpu_turn(game: &mut Game) {
    let mut options: Vec<Opt> = get_opts(&game.brd);

    for i in 0..options.len() {
        options[i].tree.grow();
        options[i].tree.grow();
        // options[i].tree.grow();
    }

    let choice: Move = pick_option(&options);
    execute_move(game, choice.from, choice.to);
    game.cur = choice.to;
}

fn main() -> io::Result<()> {
    let mut game = NEW_BOARD.clone();
    enable_raw_mode()?;
    game.print();
    while handle_event(&mut game) {
        // game.print();
        // cpu_turn(&mut game);
        game.print();
    }
    disable_raw_mode()?;
    Ok(())
}