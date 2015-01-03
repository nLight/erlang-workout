use std::comm::{channel, Sender, Receiver};
use std::thread::Thread;
use std::fmt;
use std::io;

enum Player { X, O, E }

impl std::fmt::Show for Player {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Player::X => write!(f, "X"),
      Player::O => write!(f, "O"),
      Player::E => write!(f, " ")
    }
  }
}

impl PartialEq for Player {
  fn eq(&self, other: &Player) -> bool {
    match (self, other) {
      (&Player::X, &Player::X) | (&Player::O, &Player::O) | (&Player::E, &Player::E) => true,
      _ => false
    }
  }
}

struct Move {
  player: Player,
  x: uint,
  y: uint
}

impl Move {
  fn from_str(s: &str) -> Option<Self> {
    Some(Move {player: Player::X, x: 0u, y: 0u})
  }
}

impl std::fmt::Show for Move {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{} moves to ({},{})", self.player, self.x, self.y)
  }
}

fn check_position(game: &[Player; 9], x: uint, y: uint) -> bool {
  match game[x + y * 3u] {
    Player::E => true,
    _ => false
  }
}

fn set_player(game: &mut [Player; 9], player: &Player, x: uint, y: uint) {
  match *player {
    Player::X => game[x+y*3u] = Player::X,
    Player::O => game[x+y*3u] = Player::O,
    Player::E => ()
  }

}

fn check_winner(player: &Player, game: &[Player; 9]) -> bool {
  // Couldn't come up with a better solution
  match *player {
    Player::X => (
      match *game {
        // Horizontals
        [Player::X, Player::X, Player::X, _, _, _, _, _, _ ] => true,
        [_, _, _, Player::X, Player::X, Player::X, _, _, _ ] => true,
        [_, _, _, _, _, _, Player::X, Player::X, Player::X ] => true,

        // // Verticals
        [Player::X, _, _, Player::X, _, _, Player::X, _, _ ] => true,
        [_, Player::X, _, _, Player::X, _, _, Player::X, _ ] => true,
        [_, _, Player::X, _, _, Player::X, _, _, Player::X ] => true,

        // // Diagonals
        [Player::X, _, _, _, Player::X, _, _, _, Player::X ] => true,
        [_, _, Player::X, _, Player::X, _, Player::X, _, _ ] => true,

        _ => false
      }
    ),
    Player::O => (
      match *game {
        // Horizontals
        [Player::O, Player::O, Player::O, _, _, _, _, _, _ ] => true,
        [_, _, _, Player::O, Player::O, Player::O, _, _, _ ] => true,
        [_, _, _, _, _, _, Player::O, Player::O, Player::O ] => true,

        // // Verticals
        [Player::O, _, _, Player::O, _, _, Player::O, _, _ ] => true,
        [_, Player::O, _, _, Player::O, _, _, Player::O, _ ] => true,
        [_, _, Player::O, _, _, Player::O, _, _, Player::O ] => true,

        // // Diagonals
        [Player::O, _, _, _, Player::O, _, _, _, Player::O ] => true,
        [_, _, Player::O, _, Player::O, _, Player::O, _, _ ] => true,

        _ => false
      }
    ),
    Player::E => false
  }
}

fn game_loop(_sender: &Sender<int>, receiver: &Receiver<Move>) {
  let mut game = [Player::E, Player::E, Player::E, Player::E, Player::E, Player::E, Player::E, Player::E, Player::E];
  let mut next_move: Move;
  let mut prev_player = Player::E;

  loop {
    next_move = receiver.recv();

    if next_move.player == prev_player {
      println!("On the move \"{}\". Wrong player! Last player was: {}", next_move, prev_player);
      continue;
    }

    if check_position(&game, next_move.x, next_move.y) {
      set_player(&mut game, &next_move.player, next_move.x, next_move.y);
      if check_winner(&next_move.player, &game) {
        println!("{} is a winner!", next_move.player);
        break;
      };

      prev_player = next_move.player;
    }
    else {
      println!("Wrong move!");
    }
  }
}

fn main() {
  let mut reader = io::stdin();

  let (from_parent_sender, from_parent_receiver) = channel();
  let (from_child_sender, _from_child_receiver) = channel();

  Thread::spawn(move || {
    game_loop(&from_child_sender, &from_parent_receiver);
  }).detach();;

  loop {
    let input = reader.read_line().ok().expect("Failed to read line");
    let parsed_move: Option<Move> = Move::from_str(input.as_slice());

    match parsed_move {
      Some(next_move) => from_parent_sender.send(next_move),
      None => println!("Wrong input!")
    }
  }
}

// Tests

#[test]
fn test_game_loop() {
  let (from_parent_sender, from_parent_receiver) = channel();
  let (from_child_sender, _from_child_receiver) = channel();

  Thread::spawn(move || {
    game_loop(&from_child_sender, &from_parent_receiver);
  }).detach();;

  // Test X wins
  from_parent_sender.send(Move {player: Player::X, x: 0u, y: 0u});
  from_parent_sender.send(Move {player: Player::O, x: 0u, y: 1u});
  from_parent_sender.send(Move {player: Player::X, x: 1u, y: 1u});
  from_parent_sender.send(Move {player: Player::O, x: 0u, y: 2u});
  from_parent_sender.send(Move {player: Player::X, x: 2u, y: 2u});
}

#[test]
fn test_check_winner() {
  let game = [Player::X, Player::X, Player::X, Player::O, Player::E, Player::E, Player::E, Player::E, Player::E];
  assert!(check_winner(&Player::X, &game));
  assert!(check_winner(&Player::O, &game) == false);
}

#[test]
fn test_set_player() {
  let mut game = [Player::E, Player::X, Player::E, Player::O, Player::E, Player::E, Player::E, Player::E, Player::E];

  set_player(&mut game, &Player::O, 2u, 2u);

  match game[8u] {
    Player::O => (),
    _ => panic!("No element found!")
  }
}

#[test]
fn test_check_position() {
  let game = [Player::E, Player::X, Player::E, Player::O, Player::E, Player::E, Player::E, Player::E, Player::E];
  assert!(check_position(&game, 0u, 0u));
  assert!(check_position(&game, 1u, 0u) == false);
}
