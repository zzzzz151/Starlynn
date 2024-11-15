use std::ops::{Index, IndexMut};
use std::time::{Instant, Duration};
use std::fmt;
use crate::chess_move::ChessMove;
use crate::pos_state::GameState;
use crate::position::Position;
use crate::node::Node;

#[derive(Debug)]
pub struct Tree {
    tree: Vec<Node>
}

impl Tree {
    pub fn new(mib: usize) -> Self {
        assert!(mib > 0, "{mib}");

        let bytes: usize = mib * 1024 * 1024;
        let node_bytes = std::mem::size_of::<Node>();

        let nodes = bytes / node_bytes;
        assert!(nodes > 256 && nodes as i32 <= i32::MAX, "{nodes}");

        println!("info string Tree size {mib} MiB ({nodes} nodes)");

        Self { tree: Vec::with_capacity(nodes) }
    }

    pub fn search(&mut self,
        root_pos: &mut Position,
        start_time: &Instant,
        search_duration: &Duration,
        max_depth: u8,
        max_nodes: u64,
        print_info: bool) -> (Option<ChessMove>, u64)
    {
        self.tree.clear(); // doesn't change vector capacity

        if !root_pos.has_move() { return (None, 0); }

        // Root node
        self.tree.push(Node {
            right_sibling_idx: -1,
            first_child_idx: -1,
            game_state: GameState::Ongoing,
            num_moves: u8::MAX,
            visits: 0,
            total_score: 0.0,
            mov: 0
        });

        let mut nodes: u64 = 0;
        let mut depths_sum: u64 = 0;
        let mut max_depth_reached: u64 = 0;
        let mut last_reported_depth: u64 = 0;

        let mut path: Vec<usize> = Vec::with_capacity(256);
        path.push(0); // root node idx

        let wdl = self.simulate(0, root_pos);
        self.backprop(wdl, &mut path);
        self.expand(0, root_pos);

        while self.tree.len() <= self.tree.capacity() - 256
        {
            self.make_iteration(root_pos, &mut path);

            nodes += 1;

            let this_depth = path.len() as u64 - 1;
            depths_sum += this_depth;

            if this_depth > max_depth_reached {
                max_depth_reached = this_depth;
            }

            let avg_depth = depths_sum as f64 / (nodes as f64);
            let avg_depth_rounded = avg_depth.round() as u64;

            if avg_depth >= (max_depth as f64) || nodes >= max_nodes {
                break;
            }

            if (nodes == 1 || nodes % 512 == 0) && start_time.elapsed() >= *search_duration {
                break;
            }

            if print_info && avg_depth_rounded != last_reported_depth
            {
                self.uci_info(avg_depth_rounded, max_depth_reached, nodes, &start_time);
                last_reported_depth = avg_depth_rounded;
            }
        }

        if print_info {
            let avg_depth = depths_sum as f64 / (nodes as f64);
            self.uci_info(avg_depth.round() as u64, max_depth_reached, nodes, start_time);
        }

        let best_root_child: &Node = &self[self.most_visits_root_child()];
        (Some(best_root_child.mov.into()), nodes)
    }

    fn make_iteration(&mut self, root_pos: &mut Position, path: &mut Vec<usize>)
    {
        path.clear();
        path.push(0); // root node idx

        let mut pos = root_pos.clone();

        let mut node_idx = self.select(0, &mut pos, path);

        if self[node_idx].game_state == GameState::Unknown {
            self[node_idx].game_state = pos.game_state();
        }

        if self[node_idx].game_state == GameState::Ongoing && self[node_idx].visits > 0 {
            node_idx = self.expand_to(node_idx, &mut pos, path);
        }

        let wdl = self.simulate(node_idx, &mut pos);

        self.backprop(wdl, &path);
    }

    fn select(&mut self, node_idx: usize, pos: &mut Position, path: &mut Vec<usize>) -> usize
    {
        let node: &Node = &self[node_idx];

        debug_assert!(node.game_state != GameState::Unknown);

        // Terminal or leaf?
        if node.game_state != GameState::Ongoing || node.first_child_idx == -1 {
            return node_idx;
        }

        let mut best_child_idx = node.first_child_idx as usize;
        let mut best_puct = f32::MIN;

        let mut child_idx: i32 = node.first_child_idx;

        while child_idx != -1 {
            let child: &Node = &self[child_idx];

            if child.visits == 0 { return node_idx; }

            let puct = node.puct(child);

            if puct > best_puct {
                best_child_idx = child_idx as usize;
                best_puct = puct;
            }

            child_idx = child.right_sibling_idx;
        }

        pos.make_move(self[best_child_idx].mov.into());
        path.push(best_child_idx);

        self.select(best_child_idx, pos, path)
    }

    fn expand(&mut self, node_idx: usize, pos: &mut Position)
    {
        debug_assert!(self[node_idx].num_moves == u8::MAX);
        debug_assert!(self[node_idx].first_child_idx == -1);
        debug_assert!(self[node_idx].visits == 1);

        let moves = pos.moves(false);
        debug_assert!(moves.len() > 0);

        self[node_idx].num_moves = moves.len() as u8;
        self[node_idx].first_child_idx = self.tree.len() as i32;

        for (i, mov) in moves.iter().enumerate()
        {
            let i = i as i32;

            self.tree.push(Node {
                right_sibling_idx: self[node_idx].first_child_idx + i + 1,
                first_child_idx: -1,
                game_state: GameState::Unknown,
                num_moves: u8::MAX,
                visits: 0,
                total_score: 0.0,
                mov: u16::from(*mov)
            });
        }

        debug_assert!(self.tree.len() <= self.tree.capacity());

        self.tree.last_mut().unwrap().right_sibling_idx = -1;
    }

    fn expand_to(&mut self, node_idx: usize, pos: &mut Position, path: &mut Vec<usize>) -> usize
    {
        debug_assert!(pos.has_move());

        if node_idx > 0 && self[node_idx].visits == 1 {
            self.expand(node_idx, pos);
        }

        let mut child_idx = self[node_idx].first_child_idx;

        while child_idx != -1 && self[child_idx].visits > 0 {
            child_idx = self[child_idx].right_sibling_idx;
        }

        debug_assert!(self[child_idx].visits == 0);

        pos.make_move(self[child_idx].mov.into());
        path.push(child_idx as usize);

        child_idx as usize
    }

    fn simulate(&mut self, node_idx: usize, pos: &mut Position) -> f32
    {
        let node: &mut Node = &mut self[node_idx];

        if node.game_state == GameState::Unknown {
            node.game_state = pos.game_state();
        }

        match node.game_state {
            GameState::Draw => 0.5,
            GameState::Lost => 0.0,
            GameState::Ongoing =>
                1.0 / (1.0 + (-(pos.eval() as f32) / 400.0).exp()),
            _ => panic!("Invalid game state")
        }
    }

    fn backprop(&mut self, mut wdl: f32, path: &Vec<usize>)
    {
        debug_assert!(wdl >= 0.0 && wdl <= 1.0, "{wdl}");

        for node_idx in path.iter().rev()
        {
            wdl = 1.0 - wdl;
            self[*node_idx].visits += 1;
            self[*node_idx].total_score += wdl;
        }
    }

    fn uci_info(&self, depth: u64, seldepth: u64, nodes: u64, start_time: &Instant)
    {
        let elapsed_ms = start_time.elapsed().as_millis();
        let nps = nodes * 1000 / (elapsed_ms.max(1) as u64);

        let most_visits_root_child: &Node = &self[self.most_visits_root_child()];
        let mov: ChessMove = most_visits_root_child.mov.into();

        let mut str = format!("info depth {depth} seldepth {seldepth}");
        str += &format!(" score cp {:.0}", most_visits_root_child.q() * 100.0);
        str += &format!(" nodes {nodes} nps {nps} time {elapsed_ms}");
        str += &format!(" pv {}", mov);

        str += &format!("\ninfo string Nodes allocated {}/{}",
            self.tree.len(), self.tree.capacity());

        println!("{}", str);
    }

    fn most_visits_root_child(&self) -> usize
    {
        let mut best_child_idx = self[0 as usize].first_child_idx;
        let mut child_idx = self[best_child_idx].right_sibling_idx;

        while child_idx != -1 {
            if self[child_idx].visits > self[best_child_idx].visits {
                best_child_idx = child_idx;
            }

            child_idx = self[child_idx].right_sibling_idx;
        }

        best_child_idx as usize
    }

    fn print_tree(&self, f: &mut fmt::Formatter<'_>, node_idx: usize, depth: usize) -> fmt::Result
    {
        if node_idx >= self.tree.len() { return Ok(()); }

        let node = &self.tree[node_idx];
        let mut child_idx = node.first_child_idx;

        writeln!(f, "{}{}: {}", "  ".repeat(depth), node_idx, node)?;

        while child_idx != -1 {
            self.print_tree(f, child_idx as usize, depth + 1)?;
            child_idx = self.tree[child_idx as usize].right_sibling_idx;
        }

        Ok(())
    }
}

impl fmt::Display for Tree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        println!("Nodes allocated {}/{}", self.tree.len(), self.tree.capacity());
        self.print_tree(f, 0, 0)
    }
}

impl Index<usize> for Tree {
    type Output = Node;

    fn index(&self, index: usize) -> &Self::Output
    {
        debug_assert!(index < self.tree.len());
        unsafe { self.tree.get_unchecked(index) }
    }
}

impl IndexMut<usize> for Tree
{
    fn index_mut(&mut self, index: usize) -> &mut Self::Output
    {
        debug_assert!(index < self.tree.len());
        unsafe { self.tree.get_unchecked_mut(index) }
    }
}

impl Index<i32> for Tree {
    type Output = Node;

    fn index(&self, index: i32) -> &Self::Output
    {
        debug_assert!(index >= 0 && (index as usize) < self.tree.len());
        unsafe { self.tree.get_unchecked(index as usize) }
    }
}

impl IndexMut<i32> for Tree
{
    fn index_mut(&mut self, index: i32) -> &mut Self::Output
    {
        debug_assert!(index >= 0 && (index as usize) < self.tree.len());
        unsafe { self.tree.get_unchecked_mut(index as usize) }
    }
}
