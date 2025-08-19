use std::num::{NonZeroU32, NonZeroU64};
use std::time::{Duration, Instant};

pub const HARD_TIME_PERCENTAGE: f64 = 0.75;
pub const SOFT_TIME_PERCENTAGE: f64 = 0.05;

pub struct SearchLimits {
    pub start_time: Instant,
    pub max_depth: Option<NonZeroU32>,
    pub max_nodes: Option<NonZeroU64>,
    pub max_soft_duration: Option<Duration>,
    pub max_duration: Option<Duration>,
    pub(crate) max_duration_hit: bool,
}

impl SearchLimits {
    pub const fn new(
        start_time: &Instant,
        max_depth: Option<NonZeroU32>,
        max_nodes: Option<NonZeroU64>,
        max_soft_duration: Option<Duration>,
        max_duration: Option<Duration>,
    ) -> Self {
        SearchLimits {
            start_time: *start_time,
            max_depth,
            max_nodes,
            max_soft_duration,
            max_duration,
            max_duration_hit: false,
        }
    }

    pub fn update_max_duration_hit<const IS_ROOT: bool>(
        &mut self,
        root_depth: i32,
        nodes: u64,
    ) -> bool {
        if root_depth <= 1 {
            self.max_duration_hit = false;
        } else if IS_ROOT || nodes % 1024 == 0 {
            self.max_duration_hit = self
                .max_duration
                .is_some_and(|max_dur| self.start_time.elapsed() >= max_dur);
        }

        self.max_duration_hit
    }
}
