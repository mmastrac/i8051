use std::sync::Arc;

use crate::Controller;
use tokio::sync::{Mutex, mpsc, oneshot};
use tokio::task::JoinHandle;
use tokio::time::{Duration, Instant};

const QUIESCENT: Duration = Duration::from_secs(10);
const MAX_WAIT: Duration = Duration::from_secs(30);

/// Save after edits go quiet, or on shutdown.
pub fn spawn(
    controller: Arc<Mutex<Controller>>,
    edits: mpsc::UnboundedReceiver<()>,
    shutdown: oneshot::Receiver<()>,
) -> JoinHandle<()> {
    tokio::spawn(run(controller, edits, shutdown))
}

async fn run(
    controller: Arc<Mutex<Controller>>,
    mut edits: mpsc::UnboundedReceiver<()>,
    mut shutdown: oneshot::Receiver<()>,
) {
    let mut burst: Option<Instant> = None;
    loop {
        match burst {
            None => {
                tokio::select! {
                    edit = edits.recv() => match edit {
                        Some(()) => burst = Some(Instant::now()),
                        None => break,
                    },
                    _ = &mut shutdown => break,
                }
            }
            Some(start) => {
                let deadline = (Instant::now() + QUIESCENT).min(start + MAX_WAIT);
                tokio::select! {
                    _ = tokio::time::sleep_until(deadline) => {
                        save(&controller).await;
                        burst = None;
                    }
                    edit = edits.recv() => {
                        if edit.is_none() {
                            break;
                        }
                    }
                    _ = &mut shutdown => break,
                }
            }
        }
    }

    while edits.try_recv().is_ok() {
        burst.get_or_insert_with(Instant::now);
    }
    if burst.is_some() {
        save(&controller).await;
    }
}

async fn save(controller: &Arc<Mutex<Controller>>) {
    match controller.lock().await.save() {
        Ok(report) => tracing::debug!(
            path = %report.path.display(),
            records = report.commands,
            diff = report.diff,
            "autosave"
        ),
        Err(e) => tracing::warn!(error = %e, "autosave failed"),
    }
}
