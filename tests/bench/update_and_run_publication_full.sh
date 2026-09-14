#!/usr/bin/env bash

set -euo pipefail

REPO="${CEREBRONEXUS_REPO:-/home/xuesong/Projects/CerebroNexus}"
REMOTE="${CEREBRONEXUS_REMOTE:-origin}"
BRANCH="paper/real-data-benchmark"
STATE_DIR="${BENCH_STATE_DIR:-/home/xuesong/.cache/cerebronexus-benchmark/runner}"
SOURCE_CACHE="${BENCH_SOURCE_CACHE:-/home/xuesong/.cache/cerebronexus-benchmark/sources}"
SCRATCH_PARENT="${BENCH_SCRATCH_PARENT:-/home/xuesong/.cache/cerebronexus-benchmark/scratch}"
RESULT_ROOT="$REPO/tests/bench/result/publication-full"
PID_FILE="$STATE_DIR/publication-full.pid"
EXIT_FILE="$STATE_DIR/publication-full.exit"
LOG_FILE="$STATE_DIR/publication-full.log"
SCRIPT="$REPO/tests/bench/update_and_run_publication_full.sh"

info() { printf '[INFO] %s\n' "$*"; }
run() { printf '[RUN]  %s\n' "$*"; }
ok() { printf '[OK]   %s\n' "$*"; }
fail() { printf '[FAIL] %s\n' "$*" >&2; exit 1; }

read_pid() {
  local pid=""
  if [ -f "$PID_FILE" ]; then
    read -r pid < "$PID_FILE" || true
  fi
  case "$pid" in
    ''|*[!0-9]*) return 1 ;;
    *) printf '%s\n' "$pid" ;;
  esac
}

is_running() {
  local pid
  pid=$(read_pid) || return 1
  kill -0 "$pid" 2>/dev/null
}

show_status() {
  local pid code
  if [ -f "$EXIT_FILE" ]; then
    read -r code < "$EXIT_FILE" || code="unknown"
    if [ "$code" = "0" ]; then
      ok "benchmark 已完成，exit=0"
    else
      printf '[FAIL] benchmark 已停止，exit=%s\n' "$code" >&2
    fi
  elif is_running; then
    pid=$(read_pid)
    ok "benchmark 正在后台运行，PID=$pid"
    ps -p "$pid" -o pid=,etime=,stat=,cmd= || true
  else
    info "benchmark 当前未运行"
  fi
  info "日志：$LOG_FILE"
}

worker() {
  local code tmp_exit
  run "benchmark 已进入后台，study=$BENCH_STUDY_ID"
  run "完整日志写入 $LOG_FILE"
  set +e
  nix-shell "$REPO/default.nix" -A shell \
    --run "$REPO/tests/bench/run_publication_full.sh"
  code=$?
  set -e
  tmp_exit="$EXIT_FILE.$$"
  printf '%s\n' "$code" > "$tmp_exit"
  mv -f -- "$tmp_exit" "$EXIT_FILE"
  if [ "$code" -eq 0 ]; then
    ok "benchmark 完成，结果位于 $RESULT_ROOT/runs/$BENCH_STUDY_ID"
  else
    printf '[FAIL] benchmark 失败，exit=%s；查看 %s\n' "$code" "$LOG_FILE" >&2
  fi
  exit "$code"
}

case "${1:-run}" in
  status)
    mkdir -p "$STATE_DIR"
    show_status
    exit 0
    ;;
  _worker)
    worker
    ;;
  run)
    ;;
  *)
    fail "用法：$0 [run|status]"
    ;;
esac

[ -d "$REPO/.git" ] || fail "仓库不存在：$REPO"
mkdir -p "$STATE_DIR" "$SOURCE_CACHE" "$SCRATCH_PARENT"
for external_dir in "$STATE_DIR" "$SOURCE_CACHE" "$SCRATCH_PARENT"; do
  external_path=$(cd "$external_dir" && pwd -P)
  case "$external_path/" in
    "$REPO/"*) fail "运行状态、source cache 和 scratch 必须位于仓库外：$external_path" ;;
  esac
done

if is_running; then
  show_status
  fail "已有 benchmark 在运行，未更新或清理任何内容"
fi
if command -v pgrep >/dev/null 2>&1; then
  old_pids=$(pgrep -f "$REPO/tests/bench/run_publication_full.sh" || true)
  [ -z "$old_pids" ] || fail "检测到未受本脚本管理的 benchmark 进程：$old_pids"
fi

tracked=$(git -C "$REPO" status --porcelain --untracked-files=no)
[ -z "$tracked" ] || {
  printf '%s\n' "$tracked" >&2
  fail "仓库有已跟踪文件改动，拒绝覆盖"
}

run "从 $REMOTE 获取 $BRANCH"
git -C "$REPO" fetch "$REMOTE" "$BRANCH"
if git -C "$REPO" show-ref --verify --quiet "refs/heads/$BRANCH"; then
  git -C "$REPO" switch "$BRANCH"
else
  git -C "$REPO" switch --track -c "$BRANCH" "$REMOTE/$BRANCH"
fi

before=$(git -C "$REPO" rev-parse HEAD)
git -C "$REPO" merge --ff-only "$REMOTE/$BRANCH"
after=$(git -C "$REPO" rev-parse HEAD)
remote_sha=$(git -C "$REPO" rev-parse "$REMOTE/$BRANCH")
[ "$after" = "$remote_sha" ] || fail "本地分支含远端没有的提交，拒绝运行非标准代码"
if [ "$before" = "$after" ]; then
  ok "代码已经是最新版本：${after:0:12}"
else
  ok "代码已更新：${before:0:12} -> ${after:0:12}"
fi

command -v nix-shell >/dev/null 2>&1 || fail "未找到 nix-shell；旧结果尚未清理"

run "清理旧 benchmark 结果"
for target in \
  "$RESULT_ROOT" \
  "$REPO/tests/bench/study-work" \
  "$REPO/tests/bench/scratch"; do
  if [ -e "$target" ]; then
    rm -rf -- "$target"
    ok "已清理：$target"
  else
    info "无需清理：$target"
  fi
done
rm -f -- "$PID_FILE" "$EXIT_FILE" "$LOG_FILE"
ok "source cache 已保留：$SOURCE_CACHE"

dirty=$(git -C "$REPO" status --porcelain --untracked-files=all)
[ -z "$dirty" ] || {
  printf '%s\n' "$dirty" >&2
  fail "清理后仓库仍不干净，拒绝启动"
}

storage=$(df -P "$SCRATCH_PARENT" | awk 'NR == 2 {printf "host=%s; device=%s; mount=%s", host, $1, $6}' host="$(hostname)")
export BENCH_THREADS="${BENCH_THREADS:-1}"
export BENCH_SOURCE_CACHE="$SOURCE_CACHE"
export BENCH_SCRATCH_PARENT="$SCRATCH_PARENT"
export BENCH_RESULT_ROOT="$RESULT_ROOT"
export BENCH_STORAGE_DESCRIPTION="${BENCH_STORAGE_DESCRIPTION:-$storage}"
export BENCH_STUDY_ID="$(date -u +%Y%m%dT%H%M%SZ)-${after:0:12}-publication-full"

run "后台启动完整双数据集 benchmark"
nohup "$SCRIPT" _worker > "$LOG_FILE" 2>&1 < /dev/null &
pid=$!
printf '%s\n' "$pid" > "$PID_FILE"
sleep 3
if [ -f "$EXIT_FILE" ] || ! kill -0 "$pid" 2>/dev/null; then
  tail -n 40 "$LOG_FILE" >&2 || true
  fail "benchmark 启动后立即退出"
fi

ok "benchmark 已启动，PID=$pid"
info "study：$BENCH_STUDY_ID"
info "实时日志：tail -f $LOG_FILE"
info "状态检查：bash $SCRIPT status"
