#!/usr/bin/env bash

set -euo pipefail

readonly WORK_DIR="$(mktemp --directory)"
readonly SCRIPTS_DIR="scripts"

readonly TRIALS="${TRIALS-50}"
readonly SCRIPTS_PER_TRIAL="${SCRIPTS_PER_TRIAL-25}"

trap 'rm -rf "${WORK_DIR}"' EXIT

log() {
  >&2 printf "%s\n" "$*"
}

result() {
  # TSV data output
  printf "%s" "$1"
  shift
  printf "\t%s" "$@"
  printf "\n"
}

get-scripts() {
  # Find all .sh files containing something that looks like a Bash shebang
  find "${SCRIPTS_DIR}" \
    -name "*.sh" \
    -type f \
    -exec grep \
      --files-with-matches \
      --perl-regexp \
      '^#!.*\bbash\b' \
      {} +
}

whitelist() {
  # Filter files by those that Topiary can currently handle
  local topiary_cmd="$1"

  local -i count=0
  while read -r input; do
    if ${topiary_cmd} < "${input}" >/dev/null 2>&1; then
      printf "%s\n" "${input}"
    fi

    : $(( count++ ))
    if ! (( count % 25 )); then
      log "Processed ${count} scripts..."
    fi
  done
}

timestamp() {
  # High-resolution (nanosecond) timestamp
  date --utc +%s%N
}

benchmark() {
  # Run the formatting command with the input and return its runtime
  local cmd="$1"
  local input="$2"
  local output="NA"

  local start="$(timestamp)"
  if ${cmd} < "${input}" >/dev/null 2>&1; then
    local finish="$(timestamp)"
    output="$(( finish - start ))"
  fi

  printf "%s" "${output}"
}

main() {
  local topiary_cmd="$1"
  local shfmt_cmd="$2"

  log "Establishing whitelist of input scripts..."
  local fofn="${WORK_DIR}/fofn"
  get-scripts | whitelist "${topiary_cmd}" > "${fofn}"
  log "Whitelist of $(wc -l < "${fofn}") scripts created."

  log "Performing ${TRIALS} trials..."
  result "trial" "size" "cat" "topiary.full" "topiary.noidem" "shfmt"

  local -i idx="${TRIALS}"
  while (( idx-- )); do
    # Shuffle inputs to invalidate any caching and concatenate the first
    # ${SCRIPTS_PER_TRIAL} scripts together
    local trial="${WORK_DIR}/trial-${idx}"
    shuf "${fofn}" \
    | head --lines "${SCRIPTS_PER_TRIAL}" \
    | xargs sed \
      --separate \
      --expression '$s/$/\n/' \
    > "${trial}"
    local size="$(wc --bytes < "${trial}")"

    # Warm up the filesystem cache
    for _ in {1..5}; do
      cat "${trial}"
    done >/dev/null

    local cat="$(benchmark "cat" "${trial}")"
    local topiary="$(benchmark "${topiary_cmd}" "${trial}")"
    local topiary_noidem="$(benchmark "${topiary_cmd} --skip-idempotence" "${trial}")"
    local shfmt="$(benchmark "${shfmt_cmd}" "${trial}")"

    result "$(( TRIALS - idx ))" "${size}" "${cat}" "${topiary}" "${topiary_noidem}" "${shfmt}"
  done
}

main "$@"
