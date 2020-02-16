resource "google_service_account" "buildkite_agent" {
  account_id   = "buildkite-agent"
  display_name = "Buildkite agent"
}
