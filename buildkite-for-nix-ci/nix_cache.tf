resource "google_storage_bucket" "nix_cache_bucket" {
  name     = "nix-cache-bucket-name"
  location = "EU"
  force_destroy = true
  retention_policy {
    retention_period = 7889238 # three months
  }
}

resource "google_storage_bucket_iam_member" "buildkite_nix_cache_writer" {
  bucket = "${google_storage_bucket.nix_cache_bucket.name}"
  role = "roles/storage.objectAdmin"
  member = "serviceAccount:${google_service_account.buildkite_agent.email}"
}

resource "google_storage_bucket_iam_member" "buildkite_nix_cache_reader" {
  bucket = "${google_storage_bucket.nix_cache_bucket.name}"
  role   = "roles/storage.objectViewer"
  member = "allUsers"
}
