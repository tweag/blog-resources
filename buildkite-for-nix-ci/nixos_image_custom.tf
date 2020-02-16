resource "google_storage_bucket" "nixos_image" {
  name     = "buildkite-nixos-image-bucket-name"
  location = "EU"
}

module "nixos_image_custom" {
  source      = "git::https://github.com/tweag/terraform-nixos.git//google_image_nixos_custom?ref=40fedb1fae7df5bd7ad9defdd71eb06b7252810f"
  bucket_name = "${google_storage_bucket.nixos_image.name}"
  nixos_config = "${path.module}/nixos-config.nix"
}
