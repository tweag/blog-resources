data "template_file" "buildkite_nixos_startup" {
  template = "${file("${path.module}/files/buildkite_nixos_startup.sh")}"

  vars = {
    buildkite_agent_token = "${secret_resource.buildkite_agent_token.value}"
    nix_signing_key = "${secret_resource.nix_signing_key.value}"
  }
}

resource "google_compute_instance_template" "buildkite_nixos" {
  name_prefix  = "buildkite-nixos-"
  machine_type = "n1-standard-8"

  disk {
    boot         = true
    disk_size_gb = 100
    source_image = "${module.nixos_image_custom.self_link}"
  }

  metadata_startup_script = "${data.template_file.buildkite_nixos_startup.rendered}"

  network_interface {
    network = "default"

    access_config = {}
  }

  metadata {
    enable-oslogin = true
  }

  service_account {
    email = "${google_service_account.buildkite_agent.email}"

    scopes = [
      "compute-ro",
      "logging-write",
      "storage-rw",
    ]
  }

  scheduling {
    automatic_restart   = false
    on_host_maintenance = "TERMINATE"
    preemptible         = true
  }

  lifecycle {
    create_before_destroy = true
  }
}

resource "google_compute_instance_group_manager" "buildkite_nixos" {
  provider           = "google-beta"
  name               = "buildkite-nixos"
  base_instance_name = "buildkite-nixos"
  target_size        = "3"
  zone               = "${local.zone}"

  version {
    name              = "buildkite_nixos"
    instance_template = "${google_compute_instance_template.buildkite_nixos.self_link}"
  }

  update_policy {
    type                  = "PROACTIVE"
    minimal_action        = "REPLACE"
    max_unavailable_fixed = 1
  }
}
