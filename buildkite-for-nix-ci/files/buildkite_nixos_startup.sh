# workaround https://github.com/NixOS/nixpkgs/issues/42344
chown root:keys /run/keys
chmod 750 /run/keys
umask 037
echo "${buildkite_agent_token}" > /run/keys/buildkite-agent-token
chown root:keys /run/keys/buildkite-agent-token
umask 077
echo '${nix_signing_key}' > /run/keys/nix_signing_key
chown root:keys /run/keys/nix-signing-key

cat <<EOF > /etc/nix/upload-to-cache.sh
#!/bin/sh

set -eu
set -f # disable globbing
export IFS=' '

echo "Uploading paths" $OUT_PATHS
exec nix copy --to http://localhost:3000?secret-key=/run/keys/nix_signing_key \$OUT_PATHS
EOF
chmod +x /etc/nix/upload-to-cache.sh
