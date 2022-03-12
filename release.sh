set -e

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <version> <next-version>"
  exit 2
fi

version="$1"
next_version="$2"

#if ! cargo install --list | grep 'cargo-edit: v0.8.0' ; then
#  echo "Install cargo-edit: cargo install --version 0.8.0 --locked --features vendored-openssl cargo-edit"
#  exit 1
#fi

if ! git config --get user.email > /dev/null && git config --get user.name > /dev/null; then
  echo "Set git email and name: git config --global user.name <name> && git config --global user.email <email>"
  exit 1
fi

head CHANGELOG.md

echo "Using version ${version} and next version ${next_version}"
echo "Confirm changelog looks good (y/n)? "
read good

if [ "${good}" != "y" ]; then
  exit 1
fi

make_version() {
  local readonly version="$1"

  # requires cargo-edit

  # set our workspace versions
  cargo set-version --workspace "${version}"
  # todo: why isn't this changed as part of set-version???
  cargo add --manifest-path "$(pwd)/ts-bindgen-macro/Cargo.toml" --path ts-bindgen-gen "ts-bindgen-gen@${version}"

  # set our end_to_end_template version
  cp -r ts-bindgen-rt ts-bindgen-gen/tests/end_to_end_template/
  cargo add --manifest-path "$(pwd)/ts-bindgen-gen/tests/end_to_end_template/Cargo.toml" --path ts-bindgen-gen/tests/end_to_end_template/ts-bindgen-rt "ts-bindgen-rt@${version}"
  cargo generate-lockfile --manifest-path "$(pwd)/ts-bindgen-gen/tests/end_to_end_template/Cargo.toml"
  rm -rf ts-bindgen-gen/tests/end_to_end_template/ts-bindgen-rt

  # set our ts-bindgen-web version
  cargo set-version --manifest-path "$(pwd)/ts-bindgen-web/Cargo.toml" "${version}"
  cargo add --manifest-path "$(pwd)/ts-bindgen-web/Cargo.toml" --no-default-features --path ts-bindgen "ts-bindgen@${version}"
  cargo generate-lockfile --manifest-path "$(pwd)/ts-bindgen-web/Cargo.toml"

  # set paperjs example version
  cargo set-version --manifest-path "$(pwd)/ts-bindgen/examples/paperjs/Cargo.toml" "${version}"
  cargo add --manifest-path "$(pwd)/ts-bindgen/examples/paperjs/Cargo.toml" --path ts-bindgen-rt "ts-bindgen-rt@${version}"
  cargo generate-lockfile --manifest-path "$(pwd)/ts-bindgen/examples/paperjs/Cargo.toml"

  echo "Update README.md versions"

  echo "Ready to proceed (y/n)? "
  read good

  if [ "${good}" != "y" ]; then
    exit 1
  fi

  cargo test

  if [[ "${version}" =~ ".*-pre" ]]; then
    git commit -a -m "chore: prepare ${version}"
  else
    git commit -a -m "chore: release ${version}"
    git tag "v${version}"
  fi
}

make_version "${version}"
make_version "${next_version}"

echo "Run: git checkout v${version} && cd ts-bindgen-web && ./build && wrangler publish"
