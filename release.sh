set -e

version="$1"
next_version="$2"

cat CHANGELOG.md

echo "Confirm changelog looks good (y/n)? "
read good

if [ "${good}" != "y" ]; then
  exit 1
fi

cargo workspaces version --yes --all --no-git-push --no-git-tag --exact custom "${version}"
echo "Update README.md, ts-bindgen-web/Cargo.toml, ts-bindgen-gen/tests/end_to_end_template/Cargo.toml, and ts-bindgen/examples/paper/Cargo.toml to use ${version}, cargo generate-lockfile, and git commit --amend"

echo "Ready to proceed (y/n)? "
read good

if [ "${good}" != "y" ]; then
  exit 1
fi

git tag "v${version}"

cargo workspaces version --yes --all --no-git-push --exact --no-git-tag --force '*' custom "${next_version}"

echo "Update ts-bindgen-web, Cargo.toml, and README.md to use ${next_version}, cd ts-bindgen-web, cargo generate-lockfile, and git commit --amend"
echo "Update README.md, ts-bindgen-web/Cargo.toml, ts-bindgen-gen/tests/end_to_end_template/Cargo.toml, and ts-bindgen/examples/paper/Cargo.toml to use ${next_version}, cargo generate-lockfile, and git commit --amend"
echo "Run: git checkout -b scratch v${version} && cd ts-bindgen-web && <update Cargo.toml> && ./build && wrangler publish"
