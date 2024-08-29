#!/bin/sh

set -u

clash_branch=${1}
commit_status=${2}
username=clash-lang
repo=clash-cores

curl --url "https://api.github.com/repos/${username}/${repo}/statuses/${CI_COMMIT_SHA}" \
     --header "Content-Type: application/json" \
     --header "authorization: Bearer ${GITHUB_STATUS_TOKEN}" \
     --data "{ \"state\": \"${commit_status}\", \"target_url\": \"${CI_PIPELINE_URL}\", \"description\": \"GitLab pipelines\", \"context\": \"GitLab CI with Clash ${clash_branch}\" }"
