#!/bin/bash

# Script to sync .env file to Fly.io secrets
# Usage: ./sync-fly-secrets.sh [--dry-run]

set -e

ENV_FILE=".env"
DRY_RUN=false

# Check for dry-run flag
if [ "$1" == "--dry-run" ]; then
  DRY_RUN=true
  echo "🔍 DRY RUN MODE - No secrets will be set"
  echo ""
fi

# Check if .env file exists
if [ ! -f "$ENV_FILE" ]; then
  echo "❌ Error: $ENV_FILE not found"
  exit 1
fi

# Check if flyctl is installed
if ! command -v flyctl &> /dev/null; then
  echo "❌ Error: flyctl is not installed"
  echo "Install it from: https://fly.io/docs/hands-on/install-flyctl/"
  exit 1
fi

echo "📖 Reading secrets from $ENV_FILE..."
echo ""

# Array to hold secret key-value pairs
declare -a SECRETS=()

# Read .env file line by line
while IFS= read -r line || [ -n "$line" ]; do
  # Skip empty lines and comments
  if [[ -z "$line" || "$line" =~ ^[[:space:]]*# ]]; then
    continue
  fi

  # Extract key and value
  if [[ "$line" =~ ^([A-Za-z_][A-Za-z0-9_]*)=(.*)$ ]]; then
    KEY="${BASH_REMATCH[1]}"
    VALUE="${BASH_REMATCH[2]}"

    # Remove surrounding quotes if present
    VALUE="${VALUE%\"}"
    VALUE="${VALUE#\"}"

    # Skip VITE_ prefixed variables (frontend only)
    if [[ "$KEY" =~ ^VITE_ ]]; then
      echo "⏭️  Skipping frontend variable: $KEY"
      continue
    fi

    # Skip variables already in fly.toml [env] section
    if [[ "$KEY" == "PORT" || "$KEY" == "NODE_ENV" || "$KEY" == "FORMAT_SERVER_URL" || "$KEY" == "FORMAT_SERVER_URL_MAIN" ]]; then
      echo "⏭️  Skipping fly.toml variable: $KEY"
      continue
    fi

    # Add to secrets array
    SECRETS+=("$KEY=$VALUE")
    echo "✅ Found: $KEY"
  fi
done < "$ENV_FILE"

echo ""
echo "📊 Summary: Found ${#SECRETS[@]} secrets to sync"
echo ""

if [ "$DRY_RUN" = true ]; then
  echo "🔍 Secrets that would be set:"
  for secret in "${SECRETS[@]}"; do
    KEY="${secret%%=*}"
    echo "  - $KEY"
  done
  echo ""
  echo "✨ Run without --dry-run to actually set these secrets"
  exit 0
fi

# Confirm before proceeding
read -p "🚀 Set these secrets in Fly.io? (y/N): " -n 1 -r
echo ""
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
  echo "❌ Cancelled"
  exit 0
fi

echo ""
echo "🔄 Setting secrets in Fly.io..."
echo ""

# Set all secrets in a single command for efficiency
if [ ${#SECRETS[@]} -gt 0 ]; then
  # Join all secrets with spaces
  ALL_SECRETS="${SECRETS[*]}"

  # Set all secrets at once
  flyctl secrets set $ALL_SECRETS

  echo ""
  echo "✅ Successfully synced ${#SECRETS[@]} secrets to Fly.io"
else
  echo "⚠️  No secrets to set"
fi

echo ""
echo "🎉 Done!"
