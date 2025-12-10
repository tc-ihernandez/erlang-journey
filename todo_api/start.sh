#!/bin/bash

# Simple start script for TODO API
# This script helps run the application if rebar3 is not available

echo "=========================================="
echo "TODO API - Erlang CRUD Application"
echo "=========================================="
echo ""

# Check if Erlang is installed
if ! command -v erl &> /dev/null; then
    echo "Error: Erlang is not installed"
    echo "Please install Erlang/OTP from https://www.erlang.org/downloads"
    exit 1
fi

# Check if rebar3 is installed
if ! command -v rebar3 &> /dev/null; then
    echo "Warning: rebar3 is not installed"
    echo ""
    echo "To install rebar3:"
    echo "  1. Visit: https://rebar3.org/docs/getting-started/"
    echo "  2. Or run: brew install rebar3 (on macOS)"
    echo ""
    echo "For now, you can manually install dependencies:"
    echo "  - Elli: https://github.com/elli-lib/elli"
    echo "  - jsone: https://github.com/sile/jsone"
    echo ""
    exit 1
fi

echo "Starting TODO API..."
echo ""

# Compile and run
rebar3 shell

